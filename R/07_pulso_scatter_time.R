# File: 06_pulso_scatter.R
# Created: Nov 2021
#         - Mónica Hernandez (mhernande6@eafit.edu.co)
#         - Ana M Pirela (ampirelar@eafit.edu.co)
#         - Juan Carlos Muñoz-Mora (jmunozm1@eafit.edu.co)
# Last Updated: Nov 2023
#               - German Angulo (gangulo1@eafit.edu.co)
#               - Juan Carlos Muñoz (jmunozm1@eafit.edu.co)
#               - Santiago Navas (snavasg@eafit.edu.co)
#               - Laura Quintero (lmquinterv@eafit.edu.co)
# Summary: crea graficos de dispersion por niveles (un nivel, un anio) para
# comparar poblaciones


pulso_scatter_time <- function(id, type_p, year_min, year_max, col_palette){

  # Load necessary packages
  require(glue, quietly = TRUE) # glue package
  require(stringr, quietly = TRUE) # stringr package
  require(stringi, quietly = TRUE) # stringi package
  require(tidyverse, quietly = TRUE) # tidyverse package
  require(ggrepel, quietly = TRUE) # ggrepel package
  require(ggplot2, quietly = TRUE) # ggplot2 package
  require(dplyr, quietly = TRUE) # dplyr package
  require(graphics, quietly = TRUE) # graphics package
  require(tidyr, quietly = TRUE) # tidyr package
  options(scipen = 999) # Set scientific notation off

  # Load the final data base
  ds_pulso <- PulsoSocialColombia::ds_pulso # PulsoSocialColombia package

  # Set font sizes
  caption_text <- 6*1.5
  sub_text <- 6*1.5
  theme_size <- 11
  text_size <- 11*1.5
  text_scatter <- 4

  # Select the variable of interest from the data
  df <- ds_pulso[ds_pulso$var_id == id,]

  # Find national data for the chosen variable
  df_col <- ds_pulso %>%
    dplyr::filter(id_data == unique(df$id_data) & variable == unique(df$variable) &
                    nvl_type == "nacional" & ct_type == unique(df$ct_type))

  # If there is national data and the chosen variable is not national, add the national data to the data frame
  if(nrow(df_col) > 0 & stringr::str_detect(unique(df$nvl_type), "nacional", negate = T)){
    df_col <- df_col %>%
      dplyr::mutate(id_nivel = unique(df$id_nivel), nvl_type = unique(df$nvl_type), nvl_value = 1)
    df <- dplyr::bind_rows(df, df_col)
  }

  # If there is no data for 2020 in GEIH income, set the value to NA
  geih <- ds_pulso %>%
    dplyr::filter(id_data == 1 & stringr::str_detect(variable, "ing")) %>%
    dplyr::select(variable) %>% distinct()

  df$value[df$value == 0 & df$time == 2020 & df$id_data == 1 & df$variable %in% geih$variable] <- NA

  # Check the territorial division
  niveles <- c("etnia", "gen", "edad", "grupo", "tipo","extr", "causa")
  niv <- gsub(".*_", "", unique(df$id_nivel))

  # Numero de años disponibles
  unique_years <- n_distinct(df$time)

  if (unique_years == 1 ) {
    print("No se recomienda esta funcion: Solo hay un año disponible")
  } else {

  if ("2071-2100" %in% df$time) {
    print("No se recomienda esta funcion: Solo hay un año disponible")
  } else {
  # If the data is at a level that is not recommended for this function, print a warning
  if(stringr::str_detect(niv, paste(niveles, collapse = "|"), negate = FALSE)){
    print("No se recomienda esta funcion: Datos estan en niveles")
    }else{



    # If type_p is missing, set it to an empty string
    if(missing(type_p)) {
      type_p=""
    }

    # If year_min is missing, choose the most complete maximum year available
    if(missing(year_min)) {
      # Count years with complete data
      year <- df %>% dplyr::mutate(info = ifelse(!is.na(value), 1, 0)) %>%
        dplyr::group_by(time) %>%
        dplyr::summarise(info = sum(info)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(info)
      # Delete all years with incomplete data
      year <- year[year$info == max(year$info),]
      # Select the max year with complete data
      year_max <- max(year$time)
    }

    # If year_min is missing, find the minimum year
    if(missing(year_min)) {
      year <- df %>% tidyr::drop_na(value)
      # To pick the min year we ignore National data, since it can be more recent than subnational data
      if(unique(year$id_nivel) != "nacional"){
        year <- year %>% dplyr::filter(nvl_label != "Nacional")
      }
      year_min <- min(year$time)
    }

    # If col_palette is missing, use default colors
    if(missing(col_palette)){
      col_palette <- c("#759cd4", "#c43424", "#ecbc24", "#385676", "#d4742d","#6dad55", "#a4a4a4", "#643d94")
    }

    # Filter only years of interest
    df <- df %>% dplyr::filter(time == year_min | time == year_max)

    # Get the level of the data
    level <- unique(df$id_nivel)
    levels <- ifelse(level %in% c("ciudad","dpto","nacional","zona"),0,1)

    # Fix labels
    cod_v <- cod_vars %>% dplyr::ungroup() %>% dplyr::select(-c("id_nivel", "variable"))
    df <- df %>% dplyr::left_join(cod_v, by = c("id_data", "var_id"))

    # Fix inflation in nominal variables
    deflactor <- deflactor %>% dplyr::filter(time %in% df$time)

    df <- df %>%
      dplyr::left_join(deflactor, by = "time") %>%
      dplyr::mutate(value = ifelse(var_unidad == "Pesos", value/deflactor_pib, value))

    # Convert income and production to millions
    df$value[df$var_unidad == "Pesos"] <- df$value/1000000
    df$var_unidad[df$var_unidad == "Pesos"] <- "Millones de pesos (2015=100)"

    # Set up labels for printing
    tl <- ifelse(type_p == "print", unique(df$var_etiqueta),"")
    s_tl <- ifelse(type_p=="print", paste0("Fuente: ", unique(df$db_nombre)),"")
    cap <- ifelse(type_p=="print", paste0("Nota: ", unique(df$var_descripcion)),"")

    print(tl)
    tl <- stringr::str_wrap(tl, width = 30)
    s_tl <- stringr::str_wrap(s_tl,width = 50)
    cap <- stringr::str_wrap(cap,width = 100)
    df$value_label <- stringr::str_wrap(df$var_unidad,width = 30)

    ### -----------------
    # Fix levels
    ### -----------------

    # If levels equals 1, perform the following operations
    if (levels==1) {
      # Using dplyr package for data manipulation
      df <- df %>%
        # Create a new column 't' with the values of 'nvl_value'
        dplyr::mutate(t=nvl_value) %>%
        ### Extract data
        # Create new columns 'nvl_value', 'level', 'level_value' by replacing and subsetting strings
        dplyr::mutate(nvl_value=sub("xxx.*", "",str_replace(t,"_"," xxx ")),
                      level=sub(".*xxx", "",str_replace(id_nivel,"_"," xxx ")),
                      level_value=as.numeric(sub(".*xxx", "",str_replace(t,"_"," xxx ")))) %>%
        ## Cleaning
        # Remove all spaces in 'level' column
        dplyr::mutate(level=str_replace_all(level, " ", ""))

      ### Gender and ethnic minorities
      # If the unique value in 'level' column is 'gen', replace numeric values with corresponding labels
      if(unique(df$level) == "gen") {
        df$level_value <- factor(df$level_value,levels=c(1,2),labels = c("Hombre","Mujer"))
      }
      # If the unique value in 'level' column is 'etnia', replace numeric values with corresponding labels
      if(unique(df$level) == "etnia") {
        df$level_value <- factor(df$level_value,levels=c(1,2),labels = c("Minoría","No minoría"))
      }
      # If the unique value in 'level' column is neither 'etnia' nor 'gen', convert 'level_value' to factor
      if((unique(df$level) != "etnia") & (unique(df$level) != "gen")) {
        df$level_value <- as.factor(df$level_value)
      }
    }

    ### -----------------
    # Fix levels - Territory
    ### -----------------

    # If the unique value in 'id_nivel' column is in the specified list, perform the following operations
    if(unique(df$id_nivel) %in% c("dpto","dpto_etnia","dpto_gen","dpto_sector")) {
      # Convert 'nvl_value' to numeric
      df$nvl_value <- as.numeric(df$nvl_value)
      # Rename 'nivel_value' to 'nvl_value' in 'nom_dpto' dataframe
      nom_dpto <- nom_dpto %>% dplyr::rename(nvl_value = nivel_value)
      # Join 'df' and 'nom_dpto' on 'nvl_value' and replace 'nvl_value' with 'nm'
      df <- df %>%
        dplyr::left_join(nom_dpto, by = c("nvl_value")) %>%
        dplyr::mutate(nvl_value=nm)
    }

    ### -------------------
    # Default themes
    ### -------------------

    # Define some constants
    s <- 1.2
    a_line <- 0.6
    # Define a function to create a custom ggplot theme
    theme_Publication <- function(base_size = theme_size, base_family="helvetica") {
      # Using ggplot2 package for creating the theme
      theme(plot.title = element_text(face = "bold", size = rel(2), hjust = 0.5),
            text=element_text(face = "bold", size = text_size, hjust = 0.5),
            panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5", size = 0.3, linetype = "solid"),
            plot.background = element_blank(),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2,colour = "black"),
            axis.title.x = element_text(vjust = -0.2,colour = "black"),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.minor = element_blank(),
            legend.key = element_blank(),
            legend.position = "none",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = margin(0, 0, 0, 0, unit = "cm"),
            legend.title = element_blank(),
            plot.margin=unit(c(10,5,5,5),"mm"),
            plot.subtitle=element_text(size = sub_text, hjust=0.5, face="italic", color="#8B8B8B"),
            plot.caption=element_text(size = caption_text, hjust=0.5, face="italic", color="#8B8B8B"),
            strip.background=element_rect(colour="#FAECEC",fill="#FAECEC"),
            strip.text = element_text(face="bold"))
    }

    ### --------------
    # levels
    ### --------------

    # Get unique values in 'variable' column after omitting NA values
    vari <- unique(na.omit(df$variable))
    # Replace all punctuation in 'nvl_label' column with space
    df$nvl_label <- str_replace_all(df$nvl_label, "[[:punct:]]", " ")

    ### --------------
    # Progress
    ### --------------

    # Print a message indicating the current progress
    print(glue::glue("Graficando {unique(df$var_etiqueta)} - Análisis dispersion por años"))

    # Using tidyr package to reshape the dataframe from long to wide format
    df_wide <- df %>%
      tidyr::pivot_wider(id_cols = c(nvl_label), names_from = "time", values_from = "value")

    # Standard names for levels
    names_pob <- names(df_wide)
    names_pob <- names_pob[names_pob != "nvl_label"]
    colnames(df_wide)[2] <- "cat1"
    colnames(df_wide)[3] <- "cat2"

    # Labels
    xlabel <- names_pob[1]
    ylabel <- names_pob[2]

    # Both axis must be on the same scale
    max_val <- max(df$value)
    min_val <- min(df$value)

    # Scatter plot
    # Using ggplot2 package for creating the plot
    graph <- ggplot2::ggplot(df_wide, aes(x = cat1, y = cat2, fill = nvl_label, color = nvl_label)) +
      ggplot2::geom_abline(intercept = 0, slope = 1, colour = "grey", size = 1, alpha = 0.7)+
      ggplot2::geom_point(size = 2) +
      ggrepel::geom_text_repel(aes(label = nvl_label), color = "black", size = text_scatter) +
      ggplot2::scale_color_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = F) +
      ggplot2::ylim(min_val, max_val) +
      ggplot2::xlim(min_val,max_val) +
      ggplot2::xlab(glue::glue("\n{xlabel}\n")) +
      ggplot2::ylab(glue::glue("{ylabel}\n")) +
      ggplot2::theme_minimal() +
      theme_Publication() +
      ggplot2::labs(title = tl, caption = cap)

    # Plot
    graphics::plot(graph)
    return(graph)
}
}
}
}
