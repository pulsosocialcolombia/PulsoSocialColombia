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
# Summary: creates scatter plots by levels (one level, one year) to compare populations

# Define the function 'pulso_scatter'
pulso_scatter <- function(id, type_p, year, col_palette){

  require(glue, quietly = TRUE)
  require(colorspace, quietly = TRUE)
  require(stringr, quietly = TRUE)
  require(stringi, quietly = TRUE)
  require(tidyverse, quietly = TRUE)
  require(ggrepel, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  options(scipen = 999)
  # Load the final data base
  ds_pulso <- ds_pulso
  #load("data/ds_pulso.rda")# Load data from PulsoSocialColombia package


  # Set font sizes
  caption_text <- 6*1.5
  sub_text <- 6*1.5
  theme_size <- 11
  text_size <- 11*1.5
  text_scatter <- 4

  #### Choose variable
  df <- ds_pulso[ds_pulso$var_id == id,]

  # Find national data for the selected variable
  df_col <- ds_pulso %>%
    dplyr::filter(id_data == unique(df$id_data) & variable == unique(df$variable) &
                    nvl_type == "nacional" & ct_type == unique(df$ct_type))

  # If there is national data for the selected variable and the level type is not national, add the national data to the data frame
  if(nrow(df_col) > 0 & stringr::str_detect(unique(df$nvl_type), "nacional", negate = T)){
    df_col <- df_col %>%
      dplyr::mutate(id_nivel = unique(df$id_nivel), nvl_type = unique(df$nvl_type), nvl_value = 1)
    df <- dplyr::bind_rows(df, df_col)
  }

  # We don't have data for 2020 in GEIH income
  geih <- ds_pulso %>%
    dplyr::filter(id_data == 1 & stringr::str_detect(variable, "ing")) %>%
    dplyr::select(variable) %>% distinct()

  df$value[df$value == 0 & df$time == 2020 & df$id_data == 1 & df$variable %in% geih$variable] <- NA

  # Check territorial division
  niveles <- c("etnia", "gen", "edad", "grupo", "tipo")
  niv <- stringr::str_replace_all(unique(df$id_nivel), ".*_", "")

  # If the level is not one of the specified levels, print a warning message
  if(stringr::str_detect(niv, paste(niveles, collapse = "|"), negate = TRUE)){
    print("Esta función no es recomendada: los datos no estan a niveles")
  } else {
    # If 'type_p' is missing, set it to an empty string
    if(missing(type_p)) {
      type_p=""
    }

    # If 'year' is missing, set it to the maximum year with non-missing values
    if(missing(year)) {
      year <- df %>% tidyr::drop_na(value)
      year <- max(year$time)
    }

    # If 'col_palette' is missing, use default colors
    if(missing(col_palette)){
      col_palette <- c("#759cd4", "#c43424", "#ecbc24", "#385676", "#d4742d","#6dad55", "#a4a4a4", "#643d94")
    }


  # Filter data for the specified year
  df <- df[df$time==year,]

  # Determine level
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
  tl <- stringr::str_wrap(paste0(tl," - ",year),width = 30)
  s_tl <- stringr::str_wrap(s_tl,width = 50)
  cap <- stringr::str_wrap(cap,width = 100)
  df$value_label <- stringr::str_wrap(df$var_unidad,width = 30)

  # Fix levels
  if (levels==1) {
    df <- df %>%
      dplyr::mutate(t=nvl_value) %>%
      # Extract data
      dplyr::mutate(nvl_value=stringr::str_replace(t,"_"," xxx ") %>% stringr::str_remove("xxx.*"),
                    level=stringr::str_replace(id_nivel,"_"," xxx ") %>% stringr::str_remove(".*xxx"),
                    level_value=stringr::str_replace(t,"_"," xxx ") %>% stringr::str_remove(".*xxx") %>% as.numeric()) %>%
      # Clean up
      dplyr::mutate(level=stringr::str_replace_all(level, " ", ""))

    # Sex and ethnic minorities
    if(unique(df$level) == "gen") {
      df$level_value <- factor(df$level_value,levels=c(1,2),labels = c("Hombre","Mujer"))
    }
    if(unique(df$level) == "etnia") {
      df$level_value <- factor(df$level_value,levels=c(1,2),labels = c("Minoría","No minoría"))
    }
    if((unique(df$level) != "etnia") & (unique(df$level) != "gen")) {
      df$level_value <- as.factor(df$level_value)
    }
  }

  # Fix levels - Territory
  if(unique(df$id_nivel) %in% c("dpto","dpto_etnia","dpto_gen","dpto_sector","nacional_col_extr")) {
    df$nvl_value <- as.numeric(df$nvl_value)
    nom_dpto <- nom_dpto %>% dplyr::rename(nvl_value = nivel_value)
    df <- df %>%
      dplyr::left_join(nom_dpto, by = c("nvl_value")) %>%
      dplyr::mutate(nvl_value=nm)
  }

  # Default themes
  s <- 1.2
  a_line <- 0.6
  theme_Publication <- function(base_size = theme_size, base_family="helvetica") {
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = rel(2), hjust = 0.5),
                   text=ggplot2::element_text(face = "bold", size = text_size, hjust = 0.5),
                   panel.background = ggplot2::element_rect(fill = "#F5F5F5", colour = "#F5F5F5", size = 0.3, linetype = "solid"),
                   plot.background = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(face = "bold",size = rel(1)),
                   axis.title.y = ggplot2::element_text(angle=90,vjust =2,colour = "black"),
                   axis.title.x = ggplot2::element_text(vjust = -0.2,colour = "black"),
                   axis.text = ggplot2::element_text(),
                   axis.line = ggplot2::element_line(colour="black"),
                   axis.ticks = ggplot2::element_line(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(),
                   legend.position = "none",
                   legend.direction = "horizontal",
                   legend.key.size= grid::unit(0.2, "cm"),
                   legend.margin = margin(0, 0, 0, 0, unit = "cm"),
                   legend.title = ggplot2::element_blank(),
                   plot.margin=grid::unit(c(10,5,5,5),"mm"),
                   plot.subtitle=ggplot2::element_text(size = sub_text, hjust=0.5, face="italic", color="#8B8B8B"),
                   plot.caption=ggplot2::element_text(size = caption_text, hjust=0.5, face="italic", color="#8B8B8B"),
                   strip.background=ggplot2::element_rect(colour="#FAECEC",fill="#FAECEC"),
                   strip.text = ggplot2::element_text(face="bold"))
  }

  ### --------------
  # x - Label
  ### --------------

    # Crear etiquetas ejes X y Y
    df <- df %>% dplyr::select(-id_time.y) %>% dplyr::rename(id_time = id_time.x)
    xlab_time <- ifelse(unique(na.omit(df$id_time)) == 1, "Año",
                        ifelse(unique(na.omit(df$id_time) == 2, "Mes",
                                      ifelse(unique(na.omit(df$time)) == 3, "Día", NULL))))

    ### --------------
    # y - Label
    ### --------------

    ylab <- unique(na.omit(df$value_label))

    # Etiquetas de grafica de promedios temporales (anios, meses, dias)
    options_time <- list(
      xlab(glue::glue("\n{xlab_time}")),
      ylab(glue::glue("{ylab}\n")))

    ### --------------
    # levels
    ### --------------

    # Get unique values of the variable column
    vari <- unique(na.omit(df$variable))

    # Replace all punctuation in the nvl_label column with a space
    df$nvl_label <- stringr::str_replace_all(df$nvl_label, "[[:punct:]]", " ")

    ### --------------
    # Progress
    ### --------------

    # Print progress
    print(glue::glue("Plotting {unique(df$var_etiqueta)} - Dispersion analysis by levels"))

    # Reshape data from long to wide format
    df_wide <- df %>%
      tidyr::pivot_wider(id_cols = c(nvl_label), names_from = "ct_label", values_from = "value")

    # Standard names for levels
    names_pob <- names(df_wide)
    names_pob <- names_pob[names_pob != "nvl_label"]
    colnames(df_wide)[2] <- "cat1"
    colnames(df_wide)[3] <- "cat2"

    # Labels
    xlabel <- names_pob[1]
    ylabel <- names_pob[2]

    # Both axis must be on the same scale
    # max_val <- mean(max(df_wide$cat1), max(df_wide$cat2))
    # min_val <- mean(min(df_wide$cat1), min(df_wide$cat2))
    max_val <- max(df$value)
    min_val <- min(df$value)

    # Scatter plot
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

    # Return the graph object
    return(graph)

}

}

