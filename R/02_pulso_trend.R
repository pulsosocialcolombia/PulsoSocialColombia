# File: 01_pulso_dic.R
# Created: Nov 2021
#         - Mónica Hernandez (mhernande6@eafit.edu.co)
#         - Ana M Pirela (ampirelar@eafit.edu.co)
#         - Juan Carlos Muñoz-Mora (jmunozm1@eafit.edu.co)
# Last Updated: Nov 2023
#               - German Angulo (gangulo1@eafit.edu.co)
#               - Juan Carlos Muñoz (jmunozm1@eafit.edu.co)
#               - Santiago Navas (snavasg@eafit.edu.co)
#               - Laura Quintero (lmquinterv@eafit.edu.co)
# Summary: This script create trend graph

pulso_trend <- function(id,type_p){

  #### Libraries
  require(glue, quietly = TRUE) # glue:: for string interpolation
  require(colorspace, quietly = TRUE) # colorspace:: for color manipulation
  require(stringr, quietly = TRUE) # stringr:: for string manipulation
  require(stringi, quietly = TRUE) # stringi:: for string manipulation
  require(tidyverse, quietly = TRUE) # tidyverse:: for data manipulation
  require(ggplot2, quietly = TRUE)  # This package is a system for declaratively creating graphics, based on The Grammar of Graphics
  require(stats, quietly = TRUE)  # For the calculation of quantiles and the elimination of NA values.
  require(dplyr, quietly = TRUE)  # For data manipulation
  options(scipen = 999) # Set scientific notation off

  #Call final data base
  ds_pulso <- ds_pulso
  #load("data/ds_pulso.rda")# Load data from PulsoSocialColombia package

  # Font sizes
  caption_text <- 6*1.5
  sub_text <- 6*1.5
  theme_size <- 11
  text_size <- 11*1.5

  ## If missing
  if(missing(type_p)) {
    type_p <- "" # If type_p is missing, set it to an empty string
  }

  #### Choose variable
  df <- ds_pulso[ds_pulso$var_id==id,] # Subset the data for the given id

  ### Find national data for chosen variable
  df_col <- ds_pulso %>%
    dplyr::filter(id_data == unique(df$id_data) & variable == unique(df$variable) &
                    nvl_type == "nacional" & ct_type == unique(df$ct_type)) # Filter the data for the national level

  if(nrow(df_col) > 0 & stringr::str_detect(unique(df$nvl_type), "nacional", negate = T)){
    df_col <- df_col %>%
      dplyr::mutate(id_nivel = unique(df$id_nivel), nvl_type = unique(df$nvl_type), nvl_value = 1) # Mutate the data if it's not national level
    df <-dplyr::bind_rows(df, df_col) # Bind the rows of df and df_col
  }

  # COVID data is in months
  #if(stringr::str_detect(unique(df$value_label), "Covid")){
   # df <- df %>%  dplyr::mutate(time_m = gsub(".*_", "", time), time = gsub("_.*", "", time)) # If the data is COVID data, mutate the time variable
  #}

  # We dont have data for 2020 in GEIH income
  geih <- ds_pulso %>%
    dplyr::filter(id_data == 1 & stringr::str_detect(variable, "ing")) %>%
    dplyr::select(variable) %>% dplyr::distinct() # Filter the data for GEIH income

  df$value[df$value == 0 & df$time == 2020 & df$id_data == 1 & df$variable %in% geih$variable] <- NA # Set the value to NA for missing GEIH income data

  # Only keep complete years
  df <- df[stats::complete.cases(df),] # Keep only complete cases

  ### Check number of years
  n_y <- length(df[unique(df$time),]$time) # Check the number of years

  ### Nivel
  level <- unique(df$id_nivel) # Get the unique level
  levels <- ifelse( level %in% c("ciudad","dpto","nacional","zona"),0,1) # Set levels based on the level variable

  if (n_y <= 1) {
    warning("No se puede realizar este gráfico!! - Esta serie solo cuenta con un año de observación") # Warning if there's only one year of data
  } else {

    ### -----------------
    ### Fix labels
    ## ------------------

    cod_v <- cod_vars %>% dplyr::ungroup() %>% dplyr::select(-id_nivel) # Ungroup and select variables from cod_vars
    df <- df %>% dplyr::left_join(cod_v,by=c("var_id")) # Join df and cod_v

    ### Fix inflation in nominal variables
    deflactor <- deflactor %>% dplyr::filter(time %in% df$time) # Filter deflactor for the times in df

    df <- df %>%
      dplyr::left_join(deflactor, by = "time") %>%
      dplyr::mutate(value = ifelse(var_unidad == "Pesos", value/deflactor_pib, value)) # Join df and deflactor and mutate value for inflation

    # Income and production to millions
    df$value[df$var_unidad == "Pesos"] <- df$value/1000000 # Convert income and production to millions
    df$var_unidad[df$var_unidad == "Pesos"] <- "Millones de pesos (2015=100)" # Update the unit

    tl <- ifelse(type_p=="print", unique(df$var_etiqueta),"") # Set title
    s_tl <- ifelse(type_p=="print", paste0("Fuente: ",unique(df$db_nombre)),"") # Set source title
    cap <- ifelse(type_p=="print", paste0("Nota: ",unique(df$var_descripcion)),"") # Set caption

    tl <- stringr::str_wrap(tl, width = 30) # Wrap title
    s_tl <- stringr::str_wrap(s_tl, width = 50) # Wrap source title
    cap <- stringr::str_wrap(cap, width = 100) # Wrap caption
    df$value_label <- stringr::str_wrap(df$var_unidad, width = 30) # Wrap value label

    ### -----------------
    # Fix levels
    ### ---------------
    if (levels==1) {
      #df <- df1
      df <- df %>%
        dplyr::mutate(t=nvl_value) %>% # Mutate t variable
        ### Extract data
        dplyr::mutate(nvl_value=sub("xxx.*", "",stringr::str_replace(t,"_"," xxx ")),
                      level=sub(".*xxx", "",stringr::str_replace(id_nivel,"_"," xxx ")),
                      level_value=as.numeric(sub(".*xxx", "",stringr::str_replace(t,"_"," xxx ")))) %>%
        ## Limpieza
        dplyr::mutate(level=stringr::str_replace_all(level, " ", ""))

      ### Sexo y minorias etnicas
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

    ### -----------------
    # Fix levels - Territory
    ### ---------------

    if(unique(df$id_nivel) %in% c("dpto","dpto_etnia","dpto_gen","dpto_sector")) {
      df$nvl_value <- as.numeric(df$nvl_value)
      nom_dpto <- nom_dpto %>% dplyr::rename(nvl_value = nivel_value)
      df <- df %>%
        dplyr::left_join(nom_dpto,by=c("nvl_value")) %>%
        dplyr::mutate(nvl_value=nm)
    }


    ### -------------------
    # Temas por default
    ### -------------------

    s <- 1.2
    a_line <- 0.6
    theme_Publication <- function(base_size = theme_size, base_family="helvetica") {
      theme(plot.title = element_text(face = "bold", size = rel(2), hjust = 0.5),
            text=element_text(face = "bold", size = text_size, hjust = 0.5),
            panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5", size = 0.3, linetype = "solid"),
            plot.background = element_rect(colour = NA),
            axis.title = element_text(face = "bold", size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2,colour = "black"),
            axis.title.x = element_text(vjust = -0.2,colour = "black"),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = margin(0, 0, 0, 0, unit = "cm"),
            legend.title = element_blank(),
            plot.margin=unit(c(10,5,5,5),"mm"),
            ### Subtitle
            plot.subtitle=element_text(size = sub_text, hjust=0.5, face="italic", color="#8B8B8B"),
            ### Caption
            plot.caption=element_text(size = caption_text, hjust=0.5, face="italic", color="#8B8B8B"),
            strip.background=element_rect(colour="#FAECEC",fill="#FAECEC"),
            strip.text = element_text(face="bold"))

    }


    ### --------------
    # x - Label
    ### -------------

    # Crear etiquetas ejes X y
    df <- df %>% dplyr::select(-id_time.y) %>% dplyr::rename(id_time = id_time.x)
    xlab_time <- ifelse(unique(na.omit(df$id_time)) == 1, "Año",
                        ifelse(unique(na.omit(df$id_time)) == 2, "Mes",
                               ifelse(unique(na.omit(df$time)) == 3, "Día", NULL)))

    # Cortes de anios (etiquetas eje X)
    df$time <- as.numeric(as.character(df$time))
    if (n_y >= 10) {
      years_break <- c(seq(min(df$time),max(df$time), 3), max(df$time))
    } else {
      years_break <- seq(min(df$time), max(df$time))
    }
    if(max(years_break) == 2020 & 2019 %in% years_break & n_y >= 10){
      years_break <- years_break[years_break != 2019]
    }

    # Cortes de meses (only for Covid data)
    if(stringr::str_detect(unique(df$var_etiqueta), "Covid")){
      df <- df %>%
        dplyr::mutate(time_old = time,
                      time_m = ifelse(nchar(time_m) == 1, glue::glue("0{time_m}"), time_m),
                      time = glue::glue("{time}-{time_m}"))
    }

    ### --------------
    # y - Label
    ### -------------

    ylab <- unique(na.omit(df$value_label))

    # Etiquetas de grafica de promedios temporales (anios, meses, dias)
    options_time <- list(
      xlab(glue::glue("\n{xlab_time}\n")),
      ylab(glue::glue("{ylab}\n")))

    ### --------------
    # levels
    ### --------------

    # Selecting and renaming variables in the dataframe
    df <- df %>% dplyr::select(-variable.y) %>% dplyr::rename(variable = variable.x) # dplyr::select, dplyr::rename
    # Getting unique values of the variable column, omitting NA values
    vari <- unique(na.omit(df$variable)) # base::unique, stats::na.omit
    # Cleaning nvl_label column if it's not null
    if(!is.null(df$nvl_label)) {
      df$nvl_label <- stringr::str_replace_all(df$nvl_label, "[[:punct:]]", " ") # stringr::str_replace_all
      df$nvl_label <- stringr::str_replace_all(df$nvl_label, "col ", "") # stringr::str_replace_all
    }

    # Checking if xlab_time contains "Mes"
    monthly <- stringr::str_detect(xlab_time, "Mes") # stringr::str_detect

    ### --------------
    # Progress
    ### --------------

    # Printing progress message
    print(glue::glue("Graficando {ylab} - Análisis tendencias")) # glue::glue

    ### --------------
    # Tipo gráficas - line
    ### --------------

    ## No desagregaciones y sin datos mensuales
    if(levels==0 & monthly == FALSE) {

      # Creating a line graph with ggplot2
      graph <- ggplot2::ggplot(data = df[!is.na(df$value),], # ggplot2::ggplot
                               aes(x = time, y = value, group = nvl_label)) +
        ggplot2::geom_line(aes(color=nvl_label), size = s, alpha = a_line,na.rm = FALSE) + # ggplot2::geom_line
        colorspace::scale_color_discrete_qualitative("Dark 2") +
        options_time + theme_Publication()+
        ggplot2::labs(title = tl,  caption=cap) + # ggplot2::labs
        ggplot2::scale_x_continuous(breaks = years_break) # ggplot2::scale_x_continuous

    }

    # Con agregaciones y sin datos mensuales
    if (levels==1 & monthly == FALSE) {

      # Creating a line graph with ggplot2 with facet_wrap
      graph <- ggplot2::ggplot(data = df[!is.na(df$value),], # ggplot2::ggplot
                               aes(x = time, y = value,group=nvl_label)) +
        ggplot2::geom_line(aes(color=nvl_label), size = s, alpha = a_line,na.rm = FALSE) + # ggplot2::geom_line
        colorspace::scale_color_discrete_qualitative("Dark 2") +
        options_time + theme_Publication() +
        ggplot2::facet_wrap(~ ct_label)+ # ggplot2::facet_wrap
        ggplot2::labs(title = tl, caption=cap) + # ggplot2::labs
        ggplot2::scale_x_continuous(breaks = years_break) # ggplot2::scale_x_continuous
    }

    # Sin agregaciones y con datos mensuales
    if(levels==0 & monthly == TRUE) {

      # Converting time to date format
      df <- df %>% dplyr::mutate(time_date = as.Date(paste(time,"-01",sep=""))) # dplyr::mutate, base::as.Date, base::paste

      # Removing months with less than half of the departments
      months <- df %>% dplyr::mutate(info = ifelse(!is.na(value), 1, 0)) %>% # dplyr::mutate, stats::ifelse
        dplyr::group_by(time_m, time) %>% # dplyr::group_by
        dplyr::summarise(info = sum(info)) %>% # dplyr::summarise
        dplyr::ungroup() %>% # dplyr::ungroup
        dplyr::arrange(info) %>% # dplyr::arrange
        dplyr::filter(!(info < max(info)/2)) # dplyr::filter, base::max

      df <- df %>% dplyr::filter(time %in% months$time) # dplyr::filter

      # Creating a line graph with ggplot2 with scale_x_date
      graph <- ggplot2::ggplot(data = df[!is.na(df$value),], # ggplot2::ggplot
                               aes(x = time_date, y = value, group = nvl_label)) +
        ggplot2::geom_line(aes(color=nvl_label), size = s, alpha = a_line,na.rm = FALSE) + # ggplot2::geom_line
        colorspace::scale_color_discrete_qualitative("Dark 2") +
        options_time + theme_Publication()+
        ggplot2::labs(title = tl,  caption=cap) + # ggplot2::labs
        ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%Y-%d-%m") # ggplot2::scale_x_date

    }

    # Con agregaciones y con datos mensuales
    if (levels==1 & monthly == TRUE) {

      # Converting time to date format
      df <- df %>% dplyr::mutate(time_date = as.Date(paste(time,"-01",sep=""))) # dplyr::mutate, base::as.Date, base::paste

      # Removing months with less than half of the departments
      months <- df %>% dplyr::mutate(info = ifelse(!is.na(value), 1, 0)) %>% # dplyr::mutate, stats::ifelse
        dplyr::group_by(time_m, time) %>% # dplyr::group_by
        dplyr::summarise(info = sum(info)) %>% # dplyr::summarise
        dplyr::ungroup() %>% # dplyr::ungroup
        dplyr::arrange(info) %>% # dplyr::arrange
        dplyr::filter(!(info < max(info)/2)) # dplyr::filter, base::max

      df <- df %>% dplyr::filter(time %in% months$time) # dplyr::filter

      # Creating a line graph with ggplot2 with facet_wrap
      graph <- ggplot(data = df[!is.na(df$value),], # ggplot2::ggplot
                      aes(x = time_date, y = value,group=nvl_label)) +
        ggplot2::geom_line(aes(color=nvl_label), size = s, alpha = a_line,na.rm = FALSE) + # ggplot2::geom_line
        colorspace::scale_color_discrete_qualitative("Dark 2") +
        options_time + theme_Publication() +
        ggplot2::facet_wrap(~ ct_label)+ # ggplot2::facet_wrap
        ggplot2::labs(title = tl, caption=cap) # ggplot2::labs
    }

    # Returning the graph
    return(graph) # base::return

  }


}
