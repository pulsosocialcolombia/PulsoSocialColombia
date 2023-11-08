# File: 04_pulso_map.R
# Created: Nov 2021 
#         - Mónica Hernandez (mhernande6@eafit.edu.co)
#         - Ana M Pirela
#         - Juan Carlos Muñoz-Mora (jmunozm1@eafit.edu.co)
# Last Updated: Nov 2023
#               - German Tabares (gangulo1@eafit.edu.co)
#               - Juan Carlos Muñoz (jmunozm1@eafit.edu.co)
#               - Santiago Navas (snavasg@eafit.edu.co)
#               - Laura Quintero (lmquinterv@eafit.edu.co)
# Summary: This function generates a comparative map graph between the maximum and minimum years for a specific variable in the data of Pulso Social.


pulso_map <- function(id, type_p, col_palette){

  # Load necessary packages
  require(sf, quietly = TRUE)
  require(glue, quietly = TRUE)
  require(colorspace, quietly = TRUE)
  require(stringr, quietly = TRUE)
  require(stringi, quietly = TRUE)
  require(tidyverse, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(glue, quietly = TRUE)

  options(scipen = 999)

  # Font sizes
  caption_text <- 6*1.5
  sub_text <- 6*1.5
  theme_size <- 11
  text_size <- 11*1.5
  map_text <- 1.5

  #### Choose variable
  # Call final data base
  ds_pulso <- PulsoSocialColombia::ds_pulso

  df <- ds_pulso[ds_pulso$var_id == id,]

  # We don't have data for 2020 in GEIH income
  geih <- ds_pulso %>%
    dplyr::filter(id_data == 1 & stringr::str_detect(variable, "ing")) %>%
    dplyr::select(variable) %>% dplyr::distinct()

  df$value[df$value == 0 & df$time == 2020 & df$id_data == 1 & df$variable %in% geih$variable] <- NA

  l <- unique(df$id_nivel)

  # Check territorial division
  if(stringr::str_detect("dpto", unique(df$id_nivel), negate = TRUE)){
    print(glue::glue("This function is not recommended: Data is not at departmental level - the level is: {l}"))
  } else {

    ## If missing
    if(missing(type_p)) {
      type_p=""
    }

    ## If palette is missing, use default colors
    if(missing(col_palette)){
      col_palette <- c("#FAECEC", "#D7C6CC", "#B0A3AF", "#878293", "#5B6477", "#2F4858")
    }

    # Identify first and last year in data
    min_year <- min(df$time)
    max_year <- max(df$time)

    ### Filter only first and last years
    df <- df %>% dplyr::filter(time == min_year | time == max_year)

    #### Check the elegibles
    level <- unique(df$id_nivel)
    no_ele <- ifelse(level %in% c("nacional","region","zona","area_metropolitana", "ciudad"), 1, 0)

    ### Level (minorities, gender, economic sector)
    no_level <- ifelse(level %in% c("nacional","region","zona","area_metropolitana", "dpto", "ciudad"), 1, 0)
    levels <- ifelse(level %in% c("dpto_etnia","dpto_gen","dpto_sector","nacional_etnia", "ciudad_etnia"), 1, 0)


    ### -----------------
    ### Fix labels
    ## ------------------

    cod_vars <- cod_vars %>% dplyr::ungroup() %>% dplyr::select(-c( "var_id"))
    df <- df %>% dplyr::left_join(cod_vars, by = c("id_data", "variable", "id_nivel"))

    ### Fix inflation in nominal variables
    deflactor <- deflactor %>% dplyr::filter(time %in% df$time)

    df <- df %>%
      dplyr::left_join(deflactor, by = "time") %>%
      dplyr::mutate(value = ifelse(var_unidad == "Pesos", value/deflactor_pib, value))

    # Convert income and production values to millions
    df$value[df$var_unidad == "Pesos"] <- df$value/1000000
    df$var_unidad[df$var_unidad == "Pesos"] <- "Millions of pesos (2015=100)"

    # Set the title, subtitle, and caption based on the 'type_p' variable
    tl <- ifelse(type_p=="print", unique(df$var_etiqueta),"")
    s_tl <- ifelse(type_p=="print", paste0("Source: ",unique(df$db_nombre)),"")
    cap <- ifelse(type_p=="print", paste0("Note: ",unique(df$var_descripcion)),"")

    # Print the title
    print(tl)

    # Wrap the text of the title, subtitle, and caption to fit within a certain width
    tl <- stringr::str_wrap(tl, width = 30)
    s_tl <- stringr::str_wrap(s_tl,width = 50)
    cap <- stringr::str_wrap(cap,width = 100)

    # Wrap the text of the 'value_label' variable to fit within a certain width
    df$value_label <- stringr::str_wrap(df$var_unidad,width = 30)

    ### -----------------
    # Fix levels
    ### ---------------

    # Check if data has any gender/minority/other level and fix labels
    if (levels==1) {
      df <- df %>% dplyr::mutate(t=nvl_value) %>%
        ### Extract data
        dplyr::mutate(nvl_value=sub("xxx.*", "",stringr::str_replace(t,"_"," xxx ")),
                      level=sub(".*xxx", "",stringr::str_replace(id_nivel,"_"," xxx ")),
                      level_value=as.numeric(sub(".*xxx", "",stringr::str_replace(t,"_"," xxx ")))) %>%
        ## Cleanup
        dplyr::mutate(level=stringr::str_replace_all(level, " ", ""),
                      nvl_value=as.numeric(stringr::str_replace_all(nvl_value, " ", "")))

      ### Sex and ethnic minorities
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

    # Join data with territorial shapefile
    if(unique(df$id_nivel) %in% c("dpto","dpto_etnia","dpto_gen","dpto_sector")) {

      # Convert 'nvl_value' to numeric
      df$nvl_value <- as.numeric(df$nvl_value)

      # Rename 'nivel_value' to 'nvl_value' in 'map_dptos'
      map_dptos <- map_dptos %>% dplyr::rename(nvl_value = nivel_value)

      # Join 'df' and 'map_dptos' on 'nvl_value', keeping only rows where 'time' is the minimum or maximum year
      df <- map_dptos %>%
        dplyr::left_join(df %>% subset(time == min_year | time == max_year), by = "nvl_value") %>%
        dplyr::select(-c(id_nivel.y)) %>%
        dplyr::rename(id_nivel = id_nivel.x)
    }
    ### -------------------
    # Default themes
    ### -------------------

    s <- 1.2
    a_line <- 0.6
    theme_Publication <- function(base_size = theme_size, base_family="helvetica") {
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = rel(2), hjust = 0.5),
                     text=ggplot2::element_text(face = "bold", size = text_size, hjust = 0.5),
                     panel.background = ggplot2::element_blank(),
                     plot.background = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.title = ggplot2::element_text(face = "bold", size = rel(1)),
                     axis.title.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(vjust = -0.2,colour = "black"),
                     axis.text = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     # panel.grid.major = element_line(colour="#f0f0f0"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     legend.key = ggplot2::element_blank(),
                     legend.position = "bottom",
                     legend.direction = "horizontal",
                     legend.box = "horizontal",
                     legend.key.size= ggplot2::unit(0.2, "cm"),
                     legend.margin = ggplot2::margin(0, 0, 0, 0, unit = "cm"),
                     legend.title = ggplot2::element_blank(),
                     plot.margin=ggplot2::unit(c(10,5,5,5),"mm"),
                     ### Subtitle
                     plot.subtitle=ggplot2::element_text(size = sub_text, hjust=0.5, face="italic", color="#8B8B8B"),
                     ### Caption
                     plot.caption=ggplot2::element_text(size = caption_text, hjust=0.5, face="italic", color="#8B8B8B"),
                     strip.background=ggplot2::element_rect(colour="#FAECEC",fill="#FAECEC"),
                     strip.text = ggplot2::element_text(face="bold"))
    }

    ### --------------
    # levels
    ### -------------

    # Levels
    vari <- unique(stats::na.omit(df$variable))
    df$nvl_label <- stringr::str_replace_all(df$nvl_label, vari, " ")
    df$nvl_label <- stringr::str_replace_all(df$nvl_label, "[[:punct:]]", " ")

    # Progress
    print(glue::glue("Plotting {unique(df$var_etiqueta)[1]} - Maps"))

    # Percentiles
    # Split data into percentiles
    classes <- 5
    q1 <- stats::quantile(df$value, na.rm = TRUE, probs = seq(0, 1, length.out = classes + 1))
    df$q_value <- cut(df$value, breaks = q1, include.lowest = TRUE, dig.lab = 2)
    table(df$q_value)

    # Keep labels only for polygons with data
    df$nvl_label[is.na(df$value)] <- NA

    # Map
    # Plot polygons with no data
    poly_na <- df %>% dplyr::filter(is.na(value))

    if(nrow(poly_na) != 0){
      poly_na <- poly_na %>% dplyr::distinct(nvl_value, .keep_all = TRUE)
      poly_min <- poly_na
      poly_min$time <- min_year
      poly_max <- poly_na
      poly_max$time <- max_year
      poly_na <- dplyr::bind_rows(poly_min, poly_max)
      data <- df %>% tidyr::drop_na(value) %>% dplyr::bind_rows(., poly_na)
    } else {
      data <- df
    }

    # Drop San Andres
    data <- data[data$nvl_value != 88,]

    # map_cont1 <- c("#C5DCFF", "#A1B8DC", "#7E96B8", "#5D7495", "#3D5574")
    # map_cont2 <- c("#74AC54", "#3AA367", "#00977B", "#00898B", "#007A94", "#006993")
    # map_cont3 <- c("#FAECEC", "#D7C6CC", "#B0A3AF", "#878293", "#5B6477", "#2F4858")
    # Map: first and last year

    graph <- ggplot2::ggplot(data = data) +
      ggplot2::geom_sf(aes(fill = q_value), color = "#2b2b2b", size = 0.1, alpha = 0.7) +
      ggplot2::geom_text(aes(X, Y, label = nvl_label), vjust = 1.5, color = "black",
                         position = ggplot2::position_dodge(0.9), size = map_text) +
      ggplot2::scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = FALSE) +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 6)) +
      ggplot2::xlab("") +
      ggplot2::labs(title = tl, caption = cap) +
      theme_Publication() +
      ggplot2::facet_grid(~time)

    # Plot
    graphics::plot(graph)
    return(graph)

  }

}
