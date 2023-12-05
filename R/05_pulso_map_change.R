# File: 05_pulso_map_change.R
# Created: Nov 2021
#         - Mónica Hernandez (mhernande6@eafit.edu.co)
#         - Ana M Pirela (ampirelar@eafit.edu.co)
#         - Juan Carlos Muñoz-Mora (jmunozm1@eafit.edu.co)
# Last Updated: Nov 2023
#               - German Angulo (gangulo1@eafit.edu.co)
#               - Juan Carlos Muñoz (jmunozm1@eafit.edu.co)
#               - Santiago Navas (snavasg@eafit.edu.co)
#               - Laura Quintero (lmquinterv@eafit.edu.co)
# Summary: crea mapas con el cambio porcentual de la variable entre el primer
# y ultimo años disponible


pulso_map_change <- function(id, type_p, num_percentil = 5, col_palette){

  # Load necessary packages
  require(sf, quietly = TRUE)  # sf package for spatial data
  require(glue, quietly = TRUE)  # glue package for string interpolation
  require(colorspace, quietly = TRUE)  # colorspace package for color palettes
  require(stringr, quietly = TRUE)  # stringr package for string manipulation
  require(stringi, quietly = TRUE)  # stringi package for string manipulation
  require(tidyverse, quietly = TRUE)  # tidyverse package for data manipulation and visualization
  require(ggplot2, quietly = TRUE)  # This package is a system for declaratively creating graphics, based on The Grammar of Graphics
  require(grid, quietly = TRUE)  # For manipulating units in graphics.
  require(stats, quietly = TRUE)  # For the calculation of quantiles and the elimination of NA values.
  require(dplyr, quietly = TRUE)  # For data manipulation
  require(graphics, quietly = TRUE)  # For creating graphics
  require(tidyr, quietly = TRUE)  # For data manipulation
  options(scipen = 999)  # set scientific notation penalty to 999

  # Load the final data base from PulsoSocialColombia package
  ds_pulso <- ds_pulso
  #load("data/ds_pulso.rda")# Load data from PulsoSocialColombia package

  # Set font sizes for different elements
  caption_text <- 6*1.5
  sub_text <- 6*1.5
  theme_size <- 11
  text_size <- 11*1.5
  map_text <- 1.5

  # Select rows from ds_pulso where var_id equals the input id
  df <- ds_pulso[ds_pulso$var_id == id,]

  # Filter out rows where id_data equals 1 and variable contains "ing"
  geih <- ds_pulso %>%
    dplyr::filter(id_data == 1 & stringr::str_detect(variable, "ing")) %>%
    dplyr::select(variable) %>% distinct()

  # Set value to NA for rows where value equals 0, time equals 2020, id_data equals 1, and variable is in geih$variable
  df$value[df$value == 0 & df$time == 2020 & df$id_data == 1 & df$variable %in% geih$variable] <- NA


  # Check Percentile
  if (missing(num_percentil)) {
    num_percentil <- 5  # Establecer el valor predeterminado si no se proporciona
  }
  if (num_percentil<1 || num_percentil>5) {
    print(glue::glue("El número de percentiles debe estar entre 1 y 5"))
  } else {

  # Check if the data is at the departmental level
  if(stringr::str_detect("dpto", unique(df$id_nivel), negate = T)){
    print("No se recomienda esta funcion: Datos no estan a nivel departamental")
    } else {
if(length(unique(df$time))==1){
  print("Solo Hay un Año, por lo tanto es imposible comparar")
} else {
    # If type_p is missing, set it to an empty string
    if(missing(type_p)) {
      type_p=""
    }
   # If col_palette is missing, use default colors
    if(missing(col_palette)){
      col_palette <- c("#FAECEC", "#D7C6CC", "#B0A3AF", "#878293", "#5B6477", "#2F4858")
    }

    # Identify the first and last year in the data
    min_year <- min(df$time)
    max_year <- max(df$time)

    # Filter only the first and last years and calculate the percentage change
    df <- df %>%
      dplyr::filter(time == min_year | time == max_year) %>%
      dplyr::arrange(nvl_value, time) %>%
      dplyr::group_by(nvl_value) %>%
      dplyr::mutate(value_old = value, value = 100*((value - dplyr::lag(value))/(dplyr::lag(value)))) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na(value)

    # Check the levels
    level <- unique(df$id_nivel)
    no_ele <- ifelse(level %in% c("nacional","region","zona","area_metropolitana", "ciudad"), 1, 0)

    # Check the levels (minorities, gender, economic sector)
    no_level <- ifelse(level %in% c("nacional","region","zona","area_metropolitana", "dpto", "ciudad"), 1, 0)
    levels <- ifelse(level %in% c("dpto_etnia","dpto_gen","dpto_sector","nacional_etnia", "ciudad_etnia"), 1, 0)

    # Fix labels
    cod_vars <- cod_vars %>% dplyr::ungroup() %>% dplyr::select(-c( "var_id"))
    df <- df %>% dplyr::left_join(cod_vars, by = c("id_data", "variable", "id_nivel")) %>%
      dplyr::distinct()

    # Fix inflation in nominal variables
    deflactor <- deflactor %>% dplyr::filter(time %in% df$time)

    df <- df %>%
      dplyr::left_join(deflactor, by = "time") %>%
      dplyr::mutate(value = ifelse(var_unidad == "Pesos", value/deflactor_pib, value))

    # Convert income and production to millions
    df$value[df$var_unidad == "Pesos"] <- df$value/1000000
    df$var_unidad[df$var_unidad == "Pesos"] <- "Millones de pesos (2015=100)"

    tl <- ifelse(type_p=="print", unique(df$var_etiqueta),"")
    s_tl <- ifelse(type_p=="print", paste0("Fuente: ",unique(df$db_nombre)),"")
    cap <- ifelse(type_p=="print", paste0("Nota: ",unique(df$var_descripcion), ". ",
                                          "Cambio porcentual entre ", min_year, "-", max_year), "")

    print(tl)
    tl <- stringr::str_wrap(tl, width = 30)
    s_tl <- stringr::str_wrap(s_tl,width = 50)
    cap <- stringr::str_wrap(cap,width = 100)
    df$value_label <- stringr::str_wrap(df$var_unidad,width = 30)

    # Fix levels
    # Check if data has any gender/minority/other level and fix labels
    if (levels==1) {
      df <- df %>% dplyr::mutate(t=nvl_value) %>%
        # Extract data
        dplyr::mutate(nvl_value=sub("xxx.*", "",stringr::str_replace(t,"_"," xxx ")),
                      level=sub(".*xxx", "",stringr::str_replace(id_nivel,"_"," xxx ")),
                      level_value=as.numeric(sub(".*xxx", "",stringr::str_replace(t,"_"," xxx ")))) %>%
        # Clean up
        dplyr::mutate(level=stringr::str_replace_all(level, " ", ""),
                      nvl_value=as.numeric(stringr::str_replace_all(nvl_value, " ", "")))

      # Gender and ethnic minorities
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
      df$nvl_value <- base::as.numeric(df$nvl_value)

      # Rename 'nivel_value' to 'nvl_value' in 'map_dptos'
      map_dptos <- map_dptos %>% dplyr::rename(nvl_value = nivel_value)

      # Join 'df' and 'map_dptos' on 'nvl_value', select necessary columns, and rename 'id_nivel'
      df <- map_dptos %>%
        dplyr::left_join(df %>% base::subset(time == min_year | time == max_year), by = "nvl_value") %>%
        dplyr::select(-c(id_nivel.y)) %>%
        dplyr::rename(id_nivel = id_nivel.x)
    }

    ### -------------------
    # Default themes
    ### -------------------

    s <- 1.2
    a_line <- 0.6

    # Define a custom theme for ggplot2
    theme_Publication <- function(base_size = theme_size, base_family="helvetica") {
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = rel(2), hjust = 0.5),
                     text=ggplot2::element_text(face = "bold", size = text_size, hjust = 0.5),
                     panel.background = ggplot2::element_blank(),
                     plot.background = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.title = ggplot2::element_text(face = "bold", size = rel(1)),
                     axis.title.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(vjust = -0.2, colour = "black"),
                     axis.text = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     legend.key = ggplot2::element_blank(),
                     legend.position = "bottom",
                     legend.direction = "horizontal",
                     legend.box = "horizontal",
                     legend.key.size= grid::unit(0.2, "cm"),
                     legend.margin = ggplot2::margin(0, 0, 0, 0, unit = "cm"),
                     legend.title = ggplot2::element_blank(),
                     plot.margin=grid::unit(c(10,5,5,5),"mm"),
                     plot.subtitle=ggplot2::element_text(size = sub_text, hjust=0.5, face="italic", color="#8B8B8B"),
                     plot.caption=ggplot2::element_text(size = caption_text, hjust=0.5, face="italic", color="#8B8B8B"),
                     strip.background=ggplot2::element_rect(colour="#FAECEC",fill="#FAECEC"),
                     strip.text = ggplot2::element_text(face="bold"))
    }

    ### --------------
    # levels
    ### -------------

    # Get unique values of 'variable' column in 'df' after omitting NA values
    vari <- unique(stats::na.omit(df$variable))

    # Replace 'vari' and punctuation in 'nvl_label' with space
    df$nvl_label <- stringr::str_replace_all(df$nvl_label,vari, " ")
    df$nvl_label <- stringr::str_replace_all(df$nvl_label, "[[:punct:]]", " ")

    ### --------------
    # Progress
    ### -------------

    # Print progress
    print(glue::glue("Graficando {unique(df$var_etiqueta)[1]} - Mapas cambio %"))

    ### --------------
    # Percentiles
    ### -------------

    # Split data in percentiles
    #classes <- 5
    q1 <- stats::quantile(df$value, na.rm=T, probs = seq(0, 1, length.out = num_percentil + 1))
    # Check if quantiles are unique
    if(length(q1) != length(unique(q1))) {
      print("No hay suficiente variacion en los datos, por favor disminuya el numero de percentiles")
    } else {

    df$q_value <- base::cut(df$value, breaks = q1, include.lowest = T, dig.lab = 3)
    base::table(df$q_value)

    # Keep labels only for polygons with data
    df$nvl_label[is.na(df$value)] <- "NA"

    ### --------------
    # Map
    ### -------------

    # Drop San Andres
    df <- df[df$nvl_value != 88,]

    # Map: first and last year
    graph <- ggplot2::ggplot(data = df) +
      ggplot2::geom_sf(aes(fill = q_value), color = "#2b2b2b", size = 0.1, alpha = 0.7) +
      ggplot2::geom_text(aes(X, Y, label = nvl_label), vjust = 1.5, color = "black",
                         position = ggplot2::position_dodge(0.9), size = map_text) +
      ggplot2::scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = F) +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 6)) +
      ggplot2::xlab("") +
      ggplot2::labs(title = glue::glue("{tl} (cambio porcentual)"), caption=cap) +
      theme_Publication()

    # Plot
    graphics::plot(graph)
    return(graph)

  }

}
}
}
}
