#--------------------------#
# C. fun_graph ----
# Creates graphs according to the indicated style
#--------------------------#

# load("data/ds_pulso.rda")
# load("data/cod_vars.rda")
# load("data/cod_bases.rda")
# load("data/nom_dpto.rda")
# load("data/deflactor.rda")

# ethnicity <- ds_pulso %>% dplyr::filter(stringr::str_detect(id_nivel, "etnia"))
# table(ethnicity$variable)
# table(ethnicity$var_id)
# labor <- ethnicity %>% dplyr::filter(id_data == 1) %>% dplyr::distinct(var_id, .keep_all = T)
# vars <- unique(labor$var_id)
# id <- 220
# type_p <- "print"
# pulso_static(id = id, type_p = type_p)
# path <- "C:/Users/anapi/Desktop"
# w <- 7*1.5
# h <- 4*1.5
# dpi <- 300
# ggsave(glue::glue("{path}/test.png"), dpi = dpi, width = w, height = h)

# bar_colors1 <- c("#3d5574", "#74ac54", "#b8544c", "#c0ac84", "#c8dccc", "#b0c4e4", "#d4742c")
# countries_colors1 <- c("#759cd4", "#c43424", "#ecbc24", "#385676", "#d4742d","#6dad55", "#a4a4a4", "#643d94")
# bar_colors2 <- c("#4c9ccc", "#e4546c", "#fcac24", "#0c0c0c", "#b4b4b4")
# col_palette <-  countries_colors1

#' Static Analysis of Pulso Social
#'
#'This function generates a static circular bar chart for a specific variable in the data of Pulso So.
#'
#' @param id Unique identifier of the variable.
#' @param type_p Type of graph (optional, default is "").
#' @param year Year for which to generate the analysis (optional, default is the most complete year of data).
#'
#' @return A ggplot object that displays the static graph of the "id" variable at a certain time.
#'
#' @examples
#' pulso_static(id="286",type="print")
#' pulso_static(id="290",type="",2020)
#' @export
pulso_static <- function(id,type_p,year){

  #### Library
  require(glue, quietly = TRUE)
  require(colorspace, quietly = TRUE)
  require(stringr, quietly = TRUE)
  require(stringi, quietly = TRUE)
  require(tidyverse, quietly = TRUE)
  #Call final data base
  ds_pulso<-PulsoSocialColombia::ds_pulso

  # Font sizes
  caption_text <- 6*1.5
  sub_text <- 6*1.5
  theme_size <- 11
  text_size <- 11*1.5

  #### Choose variable
  df <- ds_pulso[ds_pulso$var_id==id,]

  ## If missing
  if(missing(type_p)) {
    type_p=""
  }

  ### Find national data for chosen variable
  df_col <- ds_pulso %>%
    dplyr::filter(id_data == unique(df$id_data) & variable == unique(df$variable) &
                    nvl_type == "nacional" & ct_type == unique(df$ct_type))

  if(nrow(df_col) > 0 & stringr::str_detect(unique(df$nvl_type), "nacional", negate = T)){
    df_col <- df_col %>%
      dplyr::mutate(id_nivel = unique(df$id_nivel), nvl_type = unique(df$nvl_type), nvl_value = 1)
    df <- dplyr::bind_rows(df, df_col)
  }

  # We dont have data for 2020 in GEIH income
  geih <- ds_pulso %>%
    dplyr::filter(id_data == 1 & stringr::str_detect(variable, "ing")) %>%
    dplyr::select(variable) %>% dplyr::distinct()

  df$value[df$value == 0 & df$time == 2020 & df$id_data == 1 & df$variable %in% geih$variable] <- NA

  ### Choose the most complete year available
  if(missing(year)) {
    # Count years with complete data
    year <- df %>% dplyr::mutate(info = ifelse(!is.na(value), 1, 0)) %>%
      dplyr::group_by(time) %>%
      dplyr::summarise(info = sum(info)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(info)
    # Delete all years with incomplete data
    year <- year[year$info == max(year$info),]
    # Select the max year with complete data
    year <- max(year$time)
  }

  ### Filter only this year
  df <- df[df$time==year,]

  ### Level
  level <- unique(df$id_nivel)
  levels <- ifelse(level %in% c("ciudad","dpto","zona", "nacional"),0,1)

  ### -----------------
  ### Fix labels
  ## ------------------

  # load("data/cod_bases.rda")
  cod_v <- cod_vars %>% dplyr::ungroup() %>% dplyr::select(-id_nivel)
  name_base <- unique(df$id_data) %>% as.numeric()
  name_base <- cod_bases[cod_bases$id_data == name_base,]

  df <- df %>% dplyr::left_join(cod_v,by=c("var_id"))

  ### Fix inflation in nominal variables
  deflactor <- deflactor %>% dplyr::filter(time %in% df$time)

  df <- df %>%
    dplyr::left_join(deflactor, by = "time") %>%
    dplyr::mutate(value = ifelse(var_unidad == "Pesos", value/deflactor_pib, value))

  # Income and production to millions
  df$value[df$var_unidad == "Pesos"] <- df$value/1000000
  df$var_unidad[df$var_unidad == "Pesos"] <- "Millones de pesos (2015=100)"

  tl <- ifelse(type_p=="print", unique(df$var_etiqueta),"")
  s_tl <- ifelse(type_p=="print", paste0("Fuente: ",unique(name_base$db_nombre)),"")
  cap <- ifelse(type_p=="print", paste0("Nota: ",unique(df$var_descripcion)),"")

  print(tl)
  tl <- stringr::str_wrap(paste0(tl," - ",year),width = 30)
  s_tl <- stringr::str_wrap(s_tl,width = 50)
  cap <- stringr::str_wrap(cap,width = 100)
  df$value_label <- stringr::str_wrap(df$var_unidad,width = 30)

  ### -----------------
  # Fix levels
  ### ---------------
  if(levels==1) {
    #df <- df1
    df <- df %>%
      dplyr::mutate(t=nvl_value) %>%
      ### Extract data
      dplyr::mutate(nvl_value=sub("xxx.*", "",stringr::str_replace(t,"_"," xxx ")),
                    level=sub(".*xxx", "",stringr::str_replace(id_nivel,"_"," xxx ")),
                    level_value=as.numeric(sub(".*xxx", "",stringr::str_replace(t,"_"," xxx ")))) %>%
      ## Cleaning
      dplyr::mutate(level=stringr::str_replace_all(level, " ", ""))

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

  # If the unique level id is in the specified categories, convert the level value to numeric
  # Rename the level value in the department name dataframe
  # Join the department name dataframe with the main dataframe based on level value
  # Replace the level value with the department name
  if(unique(df$id_nivel) %in% c("dpto","dpto_etnia","dpto_gen","dpto_sector")) {
    df$nvl_value <- as.numeric(df$nvl_value)
    nom_dpto <- nom_dpto %>% dplyr::rename(nvl_value = nivel_value)
    df <- df %>% dplyr::left_join(nom_dpto,by=c("nvl_value")) %>%
      dplyr::mutate(nvl_value=nm)
  }

  ### -------------------
  # Default themes
  ### -------------------

  # Set the size and line attributes
  # Define the default theme for the plots
  s <- 1.2
  a_line <- 0.6
  theme_Publication <- function(base_size = theme_size, base_family="helvetica") {
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = rel(2), hjust = 0.5),
                   text=ggplot2::element_text(face = "bold", size = text_size, hjust = 0.5),
                   panel.background = ggplot2::element_rect(fill = "#F5F5F5",
                                                            colour = "#F5F5F5",
                                                            size = 0.3, linetype = "solid"),
                   plot.background = ggplot2::element_rect(colour = NA),
                   axis.title = ggplot2::element_text(face = "bold",size = rel(1)),
                   axis.title.y = ggplot2::element_text(angle=90,vjust =2,colour = "black"),
                   axis.title.x = ggplot2::element_text(vjust = -0.2,colour = "black"),
                   axis.text = ggplot2::element_text(),
                   axis.line = ggplot2::element_line(colour="black"),
                   axis.ticks = ggplot2::element_line(),
                   panel.grid.major = ggplot2::element_line(colour="#f0f0f0"),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(colour = NA),
                   legend.position = "bottom",
                   legend.direction = "horizontal",
                   legend.key.size= ggplot2::unit(0.2, "cm"),
                   legend.margin = ggplot2::margin(0, 0, 0, 0, unit = "cm"),
                   legend.title = ggplot2::element_blank(),
                   plot.margin=grid::unit(c(10,5,5,5),"mm"),
                   plot.subtitle=ggplot2::element_text(size = sub_text, hjust=0.5, face="italic", color="#8B8B8B"),
                   plot.caption=ggplot2::element_text(size = caption_text, hjust=0.5, face="italic", color="#8B8B8B"),
                   strip.background=ggplot2::element_rect(colour="#FAECEC",fill="#FAECEC"),
                   strip.text = ggplot2::element_text(face="bold"))

  }

  ### --------------
  # x - Label
  ### -------------

  # Select only the time id column and rename it
  # Define the x-axis label based on the unique time id
  df <- df %>% dplyr::select(-id_time.y) %>% dplyr::rename(id_time = id_time.x)
  xlab_time <- ifelse(unique(na.omit(df$id_time)) == 1, "Year",
                      ifelse(unique(na.omit(df$id_time) == 2, "Month",
                                    ifelse(unique(na.omit(df$time)) == 3, "Day", NULL))))

  ### --------------
  # y - Label
  ### -------------

  # Define the y-axis label as the unique value label
  ylab <- unique(na.omit(df$value_label))

  # Define the options for the x and y axis labels
  options_time <- list(
    xlab(glue::glue("\n{xlab_time}\n")),
    ylab(glue::glue("{ylab}\n")))

  ### --------------
  # levels
  ### -------------

  # Select only the variable column and rename it
  # Define the variable as the unique variable
  # Replace all punctuation in the level value with a space
  df <- df %>% dplyr::select(-variable.y) %>% dplyr::rename(variable = variable.x)
  vari <- unique(na.omit(df$variable))
  df$nvl_value <- stringr::str_replace_all(df$nvl_value, "[[:punct:]]", " ")

  ### --------------
  # Progress
  ### -------------

  # Print the progress of the plot generation
  print(glue::glue("Plotting {ylab} - Trend analysis"))

  ### --------------
  # Chart Type - Circular
  ### --------------

  ## Circular barplot
  if (levels==0) {

    ### Labels -- Fix Label
    # Arrange the data by value and create labels for each row
    data <- df[!is.na(df$value),] %>%
      dplyr::arrange(value) %>%
      dplyr::mutate(id=row_number(),
                    lab=
                      stringr::str_wrap(paste0(nvl_label,"\n",round(value,1)),
                                        width = 20))

    # Create label data and calculate the angle for each label
    label_data <-  data
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar
    label_data$hjust <- ifelse( angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle+180, angle)
    val <- 10

    # Set the y-axis limits and adjust the value based on the unit of the variable
    if(stringr::str_detect(unique(df$var_unidad), "(%)")){
      ylimits <- ylim(-100, 120)
    }
    if(stringr::str_detect(unique(df$var_unidad), "Coeficiente")){
      data$value <- data$value*100
      ylimits <- ylim(-100, 120)
    }
    if(stringr::str_detect(unique(df$var_unidad), "Millones de pesos")){
      ylimits <- ylim(min(data$value)*(-100), max(data$value)*130)
      data$value <- data$value*100
    }
    if(stringr::str_detect(unique(df$var_unidad), "Personas")){
      data$value <- data$value*1.3
      ylimits <- ylim(-100, 120)
    }

    if((stringr::str_detect(unique(df$var_unidad), "Unidades")) &
       max(df$value) > 1000){
      ylimits <- ylim(-100, 120)
      data$value <- data$value/100
    }

    if(stringr::str_detect(unique(df$var_unidad), "Personas") & max(df$value) > 100){
      ylimits <- ylim(-100, 120)
      data$value <- data$value/5
    }

    if(stringr::str_detect(unique(df$var_unidad), "Nacimientos") & stringr::str_detect(unique(df$subdimension), "Fecundidad")){
      ylimits <- ylim(-100, 130)
    }
    if(stringr::str_detect(unique(df$var_unidad), "Muertes por cada")){
      ylimits <- ylim(-100, 180)
    }
  }

  # Define a vector of different units
  others <- c("(%)", "Coeficiente", "Millones de pesos", "Personas", "Nacimientos", "Muertes por cada")

  # If the unit of the variable is not in the defined vector, adjust the value and set the y-axis limits
  if(stringr::str_detect(unique(df$var_unidad), paste(others, collapse = "|"), negate = T)){
    data$value <- data$value*1.3
    ylimits <- ylim(-100, 120)
  }

  # Create a circular barplot with the adjusted data
  graph <- ggplot2::ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    ggplot2::geom_bar(stat="identity", fill=alpha("green", 0.3)) +
    ylimits +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(0, 0, 0, 0, unit = "cm"),
      legend.title = ggplot2::element_blank(),
      plot.margin=grid::unit(c(10,5,5,5),"mm"),
      plot.title = ggplot2::element_text(face = "bold", size = rel(2), hjust = 0.5),
      text=ggplot2::element_text(face = "bold", size = text_size, hjust = 0.5),
      plot.subtitle=ggplot2::element_text(size = sub_text, hjust=0.5, face="italic", color="#8B8B8B"),
      ### Caption
      plot.caption=ggplot2::element_text(size = caption_text, hjust=0.5, face="italic", color="#8B8B8B")
    ) +
    ggplot2::coord_polar(start = 0) +
    ggplot2::geom_text(data=label_data,
                       aes(x=id, y=val, label=lab, hjust=hjust),
                       color="black", fontface="bold",alpha=0.6, size = sub_text*0.35,
                       angle= label_data$angle, inherit.aes = FALSE )+
    ggplot2::labs(title = tl,
                  caption=cap)


#### --- Levels
## Circular barplot
if (levels==1) {

  ### Labels -- Fix Label
  data <- df[!is.na(df$value),] %>%
    dplyr::group_by(ct_value) %>%
    dplyr::arrange(value) %>%
    dplyr::mutate(id=row_number(),
                  lab=
                    stringr::str_wrap(paste0(nvl_label,"\n",round(value,1)),
                                      width = 20))

  label_data <-  data
  number_of_bar <- nrow(label_data)/2
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  val <- 10

  # Set the y-axis limits and adjust the value based on the unit of the variable
  if(stringr::str_detect(unique(df$var_unidad), "(%)")){
    ylimits <- ylim(-100, 120)
  }
  if(stringr::str_detect(unique(df$var_unidad), "Coeficiente")){
    data$value <- data$value*100
    ylimits <- ylim(-100, 120)
  }
  if(stringr::str_detect(unique(df$var_unidad), "Millones de pesos")){
    ylimits <- ylim(min(data$value)*(-100), max(data$value)*130)
    data$value <- data$value*100
  }
  if(stringr::str_detect(unique(df$var_unidad), "Personas")){
    data$value <- data$value*1.3
    ylimits <- ylim(-100, 120)
  }
  if(stringr::str_detect(unique(df$var_unidad), "Nacimientos") & stringr::str_detect(unique(df$subdimension), "Fecundidad")){
    ylimits <- ylim(-100, 130)
  }
  if(stringr::str_detect(unique(df$var_unidad), "Muertes por cada")){
    ylimits <- ylim(-100, 180)
  }

  others <- c("(%)", "Coeficiente", "Millones de pesos", "Personas", "Nacimientos", "Muertes por cada")
  if(stringr::str_detect(unique(df$var_unidad), paste(others, collapse = "|"), negate = T)){
    data$value <- data$value*1.3
    ylimits <- ylim(-100, 120)
  }

  # Create a circular barplot with the adjusted data
  graph <- ggplot2::ggplot(data, aes(x=as.factor(id), y=value)) +
    ggplot2::geom_bar(stat="identity", fill=alpha("green", 0.3)) +
    ylimits +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.margin = grid::unit(0, "cm"),
      legend.title = ggplot2::element_blank(),
      plot.margin=grid::unit(c(10,5,5,5),"mm"),
      plot.title = ggplot2::element_text(face = "bold", size = rel(2), hjust = 0.5),
      text=ggplot2::element_text(face = "bold", size = text_size, hjust = 0.5),
      plot.subtitle=ggplot2::element_text(size = sub_text, hjust=0.5, face="italic", color="#8B8B8B"),
      ### Caption
      plot.caption=ggplot2::element_text(size = caption_text, hjust=0.5, face="italic", color="#8B8B8B")
    ) +
    ggplot2::coord_polar(start = 0) +
    ggplot2::geom_text(data=label_data,
                       aes(x=id, y=val, label=lab, hjust=hjust),
                       color="black", fontface="bold",alpha=0.6, size = sub_text*0.25,
                       angle= label_data$angle, inherit.aes = FALSE )+
    ggplot2::labs(title = tl,
                  caption=cap) +
    ggplot2::facet_wrap(~ ct_label)
}

# Plot the graph
print(graph)
return(graph)
}


