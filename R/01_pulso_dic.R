# File: 01_pulso_dic.R
# Created: Nov 2021 
#         - Mónica Hernandez (mhernande6@eafit.edu.co)
#         - Ana M Pirela
#         - Juan Carlos Muñoz-Mora (jmunozm1@eafit.edu.co)
# Last Updated: Nov 2023
#               - German Tabares (gangulo1@eafit.edu.co)
#               - Juan Carlos Muñoz (jmunozm1@eafit.edu.co)
#               - Santiago Navas (snavasg@eafit.edu.co)
#               - Laura Quintero (lmquinterv@eafit.edu.co)
# Summary: This script prepares a dictionary of variables for analysis.

## Load data
# Load the cod_vars data object
load("data/cod_vars.rda")

## Define function
#' Dictionary of Pulso Social Colombia
#'
#' @return Updates the dictionary for Pulso Social Colombia
#' @export
#'
#' @examples
#' pulso_diccionario()
# Define the function 'pulso_diccionario'
pulso_diccionario <- function(){

  # Load the necessary libraries
  # library(googlesheets4, quietly = TRUE) -- No needed because it was save on local
  require (tidyverse, quietly = TRUE)
  require (stringi, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  
  # Define the Google Sheets URL
  #dic_map <-"https://docs.google.com/spreadsheets/d/1ZEqO-bYWPYixr2xI6fNeW4MCEeyZYqFoIRUKllhbeU8/edit?usp=sharing"

  # Select certain columns from the 'cod_vars' dataframe and assign them to 'dic'
  dic <- cod_vars %>% dplyr::select(Tipo, dimension, subdimension, var_id, variable, var_etiqueta, var_periodos, var_poblacional,
                                    id_nivel, id_time, fig_trend, max_year, min_year, fig_map)

  # Define the population levels for the scatterplots
  pob_levels <- c("étnia", "etnia", "género", "minorías", "Género", "étnico")

  # Create conditional columns for the scatterplots in 'dic'
  dic <- dic %>% dplyr::mutate(fig_scatter = 
        ifelse(stringr::str_detect(var_poblacional, paste(pob_levels, collapse = "|")), 1, 0),
               fig_scatter_time = ifelse(max_year != min_year, 1, 0))

  # Fill the NAs in the 'fig_scatter' and 'fig_scatter_time' columns with 0.
  dic$fig_scatter[is.na(dic$fig_scatter)] <- 0
  dic$fig_scatter_time[is.na(dic$fig_scatter_time)] <- 0

  # Select all columns except 'var_poblacional' in 'dic'
  dic <- dic %>% dplyr::select(-var_poblacional)

  # Define the 'fix_text' function that replaces all newline and carriage return characters in a string with nothing
  fix_text <- function(x) { stringr::str_replace_all(stringi::stri_encode(tolower(x), "", "UTF-8"), "[\r\n]" , "") }

  # Apply the 'fix_text' function to the 'Tipo,' 'dimension,' and 'subdimension' columns in 'dic'
  dic <- dic %>% dplyr::mutate(Tipo=fix_text(ifelse(Tipo=="Por Definir","Contexto",Tipo)))
  dic <- dic %>% dplyr::mutate(dimension=fix_text(ifelse(dimension=="Por Definir","Migración",dimension)),
                               subdimension=fix_text(ifelse(subdimension=="Por Definir","",subdimension)),
                               subdimension=ifelse(subdimension==dimension,"",subdimension))

  # Recode certain values in the 'dimension' column in 'dic'
  dic <- dic %>% dplyr::mutate(dimension=dplyr::recode(dimension,
                                                       "infancia y niñez; juventud"="juventud",
                                                       "infancia y niñez; juventud; adultez; vejez"="juventud"))

  # Sort 'dic' by 'Tipo,' 'dimension,' and 'subdimension'
  dic <- dic %>% dplyr::arrange(Tipo,dimension,subdimension)

  # Get the current date and time and format it as a string
  dt <- format(Sys.time(), "%Y%b%d")

  ### Return
  return(dic)

  #### ---- This section was deleted because we create a webpage on this
  # Try to delete a sheet in the Google Sheets spreadsheet
  #try(googlesheets4::sheet_delete(dic_map, paste0("Vars_",dt)))

  # Write 'dic' to the Google Sheets spreadsheet
  #googlesheets4::sheet_write(dic, ss = dic_map, sheet = paste0("Vars_",dt))
}
