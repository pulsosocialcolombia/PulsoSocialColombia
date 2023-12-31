% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_pulso_map.R
\name{pulso_map}
\alias{pulso_map}
\title{Mapa Comparativo de Variables Pulso Social}
\usage{
pulso_map(id, type_p, col_palette)
}
\arguments{
\item{id}{Identificación de la variable de Pulso Social Colombia}

\item{type_p (opcional)}{Provee la opción de agregar las etiquetas y variables usando la opción "print". Este parámetro está predeterminado para estar vacio, lo cual implica que no se imprime las etiquetas.}

\item{col_palette (optional)}{Permite la definición pre-determinada de los colores que se utilizarán para el mapa. El insumo debe definir \bold{5} colores. }
}
\value{
    Objeto ggplot que muestra el gráfico la comparación de la representación geográfica de un "id" con datos de Pulso Social - Colombia. 
}
\description{
    Esta función genera un gráfico de mapa comparativo entre el primer año y último año disponible de una variable específica en los datos de Pulso Social.
}
\examples{
pulso_map(id="286", type_p="print") #Impresión con notas y etiquetas
pulso_map(id="300", type_p="print", col_palette=c("#7052ff", "#d06aff", "#72A3AF", "#ff81ff", "#5B6477", "#ff81ff")) # Personalizando los colores del gráfico
pulso_map(300) # Gráfico sin notas
}
