\name{pulso_map_change}
\alias{pulso_map_change}
\title{Mapa de Cambio Porcentual Variables Pulso Social}
\usage{
pulso_map_change(id, type_p, col_palette)
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
    Esta función genera un gráfico de mapa que muestra el cambio porcentual de una variable específica en los datos de Pulso Social entre el primer año y último año disponible.
}
\examples{
pulso_map_change(id="286",type="print") #Impresión con notas y etiquetas
pulso_map_change(id="300",type="print",c("#7052ff", "#d06aff", "#72A3AF", "#ff81ff", "#5B6477", "#ff81ff")) # Personalizando los colores del gráfico
pulso_map_change(300) # Gráfico sin notas
}
