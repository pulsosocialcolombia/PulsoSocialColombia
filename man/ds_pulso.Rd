\name{ds_pulso}
\alias{ds_pulso}
\title{Datos principales del proyecto Pulso Social
}
\description{
Esta base de datos representa la infrastura de datos de Pulso Social. La información procedente de distintas fuentes, previamente preprocesada, actualizada y normalizada. Se convierte en la principal fuente de información del proyecto Pulso Social, y contiene toda la información necesaria para su posterior análisis.
}
\usage{
ds_pulso
}
\details{

La base de datos contiene la siguientes columnas:

 \bold{id_data}: Este es el identificador único para cada punto de datos en el conjunto de datos.

 \bold{variable}: Esta columna representa el nombre de la variable asociada a cada punto de datos.

 \bold{id_nivel}: Esta columna representa el identificador de nivel, que puede ser "ciudad", "área_metropolitana", etc.

 \bold{id_time}: Esta columna representa el identificador temporal de cada punto de datos.

 \bold{tiempo}: Esta columna representa el valor de tiempo real asociado a cada punto de datos.

 \bold{nvl_tipo}: Esta columna representa el tipo de nivel (por ejemplo, "ciudad", "área_metropolitana", etc.) asociado a cada punto de datos.

 \bold{nvl_valor}: Esta columna representa el valor asociado al tipo de nivel para cada punto de datos.

 \bold{nvl_label}: Esta columna representa la etiqueta asociada al tipo de nivel para cada punto de datos.

 \bold{tipo_ct}: Esta columna representa el tipo de categoría (por ejemplo, "ninguno", etc.) asociado a cada punto de datos.

 \bold{valor_ct}: Esta columna representa el valor asociado al tipo de categoría para cada punto de datos.

 \bold{ct_label}: Esta columna representa la etiqueta asociada al tipo de categoría para cada punto de datos.

 \bold{etiqueta_valor}: Esta columna representa la etiqueta asociada al valor de cada punto de datos.

 \bold{valor}: Esta columna representa el valor real de cada punto de datos.

 \bold{id_variable}: Esta columna representa el identificador de la variable para cada punto de datos. Se crea uniendo `ds_pulso` con un diccionario de datos y asignando nuevos ID a los nuevos puntos de datos.

}

\author{
}
\seealso{
}

