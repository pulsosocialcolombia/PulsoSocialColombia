# Pulso Social Colombia

El objetivo de este paquete es poner a disposición los indicadores sociales que ponen la aplicación para Colombia de la metodología \emph{Pulso Social } propuesta por el Banco Interamericano de Desarrollo (BID) [(Duryea2016Pulso2016)](https://publications.iadb.org/es/pulso-social-en-america-latina-y-el-caribe-2017-legado-familiar-rompemos-el-molde-o-repetimos). Bajo este enfoque, se entiende que la política social debe estar centrada en las personas durante los diferentes ciclos de vida (niñez, infancia, juventud, adultez y vejez), con el fin de identificar brechas y cuellos de botella en la formación del capital humano. 

Para esto, se realiza una revisión sistemática de diferentes indicadores sociales los cuales son agrupados en dos categorías: contexto y resultados. Los primeros, hacen referencia a las condiciones generales del hogar, las cuales son esenciales para entender el contexto en que se elabora las políticas sociales. Por su parte, los indicadores de resultados describen las situaciones sociales en áreas específicas a lo largo del ciclo de vida de los individuos y reflejan el contexto.

En el caso de Colombia, se consolidó una infraestructura de datos abiertos que dieran la posibilidad de brindar análisis flexible de la situación social con enfoque territorial y diferencial. La infraestructura de datos de \emph{Pulso Social} Colombia contiene la revisión de \bold{404 indicadores} los cuales provienen de \bold{22 bases de datos}, a corte de diciembre 2023. 

Este paquete ofrece un conjunto de funciones y ayudas que permitirán una lectura precisa, directa y transparente de las realidades sociales del país, buscando resultados precisos y rápidos para el análisis.

# Instrucciones de Instalación

Instale la versión de desarrollo del paquete directamente desde GitHub con:

```r
# Install devtools
if(!require("devtools")) install.packages("devtools")
devtools::install_github("pulsosocialcolombia/PulsoSocialColombia")
```

El paquete depende de los siguientes paquetes de R:

- `tidyverse`
- `stringi`
- `glue`
- `stringr`

# Contenido

Tras la instalación del paquete estarán disponibles las siguientes funciones: 

- **pulso_diccionario**: Crea el diccionario de datos de Pulso Social Colombia
- **pulso_trend**: Crea gráficos de tendencias temporales 
- **pulso_map_change** Crear mapa con la información geográfica con los datos disponibles 
- **pulso_map** Crear mapa con la información geográfica con los datos disponibles  
- **pulso_scatter_time** Crea un gráfico de scatter plot comparando dos periodos 
- **pulso_scatter** Crea un gráfico de scatter plot 
- **pulso_static** Crea un gráfico de proporciones estáticos 
- **pulso_trend** Crea un gráfico de cambio temporal de los gráficos 

Adicionalmente, se pone a disposición la base de datos de Pulso Social Colombia: 
- **ds_pulso**: Accede a la infrastructura de datos completa de Pulso Social 
- **cod_bases**: Incluye la base de datos de fuentes de información utilizadas para la construcción de la infrastructura de Pulso Social 
- **cod_vars**: Base de datos donde se encuentran todas las variables disponibles para su análisis 

# Más información

Para mayor información del proyecto y para acceder la lista completa de variables visite la [documentación oficial](https://pulsosocialcolombia.github.io/)

# Reportes

Para cualquier duda o problema con el paquete no dude en contactar a [Juan Carlos Muñoz-Mora](maito:jmunozm1@eafit.edu.co) - Valor Público - Universidad EAFIT 
