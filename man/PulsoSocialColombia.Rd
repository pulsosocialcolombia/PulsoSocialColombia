\docType{package}
\name{PulsoSocialColombia}
\alias{PulsoSocialColombia}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pulso Social Colombia
%%  ~~function to do ... ~~
}
\author{Juan Carlos Muñoz-Mora (Valor Público - EAFIT \email{jmunozm1@eafit.edu.co}) 

}
\description{

\if{html}{\figure{logo_pulso.png}{options: width="40\%" alt="Figure: workspace_id.png"}}
\if{latex}{\figure{logo_pulso.pdf}{options: width=7cm}}


El objetivo de este paquete es poner a disposición los indicadores sociales que ponen la aplicación para Colombia de la metodología \emph{Pulso Social } propuesta por el Banco Interamericano de Desarrollo (BID) \href{https://publications.iadb.org/es/pulso-social-en-america-latina-y-el-caribe-2017-legado-familiar-rompemos-el-molde-o-repetimos}{(Duryea2016Pulso2016)}. Bajo este enfoque, se entiende que la política social debe estar centrada en las personas durante los diferentes ciclos de vida (niñez, infancia, juventud, adultez y vejez), con el fin de identificar brechas y cuellos de botella en la formación del capital humano. 

Para esto, se realiza una revisión sistemática de diferentes indicadores sociales los cuales son agrupados en dos categorías: contexto y resultados. Los primeros, hacen referencia a las condiciones generales del hogar, las cuales son esenciales para entender el contexto en que se elabora las políticas sociales. Por su parte, los indicadores de resultados describen las situaciones sociales en áreas específicas a lo largo del ciclo de vida de los individuos y reflejan el contexto.

En el caso de Colombia, se consolidó una infraestructura de datos abiertos que dieran la posibilidad de brindar análisis flexible de la situación social con enfoque territorial y diferencial. La infraestructura de datos de \emph{Pulso Social} Colombia contiene la revisión de \bold{404 indicadores} los cuales provienen de \bold{22 bases de datos}, a corte de diciembre 2023. 

Este paquete ofrece un conjunto de funciones y ayudas que permitirán una lectura precisa, directa y transparente de las realidades sociales del país, buscando resultados precisos y rápidos para el análisis.

}

\seealso{
\bold{El paquete incluye:}

\itemize{
  \item{\bold{Funciones}}
  \tabular{ll}{
    \tab \link{pulso_trend}: Crea gráficos de tendencias temporales \cr
    %\tab \link{pulso_diccionario} \cr
    \tab \link{pulso_map_change}: Crear mapa con la información geográfica con los datos disponibles \cr
    \tab \link{pulso_map}: Crear mapa con la información geográfica con los datos disponibles  \cr
    \tab \link{pulso_scatter_time}: Crea un gráfico de scatter plot comparando dos periodos \cr
    \tab \link{pulso_scatter} Crea un gráfico de scatter plot \cr
    \tab \link{pulso_static} Crea un gráfico de proporciones estáticos \cr
    \tab \link{pulso_trend} Crea un gráfico de cambio temporal de los gráficos \cr
  }
   \item{\bold{Bases de Datos}}
  \tabular{ll}{
    \tab \link{ds_pulso}: Accede a la infrastructura de datos completa de Pulso Social \cr
    %\tab \link{deflactor} \cr
    \tab \link{cod_bases}: Incluye la base de datos de fuentes de información utilizadas para la construcción de la infrastructura de Pulso Social \cr
    \tab \link{cod_vars}: Base de datos donde se encuentran todas las variables disponibles para su análisis \cr
    %\tab \link{map_dptos} \cr
    %\tab \link{map_mpios} \cr
    %\tab \link{nom_dptos} \cr
    %\tab \link{nom_mpios} \cr
    %\tab \link{nom_regiones} \cr

  }

}
}
