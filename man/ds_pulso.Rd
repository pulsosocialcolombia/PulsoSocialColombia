\name{ds_pulso}
\alias{ds_pulso}
\title{Pulso Social Project Main Data
}
\description{
This dataset collects information from different sources, previously preprocessed, updated and standardized. It becomes the main source of information for the Pulso Social project, and contains all the necessary information for further analysis.
}
\usage{
ds_pulso
}
\details{

 \bold{id_data}: This is the unique identifier for each data point in the dataset.

 \bold{variable}: This column represents the variable name associated with each data point.

 \bold{id_nivel}: This column represents the level identifier, which could be "ciudad", "area_metropolitana", etc.

 \bold{id_time}: This column represents the time identifier for each data point.

 \bold{time}: This column represents the actual time value associated with each data point.

 \bold{nvl_type}: This column represents the type of level (e.g., "ciudad", "area_metropolitana", etc.) associated with each data point.

 \bold{nvl_value}: This column represents the value associated with the level type for each data point.

 \bold{nvl_label}: This column represents the label associated with the level type for each data point.

 \bold{ct_type}: This column represents the type of category (e.g., "ninguno", etc.) associated with each data point.

 \bold{ct_value}: This column represents the value associated with the category type for each data point.

 \bold{ct_label}: This column represents the label associated with the category type for each data point.

 \bold{value_label}: This column represents the label associated with the value for each data point.

 \bold{value}: This column represents the actual value for each data point.

 \bold{var_id}: This column represents the variable identifier for each data point. It's created by joining `ds_pulso` with a data dictionary and assigning new IDs for new variables.
}

\author{
}
\seealso{
}

