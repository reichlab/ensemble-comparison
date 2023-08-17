library(zoltr)
library(tidyverse)
library(lubridate)
library(readr)
library(stringr)

# parallelize code
library(doParallel)
num_cores <- detectCores(logical=TRUE)
cl <- makeCluster(num_cores-1)
registerDoParallel(cl)

# Establish connection
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))
# zoltar_connection

# Get projects and associated info
the_projects <- projects(zoltar_connection)
#str(the_projects)
project_url <- the_projects[the_projects$name == "CDC Influenza Hospitalization Forecasts", "url"]
the_project_info <- project_info(zoltar_connection, project_url)
# names(the_project_info)
# the_project_info$description
the_models <- models(zoltar_connection, project_url)
# str(the_models)

# Query forecasts
forecast_data <- zoltar_connection |> 
  do_zoltar_query(project_url, query_types ="forecasts", 
#                  models = "docs forecast model", 
                  units = c("loc1", "loc2"), 
                  targets = c("pct next week", "cases next week"),
                  timezeros = c("2011-10-02", "2011-10-09", "2011-10-16"), 
                  types = "quantile")





