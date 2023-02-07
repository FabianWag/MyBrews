## code to prepare `my_dataset` dataset goes here
library(httr)
library(jsonlite)
library(purrr)
library(dplyr)



UserID <- "1zeKdqvoURRGQtrXZndZGwYdV0j2"
APIKey <- "3sRziL7Gk1dvKJf0OhHgKMVp7ZNQtmYC2AkOa7Plvf4lvnG6G1hjCfTSLD0ac4Qd"

getcompleted <- GET("https://api.brewfather.app/v1/batches",
                    query = list(complete = TRUE,
                                 status = c("Completed"),
                                 limit = 50),
                    authenticate(UserID, APIKey))


getfermenting <- GET("https://api.brewfather.app/v1/batches",
                     query = list(complete = TRUE,
                                  status = c("Fermenting"),
                                  limit = 5),
                     authenticate(UserID, APIKey))

getconditioning <- GET("https://api.brewfather.app/v1/batches",
                       query = list(complete = TRUE,
                                    status = c("Conditioning"),
                                    limit = 5),
                       authenticate(UserID, APIKey))

rm(UserID, APIKey)

completed <- content(getcompleted)
fermenting <- content(getfermenting)
conditioning <- content(getconditioning)
rm(getcompleted,
   getfermenting,
   getconditioning)



### create file for completed brews. compare elements and update file if more brews have been complete ------------

call_id <- function(x, list){
  id <- list %>%
    pluck(x, "_id")
  return(id)
}

compare_list <- function(old, new){
  saved_ids <- 1:length(old) %>%
    map_vec(call_id, old)

  new_ids <- 1:length(new) %>%
    map_vec(call_id, new)

  ids <- list(saved_ids = saved_ids,
              new_ids = new_ids)
  return(ids)

}


update_list <- function(old, new){

  ids <- compare_list(old, new)

  updated <- append(new, old[!(ids$saved_ids %in% ids$new_ids)])

  saveRDS(object = updated, file = "data/rawlists.rds")
}
### load data file with completed batches, check for updates
#saveRDS(object = completed, file = "data/rawlists.rds")
completed_saved <- readRDS("data/rawlists.rds")

check <- compare_list(completed_saved, completed)



#### check if ids of the completed brews are the same, if not, update file by adding older ids to the current donwloaded files
#### I decided to do this, in case i changed something in brewfather batches, the older information tends to be overwritten
#### this is not triggered if there is no new completed brew added... this might be a todo for the future

if(!all.equal(check[[1]], check[[2]])){
  update_list(completed_saved, completed)
  rm(completed_saved, completed, check)
  completed <- readRDS("data/rawlists.rds")
} else {
  rm(completed_saved, check)
}





#### BrewHistory -------------

### i need to create multiple data.frames
### one for rough brew description
### one for hops and one for malts

get_basics <- function(x){
  basics <- list(name = completed %>%
                   pluck(x, "name"),

                 style_type = completed %>%
                   pluck(x, "recipe", "style", "type"),

                 style_name = completed %>%
                   pluck(x, "recipe", "style", "name"),

                 brewdate = completed %>%
                   pluck(x, "brewDate"),

                 bottlingdate = completed %>%
                   pluck(x, "bottlingDate"),

                 batchno = completed %>%
                   pluck(x, "batchNo"),

                 estOG = completed %>%
                   pluck(x, "estimatedOg"),

                 measuredOG = completed %>%
                   pluck(x, "measuredOg"),

                 estFG = completed %>%
                   pluck(x, "estimatedFg"),

                 measuredFG = completed %>%
                   pluck(x, "measuredFg"),

                 estABV_fromrecipe = completed %>%
                   pluck(x, "recipe", "abv"),

                 measuredABV = completed %>%
                   pluck(x, "measuredAbv"),

                 estimatedIBU = completed %>%
                   pluck(x, "estimatedIbu"),

                 fermentables_kg_fromrecipe = completed %>%
                   pluck(x, "recipe", "fermentablesTotalAmount"),

                 yeast = completed %>%
                   pluck(x, "batchYeasts", "name"),

                 batchsize_liter = completed %>%
                   pluck(x, "recipe", "batchSize"))
}

basics_data <- map_dfr(1:length(completed), get_basics) %>%
  mutate(brewdate = lubridate::as_datetime(brewdate/1000),
         bottlingdate = lubridate::as_datetime(bottlingdate/1000),
         estOGplato = (-1 * 616.868) + (1111.14 * estOG) - (630.272 * estOG^2) + (135.997 * estOG^3))

get_hops <- function(x){
  completed %>%
    pluck(x, "batchHops")
}

get_hops(1)
map_dfr(1, get_hops)

get_malts <- function(x){
  completed %>%
    pluck(x, "batchFermentables")
}

map_dfr(1, get_malts)


get_steps <- function(x){
  completed %>%
    pluck(x, "batchFermentables")
}

## old code

data_size <- length(completed)


completed_batches <- data.frame(style = character(data_size),
                                equipment = character(data_size),
                                brewhouse_efficiency = numeric(data_size),
                                abv = numeric(data_size),
                                batch_start = TRUE)



for(i in 1:data_size){
  list <- completed[[i]]

  if(is.null(list$recipe$style$name)){
    completed_batches$style[i] <- "none selected"
  } else {
    completed_batches$style[i] <- list$recipe$style$name
  }

  if(is.null(list$recipe$equipment$name)){
    completed_batches$equipment[i] <- "none selected"
  } else {
    completed_batches$equipment[i] <- list$recipe$equipment$name
  }

  completed_batches$brewhouse_efficiency[i] <- list$measuredEfficiency
  completed_batches$abv[i] <- list$measuredAbv

  completed_batches$batch_start[i] <- list$brewDate/1000
}

rm(completed, list, data_size, i)

completed_batches$batch_start <- lubridate::as_datetime(completed_batches$batch_start)

my_dataset <- completed_batches
rm(completed_batches)

usethis::use_data(my_dataset, overwrite = TRUE)

