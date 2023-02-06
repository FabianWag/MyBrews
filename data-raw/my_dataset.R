## code to prepare `my_dataset` dataset goes here
library(httr)
library(jsonlite)


# res = GET(url = "http://api.open-notify.org/astros.json")
# res
# http_status(res)

UserID <- "1zeKdqvoURRGQtrXZndZGwYdV0j2"
APIKey <- "3sRziL7Gk1dvKJf0OhHgKMVp7ZNQtmYC2AkOa7Plvf4lvnG6G1hjCfTSLD0ac4Qd"

getcompleted <- GET("https://api.brewfather.app/v1/batches",
                    query = list(complete = TRUE,
                                 status = c("Completed"),
                                 limit = 50),
                    authenticate(UserID, APIKey))

rm(UserID, APIKey)
# getfermenting <- GET("https://api.brewfather.app/v1/batches",
#                      query = list(complete = TRUE,
#                                   status = c("Fermenting"),
#                                   limit = 5),
#                      authenticate(UserID, APIKey))

# getconditioning <- GET("https://api.brewfather.app/v1/batches",
#                        query = list(complete = TRUE,
#                                     status = c("Conditioning"),
#                                     limit = 5),
#                        authenticate(UserID, APIKey))



completed <- content(getcompleted)
rm(getcompleted)



#### BrewHistory

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

