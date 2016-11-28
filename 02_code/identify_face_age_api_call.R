# 0. 환경설정--------------------------------------------------
library(httr)
library(XML)
library(ggplot2)
library(png)
library(grid)
library(jsonlite)
library(dplyr)
library(lubridate)
library(magick)
library(dygraphs)

# 1. 데이터 불러오기 ----------------------------------------------

img_list <- list.files("03_data/")

# 2. 얼굴인식 API 호출 ------------------------------------------------

face_api_url <- "https://api.projectoxford.ai/face/v1.0/detect?returnFaceAttributes=age,gender"

source("02_code/secret_key_from_api.R")
# emotionKEY <- '53xxxxxxxxxxxxxxxxxx'

img_bucket <- list()

for(lst in seq_along(img_list)){
  img_name <- paste0("03_data/", img_list[lst])
  img <- httr::upload_file(img_name)
  
  result <- POST(url = face_api_url,
                 body = img,
                 add_headers(.headers = c('Content-Type' = 'application/octet-stream',
                                          'Ocp-Apim-Subscription-Key' = emotionKEY))
  )
  
  img_bucket[[lst]] <- as.data.frame(content(result))[,c("faceAttributes.gender", "faceAttributes.age")]
}

# 3. 데이터 정리-------------------------------------
library(eeptools)
# x <- as.Date(c("2011-01-01", "1996-02-29"))
# age_calc(x[2],x[1], units='years')

img_buckets <- do.call(rbind, img_bucket)

img_buckets <- data.frame(idate=substr(as.vector(img_list),5,12), img_buckets)
img_buckets <- img_buckets %>% 
  rename(gender = faceAttributes.gender, age=faceAttributes.age) %>% 
  mutate(idate = ymd(idate), 
         dob = ymd("19520202"),
         age = round(age, 1), 
         actual_age = round(age_calc(ymd(dob), ymd(fig_date), units='years')),1)
  
                            
glimpse(img_buckets)
img_buckets


