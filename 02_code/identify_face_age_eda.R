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
library(stringr)
library(grid)

# 1. 데이터 불러오기 ----------------------------------------------

img_list <- list.files("03_data/") 
img_list <- stringr::str_replace(img_list, ".jpg", "")


img_info_bucket <- list()

# 2. 사진 정보 확인 ------------------------------------------------
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html
for(lst in seq_along(img_list)){
  tmp <- image_read(paste0("03_data/", img_list[lst], ".jpg"))
  img_info_bucket[[lst]] <- image_info(tmp)
}

img_info_buckets <- do.call(rbind, img_info_bucket)

img_info_df <- data.frame(idate = substr(as.vector(img_list), 5,13), img_info_buckets)

summary(img_info_df)

# 3. 이미지 쫙 붙이기 ------------------------------------------------
# 전체 이미지 불러오기
for(lst in seq_along(img_list)){
  img_name <- img_list[lst]
  assign(img_name, image_read(paste0("03_data/", img_list[lst], ".jpg")))
}

img_vec <- c(img_20040414, img_20040723, img_20090702, img_20110603, 
             img_20120508, img_20121108, img_20121224, img_20130529, 
             img_20140113, img_20140724, img_20141229, img_20150504,
             img_20150804, img_20160204, img_20161104)


img_left2right <- image_append(image_scale(img_vec, "x77"))

img_left2right

image_write(img_left2right, "04.result/left2right.png", format = "png")

img_top2bottom <- image_append(image_scale(img_vec, "x77"), stack = TRUE)

# 4. GIF 애니메이션 ------------------------------------------------

# 이미지 좌우 반전
img_20161104 <- image_flop(img_20161104)

img_transition <- image_morph(c(img_20040414, img_20161104), frames = 10)
img_animation <- image_animate(img_transition, fps=5)

image_write(img_animation, "04.result/img_transition.gif")


