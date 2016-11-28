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
library(ggthemes)

# 1. 나이변화 정적 변화 추이 ----------------------------------------------------

img_viz_df <- img_buckets %>% dplyr::select(idate, age, actual_age)

img_20040414_g <- rasterGrob(img_20040414, interpolate=TRUE)
img_20040723_g <- rasterGrob(img_20040723, interpolate=TRUE)
img_20090702_g <- rasterGrob(img_20090702, interpolate=TRUE)
img_20110603_g <- rasterGrob(img_20110603, interpolate=TRUE)
img_20120508_g <- rasterGrob(img_20120508, interpolate=TRUE)
img_20121108_g <- rasterGrob(img_20121108, interpolate=TRUE)
img_20121224_g <- rasterGrob(img_20121224, interpolate=TRUE)
img_20130529_g <- rasterGrob(img_20130529, interpolate=TRUE)
img_20140113_g <- rasterGrob(img_20140113, interpolate=TRUE)
img_20140724_g <- rasterGrob(img_20140724, interpolate=TRUE)
img_20141229_g <- rasterGrob(img_20141229, interpolate=TRUE)
img_20150504_g <- rasterGrob(img_20150504, interpolate=TRUE)
img_20150804_g <- rasterGrob(img_20150804, interpolate=TRUE)
img_20160204_g <- rasterGrob(img_20160204, interpolate=TRUE)
img_20161104_g <- rasterGrob(img_20161104, interpolate=TRUE)

static_img <- ggplot(img_viz_df, aes(x=idate, age)) + 
  geom_line() +
  geom_point() +
  geom_hline(yintercept = c(min(img_viz_df$age),max(img_viz_df$age)), lty=2) + 
  theme_tufte() + 
  scale_y_continuous(limits = c(20, 55)) +
  xlab("") + ylab("") +
  ggtitle("사진속 나이") +
  theme(plot.title = element_text(lineheight=.7, face="bold")) +
  annotation_custom(img_20040414_g, xmin=as.numeric(img_viz_df$idate[1])-450,  xmax=as.numeric(img_viz_df$idate[1])+ 450, ymin=50, ymax=55) +
  annotation_custom(img_20040723_g, xmin=as.numeric(img_viz_df$idate[2])-450,  xmax=as.numeric(img_viz_df$idate[2])+ 450, ymin=20, ymax=25) +
  annotation_custom(img_20090702_g, xmin=as.numeric(img_viz_df$idate[3])-450,  xmax=as.numeric(img_viz_df$idate[3])+ 450, ymin=50, ymax=55) +
  annotation_custom(img_20110603_g, xmin=as.numeric(img_viz_df$idate[4])-450,  xmax=as.numeric(img_viz_df$idate[4])+ 450, ymin=20, ymax=25) +
  annotation_custom(img_20120508_g, xmin=as.numeric(img_viz_df$idate[5])-450,  xmax=as.numeric(img_viz_df$idate[5])+ 450, ymin=50, ymax=55) +
  annotation_custom(img_20121108_g, xmin=as.numeric(img_viz_df$idate[6])-450,  xmax=as.numeric(img_viz_df$idate[6])+ 450, ymin=20, ymax=25) +
  annotation_custom(img_20121224_g, xmin=as.numeric(img_viz_df$idate[7])-450,  xmax=as.numeric(img_viz_df$idate[7])+ 450, ymin=50, ymax=55) +
  annotation_custom(img_20130529_g, xmin=as.numeric(img_viz_df$idate[8])-450,  xmax=as.numeric(img_viz_df$idate[8])+ 450, ymin=20, ymax=25) +
  annotation_custom(img_20140113_g, xmin=as.numeric(img_viz_df$idate[9])-450,  xmax=as.numeric(img_viz_df$idate[9])+ 450, ymin=50, ymax=55) +
  annotation_custom(img_20140724_g, xmin=as.numeric(img_viz_df$idate[10])-450, xmax=as.numeric(img_viz_df$idate[10])+450, ymin=20, ymax=25) +
  annotation_custom(img_20141229_g, xmin=as.numeric(img_viz_df$idate[11])-450, xmax=as.numeric(img_viz_df$idate[11])+450, ymin=50, ymax=55) +
  annotation_custom(img_20150504_g, xmin=as.numeric(img_viz_df$idate[12])-450, xmax=as.numeric(img_viz_df$idate[12])+450, ymin=20, ymax=25) +
  annotation_custom(img_20150804_g, xmin=as.numeric(img_viz_df$idate[13])-450, xmax=as.numeric(img_viz_df$idate[13])+450, ymin=50, ymax=55) +
  annotation_custom(img_20160204_g, xmin=as.numeric(img_viz_df$idate[14])-450, xmax=as.numeric(img_viz_df$idate[14])+450, ymin=20, ymax=25) +
  annotation_custom(img_20161104_g, xmin=as.numeric(img_viz_df$idate[15])-450, xmax=as.numeric(img_viz_df$idate[15])+450, ymin=50, ymax=55)

static_img
ggsave("04.result/static_img.png")

# 1. 나이변화 동적 추이 ------------------------------------------------------

library(xts)
write_csv(img_buckets, "04.result/img_buckets.csv")

img_buckets <- read_csv(img_buckets, col_names = TRUE)

viz_df <- img_buckets %>% dplyr::select(idate, age, actual_age)
viz_df <- xts(viz_df, order.by=viz_df$idate)[, c("age", "actual_age")]

dygraph(viz_df, main="나이 변화") %>%
  dySeries("age", label = "Age_in_Image") %>%
  # dySeries("actual_age", label = "Actual_Age") %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.1) %>% 
  dyRangeSelector(height = 20) %>% 
  dyLegend(width = 400) %>% 
  dyAnnotation("2004-4-14", text = "A", tooltip = "제17대 국회의원 투표를 하루 전날 수원시 수원역 한나라당 유세장") %>%
  dyAnnotation("2004-7-23", text = "B", tooltip = "한나라당 대표") %>%
  dyAnnotation("2009-7-2", text = "C", tooltip = "몽골 방문 울란바토로에서 기자간담회") %>%
  dyAnnotation("2011-6-3", text = "D", tooltip = "국회 의원회관 사무실에서 이명박 대통령과의 회동 결과를 취재진에게 설명") %>%
  dyAnnotation("2012-5-8", text = "E", tooltip = "비대위원장 시절 어버이날 서울 용산구 용산노인종합복지관") %>%
  dyAnnotation("2012-11-8", text = "F", tooltip = "대선후보 시절 중구 프레스센터 외신기자클럽 기자회견") %>%
  dyAnnotation("2012-12-24", text = "G", tooltip = "대통령 당선인 성탄절을 전날 관악구 난곡 사랑의 밥집") %>%
  dyAnnotation("2013-5-29", text = "H", tooltip = "청와대 본관 무궁화실에서 벤저민 카딘 미국 상원 동아태소위원장 접견") %>%
  dyAnnotation("2014-1-13", text = "I", tooltip = "청와대에서 미국 CNN 서울주재 특파원 폴라 행콕스와 인터뷰") %>%
  dyAnnotation("2014-7-24", text = "J", tooltip = "정부세종청사에서 확대경제관계장관회의 주재") %>%
  dyAnnotation("2014-12-29", text = "K", tooltip = "청와대에서 핵심 국정과제 점검회의 참석") %>%
  dyAnnotation("2015-5-4", text = "L", tooltip = "중남미 순방 후 청와대에서 수석비서관회의 주재") %>%
  dyAnnotation("2015-8-4", text = "M", tooltip = " 청와대에서 국무회의를 주재") %>%
  dyAnnotation("2016-2-4", text = "N", tooltip = " 청와대에서 열린 사립대학 총장 간담회") %>%
  dyAnnotation("2016-11-4", text = "O", tooltip = " 청와대 춘추관에서 대국민 담화") %>% 
  dyShading(from = "2015-3-17", to = "2015-6-17", color = "#FFE6E6")


