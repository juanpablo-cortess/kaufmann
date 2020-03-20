library(httr)
library(httpuv)
library(googleAnalyticsR)
library(dplyr)
library(stringr)
library(googleAuthR)
library(googlesheets)
library(ggplot2)
library(ggrepel)
library(googlesheets4)
library(reshape2)
library(openxlsx)
ga_auth()

kaufmann_id<-162969725
start_date<-"2019-02-01"
end_date<-"2019-12-31"
necesary_metrics<-c("uniquePageviews")
necesary_dimensions<-c("month","sourceMedium","landingPagePath","campaign")

canales_pagados<-c("(Other)","Display","Paid Search","Social")
canales_no_pagados<-c("Direct","Email","Organic Search","Referral","Referral","No Pagado","(Other)","Social")
"de acuerdo a an??lisis desde analytics el 20,99% de los leads Others se asignar??n a pagado"

canales_kaufmann<-google_analytics(kaufmann_id,
                                  date_range = c(start_date,end_date),
                                  metrics = c('users'),
                                  dimensions = c('channelGrouping'),
                                  anti_sample = TRUE)


porcentaje_fb<-sum(filter(visitas_pws,facebook==TRUE)$uniquePageviews)/sum(visitas_pws$uniquePageviews)
porcentaje_fb_carrusel<-sum(filter(visitas_pws,tipo_Ad=="Carrusel")$uniquePageviews)/sum(filter(visitas_pws,facebook==TRUE)$uniquePageviews)

visitas_repuestos<-google_analytics(kaufmann_id,
                              date_range = c(start_date,end_date),
                              metrics = necesary_metrics,
                              dimensions = necesary_dimensions,
                              anti_sample = TRUE,
                              filtersExpression ='ga:pagePath=~landing-repuestos\\/$'
)%>%
  mutate(facebook=case_when(
    str_detect(sourceMedium,regex('facebook',ignore_case = TRUE)) & str_detect(sourceMedium,'social',negate = TRUE)~TRUE,
    TRUE~FALSE
  ))%>%
  mutate(tipo_Ad=case_when(
    str_detect(sourceMedium,regex("Carrusel",ignore_case = TRUE))~"Carrusel",
    str_detect(sourceMedium,regex("link_ads",ignore_case = TRUE))~"Link_Ads",
    str_detect(sourceMedium,regex("video",ignore_case = TRUE))~'video ads',
    TRUE~''
  ))

porcentaje_fb_repuestos<-sum(filter(visitas_repuestos,facebook==TRUE)$uniquePageviews)/sum(visitas_repuestos$uniquePageviews)
porcentaje_fb_carrusel_repuestos<-sum(filter(visitas_repuestos,tipo_Ad=="Carrusel")$uniquePageviews)/sum(filter(visitas_repuestos,facebook==TRUE)$uniquePageviews)
porcentaje_fb_video_repuestos<-sum(filter(visitas_repuestos,tipo_Ad=="video ads")$uniquePageviews)/sum(filter(visitas_repuestos,facebook==TRUE)$uniquePageviews)
porcentaje_fb_link_ads_repuestos<-sum(filter(visitas_repuestos,tipo_Ad=="Link_Ads")$uniquePageviews)/sum(filter(visitas_repuestos,facebook==TRUE)$uniquePageviews)

visitas_servicio<-google_analytics(kaufmann_id,
                                    date_range = c(start_date,end_date),
                                    metrics = necesary_metrics,
                                    dimensions = necesary_dimensions,
                                    anti_sample = TRUE,
                                    filtersExpression ='ga:pagePath=~landing-servicio-extendido\\/$'
)%>%
  mutate(facebook=case_when(
    str_detect(sourceMedium,regex('facebook',ignore_case = TRUE)) & str_detect(sourceMedium,'social',negate = TRUE)~TRUE,
    TRUE~FALSE
  ))%>%
  mutate(tipo_Ad=case_when(
    str_detect(sourceMedium,regex("Carrusel",ignore_case = TRUE))~"Carrusel",
    str_detect(sourceMedium,regex("link_ads",ignore_case = TRUE))~"Link_Ads",
    str_detect(sourceMedium,regex("video",ignore_case = TRUE))~'video ads',
    TRUE~''
  ))

porcentaje_fb_servicios<-sum(filter(visitas_servicio,facebook==TRUE)$uniquePageviews)/sum(visitas_servicio$uniquePageviews)
porcentaje_fb_carrusel_servicios<-sum(filter(visitas_servicio,tipo_Ad=="Carrusel")$uniquePageviews)/sum(filter(visitas_servicio,facebook==TRUE)$uniquePageviews)
porcentaje_fb_video_servicios<-sum(filter(visitas_servicio,tipo_Ad=="video ads")$uniquePageviews)/sum(filter(visitas_servicio,facebook==TRUE)$uniquePageviews)
porcentaje_fb_link_ads_servicios<-sum(filter(visitas_servicio,tipo_Ad=="Link_Ads")$uniquePageviews)/sum(filter(visitas_servicio,facebook==TRUE)$uniquePageviews)
su
