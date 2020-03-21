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
start_date<-"2020-01-01"
end_date<-"2020-03-18"
necesary_metrics<-c("totalEvents",'uniqueEvents')
necesary_dimensions<-c("eventCategory",'eventAction')

#Con esto podemos ver los segmentos disponibles en la cuenta
segs_conv_kaufmann <- ga_segment_list() %>% filter(str_detect(name,regex('conversiones cotizador general',ignore_case = TRUE)))

seg_conv_autos <- segment_ga4("Conversión cotizador automóviles",segment_id = "gaid::0W5mf4QzTfKhlh3MXa6xGA")
seg_conv_camiones<-segment_ga4("Conversión cotizador camiones",segment_id = "gaid::XhkADWCuRF6HyZk6QaAFbg")
seg_conv_minibuses<-segment_ga4("Conversión cotizador minibuses",segment_id = "gaid::T-_qvVKcRQGVH1v3KdesYQ")
seg_conv_post<-segment_ga4("Conversión cotizador post",segment_id = "gaid::jP8rmVMISGiTpuOimXRIxg")
seg_conv_rodados<-segment_ga4("Conversión cotizador rodados",segment_id = "gaid::jvbiPB2cQWy-64pKcIxL0Q")
seg_conv_buses<-segment_ga4("Conversión cotizador buses",segment_id = "gaid::tJYrnGkmSzexi52Jmefziw")
seg_conv_accesorios<-segment_ga4("Conversión cotizador accesorios",segment_id = "gaid::DaEKNkSgSuygilyyJtj7pw")
seg_conv_power<-segment_ga4("Conversión cotizador power",segment_id = "gaid::dDpL5v1cQbeBKSklJ6kGCA")

#autos 1
conv_cruce_autos<-google_analytics(kaufmann_id,
                                  date_range = c(start_date,end_date),
                                  metrics = c('users',necesary_metrics),
                                  dimensions = necesary_dimensions,
                                  anti_sample = TRUE,
                                  filtersExpression = 'ga:eventCategory==COTIZADOR GENERAL',
                                  segments =seg_conv_autos)
#camiones 2
conv_cruce_camiones<-google_analytics(kaufmann_id,
                                   date_range = c(start_date,end_date),
                                   metrics = c('users',necesary_metrics),
                                   dimensions = necesary_dimensions,
                                   anti_sample = TRUE,
                                   filtersExpression = 'ga:eventCategory==COTIZADOR GENERAL',
                                   segments =seg_conv_camiones)

#minubuses 3
conv_cruce_minibuses<-google_analytics(kaufmann_id,
                                      date_range = c(start_date,end_date),
                                      metrics = c('users',necesary_metrics),
                                      dimensions = necesary_dimensions,
                                      anti_sample = TRUE,
                                      filtersExpression = 'ga:eventCategory==COTIZADOR GENERAL',
                                      segments =seg_conv_minibuses)
#post 4
conv_cruce_post<-google_analytics(kaufmann_id,
                                      date_range = c(start_date,end_date),
                                      metrics = c('users',necesary_metrics),
                                      dimensions = necesary_dimensions,
                                      anti_sample = TRUE,
                                      filtersExpression = 'ga:eventCategory==COTIZADOR GENERAL',
                                      segments =seg_conv_post)

#rodados 5
conv_cruce_rodados<-google_analytics(kaufmann_id,
                                      date_range = c(start_date,end_date),
                                      metrics = c('users',necesary_metrics),
                                      dimensions = necesary_dimensions,
                                      anti_sample = TRUE,
                                      filtersExpression = 'ga:eventCategory==COTIZADOR GENERAL',
                                      segments =seg_conv_rodados)

#buses 6
conv_cruce_buses<-google_analytics(kaufmann_id,
                                      date_range = c(start_date,end_date),
                                      metrics = c('users',necesary_metrics),
                                      dimensions = necesary_dimensions,
                                      anti_sample = TRUE,
                                      filtersExpression = 'ga:eventCategory==COTIZADOR GENERAL',
                                      segments =seg_conv_buses)

#accesorios 7
conv_cruce_accesorios<-google_analytics(kaufmann_id,
                                      date_range = c(start_date,end_date),
                                      metrics = c('users',necesary_metrics),
                                      dimensions = necesary_dimensions,
                                      anti_sample = TRUE,
                                      filtersExpression = 'ga:eventCategory==COTIZADOR GENERAL',
                                      segments =seg_conv_accesorios)

#power 8
conv_cruce_power<-google_analytics(kaufmann_id,
                                      date_range = c(start_date,end_date),
                                      metrics = c('users',necesary_metrics),
                                      dimensions = necesary_dimensions,
                                      anti_sample = TRUE,
                                      filtersExpression = 'ga:eventCategory==COTIZADOR GENERAL',
                                      segments =seg_conv_power)

total_cruce<-rbind(conv_cruce_autos,
                     conv_cruce_camiones,
                     conv_cruce_minibuses,
                     conv_cruce_post,
                     conv_cruce_rodados,
                     conv_cruce_buses,
                     conv_cruce_accesorios,
                     conv_cruce_power)

tabla_resumen<-dcast(total_cruce,segment~eventAction,value.var = "totalEvents")

