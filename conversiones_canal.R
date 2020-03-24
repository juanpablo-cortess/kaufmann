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
end_date<-"2020-03-23"
necesary_metrics<-c("totalEvents",'uniqueEvents')
necesary_dimensions<-c('source','medium')


#autos 1
conversiones<-google_analytics(kaufmann_id,
                                   date_range = c(start_date,end_date),
                                   metrics = necesary_metrics,
                                   dimensions = necesary_dimensions,
                                   anti_sample = TRUE,
                                   filtersExpression = 'ga:eventCategory==COTIZADOR GENERAL')

conversiones<-conversiones %>% mutate(source=str_to_lower(source))

conversiones<- conversiones %>%
  mutate(manual_chanelGrouping = case_when(
    str_detect(source,regex('direct')) ~ 'direct',
    str_detect(source,regex('crm|email|newsletter')) ~ 'email',
    str_detect(source,regex('facebook|fb|instagram')) & medium !='referral' ~ 'paidsocial',
    str_detect(source,regex('facebook|fb|instagram')) & medium =='referral' ~ 'social',
    str_detect(source,regex('google')) & medium =='cpc' ~ 'paidsearch',
    str_detect(source,regex('google-search')) ~ 'paidsearch',
    str_detect(source,regex('google')) & medium =='organic' ~ 'organic',
    !str_detect(source,regex('facebook|fb|instagram')) & medium =='referral' ~ 'referral',
    str_detect(source,regex('programmatic|pm')) ~ 'programmatic',
    TRUE ~'others'
  ))

conversiones_canal_manual<-conversiones %>% 
  group_by(manual_chanelGrouping) %>% 
  summarise(uniqueEvents=sum(uniqueEvents),totalEvents=sum(totalEvents)) %>%
  mutate(p_total= paste(format(totalEvents/sum(totalEvents)*100,digits = 2),'%',sep =''))

ggplot(conversiones_canal_manual,aes(x=manual_chanelGrouping ,y=totalEvents,fill=manual_chanelGrouping, label = p_total)) + 
  geom_bar(stat = 'identity') + 
  geom_text() +labs(subtitle=paste("Ordenamiento fuente/medio manual entre",start_date,'al',end_date), 
                    y="canal", 
                    x="Total de cotizaciones", 
                    title="Cotizaciones por canal",
                    caption="Source: Google Analytics API")+
  theme(legend.position = "none")

ggsave(paste('figs/conversiones_canal_',start_date,'_',end_date,".png",sep=""))
