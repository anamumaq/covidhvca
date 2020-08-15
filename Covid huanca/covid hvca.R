library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(highcharter)
library(leaflet)



covid_bd = read_csv("https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download")

XY_HVCA <- read_csv("XY HVCA.csv")


########################PREPARACION DEDATA###############################
covid_bd = filter(covid_bd, covid_bd$DEPARTAMENTO == "HUANCAVELICA")
covid_bd = select(covid_bd, -UUID, -DEPARTAMENTO)

Encoding(covid_bd$PROVINCIA)= "latin1"
Encoding(covid_bd$DISTRITO)= "latin1"



covid_bd$FECHA_RESULTADO = as.character(covid_bd$FECHA_RESULTADO)


covid_bd$FECHA_RESULTADO = 
  str_c(
  str_sub(covid_bd$FECHA_RESULTADO, 1, 4), "/",
  str_sub(covid_bd$FECHA_RESULTADO, 5, 6), "/",
  str_sub(covid_bd$FECHA_RESULTADO, 7, 8))

covid_bd$FECHA_RESULTADO = 
    as.Date(covid_bd$FECHA_RESULTADO)

covid_bd = merge(covid_bd, XY_HVCA[,2:4], by = "DISTRITO", all.x = TRUE)


###################FIN DE LIMPIEZA###########################################


######SIN HACER GROUP BY ######
hchart(covid_bd$PROVINCIA, name = "Precio")

hchart(covid_bd$DISTRITO, name = "Precio")

hchart(covid_bd$SEXO, name = "Casos Positivos")%>%
  hc_add_theme(hc_theme_sandsignika())

covid_bd %>% 
  count(SEXO) %>% 
  hchart('bar', hcaes(x = SEXO, y = 'n')) %>%
  hc_xAxis(title = list(text = "Positivos"))



hchart(covid_bd$METODODX, name = "Precio")

hchart(covid_bd$EDAD, name = "Precio")%>%
  hc_colors(cols)


hcboxplot(
  outliers = FALSE,
  x = covid_bd$EDAD,
  var = covid_bd$SEXO) %>%
  hc_title(text = "Boxplot Edad Infectados") %>%
  hc_yAxis(title = list(text = "Edad positivos"))



pos_hvca  = covid_bd %>%
  group_by(METODODX)%>%
  summarise(positivos = n())

pos_hvca%>% 
  hchart("pie", 
    hcaes(x = METODODX, y = positivos),
    name = "Postivos")%>%
  hc_add_theme(hc_theme_economist)
    
    
##MASCULONO FEMENINO TREEMAP
covid_bd %>%
count(SEXO) %>% 
  hchart('treemap', hcaes(x = SEXO, value = 'n', color = )) 



pos_hvca  = covid_bd %>%
  group_by(SEXO)%>%
  summarise(positivos = n())

pos_hvca %>% 
  hchart('column', hcaes(y = positivos, group = SEXO),  stacking = "normal") %>%
  hc_colors(c("#0073C2FF", "#EFC000FF"))





##covid_bd %>%
  ###count(PROVINCIA, METODODX) %>% 
  ###hchart('bar', hcaes(x = PROVINCIA, value = n, group = METODODX))


pos_hvca  = covid_bd %>%
  group_by(PROVINCIA)%>%
  summarise(positivos = n())%>%
  top_n(3,positivos)


top_hvca  = covid_bd %>%
  merge(pos_hvca, by = "PROVINCIA")%>%
  group_by(FECHA_RESULTADO, PROVINCIA)%>%
  summarise(positivos = n())


top_hvca %>%
hchart("spline", 
  hcaes(x = FECHA_RESULTADO, y = positivos, group = PROVINCIA))%>%
  hc_add_theme(hc_theme_sandsignika())


##################GRAFICO DE LINEAS POR MES NO ACUMULADAS
###OK
pos_hvca  = covid_bd %>%
  group_by(FECHA_RESULTADO, METODODX)%>%
  summarise(positivos = n())


hchart(pos_hvca, "spline", 
  hcaes(x = pos_hvca$FECHA_RESULTADO,
        y = pos_hvca$positivos))
  hc_add_theme(hc_theme_google())

max(covid_bd$FECHA_RESULTADO)


##### bar POR PROVCINCIA sexo

pos_hvca2  = covid_bd %>%
  group_by(PROVINCIA, SEXO)%>%
  summarise(positivos = n())


hchart( pos_hvca2, "bar", 
  hcaes(x = pos_hvca2$PROVINCIA, y = pos_hvca2$positivos, group = pos_hvca2$SEXO))%>% 
  hc_add_theme(hc_theme_538()) %>%
  


### provincia con mayor positivos
pos_hvca  = covid_bd %>%
  group_by(PROVINCIA)%>%
  summarise(positivos = n())%>%
  filter(positivos == max(positivos))
    

##### bar POR PROVCINCIA prueba ??????

pos_hvca3  = covid_bd %>%
  group_by(PROVINCIA, METODODX)%>%
  summarise(positivos = n())

hchart(pos_hvca3, "bar", 
  hcaes(x = pos_hvca3$PROVINCIA,
    y = pos_hvca3$positivos,
    group = pos_hvca3$METODODX)) %>% 
  hc_add_theme(hc_theme_google())

###### EDAD PROVINCIA
pos_hvca4  = covid_bd %>%
  group_by(PROVINCIA)%>%
  summarise(EDAD= round(mean(EDAD),2))



  hchart(pos_hvca4, "bar", 
  hcaes(x = 'PROVINCIA', y = 'EDAD')) %>%
  hc_title(text = "How much did the gang really drink?", align = "center") %>% 
  hc_add_theme(hc_theme_economist())

  
  #############edad sexo

  pos_hvca4  = covid_bd %>%
    group_by(SEXO, EDAD)%>%
    summarise(positivos = n())
  

  
  hchart(pos_hvca4, "areaspline", 
    hcaes(x = pos_hvca4$EDAD,
      y = pos_hvca4$positivos, group = SEXO)) %>%
    hc_title(text = "How much did the gang really drink?", align = "center") %>% 
    hc_add_theme(hc_theme_economist())
  

###### TIPO PREUBA PROVINCIAS
  ###### sexo y edad de positivos

###### SEXO PROVINCIA

##############PROVINCI DISTRITO

pos_hvca4  = covid_bd %>%
  group_by(PROVINCIA)%>%
  summarise(positivos = n())



sort(num, decreasing = F)

head(tail(covid_bd, 2),1) 

pos_hvca  = covid_bd %>%
  group_by(PROVINCIA)%>%
  summarise(positivos = n())%>%
  top_n(2, positivos)%>%
  top_n(-1, positivos)




###########

pos_hvca  = covid_bd %>%
  group_by(PROVINCIA)%>%
  summarise(positivos = n())%>%
  top_n(1,positivos)

top_hvca  = covid_bd %>%
  filter(PROVINCIA == pos_hvca$PROVINCIA)%>%
  group_by(DISTRITO)%>%
  summarise(positivos = n())

hchart(top_hvca, "bar", 
  hcaes(x = DISTRITO,
    y = positivos))%>%
  hc_add_theme(hc_theme_sandsignika())

######## MAPA CON POPUPS

pos_hvca5  = covid_bd %>%
  group_by(PROVINCIA, lat, long)%>%
  summarise(positivos = n())

leaflet(pos_hvca5) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(pos_hvca5$long, pos_hvca5$lat, popup= ~paste0("pruebas positivas:",positivos))

## MAPA AGRUPADO

leaflet(covid_bd1) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers( clusterOptions = markerClusterOptions()) 






############################################

#VALIDADOR DE MARGE XY

covid_null = filter(covid_bd, is.na(covid_bd$lat))

covid_null%>%
  group_by(DISTRITO)%>%
  summarise(sum=n())


####################### series de tiempo

highchart() %>% 
  hc_add_series(covid_bd)



pos_hvca  = covid_bd %>%
  group_by(FECHA_RESULTADO)%>%
  summarise(positivos = n())


hchart(pos_hvca$FECHA_RESULTADO)

summary(pos_hvca)
pos_hvca$FECHA_RESULTADO = format(pos_hvca$FECHA_RESULTADO, "%d/%m/%Y")

pos_hvca$FECHA_RESULTADO = as.Date(pos_hvca$FECHA_RESULTADO,tz = "UTC")
str(pos_hvca)

hchart(pos_hvca, "area", 
  hcaes(x = FECHA_RESULTADO,
    y = positivos), name ='Positivos')%>%
hc_rangeSelector(enabled = TRUE, buttons = list(
      list(type = "all", text = "All"),
      list(type = "month", count = 1, text = "1m"),
      list(type = "day", count = 15, text = "15d"),
      list(type = "day", count = 7, text = "7d")
      ), selected = 3)


pos_hvca$FECHA_RESULTADO =datetime_to_timestamp(pos_hvca$FECHA_RESULTADO)

####hcharter version antigui
highchart(type = "stock") %>%
  hc_add_series_times_values(pos_hvca$FECHA_RESULTADO, pos_hvca$positivos, type = "area")
  hc_yAxis(min = 0, title = list(text = "Positivos)")) %>%
  hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %d}')) %>%
  hc_add_theme(hc_theme_sandsignika())%>%
  hc_tooltip(crosshairs = TRUE)

  ####hcharter version nueva
highchart(type = "stock") %>%
    hc_add_series(pos_hvca, type = "area", hcaes(pos_hvca$FECHA_RESULTADO, pos_hvca$positivos), name = "positivs")%>%
    hc_yAxis(title = list(text = "Positivos)")) %>%
    hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %d}')) %>%
    hc_add_theme(hc_theme_sandsignika())%>%
    hc_tooltip(crosshairs = TRUE)

pos_hvca  = covid_bd %>%
  group_by(FECHA_RESULTADO)%>%
  summarise(positivos = n())


pos_hvca  = covid_bd %>%
  group_by(weekdays(covid_bd$FECHA_RESULTADO))%>%
  summarise(positivos = n())

weekdays(pos_hvca$FECHA_RESULTADO)

#####################agrupacion semanasl
library(lubridate)

pos_hvca = covid_bd %>% group_by(semana = floor_date(FECHA_RESULTADO, "week")) %>%
  summarize(positivos = n())

hchart(pos_hvca, "spline", 
  hcaes(x = semana,
    y = positivos), name ='Positivos')

###############################