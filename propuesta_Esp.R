library(mongolite)
library(ggmap)
library(dplyr)
library(sp)
library(lubridate)
library(tidyr)
library(reshape2)
library(stringr)


#us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
#get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap() 


register_google(key = "AIzaSyDNEh_BUnqoTTsABfuSB_BftOm5fvlHpuw")

sucursales_mongo <- mongo(collection = "sucursales", db = "precios_caba")
sucursales <- sucursales_mongo$find()
productos <- mongo(collection = "productos", db = "precios_caba")$find()
precios <- mongo(collection = "precios", db = "precios_caba")$find()
barrios <- mongo(collection = "barrios", db = "precios_caba")
dolar <- mongo(collection = "dolar", db = "precios_caba")$find()
inflacion <- mongo(collection = "inflacion", db = "precios_caba")$find()

# ------------------- Pre procesamiento de datos --------------------------------- #


seleccionar_barrio <- function(los_barrios, la_sucursal) {
  query <- paste0('{"geometry": {"$near": {"$geometry": {"type": "Polygon", "coordinates": [',la_sucursal['lng'],',',la_sucursal['lat'],']}}}}')
  
  return (los_barrios$find(query, limit=1)$properties$BARRIO)
}

# me quedo unicamente con las sucursales que tienen datos de precios informados
#sucursales_con_datos = filter(sucursales, id %in% distinct(precios, sucursal)$sucursal)

## ****Agrego las sucursales el barrio de pertenencia****
#barrios_sucursales = apply(sucursales_con_datos, 1, seleccionar_barrio, los_barrios=barrios)
#sucursales_con_datos$barrio <- barrios_sucursales

sucursales_con_datos <- mongo(collection = "sucursales_con_datos", db = "precios_caba")$find()

# Transforma los datos de los valores del dolar
agrupar_valores_dolar <- function(dolar_valores, intervalo) {
  dolar_por_fecha = group_by(dolar, fecha=floor_date(fecha, intervalo))
  dolar_media = summarize(dolar_por_fecha, compra=mean(compra), venta=mean(venta))
  compra=select(dolar_media, compra, fecha)
  compra$fecha=as.Date(compra$fecha, format="%m/%d/%y")
  compra$operacion="compra"
  colnames(compra)[which(names(compra) == "compra")] = "valor"
  venta=select(dolar_media, venta, fecha)
  venta$operacion="venta"
  venta$fecha=as.Date(venta$fecha, format="%m/%d/%y")
  colnames(venta)[which(names(venta) == "venta")] = "valor"
  
  return(rbind(compra, venta))
}

# Se generan distintos cortes del valor del dolar para luego comparar
dolar_7_dias = agrupar_valores_dolar(dolar, "7 days")
dolar_15_dias = agrupar_valores_dolar(dolar, "15 days")
dolar_1_mes = agrupar_valores_dolar(dolar, "1 month")

# Formateo la fecha
inflacion$fecha=as.Date(inflacion$fecha, format="%m/%d/%y")

# ----------------- Fin pre procesamiento de datos ------------------------------- #

# Grafico de la evolucion del dolar
graficar_dolar <- function(dolar_valores, intervalo) {
  todas_las_cotizaciones=agrupar_valores_dolar(dolar_valores, intervalo)
  
  return (ggplot(todas_las_cotizaciones, aes(x=fecha, y=valor, group=operacion)) +
            geom_line(aes(colour=operacion)) +
            geom_point(aes(shape = operacion, colour = operacion), size = 1))
}

# ------------------- Mapa de buenos aires con las sucursales -------------------- #
## Revisar la validez de la key de google
#map <- qmap('Buenos Aires', zoom = 11, crop=TRUE)
#map + geom_point(data = sucursales_con_datos, mapping = aes(x = sucursales_con_datos$lng, y = sucursales_con_datos$lat), colour="red")
# -------------------------------------------------------------------------------- #

# Dado un id producto me da los precios por barrio y tipo de sucursal
precios_por_barrio <- function(id_producto, precios, sucursales_con_datos) {
  sucursales_con_el_producto = precios%>%filter(producto==id_producto)%>%select(sucursal, precio, producto, fecha)
  barrios_sucursales = left_join(sucursales_con_el_producto, sucursales_con_datos, by=c("sucursal" = "id"))%>%select(precio, sucursalTipo, barrio, fecha)
  barrios_sucursales$fecha=as.Date(barrios_sucursales$fecha, format="%m/%d/%y")
  return (barrios_sucursales)
}

# Precios por barrio y sucursal para todos los productos
precios_barrio_sucursal = left_join(precios, sucursales_con_datos, by=c("sucursal" = "id"))%>%select(producto, precio, sucursalTipo, barrio, fecha, comercioRazonSocial)
precios_barrio_sucursal$fecha=as.Date(precios_barrio_sucursal$fecha, format="%m/%d/%y")
precios_barrio = precios_barrio_sucursal%>%group_by(barrio, fecha=floor_date(fecha, "15 days"))%>%summarize(precio=mean(precio))
precios_sucursal_tipo = precios_barrio_sucursal%>%group_by(sucursalTipo, fecha=floor_date(fecha, "15 days"))%>%summarize(precio=mean(precio))
precios_cadena = precios_barrio_sucursal%>%group_by(comercioRazonSocial, fecha=floor_date(fecha, "15 days"))%>%summarize(precio=mean(precio))

ggplot(precios_barrio, aes(x=fecha, y=precio, group=barrio)) +
  geom_line(aes(colour=barrio)) +
  geom_point(aes(colour = barrio), size = 1)

# Grafico de la media de todos los precios agurpados por tipo de sucursal
ggplot(precios_sucursal_tipo, aes(x=fecha, y=precio, group=sucursalTipo)) +
  geom_line(aes(colour=sucursalTipo)) +
  geom_point(size = 1, aes(colour = sucursalTipo)) +
  #stat_smooth(aes(x = fecha, y = precio), method = "lm", formula = y ~ poly(x, 4), se = TRUE) +
  ylab('Precio medio')+xlab('Fecha') +
  labs(colour = "Tipo de sucursal") +
  labs(title="Precios medio por tipo de sucursal")

# Grafico de la media de todos los precios agurpados por cadena
ggplot(precios_cadena, aes(x=fecha, y=precio, group=comercioRazonSocial)) +
  geom_line(aes(colour=comercioRazonSocial)) +
  geom_point(size = 1, aes(colour = comercioRazonSocial)) +
  #stat_smooth(aes(x = fecha, y = precio), method = "lm", formula = y ~ poly(x, 4), se = TRUE) +
  ylab('Precio medio')+xlab('Fecha') +
  labs(colour = "Cadena") +
  labs(title="Precios medio por cadena")

# Grafico de la evolucion del precio del dolar y la inflacion
ggplot() + geom_line(aes(x=inflacion$fecha, y=inflacion$porcentaje, colour="Inflacion")) +
  geom_line(aes(x=dolar_1_mes$fecha, y=dolar_1_mes$valor, group=dolar_1_mes$operacion, colour=dolar_1_mes$operacion)) +
  ylab('Valores')+xlab('Fecha') +
  labs(colour = "Referencia") +
  labs(title="Comparativa de las curvas de variacion del dolar y la inflacion mes a mes")

# ---------------------------PRECIOS POR BARRIO----------------------------------- #
# Calcula el promedio de precio de un producto agrupado por barrio
precios_prod_agrupado_barrio <- function(id_producto) {
  result = precios_por_barrio(id_producto, precios, sucursales_con_datos)
  result = precios_barrio_sucursal %>% group_by(barrio) %>% summarize(precio=mean(precio))
  return (result)
}

precios_prod_agrupado_barrio("7790580470005")
NROW(precios_prod_agrupado_barrio("7790580470005"))
ggplot(precios_prod_agrupado_barrio("7790580470005"), aes(x=barrio, y=precio))+ geom_point()

# Calcula el promedio de todos los precios agrupados por barrio
precios_prod_agrupado_barrio <- function() {
  sucus =  precios%>%select(sucursal, precio, producto, fecha)
  result = left_join(sucus, sucursales_con_datos, by=c("sucursal" = "id"))%>%select(precio, sucursalTipo, barrio, fecha)%>% group_by(barrio) %>% summarize(precio=mean(precio))
  return (result)
}

precios_prod_agrupado_barrio()
NROW(precios_prod_agrupado_barrio())
ggplot(precios_prod_agrupado_barrio(), aes(x=barrio, y=precio))+ geom_point()

ggplot(precios_prod_agrupado_barrio(), aes(x=barrio, y=precio))+ geom_point()
#write.csv(barrios_sucursales2,"/home/ignacio/datos/facultad/repos/datamining/Tp1/data/graficos/todosLosBarrios.csv", row.names = FALSE)

# -------------------- Calcula el promedio de precios en el tiempo --------------- #
getAvgPrecioFecha <- function(intervalo) {
  AvgPrecioFecha=precios%>%select(precio,fecha)
  AvgPrecioFecha$fecha=as.Date(AvgPrecioFecha$fecha, format="%m/%d/%y")
  AvgPrecioFecha = AvgPrecioFecha%>% group_by(fecha=floor_date(fecha, intervalo)) %>%
    summarize(precio=mean(precio))
  return (AvgPrecioFecha)
}

# Precio medio vs evolucion del dolar cada 7 dias
avgPrecioFecha = getAvgPrecioFecha("7 days")
ggplot() + 
  #geom_line(aes(x=inflacion$fecha, y=inflacion$porcentaje, colour="Inflacion")) +
  geom_line(aes(x=dolar_7_dias$fecha, y=dolar_7_dias$valor, group=dolar_7_dias$operacion, colour=dolar_7_dias$operacion)) +
  geom_line(aes(x=avgPrecioFecha$fecha, y=avgPrecioFecha$precio, colour="Precio medio")) +
  ylab('Valores')+xlab('Fecha') +
  labs(colour = "Referencia") +
  labs(title="Evolución del dolar y el promedio de los precios cada 7 días")

# Tendencia del precio medio dia a dia
avgPrecioFecha = getAvgPrecioFecha("1 day")
ggplot(avgPrecioFecha, aes(x=fecha, y=precio)) +
  geom_line(color="dark green") +
  geom_point(color="dark green", shape=7) +
  labs(title="Tendencia de los precios en CABA día a día") +
  stat_smooth(aes(x = avgPrecioFecha$fecha, y = avgPrecioFecha$precio), method = "lm", formula = y ~ poly(x, 3), se = TRUE)
#ggsave("/home/ignacio/datos/facultad/repos/datamining/Tp1/data/graficos/tendenciaPreciosAVG.png")

# Evolucion del dolar e inflacion
ggplot() + 
  geom_line(aes(x=inflacion$fecha, y=inflacion$porcentaje, colour="Inflacion")) +
  geom_line(aes(x=dolar_1_mes$fecha, y=dolar_1_mes$valor, group=dolar_1_mes$operacion, colour=dolar_1_mes$operacion)) +
  ylab('Valores')+xlab('Fecha') +
  labs(colour = "Referencia") +
  labs(title="Evolución del dolar y la inflación por mes")

# Grafico de la inflacion mensual desde septiembre 2018 hasta abril 2019
ggplot(inflacion, aes(x=fecha, y=porcentaje)) +
  geom_point(size = 2, colour="blue", shape=8) + 
  ylab('Porcentaje')+xlab('Fecha') +
  labs(title="Inflación mes a mes") +
  #geom_line(colour="blue") + 
  stat_smooth(aes(x = fecha, y = porcentaje), method = "lm", formula = y ~ poly(x, 6), se = FALSE)

# -------------------------------------------------------------------------------- #

#Dado un id producto me da la lista de precios
getPrecioFecha <- function(idProducto) {
  preciosFecha=precios%>%filter(producto==idProducto)%>%select(precio,fecha)
  preciosFecha$fecha=as.Date(preciosFecha$fecha, format="%m/%d/%y")
  return (preciosFecha)
}

# --------------------------------- Outliers ------------------------------------- #
# Box plot para todos los graficos, se observan outliers con precios altos
boxplot(sort(precios$precio, decreasing = FALSE))

# Calculo de cuantiles y rango intercuartil
cuantiles<-quantile(precios$precio, c(0.25, 0.5, 0.75), type = 7)
riq = cuantiles[3] - cuantiles[1]
outliers_min<-as.numeric(cuantiles[1])-1.5*riq
outliers_max<-as.numeric(cuantiles[3])+1.5*riq

# Box plot para los precios dentro del rango intercuartil, se siguen observando outliers con precios altos
boxplot(sort(precios$precio[precios$precio>outliers_min & precios$precio<outliers_max], decreasing = FALSE))

# Calculo el desvio para ver la existencia de outliers con datos hasta 3 veces mayores o menores
desvio<-sd(precios$precio)
outliers_min_sd<-as.numeric(cuantiles[1])-3*desvio
outliers_max_sd<-as.numeric(cuantiles[3])+3*desvio
boxplot(sort(precios$precio[precios$precio>outliers_min_sd & precios$precio<outliers_max_sd], decreasing = FALSE))

# La cantidad de precios tomados mayores que el máximo outlier permitido
nrow(precios%>%filter(precio>outliers_max))*100/nrow(precios)
nrow(precios%>%filter(precio>outliers_max_sd))*100/nrow(precios)

# Teniendo en cuenta los outliers con respecto a 3 veces la media, grafico box plot solo con esos precios
precios_mayores_desvio = precios%>%filter(precio>outliers_max_sd)
boxplot(sort(precios_mayores_desvio$precio, decreasing = FALSE))
# -------------------------------------------------------------------------------- #


#Calculo la lista de precios y la media del precio agrupando por la cantidad de dias que indique.
# idProducto: id del producto que quiero buscar
# paso: cantidad de dias por los que quiero agrupar ("2 days", "week", etc)
getInfo2Graph <- function(idProducto, paso){
    prodxDias = getPrecioFecha(idProducto) %>% group_by(fecha=floor_date(fecha, paso)) %>%
    summarize(precio=mean(precio))
  return (prodxDias)
}


paso="3 days"
# -------------------ANALISIS DE PRECIO DE LA CANASTA BASICA---------------------- #
#Fideos Tallarin Don Vicente 500 Gr: 7790070318114
#Arroz Gallo Oro 1 Kg: 7790070411716
#Harina de Trigo 000 Cañuelas 1 Kg: 7792180001528
#Polenta Instantanea Magica Quaker 500 Gr: 7792170007196
#Queso Crema Untable Casancrem Clasico 500 Gr: 7791337011304
#Tomate Pelado Perita en Lata Arcor 400 Gr: 7790580567903
#Mermelada de Durazno Frasco Arcor 454 Gr: 7790580509507
#Aceite de Girasol Cocinero 900 Ml: 7790070012050
#Yogur Bebible Entero Frutilla Yogurisimo 1 Lt: 7791337965621
#Te en Saquitos Green Hills 50 Un: 7790480008261
ggplot(getInfo2Graph("7790580509507",paso),aes(x=fecha, y=precio, colour="Precio"))+geom_point()+geom_line()+
  labs(title="Fideos Tallarin Don Vicente 500 Gr")+
  stat_smooth(aes(x=fecha, y=precio), method = "lm", formula = y ~ poly(x, 3), se = FALSE)


ggplot() + geom_line(aes(x=getInfo2Graph("7790070318114",paso)$fecha, y=getInfo2Graph("7790070318114",paso)$precio, colour="Fideos")) + 
           geom_line(aes(x=getInfo2Graph("7790070411716",paso)$fecha, y=getInfo2Graph("7790070411716",paso)$precio, colour="Arroz")) + 
           geom_line(aes(x=getInfo2Graph("7792180001528",paso)$fecha, y=getInfo2Graph("7792180001528",paso)$precio, colour="Harina")) + 
           geom_line(aes(x=getInfo2Graph("7792170007196",paso)$fecha, y=getInfo2Graph("7792170007196",paso)$precio, colour="Polenta")) + 
           geom_line(aes(x=getInfo2Graph("7791337011304",paso)$fecha, y=getInfo2Graph("7791337011304",paso)$precio, colour="Queso")) + 
           geom_line(aes(x=getInfo2Graph("7790580567903",paso)$fecha, y=getInfo2Graph("7790580567903",paso)$precio, colour="Tomate")) + 
           geom_line(aes(x=getInfo2Graph("7790580509507",paso)$fecha, y=getInfo2Graph("7790580509507",paso)$precio, colour="Mermelada")) + 
           geom_line(aes(x=getInfo2Graph("7790070012050",paso)$fecha, y=getInfo2Graph("7790070012050",paso)$precio, colour="Aceite")) + 
           geom_line(aes(x=getInfo2Graph("7791337965621",paso)$fecha, y=getInfo2Graph("7791337965621",paso)$precio, colour="Yogur")) +
           geom_line(aes(x=getInfo2Graph("7790480008261",paso)$fecha, y=getInfo2Graph("7790480008261",paso)$precio, colour="Te")) +
           ylab('Precios')+xlab('Fecha')+
           labs(title="Evolucion de precios canasta Basica")
# -------------------------------------------------------------------------------- #

# -------------------ANALISIS DE PRECIO CANASTA NAvideña---------------------- #
#Turron Arcor 25 Gr 0000077940131
#Vino Espumante Extra Brut Chandon 750 Ml 7790975000183
#Coca Cola Sabor Original 2.25 Lt 7790895000997
#Cerveza Rubia Cristal Quilmes 1 Lt 7792798007387
#Mani Salado Pelado KrachItos 120 Gr 7794520412216
#Palitos Salados Queso Pehuamar 200 Gr 7790310004357


ggplot(getInfo2Graph("0000077940131",paso),aes(x=fecha, y=precio))+
  labs(title="Turron Arcor 25 Gr")+
  stat_smooth(aes(x=fecha, y=precio), method = "lm", formula = y ~ poly(x, 3), se = FALSE)
ggplot(getInfo2Graph("7790975000183",paso),aes(x=fecha, y=precio))+
  labs(title="Vino Espumante Extra Brut Chandon 750 Ml")+
  stat_smooth(aes(x=fecha, y=precio), method = "lm", formula = y ~ poly(x, 3), se = FALSE)
ggplot(getInfo2Graph("7790895000997",paso),aes(x=fecha, y=precio))+
  labs(title="Coca Cola Sabor Original 2.25")+
  stat_smooth(aes(x=fecha, y=precio), method = "lm", formula = y ~ poly(x, 3), se = FALSE)
ggplot(getInfo2Graph("7792798007387",paso),aes(x=fecha, y=precio))+
  labs(title="Cerveza Rubia Cristal Quilmes 1 Lt")+
  stat_smooth(aes(x=fecha, y=precio), method = "lm", formula = y ~ poly(x, 3), se = FALSE)
ggplot(getInfo2Graph("7794520412216",paso),aes(x=fecha, y=precio))+
  labs(title="Mani Salado Pelado KrachItos 120 Gr")+
  stat_smooth(aes(x=fecha, y=precio), method = "lm", formula = y ~ poly(x, 3), se = FALSE)
ggplot(getInfo2Graph("7790310004357",paso),aes(x=fecha, y=precio))+
  labs(title="Palitos Salados Queso Pehuamar 200 Gr")+
  stat_smooth(aes(x=fecha, y=precio), method = "lm", formula = y ~ poly(x, 3), se = FALSE)


# -------------------------------------------------------------------------------- #
# -------------------ANALISIS DE PRECIO CERVEZA---------------------- #
#Cerveza Rubia Iguana 970 Cc 7792798000708
#Cerveza Rubia Stella Artois 970 Cc 7792798006199
#Cerveza Rubia Chopp Brahma 1 Lt 7792798007493
#Cerveza Rubia Cristal Quilmes 1 Lt 7792798007387
#Cerveza Rubia Lager Imperial 1 Lt 7793147000981
#Cerveza Rubia Miller Genuine Draft 1 Lt 7793147570088
#Cerveza Rubia Retornable Budweiser 1 Lt 7793147001056
#Cerveza Rubia Retornable Schneider 1 Lt 7793147001025
#Cerveza Rubia Retornable Heineken 1 Lt 7793147000899
#Cerveza Rubia Isenbeck 970 Cc 7795697001708

ggplot() + geom_line(aes(x=getInfo2Graph("7792798000708",paso)$fecha, y=getInfo2Graph("7792798000708",paso)$precio, colour="Iguana")) + 
  geom_line(aes(x=getInfo2Graph("7792798006199",paso)$fecha, y=getInfo2Graph("7792798006199",paso)$precio, colour="Stella Artois")) + 
  geom_line(aes(x=getInfo2Graph("7792798007493",paso)$fecha, y=getInfo2Graph("7792798007493",paso)$precio, colour="Brahma")) + 
  geom_line(aes(x=getInfo2Graph("7792798007387",paso)$fecha, y=getInfo2Graph("7792798007387",paso)$precio, colour="Quilmes")) + 
  geom_line(aes(x=getInfo2Graph("7793147000981",paso)$fecha, y=getInfo2Graph("7793147000981",paso)$precio, colour="Imperial")) + 
  geom_line(aes(x=getInfo2Graph("7793147570088",paso)$fecha, y=getInfo2Graph("7793147570088",paso)$precio, colour="Miller")) + 
  geom_line(aes(x=getInfo2Graph("7793147001056",paso)$fecha, y=getInfo2Graph("7793147001056",paso)$precio, colour="Budweiser")) + 
  geom_line(aes(x=getInfo2Graph("7793147001025",paso)$fecha, y=getInfo2Graph("7793147001025",paso)$precio, colour="Schneider")) + 
  geom_line(aes(x=getInfo2Graph("7793147000899",paso)$fecha, y=getInfo2Graph("7793147000899",paso)$precio, colour="Heineken")) +
  geom_line(aes(x=getInfo2Graph("7795697001708",paso)$fecha, y=getInfo2Graph("7795697001708",paso)$precio, colour="Isenbeck")) +
  ylab('Precios')+xlab('Fecha')+
  labs(title="Evolucion de precios Cerveza 1 Lt")
#ggsave("/home/ignacio/datos/facultad/repos/datamining/Tp1/data/graficos/preciosCerveza.png")

prodCerveza = productos%>%filter(str_detect(nombre,"^Cerveza"))%>%filter(str_detect(nombre,"1 Lt") | str_detect(nombre,"970")) #%>%filter(str_detect(nombre,"970 Cc"))  
barriosBirra =  left_join(prodCerveza,precios%>%select(sucursal, precio, producto, fecha), by=c("id"="producto"))
barriosBirra = left_join(barriosBirra, sucursales_con_datos, by=c("sucursal" = "id"))%>%select(precio, sucursalTipo, barrio, fecha)%>% group_by(barrio) %>% summarize(precio=mean(precio))
nrow(barriosBirra)
ggplot(barriosBirra, aes(x=barrio, y=precio))+ geom_point()

# -------------------------------------------------------------------------------- #
###### ###### ANALISIS DE PRECIO DEL PRODUCTO DE MAYOR; MENOR Y PROMEDIO ###### ###### 
ggplot(getInfo2Graph("7790975000183",paso),aes(x=fecha, y=precio))+geom_point()+geom_line()+
       labs(title="Producto de Menor Precio")

ggplot(getInfo2Graph("7791250001994",paso),aes(x=fecha, y=precio))+geom_point()+geom_line()+
       labs(title="Producto de Mayor Precio")

ggplot(getInfo2Graph("7790580470005",paso),aes(x=fecha, y=precio))+geom_point()+geom_line()+
       labs(title="Producto de Precio Promedio")


ggplot() + geom_line(aes(x=getInfo2Graph("7790975000183",paso)$fecha, y=getInfo2Graph("7790975000183",paso)$precio), color='blue') + 
           geom_line(aes(x=getInfo2Graph("7791250001994",paso)$fecha, y=getInfo2Graph("7791250001994",paso)$precio), color='red') +
           geom_line(aes(x=getInfo2Graph("7790580470005",paso)$fecha, y=getInfo2Graph("7790580470005",paso)$precio), color='yellow') +
           ylab('Precios')+xlab('Fecha')+
           labs(title="Comparativa entre producto de precio minimo, maximo, promedio")

# -------------------------------------------------------------------------------- #

# Producto con el precio mas bajo
precios%>%filter(precio==2.85)
productos%>%filter(id=="7622300861049")
preciosJugoVerao=precios%>%filter(producto=="7622300861049")%>%select(precio,fecha)
preciosJugoVerao$fecha=as.Date(preciosJugoVerao$fecha, format="%m/%d/%y")
ggplot(preciosJugoVerao,aes(x=fecha, y=precio))+geom_point(size=0.5, colour="red")

# Producto con el precio mas alto
precios%>%filter(precio==693)
productos%>%filter(id=="7791250001994")
preciosJyB=precios%>%filter(producto=="7791250001994")%>%select(precio,fecha)
preciosJyB$fecha=as.Date(preciosJyB$fecha, format="%m/%d/%y")
ggplot(preciosJyB,aes(x=fecha, y=precio))+geom_point(size=0.5, colour="red")

# Productos con el precio en la mediana
precios%>%filter(precio==62.9) #ojo hay mas de uno
productos%>%filter(id=="7790580470005")
preciosMediana=precios%>%filter(producto=="7790580470005")%>%select(precio,fecha)
preciosMediana$fecha=as.Date(preciosMediana$fecha, format="%m/%d/%y")
ggplot(preciosMediana,aes(x=fecha, y=precio))+geom_point(size=0.5, colour="red")
