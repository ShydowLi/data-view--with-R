#用maps包直接映射中国地图
library(mapdata)
library(maps)
library(ggplot2)
china1<-map_data('china')
p1<-ggplot(china1,aes(long,lat,group=group))+geom_polygon()
