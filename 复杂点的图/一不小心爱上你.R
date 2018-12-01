data<-data.frame(Name = c("苹果","谷歌","脸书","亚马逊","腾讯"),Conpany = c("Apple","Google","Facebook","Amozon","Tencent"),Sale2013 = c(5000,3500,2300,2100,3100),Sale2014 = c(5050,3800,2900,2500,3300),Sale2015 = c(5050,3800,2900,2500,3300),Sale2016 = c(5050,3800,2900,2500,3300))

dat1<-gather(data,year,sale,-Name,-Conpany)
p1<-ggplot(dat1,aes(Name,sale,fill=factor(year)))+geom_bar(stat = 'identity',position = 'dodge',width = 0.3)+facet_grid(year~.)
p1+coord_flip()+theme_economist()+scale_fill_economist()+ggtitle('各个互联网公司的年销售额',subtitle = 'i have a dream')


#环比图分析（比较）
Rellglon<-data.frame(rell=c("protestant","catholic","mormon","orther christian","jewish","other religon","no religion"),clinton=c(37,45,25,43,71,58,68),trump=c(60,52,61,55,24,33,26))
rell<-gather(Rellglon,Name,level,-rell)
p2<-ggplot(rell,aes(rell,level,fill=Name))+geom_bar(stat = 'identity',width = 0.8)
p2+coord_polar(theta = 'x',start = 0)+facet_wrap(Name~.)
p2+coord_polar(theta = 'x',start = 0)+facet_wrap(Name~.)+theme(axis.title = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())
