#tes##
Sys.setlocale(category = "LC_CTYPE", locale = "UTF-8") 

library(httr)
library(stringr)
library(rvest)
library(data.table)
library(lubridate)
library(date)
require(rCharts)
require(ggplot2) 
library(plyr)

url = "https://www.ptt.cc/bbs/MacShop/index.html"

res = GET(url) %>% 
  content(as = "text") %>% 
  read_html(res)
res_href = res %>% 
  html_nodes(xpath = '//*[@id="action-bar-container"]/div/div[2]/a[2]') %>% 
  html_attr("href")
tmpIndex = res_href %>% 
  str_extract('[0-9]+') %>% 
  (function(x){as.numeric(x)+1})
index=sprintf("https://www.ptt.cc/bbs/MacShop/index%s.html",(tmpIndex-100):(tmpIndex))

subindex=c()
for (i in 1:length(index)){
  res = GET(index[i]) %>% 
    content(as = "text") %>% 
    read_html(res)
  res_href = res %>% 
    html_nodes(xpath ="//div[@class='r-list-container bbs-screen']//a" ) %>% 
    html_attr("href")
  tmpIndex = res_href %>% 
    gsub('/bbs/MacShop/*','',.) 
  index2=as.character(sprintf("https://www.ptt.cc/bbs/MacShop/%s",tmpIndex))
  subindex=c(subindex,index2)
  if(i%%10==0){
    Sys.sleep(1)
  }
}
length(subindex)

data=data.frame(Title=NA, Date=NA, Prize=NA, URL=NA)
for (i in 1:length(subindex)){
  URL=subindex[i]
  res_text = GET(URL) %>% 
    read_html(res_text, encoding = "UTF-8") %>% 
    html_nodes(xpath ="//div[@id='main-content']" ) %>% 
    html_text()
  text=unlist(strsplit(res_text,'\n'));text
  
  res_date = GET(URL) %>% 
    read_html(res_date, encoding = "UTF-8") %>% 
    html_nodes(xpath ="//div[@class='article-metaline'][3]" ) %>% 
    html_text()
  
  res_title = GET(URL) %>% 
    read_html(res_title, encoding = "UTF-8") %>% 
    html_nodes(xpath ="//div[@class='article-metaline'][2]" ) %>% 
    html_text()
  
  if(identical(integer(0),grep('交易價格', text))==TRUE){
    'DO_NOTHING'}else{
    data=rbind(data,c(res_title, res_date, grep('交易價格', text,value=TRUE),URL))
    }
  if(i%%8==0){
    Sys.sleep(2)
  }
}

data=as.data.frame(data)
write.csv(x=data,file=paste('iphone6s_',Sys.Date(),'.csv',sep=''))
#data=read.csv(paste('iphone6s_',Sys.Date(),'.csv',sep=''),colClasses = c('character','character','character','character','character'))
#data=read.csv('macshop.csv',colClasses = c('character','character','character','character','character'))
#data2=read.csv('macshop2.csv',colClasses = c('character','character','character','character','character'))
#data=rbind(data,data2)
data=data[,-1]
str(data)

select=data[grep('6s',data[,1]),]
select=select[grep('plus',select[,1],invert=TRUE),]
select=select[grep('Plus',select[,1],invert=TRUE),]
select=select[grep('PLUS',select[,1],invert=TRUE),]
select=select[grep('6s\\+',select[,1],invert=TRUE),]
dim(select)

#select=data
select$Date=gsub('時間','',select$Date) %>%
  strptime(.,'%a %b %e %H:%M:%S %Y')
select$Prize=as.numeric(gsub('[^0-9]','',select$Prize))
select=select[select$Prize>10000&select$Prize<40000,]
dim(select)

#select=read.csv('iphone6s.csv', encoding = 'UTF-8' )

select$Date=as.character(select$Date) %>%
  gsub('\\-','/', .) %>%
  subst r(.,3,20)%>%
  strptime(., '%x %X')

select$D.value= as.numeric(difftime(select$Date,"2015-10-01 00:00:00"))
select=na.omit(select)

i64=select[grep('64',select[,1]),];dim(i64)
i16=select[grep('16',select[,1]),];dim(i16)
i128=select[grep('128',select[,1]),];dim(i128)

#i128=i128[-c(3,32,33),]
 #i16=i16[-c(57,59),]
 #i64=i64[-(641),]

reg=subset(i64, select=c(Prize,D.value))
model=lm(Prize ~ D.value, data = reg)
reg=subset(fortify(model),select=c(Prize,D.value,.fitted))
names(reg)[3]='FIT'
i64=  join(i64,reg);head(i64)

reg=subset(i128, select=c(Prize,D.value))
model=lm(Prize ~ D.value, data = reg)
reg=subset(fortify(model),select=c(Prize,D.value,.fitted))
names(reg)[3]='FIT'
i128=  join(i128,reg);head(i128)

reg=subset(i16, select=c(Prize,D.value))
model=lm(Prize ~ D.value, data = reg)
reg=subset(fortify(model),select=c(Prize,D.value,.fitted))
names(reg)[3]='FIT'
i16=  join(i16,reg);head(i16)

dim(i16)
dim(i128)
dim(i64)

# 
# library(ggplot2)
# g=ggplot(data=select,aes(x=Date,y=Prize,group=1))+geom_point(colour='red',alpha=.8)+stat_smooth(method='glm')+geom_line()+
#   scale_y_continuous(breaks=seq(0,35000,1000),limits=c(10000,35000))
# g+scale_x_datetime(breaks="2 weeks")
# 
# ggsave(filename = 'iphon6s.png',plot = g,scale=2)
# write.csv(x = select,file='iphone6s.csv',fileEncoding =  'UTF-8')
# write.csv(x = D,file='macshop.csv',fileEncoding =  'UTF-8')
str(i16)
i16$D2=as.numeric(i16$Date)*1000
i64$D2=as.numeric(i64$Date)*1000
i128$D2=as.numeric(i128$Date)*1000

p16=subset(i16,select=c('D2','Prize','Title','URL'))
p64=subset(i64,select=c('D2','Prize','Title','URL'))
p128=subset(i128,select=c('D2','Prize','Title','URL'))

l16=subset(i16,select=c('D2','FIT'))
l64=subset(i64,select=c('D2','FIT'))
l128=subset(i128,select=c('D2','FIT'))

names(p16)=c('x','y','z','u')
names(p64)=c('x','y','z','u')
names(p128)=c('x','y','z','u')
names(l16)=c('x','y')
names(l64)=c('x','y')
names(l128)=c('x','y')

hm <- rCharts:::Highcharts$new()
hm$series(data = toJSONArray2(l16, json = F,names=T), name = "16G",type = c("line"), marker = list(radius = 0))
hm$series(data = toJSONArray2(l64, json = F,names=T), name = "64G",type = c("line"), marker = list(radius = 0))
hm$series(data = toJSONArray2(l128, json = F,names=T), name = "128G",type = c("line"), marker = list(radius = 0))
hm$series(data = toJSONArray2(p16, json = F,names=T), name = "16G",type = c("scatter"), marker = list(radius = 3))
hm$series(data = toJSONArray2(p64, json = F,names=T), name = "64G",type = c("scatter"), marker = list(radius = 3))
hm$series(data = toJSONArray2(p128, json = F,names=T), name = "128G",type = c("scatter"), marker = list(radius = 3))
hm$colors('rgba(223, 83, 83, .5)', 'rgba(119, 152, 191, .5)', 'rgba(60, 179, 113, .5)',
          'rgba(223, 83, 83, .5)', 'rgba(119, 152, 191, .5)', 'rgba(60, 179, 113, .5)')
hm$plotOptions(column = list(stacking = "normal", pointPadding = 0, groupPadding = 0, borderWidth = 0))
hm$tooltip(borderWidth=0, followPointer=TRUE, followTouchMove=TRUE, shared = FALSE,formatter = "#! function(){return this.point.z + '<br>'+ 'Prize:' + this.point.y ;} !#")
hm$plotOptions(
  scatter = list(
    cursor = "pointer", 
    point = list(
      events = list(
        click = "#! function() { window.open(this.options.u); } !#")), 
    marker = list(
      symbol = "circle", 
      radius = 10
    )
  )
)
hm$xAxis(type = 'datetime', labels = list(
  format = '{value:%Y-%m-%d}' 
))
hm$save(paste('iphone6s_',Sys.Date(),'.html',sep=''),standalone=TRUE)
browseURL(paste('iphone6s_',Sys.Date(),'.html',sep=''))
hm$publish(paste('iphone6s_',Sys.Date(),'.html',sep=''), host ='rpubs')
#hm$publish('iPhone6s_PrizeTrend', host ='rpubs', user='Poissonfish')
