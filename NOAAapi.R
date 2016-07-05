library(reshape2)
library(httr)
library(jsonlite)

# ur <- "http://www.ncdc.noaa.gov/cdo-web/api/v2/stations?locationid=FIPS:37&&datatypeid=TMAX&datatypeid=TMIN&datatypeid=TAVG&sortfield=name&sortorder=asc&limit=100"
# req <- GET(ur, add_headers("token" = "TDuBkJdeeDDBILwDWWxiaUcQxtoxeFNu"))
# json <- content(req, as = "text")
# out <- fromJSON(json)$results
# out
# write.csv(out,"stations.csv")

sta <- read.csv("stations.csv")


# dataset id NORMAL_DLY
# req <- GET(paste0("http://www.ncdc.noaa.gov/cdo-web/api/v2/data?datasetid=NORMAL_DLY&stationid=GHCND:USC00311677&startdate=2010-01-01&enddate=2010-12-31&limit=1000&datatypeid=DLY-TMAX-NORMAL,DLY-TMIN-NORMAL"), add_headers("token" = "TDuBkJdeeDDBILwDWWxiaUcQxtoxeFNu"))
# fromJSON(content(GET("http://www.ncdc.noaa.gov/cdo-web/api/v2/stations?datasetid=NORMAL_DLY&datacategoryid=TEMP&locationid=FIPS:37&datatypeid=DLY-TMAX-NORMAL,DLY-TMIN-NORMAL",add_headers("token" = "TDuBkJdeeDDBILwDWWxiaUcQxtoxeFNu")),as="text"))


stn <- sta$id[47]
req <- GET(paste0("http://www.ncdc.noaa.gov/cdo-web/api/v2/data?datasetid=GHCND&stationid=",stn,"&startdate=2016-01-01&enddate=2016-12-31&datatypeid=TMAX,TMIN&limit=1000"), add_headers("token" = "TDuBkJdeeDDBILwDWWxiaUcQxtoxeFNu"))
json <- content(req, as = "text")
dfd <- fromJSON(json)

dfd

dfdd <- dcast(dfd$results,date~datatype,value.var="value")

jday <- julian(as.Date(do.call(rbind,strsplit(dfdd$date,"T"))[,1]),origin=as.Date(paste0(substr(dfdd$date[1],1,4),"-01-01")))+1
mdtmp <- apply(dfdd[,2:3],1,mean)
cutpts <- cut(mdtmp,breaks=100)
ptcol <- rev(rainbow(100,end=4/6))[as.numeric(cutpts)]
#horizontal plot
#plot(0,0,type="n",ylim=c(-20,50),xlim=c(1,365))
#segments(1:nrow(dfdd),dfdd$TMIN/10,1:nrow(dfdd),dfdd$TMAX/10,col=ptcol)
offset <- 30 # center temperature offset
dfdds <- dfdd[,2:3] + offset*10
#phi <- seq(0,2*pi,length.out=365)[c(92:1,365:93)] # if all days present
phi <- seq(0,2*pi,length.out=365)[match(jday,c(92:1,365:93))] # to accomodate missing days

xl <- cos(phi)*dfdds$TMIN/10
yl <- sin(phi)*dfdds$TMIN/10
xh <- cos(phi)*dfdds$TMAX/10
yh <- sin(phi)*dfdds$TMAX/10

# xl <- cos(phi)*dfdds$'DLY-TMIN-NORMAL'/10
# yl <- sin(phi)*dfdds$'DLY-TMIN-NORMAL'/10
# xh <- cos(phi)*dfdds$'DLY-TMAX-NORMAL'/10
# yh <- sin(phi)*dfdds$'DLY-TMAX-NORMAL'/10





par(mar=c(0,0,0,0))
plot(0,0,type="n",ylim=c(-max(dfdds)/10,max(dfdds)/10),xlim=c(-max(dfdds)/10,max(dfdds)/10))
segments(xl,yl,xh,yh,col=ptcol,lwd=1)
text(0,0,"Durham, NC",offset=-0.5,pos=3)
#abline(h=0);abline(v=0)
phix <- seq(0,2*pi,length.out=100)

for(v in unique(c(seq(0,min(dfdd[,2:3])/10,by=20),seq(0,max(dfdd[,2:3])/10,by=20)))){
  polygon(cos(phix)*(v+offset),sin(phix)*(v+offset),border="#00000020")
  text(v+offset,0,paste0(v,"°C"),pos=3,offset=-0.5)    
}


polygon(cos(seq(0,2*pi,length.out=100))*(0+offset),sin(phi)*(0+offset),border="#00000020")
polygon(cos(phi)*(20+offset),sin(phi)*(20+offset),border="#00000020")
polygon(cos(phi)*(40+offset),sin(phi)*(40+offset),border="#00000020")
text(c(0,20,40)+offset,rep(0,3),c("0°C","20°C","40°C"),pos=3,offset=-0.5)
