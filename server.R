library(httr)
library(jsonlite)
library(leaflet)
library(reshape2)
library(shiny)
library(sp)
sta <- read.csv("stations.csv")
offset <- 30 # center temperature offset
phix <- seq(0,2*pi,length.out=100)

m <- leaflet() %>% addTiles() %>% addMarkers(data=sta[,c("longitude","latitude")], 
                 popup=sta[,"name"], 
                 layerId=sta[,"id"])

shinyServer(function(input, output, session) {
  
  output$leafmap <- renderLeaflet(m)
  
  site <- reactive({
    if(!is.null(input$leafmap_marker_click)) site <- as.character(input$leafmap_marker_click$id)
    else NULL
  })
  
  observeEvent(input$leafmap_marker_click,{
    yrs <- seq(as.numeric(substr(sta$mindate[which(sta$id==site())],1,4)),as.numeric(substr(sta$maxdate[which(sta$id==site())],1,4)))
    updateSelectInput(session, "year",
                      choices = yrs,
                      selected = yrs[length(yrs)]
    )
  })
  
  dat <- eventReactive(input$getdat,{
    req <- GET(paste0("http://www.ncdc.noaa.gov/cdo-web/api/v2/data?datasetid=GHCND&stationid=",site(),"&startdate=",input$year,"-01-01&enddate=",input$year,"-12-31&datatypeid=TMAX,TMIN&limit=1000"), add_headers("token" = "TDuBkJdeeDDBILwDWWxiaUcQxtoxeFNu"))
    json <- content(req, as = "text")
    dfd <- fromJSON(json)
    sitename <- unlist(strsplit(as.character(sta$name[which(sta$id==site())]),","))[1]
    dfdd <- dcast(dfd$results,date~datatype,value.var="value")
    jday <- julian(as.Date(do.call(rbind,strsplit(dfdd$date,"T"))[,1]),origin=as.Date(paste0(substr(dfdd$date[1],1,4),"-01-01")))+1
    mdtmp <- apply(dfdd[,2:3],1,mean)
    cutpts <- cut(mdtmp,breaks=100)
    ptcol <- rev(rainbow(100,end=4/6))[as.numeric(cutpts)]
    #horizontal plot
    #plot(0,0,type="n",ylim=c(-20,50),xlim=c(1,365))
    #segments(1:nrow(dfdd),dfdd$TMIN/10,1:nrow(dfdd),dfdd$TMAX/10,col=ptcol)
    dfdds <- dfdd[,2:3] + offset*10
    #phi <- seq(0,2*pi,length.out=365)[c(92:1,365:93)] # if all days present    
    phi <- seq(0,2*pi,length.out=365)[match(jday,c(92:1,365:93))] # to accomodate missing days

    xl <- cos(phi)*dfdds$TMIN/10
    yl <- sin(phi)*dfdds$TMIN/10
    xh <- cos(phi)*dfdds$TMAX/10
    yh <- sin(phi)*dfdds$TMAX/10
    
    list('xl'=xl,'yl'=yl,'xh'=xh,'yh'=yh,'dfdds'=dfdds,'ptcol'=ptcol,'phi'=phi,'phix'=phix,'stnm'=sitename)
  })

  output$plt <- renderPlot({
    dd <- dat()
    par(mar=c(0,1,2,1))
    plot(0,0,type="n",ylim=c(-max(dd$dfdds)/10,max(dd$dfdds)/10),xlim=c(-max(dd$dfdds)/10,max(dd$dfdds)/10),main=dd$stnm,bty="n",xaxt="n",yaxt="n")
    segments(dd$xl,dd$yl,dd$xh,dd$yh,col=dd$ptcol,lwd=2)
    #text(0,0,dd$stnm,offset=-0.5,pos=3)
    #abline(h=0);abline(v=0)
    for(v in unique(c(seq(0,(min(dd$dfdds)/10-offset),by=-20),seq(0,(max(dd$dfdds)/10-offset),by=20)))){
      polygon(cos(dd$phix)*(v+offset),sin(dd$phix)*(v+offset),border="#00000020")
      text(v+offset,0,paste0(v,"°C"),pos=3,offset=-0.5)    
    }
    for(v in unique(c(seq(0,(min(dd$dfdds)/10-offset),by=-10),seq(0,(max(dd$dfdds)/10-offset),by=10)))){
      polygon(cos(dd$phix)*(v+offset),sin(dd$phix)*(v+offset),border="#00000010")
    }  
  })
  
})
