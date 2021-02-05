###### this function return a full list of current AmeriFlux sites, with basic site general info, like ID, geolocation, data year, url
get_ameriflux_sgi<-function(){
  
  na.max<-function(x){ifelse(sum(!is.na(x))>0,max(x,na.rm=T),NA)}
  na.min<-function(x){ifelse(sum(!is.na(x))>0,min(x,na.rm=T),NA)}
  
  ## webservice returning a full site list with basic site general info
  sgi.ameriflux <- jsonlite::fromJSON(content(POST("https://ameriflux-data.lbl.gov/AmeriFlux/SiteSearch.svc/SiteMapData/AmeriFlux"),
                                              as="text"),  flatten=TRUE)
  
  ## webservice returning a full site list with most-updated data available years in AmeriFlux BASE dataset
  data.yr.ameriflux <- jsonlite::fromJSON(content(POST("https://ameriflux-data.lbl.gov/AmeriFlux/SiteSearch.svc/PublishYears/AmeriFlux"),
                                                  as="text"),  flatten=TRUE)
  
  sgi.ameriflux<-merge.data.frame(sgi.ameriflux,data.yr.ameriflux)
  sgi.ameriflux$DATA_START<-sapply(sgi.ameriflux$publish_years,na.min)
  sgi.ameriflux$DATA_END<-sapply(sgi.ameriflux$publish_years,na.max)
  
  sgi.ameriflux$GRP_LOCATION.LOCATION_LAT<-as.numeric(sgi.ameriflux$GRP_LOCATION.LOCATION_LAT)
  sgi.ameriflux$GRP_LOCATION.LOCATION_LONG<-as.numeric(sgi.ameriflux$GRP_LOCATION.LOCATION_LONG)
  
  sgi.ameriflux$TEAM_MEMBER_PI1<-NA
  sgi.ameriflux$TEAM_MEMBER_PI2<-NA
  
  for(j1 in 1:nrow(sgi.ameriflux)){
    
    target.site<-sgi.ameriflux$SITE_ID[j1]
    
    ## webservice returning complete site general info for a single site
    sgi.site.member <- jsonlite::fromJSON(content(GET(paste("https://ameriflux-data.lbl.gov/BADM/Anc/SiteInfo/",
                                                            target.site,sep="")),as="text"),flatten=T)[[2]][2][[1]]
    get.pi<-NULL
    for(j2 in 1:length(sgi.site.member)){
      if(length(which(names(sgi.site.member[[j2]])=="TEAM_MEMBER_ROLE"))>0){
        if(sgi.site.member[[j2]]$TEAM_MEMBER_ROLE=="PI") get.pi<-c(get.pi,j2)  
      }
    }
    
    if(!is.null(get.pi)) sgi.ameriflux$TEAM_MEMBER_PI1[j1]<-sgi.site.member[[get.pi[1]]]$TEAM_MEMBER_NAME
    if(length(get.pi)>1) sgi.ameriflux$TEAM_MEMBER_PI2[j1]<-sgi.site.member[[get.pi[2]]]$TEAM_MEMBER_NAME
    
  }
  return(sgi.ameriflux)
}