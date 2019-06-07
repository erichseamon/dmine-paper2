climatematrix_map <- function(climate_var, predictor_var) {
 #climate_var <- "tmmx"
 #predictor_var <- "cube_root_loss"
 #monthend <- "jul"
 #monthnumber <- 2
    
      library(RColorBrewer)
      library(dplyr)
      
      setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_correlations_4/")
      
      
      if (predictor_var  == "loss") {
        
        predictor <- "crop_commodity_loss"
        
      }else {
        
        predictor <- predictor_var
      }
      
      
      
      files  <- list.files(pattern = predictor)
      filey <- do.call(rbind, strsplit(files, '[_]'))
      
      filey <- subset(filey, filey[,5] == climate_var)
      
      colnames(filey) <- c("state", "county", "commodity", "damage", "climate", "crop1", "crop2", "response", "crop3")
      filey <- as.data.frame(filey)
      data <- with(filey, paste(state, "_", county, "_", commodity, "_", damage, "_", climate, "_", crop1, "_", crop2, "_", response, "_", crop3, sep=""))
      
      
      
      
      
      
      tables <- lapply(data, read.csv, header = TRUE)
      
      
      
      tables <- lapply(tables, function(x) { x["X"] <- NULL; x }) #--remove first index row from each list
      
      tables <- lapply(tables, function(x) arrange(x, -row_number())) #--(flips matrix - puts jan as 1st row and sept as 9th row)
      
      
      for (i in 1:26) {
      tables[[i]][1,4:12] <- NA
      tables[[i]][2,5:12] <- NA
      tables[[i]][3,6:12] <- NA
      tables[[i]][4,7:12] <- NA
      tables[[i]][5,8:12] <- NA
      tables[[i]][6,9:12] <- NA
      tables[[i]][7,10:12] <- NA
      tables[[i]][8,11:12] <- NA
      tables[[i]][9,12:12] <- NA
      }
      
      
      monthly <- match(monthend, tolower(month.abb))
      
      
      if(climate_var=='pr'){
        
        bestcounty <- matrix(NA,nrow=26,ncol=3)
        for (i in 1:26) {
          temp <- which(tables[[i]] == min(tables[[i]], na.rm=T), arr.ind = TRUE)
          temp2 <- min(tables[[i]], na.rm=T)
          bestcounty[i,1] <- temp[1,1]
          bestcounty[i,2] <- temp[1,2]
          bestcounty[i,3] <- temp2
          temp <- NA
          temp2 <- NA
          
        }
      } else {
        if(climate_var=='rmin'){
          
          bestcounty <- matrix(NA,nrow=26,ncol=3)
          for (i in 1:26) {
            temp <- which(tables[[i]] == min(tables[[i]], na.rm=T), arr.ind = TRUE)
            temp2 <- min(tables[[i]], na.rm=T)
            bestcounty[i,1] <- temp[1,1]
            bestcounty[i,2] <- temp[1,2]
            bestcounty[i,3] <- temp2
            temp <- NA
            temp2 <- NA
          }
        } else {
          if(climate_var=='rmax'){
            
            bestcounty <- matrix(NA,nrow=26,ncol=3)
            for (i in 1:26) {
              temp <- which(tables[[i]] == max(tables[[i]], na.rm=T), arr.ind = TRUE)
              temp2 <- max(tables[[i]], na.rm=T)
              bestcounty[i,1] <- temp[1,1]
              bestcounty[i,2] <- temp[1,2]
              bestcounty[i,3] <- temp2
              temp <- NA
              temp2 <- NA
            }
          } else {  
            if(climate_var=='tmmx'){ 
              bestcounty <- matrix(NA,nrow=26,ncol=3)
              for (i in 1:26) {
                temp <- which(tables[[i]] == max(tables[[i]], na.rm=T), arr.ind = TRUE)
                temp2 <- max(tables[[i]], na.rm=T)
                bestcounty[i,1] <- temp[1,1]
                bestcounty[i,2] <- temp[1,2]
                bestcounty[i,3] <- temp2
                temp <- NA
                temp2 <- NA
              }
            } else {
              if(climate_var=='tmmn'){
                bestcounty <- matrix(NA,nrow=26,ncol=3)
                for (i in 1:26) {
                  temp <- which(tables[[i]] == max(tables[[i]], na.rm=T), arr.ind = TRUE)
                  temp2 <- max(tables[[i]], na.rm=T)
                  bestcounty[i,1] <- temp[1,1]
                  bestcounty[i,2] <- temp[1,2]
                  bestcounty[i,3] <- temp2
                  temp <- NA
                  temp2 <- NA
                }
              } else {
                if(climate_var=='fm100'){
                  bestcounty <- matrix(NA,nrow=26,ncol=3)
                  for (i in 1:26) {
                    temp <- which(tables[[i]] == max(tables[[i]], na.rm=T), arr.ind = TRUE)
                    temp2 <- max(tables[[i]], na.rm=T)
                    bestcounty[i,1] <- temp[1,1]
                    bestcounty[i,2] <- temp[1,2]
                    bestcounty[i,3] <- temp2
                    temp <- NA
                    temp2 <- NA
                  }
                } else {
                  if(climate_var=='fm1000'){
                    bestcounty <- matrix(NA,nrow=26,ncol=3)
                    for (i in 1:26) {
                      temp <- which(tables[[i]] == max(tables[[i]], na.rm=T), arr.ind = TRUE)
                      temp2 <- max(tables[[i]], na.rm=T)
                      bestcounty[i,1] <- temp[1,1]
                      bestcounty[i,2] <- temp[1,2]
                      bestcounty[i,3] <- temp2
                      temp <- NA
                      temp2 <- NA
                    }
                  } else {
                    if(climate_var=='pet'){
                      bestcounty <- matrix(NA,nrow=26,ncol=3)
                      for (i in 1:26) {
                        temp <- which(tables[[i]] == max(tables[[i]], na.rm=T), arr.ind = TRUE)
                        temp2 <- max(tables[[i]], na.rm=T)
                        bestcounty[i,1] <- temp[1,1]
                        bestcounty[i,2] <- temp[1,2]
                        bestcounty[i,3] <- temp2
                        temp <- NA
                        temp2 <- NA
                      }
                    } else {
                      if(climate_var=='pdsi'){
                        bestcounty <- matrix(NA,nrow=26,ncol=3)
                        for (i in 1:26) {
                          temp <- which(tables[[i]] == min(tables[[i]], na.rm=T), arr.ind = TRUE)
                          temp2 <- min(tables[[i]], na.rm=T)
                          bestcounty[i,1] <- temp[1,1]
                          bestcounty[i,2] <- temp[1,2]
                          bestcounty[i,3] <- temp2
                          temp <- NA
                          temp2 <- NA
                        }
                        
                        
                        
                        
                        
                        
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
      
      
      
      bestcounty[,1] <- tolower(month.abb[bestcounty[,1]])
      
      bestcounty2 <- cbind(data.frame(filey$county), bestcounty)
      colnames(bestcounty2) <- c("NAME", "MONTH", "ENDMONTH", "CORRELATION")
      #new
      
      
      
      
      
      #!!!!!!fix-row by column, or number of months by ending month
      table2 <- lapply(tables, function(x) x[monthly, as.numeric(monthnumber)])
      
      
      table3 <- data.frame(matrix(unlist(table2), nrow=length(table2), byrow=T))
      colnames(table3) <- "correlations"
      #combined <- do.call(rbind , tables)
      
      table4 <- cbind(filey, table3)
      
      #if (predictor_var  == "loss") {
      
      #predictor_var <- "crop_commodity_loss"
      
      #}
      
      table5 <- table4[c(2:5,10)]
      
      colnames(table5) <- c("NAME", "COMMODITY", "DAMAGE", "climate", "correlations")
      
      #table5$STATE_NAME <-  state.name[match(table5[,1],state.abb)]
      
      
      
      
      
      setwd("/dmine/data/counties/")
      
      counties <- readShapePoly('UScounties.shp', 
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      statez = c("Idaho", "Washington", "Oregon")
      Idaho_list1 <- paste("Idaho", "Lewis", "Nez Perce", "Latah", "Benewah", sep="|")
      Washington_list1 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", sep="|")
      Oregon_list1 <- paste("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", sep="|")
      
      
      combinedlist2 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", "Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", "Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
      combinedlist <- c(Idaho_list1, Washington_list1, Oregon_list1)
      
      #alllist <- c("Idaho", "Oregon", "Washington")
      
      
      #--Oregon
      
      setwd("/dmine/data/counties/")
      
      counties <- readShapePoly('UScounties.shp', 
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      or_counties <- counties[grep("Oregon", counties@data$STATE_NAME),]
      palouse_Oregon_counties <- or_counties[grep(Oregon_list1, or_counties@data$NAME),]
      kk="Oregon"
      
      #counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
      OR_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Oregon_counties)
      #counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
      
      #--loop list for county by fip
      #countyfiploop <- counties@data$FIPS
      
      #--data frame of county fip list
      #countyfiplist <- data.frame(counties@data$FIPS)
      
      #--data frame of county names
      #countynames <- data.frame(counties@data$NAME)
      
      #combo of county names and fip for this list
      #countylist <- cbind(countynames, countyfiplist)
      
      #--number of rows in county list
      #countylistrows <- 12 * nrow(countylist)
      
      
      
      #---Washington
      
      
      
      setwd("/dmine/data/counties/")
      
      counties <- readShapePoly('UScounties.shp', 
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      wa_counties <- counties[grep("Washington", counties@data$STATE_NAME),]
      palouse_Washington_counties <- wa_counties[grep(Washington_list1, wa_counties@data$NAME),]
      kk="Washington"
      
      #counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
      WA_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Washington_counties)
      #counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
      
      #-----Idaho
      
      
      setwd("/dmine/data/counties/")
      
      counties <- readShapePoly('UScounties.shp', 
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      id_counties <- counties[grep("Idaho", counties@data$STATE_NAME),]
      palouse_Idaho_counties <- id_counties[grep(Idaho_list1, id_counties@data$NAME),]
      kk="Idaho"
      #counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
      ID_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Idaho_counties)
      #counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
      
      counties <- rbind(ID_counties, WA_counties, OR_counties)
      
      
      
      
      counties2 <- merge(counties, table5, by = "NAME" )
      
      #--new
      
      counties3 <- merge(counties2, bestcounty2, by = "NAME")
      counties3$MONTHCOMBO <- paste(counties3$MONTH, counties3$ENDMONTH, sep="")
      
      #--new
      
      
      
      #colorbrew <- list(color = brewer.pal(26, c("green", "blue", "yellow")))
      my_palette <- colorRampPalette(c("yellow", "blue"))(n = 26)
      
      counties3$CORRELATION <- as.numeric(levels(counties3$CORRELATION))[counties3$CORRELATION]
      
      pal <- colorNumeric(rev(my_palette),  na.color = "#ffffff", domain = eval(parse(text=paste("counties3$", "CORRELATION", sep=""))))
      
      #--
      
      #colorss = colorRampPalette(brewer.pal(11,"Spectral"))
      
      #finalcol <- colorss(len <- length(counties3$CORRELATION))
      #finalcol2 <- topo.colors(length(counties3$CORRELATION))[order(order(counties3$CORRELATION))]
      
      #cellselect <- paste(monthend, monthnumber, sep="")
      
      #par(mfrow=c(1,4))
      #layout(matrix(c(1,1,1,2), 1, 4, byrow = TRUE)) 
      #par(mar = c(1,1,1,1) + 0.1)
      #plot(counties3, col = finalcol2, xaxs="i", yaxs="i")
      ##text(coordinates(counties2), labels=round(counties2$correlations, 2), cex=1.5, col = "black")
      
      #added from ID
      #corre <- round(as.numeric(as.character(counties3$CORRELATION)), 2)
      #text(coordinates(counties2), labels=paste(counties3$MONTHCOMBO, "\n", corre,  sep=""), cex=1.5, col = "white", font = 2)
      
      #--
      
      exte <- extent(counties3)
      
      library(htmltools)
      
      tag.map.title <- tags$style(HTML("
                                       .leaflet-control.map-title { 
                                       transform: translate(-50%,20%);
                                       position: fixed !important;
                                       left: 50%;
                                       text-align: center;
                                       padding-left: 10px; 
                                       padding-right: 10px; 
                                       background: rgba(255,255,255,0.75);
                                       font-weight: bold;
                                       font-size: 24px;
                                       }
                                       "))
      
      title <- tags$div(
        tag.map.title, HTML(paste("IPNW  Correlation, Climate vs. ", predictor_var, " by County for ", climate_var, sep=""))
      )  

      
      lat_long <- coordinates(counties3)
      
     
      #labels <- paste(counties3$MONTHCOMBO, as.character(round(counties3$CORRELATION, 2)), sep = "<br/>")
   
      
      #counties3$CORRELATION <- as.numeric(levels(counties3$CORRELATION))[counties3$CORRELATION]
      counties3a <- data.frame(counties3)
      labs <- lapply(seq(nrow(counties3a)), function(i) {
        paste0(as.character(round(counties3a[i, "CORRELATION"],2)), '<br/>',
                counties3a[i, "MONTHCOMBO"]) 
      })

 
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_rev3")  
monthcombo <- 1:nrow(counties3a)

empty_df <- data.frame(matrix(ncol = 1, nrow = 0))
x <- c("climate")
colnames(empty_df) <- x  

 for (j in monthcombo)   {
   ind <- counties3a[j,]
   state1 <- state.abb[match(ind[,2],state.name)]
   csv <- read.csv(paste(state1, "_", ind$NAME, "_", "WHEAT_Drought_", ind$MONTHCOMBO, ".csv", sep=""))
   climm <- data.frame(eval(parse(text=paste("csv$", climate_var, sep=""))) )
   countee <- data.frame(csv$year)
   climcountee <- cbind(climm)
   colnames(climcountee) <- c("climate")
   empty_df <- rbind(empty_df, climcountee)

 }

empty_df
}


climlist <- c("pr", "pet", "tmmx", "tmmn", "pdsi")

empty_df2 <- data.frame(matrix(ncol = 1, nrow = 624))


for (jj in climlist) {
  
empty_df <- climatematrix_map(jj, "cube_root_loss")
 
empty_df2 <- cbind(empty_df2, empty_df)

}

newclim <- cbind(empty_df2, empty_df_county)
newclim <- newclim[,2:9]
colnames(newclim) <- c("pr", "pet", "tmmx", "tmmn", "pdsi", "year", "county", "state")

#--loss aggregation

#---2001

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/month_2001/")
files  <- list.files(pattern = 'Idaho')
table1a <- lapply(files, read.csv, header = TRUE)
table11a <- do.call(rbind , table1a)

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/month_2001/")
files  <- list.files(pattern = 'Washington')
table2a <- lapply(files, read.csv, header = TRUE)
table22a <- do.call(rbind , table2a)

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/month_2001/")
files  <- list.files(pattern = 'Oregon')
table3a <- lapply(files, read.csv, header = TRUE)
table33a <- do.call(rbind , table3a)

table2001 <- rbind(table11a,table22a,table33a)


#---1989

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/month_1989/")
files  <- list.files(pattern = 'Idaho')
table1 <- lapply(files, read.csv, header = TRUE)
table11 <- do.call(rbind , table1)

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/month_1989/")
files  <- list.files(pattern = 'Washington')
table2 <- lapply(files, read.csv, header = TRUE)
table22 <- do.call(rbind , table2)

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/month_1989/")
files  <- list.files(pattern = 'Oregon')
table3 <- lapply(files, read.csv, header = TRUE)
table33 <- do.call(rbind , table3)

table1989 <- rbind(table11,table22,table33)
table1989 <- cbind(table1989[,1:15], table1989[,17], table1989[,16])
colnames(table1989) <- c("X" ,                "year" ,             "statecode",         "state",             "countycode",       
                    "county",            "commoditycode",     "commodity",         "insuranceplancode", "insurancename",    
                    "stagecode",         "damagecausecode",   "damagecause",       "monthcode",         "month",            
                    "acres",             "loss"     )

table1989$acres <- 0

tableall <- rbind(table1989, table2001)

tableall[,8] <- trimws(tableall[,8])
tableall[,13] <- trimws(tableall[,13])


tableall1 <- subset(tableall, commodity == "WHEAT" & damagecause == "Drought")
tableall2 <- subset(tableall1, monthcode <= 9 & monthcode != 0)

tableall3 <- aggregate(tableall2$loss, by=list(tableall2$year, tableall2$county, tableall2$state), FUN = "sum")
tableall3[,3] <- state.name[match(tableall3[,3],state.abb)]
colnames(tableall3) <- c("year", "county", "state", "loss")
tableall3[,2] <- trimws(tableall3[,2])


tableall4 <- merge(newclim, tableall3, by = c("year", "county", "state"))

Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

tableall4$cube_root_loss <- Math.cbrt(tableall4$loss)




tableall5 <- subset(tableall4, year >= 2001)


#---

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_summaries/")
file2 <- as.data.frame(read.csv(paste(state2, "_", county1, "_", commodity1, "_", damage1, "_", response, ".csv", sep="")))
file2 <- subset(file2, state == state2)
file2 <- subset(file2, county == county1)
colnames(file2) <- c("X", "year", "damagecause", "county", "state", "commodity", response)