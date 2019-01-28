

county_zip <- read.csv('data/county_zip.csv')

one_county <- subset(county_zip, county_zip$County == 'Marion')


zip.shp <- read_sf(dsn = "data/in_zip", layer = "ZCTA_TIGER05_IN")

zip.shp <- as.data.frame(zip.shp)

zip.shp.II <-
  merge(zip.shp, one_county, by.x = "ZCTA5", by.y = "ZIP.Code")

zip.shp <- st_sf(zip.shp.II)

zip.shp <- as_Spatial(zip.shp)



my.proj <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

##Pathways Map

getCat <- function(lev){
    
    #cat.q <- paste("MATCH (a:category{level:'", lev, "'}) RETURN DISTINCT(a.category) AS cat", sep = "")
    #res <- cypher(graph, cat.q)
  
    res <- get_cat_py(lev)
   
    catC <- c()
    
    
    for(i in 1:length(res$cat)){
      
      catC <- c(catC, simpleCap(tolower(res$cat[i])))
      
    }
    
    catC <- data.frame(catC)
    colnames(catC) <- "Categories:"
    return(catC)
}


getStart <- function(cat){
   
  
    cat <- toupper(cat)  
    
    #start.q <- paste("MATCH (a:category{category:'", cat, "'})-[:PART_OF]-(b:organization{opioid_involved:'1'})
    #RETURN b.org_name AS org_name,  toFloat(b.latitude) AS latitude, toFloat(b.longitude) AS longitude, a.level AS level, a.category AS category", sep = "")
    
    #org.df <- cypher(graph, start.q)
    
    org.df <- get_start_py(cat)
    
    
    org.df[org.df == "NULL"]=NA
    
    org.df <- org.df[complete.cases(org.df), ]
    
    #org.df <- org.df[complete.cases(org.df), ]
    
    #org.df$lon <- as.numeric(org.df$lon)
    #org.df$lat <- as.numeric(org.df$lat)
    
}


getStep <- function(current.list, dist){
  
  if(class(current.list) == "data.frame"){
    
    res <- current.list
    
  }else{
  
    res <- unique(do.call(rbind.data.frame, current.list))
  
  }
  
  len2 <- 1:nrow(res)
  
  if(res$level[1] == "Entry Point"){
    
    next.lev <- "Treatment Point"
    
  } else if (res$level[1] == "Treatment Point"){
    
    next.lev <- "Recovery Point"
    
  } else if (res$level[1] == "Recovery Point"){
    
    next.lev <- "Stabilization Point"
    
  }
  
   #future_lapply(len2, FUN = getNext, org.df = res, dist = dist, lev = next.lev)
   
  progress <- shiny::Progress$new()
  
  on.exit(progress$close())
  
  n <- length(len2)
  
   lapply(len2, function(x){
     
     #percentage <<- percentage + 1/length(len2)*100
     #incProgress(1/length(len2), detail = paste0("Progress: ", round(percentage,2)))
     
     progress$set(message = "Mapping...", value = 0)
     progress$inc(1/n)
     
     getNext(x, org.df = res, dist = dist, lev = next.lev)
   })
   
}


getNext <- function(no, org.df, dist, lev){
  
  one.res <- org.df[no,]
  
  name <- one.res[1,1]
  
  # get.one.next <- paste("MATCH (a:organization{org_name:'", name,"'})-[]-(b:category)-[:TRANSITIONS_TO]-(c:category{level:'",lev,"'})-[]-(d:organization{opioid_involved:'1'})
  #                  WITH point({longitude:toFloat(a.longitude), latitude:toFloat(a.latitude)}) AS first,
  #                       point({longitude:toFloat(d.longitude), latitude:toFloat(d.latitude)}) AS finish,
  #                       a.latitude AS start_lat,
  #                       a.longitude AS start_lon,
  #                       d.latitude AS latitude,
  #                       d.longitude AS longitude, 
  #                       d.org_name AS org_name, 
  #                       c.level AS level, 
  #                       c.category AS category
  #                       WHERE distance(first, finish) < ", dist,
  #                       " RETURN 
  #                       DISTINCT(org_name),
  #                       start_lat,
  #                       start_lon,
  #                       latitude,
  #                       longitude,
  #                       level,
  #                       category, 
  #                       distance(first, finish) AS dist
  #                       ORDER BY dist ASC
  #                       LIMIT 5
  #                       "
  #                       , sep = "")
  # 
  # 
  # one.next <- cypher(graph, get.one.next)   
  
  
  one.next <- get_next_py(name = name, lev = lev, dist = dist)
  

  one.next[1:(length(one.next)-1)]
  
}
 

makeMap <- function(edges, edges.II, edges.III, edges.IV){
  

  if(missing("edges.II")==T && missing("edges.III")==T && missing("edges.IV")==T){
  
  edges$start_lat <- edges$latitude
  edges$start_lon <- edges$longitude
  edge.org <- edges[,c("org_name", "start_lat", "start_lon", "latitude", "longitude", "level", "category")]
  
    
  }else 
  if(missing("edges.II")==F && missing("edges.III")==T && missing("edges.IV")==T){
  
    edges$start_lat <- edges$latitude
    edges$start_lon <- edges$longitude
    edge.org <- edges[,c("org_name", "start_lat", "start_lon", "latitude", "longitude", "level", "category")]
  
  edge.orgII <- unique(do.call(rbind.data.frame, edges.II))
  
  edge.org <- rbind(edge.org, edge.orgII)
  
  
}else 
  if(missing("edges.II")==F && missing("edges.III")==F && missing("edges.IV")==T){
  
  edges$start_lat <- edges$latitude
  edges$start_lon <- edges$longitude
  edge.org <- edges[,c("org_name", "start_lat", "start_lon", "latitude", "longitude", "level", "category")]
  
  edge.orgII <- unique(do.call(rbind.data.frame, edges.II))
  
  edge.orgIII <- unique(do.call(rbind.data.frame, edges.III))
  
  edge.org <- rbind(edge.org, edge.orgII, edge.orgIII)
  
}else 
  if(missing("edges.II")==F && missing("edges.III")==F && missing("edges.IV")==F){
  
  edges$start_lat <- edges$latitude
  edges$start_lon <- edges$longitude
  edge.org <- edges[,c("org_name", "start_lat", "start_lon", "latitude", "longitude", "level", "category")]
  
  edge.orgII <- unique(do.call(rbind.data.frame, edges.II))
  
  edge.orgIII <- unique(do.call(rbind.data.frame, edges.III))
  
  edge.orgIV <- unique(do.call(rbind.data.frame, edges.IV))
  
  edge.org <- rbind(edge.org, edge.orgII, edge.orgIII, edge.orgIV)
  
}


  edge.org <- unique(edge.org)
  
  edge.count <- nrow(edge.org)
  
  ####
  write.csv(edge.count, 'data/edge.org.csv', row.names = F)
  ####
  
  edge.org$start_lat <- as.numeric(edge.org$start_lat)
  edge.org$start_lon <- as.numeric(edge.org$start_lon)
  edge.org$latitude  <- as.numeric(edge.org$latitude)
  edge.org$longitude <- as.numeric(edge.org$longitude)

  edge.org$lab <- paste("<b>Organization:</b>", edge.org$org_name, " \n <b>Category:</b>", edge.org$category, sep = "")

  
pinIcons <- icons(
  iconUrl = ifelse(edge.org$level == "Entry Point", "red.png", 
                   ifelse(edge.org$level == "Treatment Point", "orange.png", 
                          ifelse(edge.org$level == "Recovery Point", "yellow.png",
                              ifelse(edge.org$level == "Stabilization Point", "green.png", "purple.png")))),
  
  iconWidth = 30,
  iconHeight = 45,
  iconAnchorX = 15,
  iconAnchorY = 40
  
  )


op_dat <- read.csv('data/op_zip_placeholder.csv', header = F)

op_dat$V1 <- as.character(op_dat$V1)
op_dat$V2 <- as.numeric(op_dat$V2)

m <- spTransform(zip.shp, my.proj)


m.chl.comb <- sp::merge(m, op_dat, by.x = 'ZCTA5', by.y = 'V1', duplicateGeoms = T)



max.var <- sort(na.omit(as.numeric(unlist(op_dat$V2))))

max.var <- max.var[length(max.var)]

one.bin <- (max.var/6)

bins <- seq(0, (max.var + one.bin), by = one.bin)

bins <- round_any(bins, 1)

my.pal <- colorBin("Purples", domain = as.numeric(unlist(op_dat$V2)), bins = bins, na.color="transparent")



map <-   leaflet() %>%
  addTiles() %>%
  addMarkers(data = edge.org,
             ~ longitude,
             ~ latitude,
             icon = pinIcons,
             popup = ~ as.character(lab)
             ) %>%
  setView(zoom = 11, lng = -86.15804, lat = 39.76838) %>%
  addPolygons(
    data = m.chl.comb,
    fillColor = ~my.pal(as.numeric(unlist(m.chl.comb$V2))),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.4,
    #highlightOptions = highlightOptions(
    #  weight = 5,
    #  color = "#666",
    #  dashArray = "",
    #  fillOpacity = 0.7,
    #  bringToFront = TRUE
    #),
    #label = ~as.character(unlist(m.chl.comb$ZCTA5)),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
    
  )




for(j in 1:nrow(edge.org)){
  
  map <- map %>% addPolylines( lat = c(edge.org[j,]$start_lat, edge.org[j,]$latitude), 
                               lng = c(edge.org[j,]$start_lon, edge.org[j,]$longitude),
                               fillOpacity = 1, 
                               opacity = 1
  )
  
}

return(map)



  }

edge.org.count <- function(){
  
  count <- read.csv('data/edge.org.csv')
  
  message <- paste('Number of Organizations being Mapped: ', count[1,1], sep = "")
  
  return(message)
}


createOutput <- function(org.l, dist, steps){
  
  org.df <- do.call(rbind.data.frame, org.l)
  
  
  dist <- dist*1609.34
  #dist2 <- dist2*1609.34
  
  if(steps == 0){
  
  #org.df <- getStart(cat)
  
      makeMap(edges = org.df)
  
  
  } else if(steps == 1){
    
   # org.df <- getStart(cat)
    
      org.second <- getStep(org.df, dist)
    
        makeMap(edges = org.df, edges.II = org.second)
    
  } else if(steps == 2){
    
    #org.df <- getStart(cat)
    
      org.second <- getStep(org.df, dist)
    
        org.third <- getStep(org.second, dist)
        
        makeMap(edges = org.df, edges.II = org.second, edges.III = org.third)
  
  } else if(steps == 3){
    
    #org.df <- getStart(cat)
    
      org.second <- getStep(org.df, dist)
    
        org.third  <- getStep(org.second, dist)
    
          org.fourth <- getStep(org.third, dist)
    
            makeMap(edges = org.df, edges.II = org.second, edges.III = org.third, edges.IV = org.fourth)
    
  } 
  
}
  
  
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}



##Potential Collaborators

#All Opiate org map  
all.oi <- function(issue){
  
  if(issue == 'Opioid Addiction/Substance Abuse'){
  
  #oi.q <- "MATCH (n:organization{opioid_involved:'1'}) RETURN toFloat(n.latitude) AS latitude, toFloat(n.longitude) AS longitude, n.org_name AS org "
    
    oi.df <- get_all_oi_py()
  
  } else if(issue == 'Education/Workforce Development'){
    
   #oi.q <- "MATCH (n:organization{workforce_involved:'1'}) RETURN toFloat(n.latitude) AS latitude, toFloat(n.longitude) AS longitude, n.org_name AS org "
    
    oi.df <- get_all_ed_py()
    
  }
  
  #oi.df <- cypher(graph, oi.q)
  
  oi.df <- oi.df[complete.cases(oi.df), ]
  
  count.oi <- nrow(oi.df)
  
  if(file.exists("data/oi.org.count.csv")){
    file.remove("data/oi.org.count.csv")
  }
  
  write.csv(count.oi, "data/oi.org.count.csv", row.names = F)
  
  oi.df$org <- tolower(oi.df$org)
  
  oi.df$lab <- lapply(oi.df[,3], FUN = simpleCap)
  
  
  ####
  op_dat <- read.csv('data/op_zip_placeholder.csv', header = F)
  
  op_dat$V1 <- as.character(op_dat$V1)
  op_dat$V2 <- as.numeric(op_dat$V2)
  
  m <- spTransform(zip.shp, my.proj)
  
  
  m.chl.comb <- sp::merge(m, op_dat, by.x = 'ZCTA5', by.y = 'V1', duplicateGeoms = T)
  

  max.var <- sort(na.omit(as.numeric(unlist(op_dat$V2))))
  
  max.var <- max.var[length(max.var)]
  
  one.bin <- (max.var/6)
  
  bins <- seq(0, (max.var + one.bin), by = one.bin)
  
  bins <- round_any(bins, 1)
  
  my.pal <- colorBin("Purples", domain = as.numeric(unlist(op_dat$V2)), bins = bins, na.color="transparent")

  ####

  bluePin <- makeIcon(
    iconUrl= "blue.png",
    iconWidth = 30,
    iconHeight = 45,
    iconAnchorX = 20,
    iconAnchorY = 40
  )

  ####  
  map <-   leaflet() %>%
    addTiles() %>%
    addMarkers(data = oi.df,
               ~ longitude,
               ~ latitude,
               icon = bluePin,
               popup = ~ as.character(lab)
    ) %>%
    setView(zoom = 11, lng = -86.15804, lat = 39.76838) %>%
    addPolygons(
      data = m.chl.comb,
      fillColor = ~my.pal(as.numeric(unlist(m.chl.comb$V2))),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.4,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    )%>%
    addLegend(pal      = my.pal, 
              values   = as.numeric(unlist(m.chl.comb$V2)),
              opacity  = 0.7,
              title    = 'Number of Overdoses', 
              position = "bottomleft"
    )
  
  return(map)
  
}
oi.org.count <- function(){
  
  count <- read.csv('data/oi.org.count.csv')
  
  message <- paste('Number of Organizations being Mapped: ', count[1,1], sep = "")
  
  return(message)
}


get.oi.org <- function(issue){
  
 if(issue =='Opioid Addiction/Substance Abuse'){
   
   # oi.org.q <- "MATCH (n:organization{opioid_involved:'1'})-[:PART_OF]-(b:category)
   #              WHERE  b.category <> ''
   #              RETURN DISTINCT(n.org_name) AS org"
   
   oi.org <- get_oi_org_py()
   

 }else if(issue == 'Education/Workforce Development'){
   
   # oi.org.q <- "MATCH (n:organization{workforce_involved:'1'})-[:PART_OF]-(b:category) 
   #              WHERE b.category <> ''
   #              RETURN DISTINCT(n.org_name) AS org"
   
   
   oi.org <- get_ed_org_py()
   
 }
   
   #oi.org <- cypher(graph, oi.org.q)

   oi.org$org <- lapply(oi.org[,1], FUN = tolower)

   oi.org$org <- lapply(oi.org[,1], FUN = simpleCap)

   oi.out <- unique(sort(unlist(oi.org$org)))

   return(oi.out)

 }

  
get.cat.combo <- function(org.nm, no){

  #org.nm2 <- paste(toupper(org.nm), " ", sep = "")

  org.nm2 <- toupper(org.nm)

  # org.cats.q <- paste("MATCH (n:organization{org_name:'", org.nm2, "'})-[]-(a:category)
  #                     WHERE a.category <> 'NA'
  #                     AND   a.category <> 'NULL'
  #                     RETURN DISTINCT(a.category) AS cat,
  #                                     n.org_name  AS org_name,
  #                                     n.latitude  AS lat,
  #                                     n.longitude AS long", sep = "")
  # 
  # org.cats <- cypher(graph, org.cats.q)
  
  
  org.cats <- get_cat_combo_py(orgNm2=org.nm2)
  
  

  if(is.null(org.cats)){
    stop("No categories associated with this organization")
  }
  
  if(length(org.cats$cat) > no){  
  cat.combo <- data.frame(combn(org.cats$cat, no))
  
  cat.combo <- cat.combo[complete.cases(cat.combo),]
  
  }else{
    
  cat.combo <- data.frame(not_enough_cats = org.cats$cat)
  }
  
  
}


get.source.org <- function(org.nm, no){
  
  org.nm2 <- toupper(org.nm)
  
  # org.cats.q <- paste("MATCH (n:organization{org_name:'", org.nm2, "'})-[]-(a:category)
  #                     WHERE a.category <> 'NA'
  #                     AND   a.category <> 'NULL'
  #                     RETURN DISTINCT(a.category) AS cat,
  #                                     n.org_name  AS org_name,
  #                                     n.latitude  AS lat,
  #                                     n.longitude AS long", sep = "")
  # 
  # org.cats <- cypher(graph, org.cats.q)
  
  org.cats <- get_source_org_py(orgNm2 = org.nm2)
  
  
  if(is.null(org.cats)){
    stop(org.cats.q)
  }
  
  source.org <- data.frame(org.cats[1,2:4], type = "source")
  
  
}

#Potential Organizational Collaborators
org.cat.match <- function(cat.combo, no, source.org, topic){
  
  if(colnames(cat.combo)[1] == 'not_enough_cats'){
    
    res.out <- source.org 
    
    colnames(res.out) <- c("org_name", "latitude", "longitude", "type")
      
    }else{
    
    res.out <- get.match.org.oi(one.size = no, cat.combo, issue = topic)
    
    }
  
    #Create Lable for source map pin
    source.lab <- unique(unlist(cat.combo[1:nrow(cat.combo), 1:ncol(cat.combo)]) )
    source.lab <- paste(tolower(source.lab), collapse = ", ")
    source.lab <- simpleCap(source.lab)
  

  res.out$org <- tolower(res.out$org)
  
  res.out$lab <- lapply(res.out$org, FUN = simpleCap)
  
  res.out$lab <- paste("<b> Organization: </b> ",       res.out$lab, 
                       "<br> <b> Shared Characteristics: </b> ", res.out$category, sep = "" )
  
  
  
  res.out$latitude  <- as.numeric(res.out$latitude)
  res.out$longitude <- as.numeric(res.out$longitude)
  
  res.out[res.out==0] <- NA
  
  res.out <- na.omit(res.out)
  
  res.out <- data.frame(res.out, type = "target")
  

  
  last <- nrow(res.out)+1
  
  res.out[nrow(res.out)+1,] <- NA
  
  res.out$org[last]       <- source.org[1,1]
  res.out$latitude[last]  <- as.numeric(source.org[1,2])
  res.out$longitude[last] <- as.numeric(source.org[1,3])
  res.out$type[last]      <- source.org[1,4]
  res.out$lab[last]       <- paste("<b>", simpleCap(tolower(source.org[1,1])), "</b>",
                                   "<br> <b> Characteristics: </b>", source.lab, sep ="")
  
  
  pinIcons <- icons(
    iconUrl = ifelse(res.out$type == "source", "blue.png", "green.png"),
    iconWidth = 30,
    iconHeight = 45,
    iconAnchorX = 20,
    iconAnchorY = 40
  )
  
  op_dat <- read.csv('data/op_zip_placeholder.csv', header = F)
  
  op_dat$V1 <- as.character(op_dat$V1)
  op_dat$V2 <- as.numeric(op_dat$V2)
  
  m <- spTransform(zip.shp, my.proj)
  
  
  m.chl.comb <- sp::merge(m, op_dat, by.x = 'ZCTA5', by.y = 'V1', duplicateGeoms = T)
  
  
  
  max.var <- sort(na.omit(as.numeric(unlist(op_dat$V2))))
  
  max.var <- max.var[length(max.var)]
  
  one.bin <- (max.var/6)
  
  bins <- seq(0, (max.var + one.bin), by = one.bin)
  
  bins <- round_any(bins, 1)
  
  my.pal <- colorBin("Purples", domain = as.numeric(unlist(op_dat$V2)), bins = bins, na.color="transparent")
  

  
  map <-   leaflet() %>%
    addTiles() %>%
    addMarkers(data = res.out,
               ~ longitude,
               ~ latitude,
               icon = pinIcons,
               popup = ~ as.character(lab)
    ) %>%
    setView(zoom = 11, lng = -86.15804, lat = 39.76838) %>%
    addPolygons(
      data = m.chl.comb,
      fillColor = ~my.pal(as.numeric(unlist(m.chl.comb$V2))),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.4,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
                          )
      )%>%
     addLegend(pal      = my.pal, 
               values   = as.numeric(unlist(m.chl.comb$V2)),
               opacity  = 0.7,
               title    = 'Number of Overdoses', 
               position = "bottomleft"
      )
  
  return(map)
  
}


#Called by org.cat.match

get.match.org.oi <- function(one.size, cats, issue){
  
  
   progress <- shiny::Progress$new()
   
   on.exit(progress$close())
   
   progress$set(message = "Mapping...", value = 0)
   
   n <- length(cats)
  
  res.l <- list()
  
  for(m in 1:ncol(cats)){
    
    
    no.cats <- 2:(one.size + 1)
    
    node.labels <- letters[no.cats]
    
    all.pattern <- c()
    
    for (i in 1:one.size) {
      one.pattern <-
        
        paste0("(", node.labels[i], ":category)-[]-(a:organization)")
      
      all.pattern <- c(all.pattern, ',', one.pattern)
      
    }
    
    all.pattern <- paste(all.pattern[-1], sep = ",", collapse = "")
    
    all.where <- c()
    
    for (j in 1:one.size) {
      
      one.where <- paste0(node.labels[j], ".category = '", cats[j, m], "'")
      
      all.where <- c(all.where, ' AND ', one.where)
      
    }
    
    all.where <- paste(all.where[-1], sep = " ", collapse = " ")
    
    
    all.return <- c()
    
    for (k in 1:one.size) {
      one.return <- paste0(node.labels[k], ".category")
      
      all.return <- c(all.return, one.return)
      
    }
    
    all.return <- paste(all.return, sep = " ", collapse = ", ")
    
  if(issue == 'Opioid Addiction/Substance Abuse'){
    
    # match.org.q <-
    #   paste0(
    #     "MATCH ",
    #     all.pattern,
    #     " WHERE ",
    #     all.where,
    #     "
    #     AND    a.opioid_involved =  '1'
    #     
    #     RETURN a.org_name as org, ",
    #     all.return,
    #     ", toFloat(a.latitude) AS latitude, toFloat(a.longitude) AS longitude"
    #   )
    
    
    match.org <-
      get_mat_org_oi_py(allPattern = all.pattern,
                        allWhere   = all.where,
                        allReturn  = all.return)
    
    
    
  }else if(issue == 'Education/Workforce Development'){
    
    # match.org.q <-
    #   paste0(
    #     "MATCH ",
    #     all.pattern,
    #     " WHERE ",
    #     all.where,
    #     "
    #     AND    a.workforce_involved =  '1'
    #     
    #     RETURN a.org_name as org, ",
    #     all.return,
    #     ", toFloat(a.latitude) AS latitude, toFloat(a.longitude) AS longitude"
    #   )
    
    
    match.org <-
      get_mat_org_ed_py(allPattern = all.pattern,
                        allWhere   = all.where,
                        allReturn  = all.return)
    
  }

    
   # match.org <-  cypher(graph, match.org.q)
    
    if(is.null(nrow(match.org))){
      
      stop('No matching organizations found with that number of shared categories')
      
    } 
    
    
    for(i in 2:(one.size+1)){
      
      match.org$category <- paste(match.org$category, match.org[,i], ', ', sep = "" )
      
    }
    
    
    match.org$category <- lapply(tolower(match.org$category), FUN = simpleCap)
    
    match.org$category = substr(match.org$category,1,nchar(match.org$category)-1)
    
    res.l[[m]] <- match.org
    
     progress$inc(1/n, detail = paste("Doing part", m))
    
  } 


  res.out <- ldply(res.l, data.frame)
  
  return(res.out)
  
}

collab.org.count <- function(){
  
  count <- read.csv('data/count.collab.csv')
  
  message <- paste('Number of Organizations being Mapped: ', count[1,1], sep = "")
  
  return(message)
}
  
##IDP Home
home.map <- function(){
  
  map <-   leaflet() %>%
    addTiles() %>%
    setView(zoom = 7, lng = -86.15804, lat = 39.76838)
  
  return(map)
  
}


##Involved Organizations Map

make_net <- function(unconn, issue, node_size){
  
  if(issue == 'Opioid Addiction/Substance Abuse'){
    
    # net1.q <- "MATCH (a:organization)-[r:REFERRED_TO]->(b:organization)
    #            WHERE a.opioid_involved = '1'
    #            RETURN a.blended_id       AS From, 
    #                   b.blended_id       AS To,
    #                   a.org_name         AS name1, 
    #                   b.org_name         AS name2,
    #                   a.naicsmap         AS from_group, 
    #                   b.naicsmap         AS to_group
    #          "
    # 
    # 
    # edge_node.1 <- cypher(graph, net1.q)
    # 
    # 
    # net2.q <- "MATCH (a:organization)-[:MEMBER_OF]->(b:coalition)
    #            WHERE a.opioid_involved = '1'
    #            RETURN a.blended_id      AS From, 
    #                   b.name            AS To,
    #                   a.org_name        AS name1, 
    #                   b.full_name       AS name2, 
    #                   a.naicsmap        AS from_group
    # "
    # 
    # edge_node.2 <- cypher(graph, net2.q)
    # 
    # edge_node.2$to_group <- 'Coalition'
    # 
    # 
    # net3.q <- "MATCH (a:organization)-[:COLLABORATES_WITH]->(b:organization)
    #            WHERE a.opioid_involved = '1'
    #            RETURN a.blended_id       AS From, 
    #                   b.blended_id       AS To,
    #                   a.org_name         AS name1, 
    #                   b.org_name         AS name2,
    #                   a.naicsmap         AS from_group, 
    #                   b.naicsmap         AS to_group
    #            "
    # 
    # edge_node.3 <- cypher(graph, net3.q)
    
    
    edge_node.1 <- get_net1_py()
    
    edge_node.2 <- get_net2_py()
    
    edge_node.2$to_group <- 'Coalition'
    
    edge_node.3 <- get_net3_py()
    
    
    edge_node <- rbind(edge_node.1, edge_node.2, edge_node.3)
    

    
  }else if(issue == "Education/Workforce Development"){
    
    # net.q <- "MATCH (a:organization)-[:MEMBER_OF]->(b:program)
    #           RETURN a.blended_id      AS From, 
    #                  b.name            AS To,
    #                  a.org_name        AS name1, 
    #                  b.name            AS name2,
    #                  a.group           AS from_group
    #           "
    # 
    # edge_node <- cypher(graph, net.q)
    
    edge_node <- get_net_py()
    
    edge_node$to_group <- "Program"
    
  }
    
    
  #Merge and rename nodes
  for(i in 1:length(edge_node$name1)){
    
    name1 <- edge_node$name1[i]
    
    #Merge Marion County Offices - FROM
    if(name1 == 'MARION COUNTY DOMESTIC RLTNS'   |
       name1 == 'MARION COUNTY ASSESSOR'         |
       name1 == 'MARION COUNTY RECORDER'         |
       name1 == 'MARION COUNTY ELECTION BOARD'   |
       name1 == 'MARION COUNTY INHERITANCE TAX'  |
       name1 == 'MARION COUNTY PROBATE CLERK'    |
       name1 == 'MARION COUNTY CORP COUNSEL'     |
       name1 == 'MARION COUNTY AUDITORS OFFICE'  |
       name1 == 'MARION COUNTY SURVEYOR'         |
       name1 == 'MARION COUNTY MUNICIPAL COURT'  |
       name1 == 'MARION COUNTY TREASURER'        |
       name1 == 'MARION COUNTY PROBATION DEPT'   |
       name1 == 'MARION COUNTY COMMISSIONERS'    |
       name1 == 'MARION COUNTY JUSTICE'          |
       name1 == 'MARION COUNTY JOB LINE'         |
       name1 == 'MARION COUNTY NEIGHBORHOOD SVC' 
    ){
      
      edge_node$From[i]       <- '100041_402314256'
      
      edge_node$name1[i]      <- "MARION COUNTY GOV'T OFFICE"
      
      edge_node$from_group[i] <- "92"
      
    } 
    
    #Merge Eskenazi Health nodes - FROM 
    
    if(name1 == 'ESKENAZI HEALTH'|
       name1 == 'ESKENAZIPROJECT POIN'|
       name1 == 'MIDTOWN MENTAL HEALTH CENTER  ESKENAZI HEALTH'){
      
      edge_node$From[i] <- '868200_721591505'
      
      edge_node$from_group[i] <- "62"
      
    }
    
    #Merge Marion County Sheriff Jail - FROM 
    if(name1 == 'MARION COUNTY SHERIFF  JAIL II'|
       name1 == 'MARION COUNTY SHERIFF  JAIL I'){
      
      edge_node$From[i] <- '867124_NA'
      
      edge_node$name1[i] <- 'MARION COUNTY SHERIFF JAIL'
      
      edge_node$from_group[i] <- "92"
      
    }
    
    
    #Merge Police stations - FROM 
    if(name1 == 'INDIANAPOLIS METROPOLITAN POLICE DEPARTMENT EAST DISTRICT'  |
       name1 == 'INDIANAPOLIS METROPOLITAN POLICE DEPARTMENT NORTH DISTRICT' |
       name1 == 'INDIANAPOLIS METROPOLITAN POLICE DEPARTMENT NORTHWEST DISTRICT' |
       name1 == 'INDIANAPOLIS METROPOLITAN POLICE DEPARTMENT SOUTHEAST DISTRICT' |
       name1 == 'INDIANAPOLIS METROPOLITAN POLICE DEPARTMENT SOUTHWEST DISTRICT'){
      
      edge_node$name1[i] <- 'INDY METRO POLICE'
      edge_node$From[i]  <- '864294_NA'
      edge_node$from_group[i] <- "92"
      
    }
    
    
    #Merge Goodwill - FROM 
    if(name1 == 'GOODWILL INDUSTRIES OF CENTRAL INDIANA'){
      
      edge_node$From[i] <- '864837_418626345'
      
    }
    
    
    #Merge Community Hospital East - FROM 
    if(name1 == 'COMMUNITY HOSPITAL EAST'){
      
      edge_node$From[i] <- '867109_406699679'
      
      edge_node$from_group[i] <- "62"
      
    }
    
    
    ###################
    
    name2 <- edge_node$name2[i]
    
    #Merge Marion County Offices - TO 
    if(name2 == 'MARION COUNTY DOMESTIC RLTNS '   |
       name2 == 'MARION COUNTY ASSESSOR '         |
       name2 == 'MARION COUNTY RECORDER '         |
       name2 == 'MARION COUNTY ELECTION BOARD '   |
       name2 == 'MARION COUNTY INHERITANCE TAX '  |
       name2 == 'MARION COUNTY PROBATE CLERK '    |
       name2 == 'MARION COUNTY CORP COUNSEL '     |
       name2 == 'MARION COUNTY AUDITORS OFFICE '  |
       name2 == 'MARION COUNTY SURVEYOR '         |
       name2 == 'MARION COUNTY MUNICIPAL COURT '  |
       name2 == 'MARION COUNTY TREASURER '        |
       name2 == 'MARION COUNTY PROBATION DEPT '   |
       name2 == 'MARION COUNTY COMMISSIONERS '    |
       name2 == 'MARION COUNTY JUSTICE '          |
       name2 == 'MARION COUNTY JOB LINE '         |
       name2 == 'MARION COUNTY NEIGHBORHOOD SVC ' 
    ){
      
      edge_node$To[i]  <- '100041_402314256'
      
      edge_node$name2[i] <- "MARION COUNTY GOV'T OFFICE"
      
      edge_node$to_group[i] <- "92"
      
    } 
    
    
    #Merge Eskenazi Health nodes - TO
    if(name2 == 'ESKENAZI HEALTH'|
       name2 == 'ESKENAZIPROJECT POIN'|
       name2 == 'MIDTOWN MENTAL HEALTH CENTER  ESKENAZI HEALTH'){
      
      edge_node$To[i] <- '868200_721591505'
      
      edge_node$to_group[i] <- "62"
      
    }
    
    
    #Merge Marion County Sheriff Jail - TO
    if(name2 == 'MARION COUNTY SHERIFF  JAIL II'|
       name2 == 'MARION COUNTY SHERIFF  JAIL I'){
      
      edge_node$To[i] <- '867124_NA'
      
      edge_node$name2[i] <- 'MARION COUNTY SHERIFF JAIL'
      
      edge_node$to_group[i] <- "92"
      
    }
    
    
    #Merge Police stations - TO 
    if(name2 == 'INDIANAPOLIS METROPOLITAN POLICE DEPARTMENT EAST DISTRICT'  |
       name2 == 'INDIANAPOLIS METROPOLITAN POLICE DEPARTMENT NORTH DISTRICT' |
       name2 == 'INDIANAPOLIS METROPOLITAN POLICE DEPARTMENT NORTHWEST DISTRICT' |
       name2 == 'INDIANAPOLIS METROPOLITAN POLICE DEPARTMENT SOUTHEAST DISTRICT' |
       name2 == 'INDIANAPOLIS METROPOLITAN POLICE DEPARTMENT SOUTHWEST DISTRICT'){
      
      edge_node$name2[i] <- 'INDY METRO POLICE'
      edge_node$To[i]    <- '864294_NA'
      edge_node$to_group[i] <- "92"
    }
    
    
    #Merge Goodwill - TO
    if(name2 == 'GOODWILL INDUSTRIES OF CENTRAL INDIANA'){
      
      edge_node$To[i] <- '864837_418626345'
      
    }
    
    #Merge Community Hospital East - TO
    if(name2 == 'COMMUNITY HOSPITAL EAST'){
      
      edge_node$To[i] <- '867109_406699679'
      edge_node$to_group[i] <- "62"
      
    }
    
    #progress$inc(1/n, detail = paste("Doing part", i))
    
  }
  
  
  edge <- edge_node[,1:2]

  colnames(edge) <- c("from", "to")
  
  write.csv(edge, "data/edge.csv", row.names = F)
  
  #Color code From
  if(issue == "Education/Workforce Development" ){
    
    group.pal <- c('#B22222', '#CD5C5C', '#FF7F50', '#F0E68C', '#90EE90', '#ADD8E6', '#B0C4DE',  '#D8BFD8', ' #FFB6C1', '#808080')
    
    
    node1 <- data.frame(edge_node$From, 
                        edge_node$name1, 
                        ifelse(edge_node$from_group == "Elementary & Secondary Schools", group.pal[1],
                               ifelse(edge_node$from_group == "Post-Secondary Education", group.pal[2],
                                      ifelse (edge_node$from_group == "Training", group.pal[3],
                                              ifelse(edge_node$from_group == "Job placement & Career Advancement", group.pal[4],
                                                     ifelse(edge_node$from_group == "Employer Services", group.pal[5],
                                                            ifelse(edge_node$from_group == "Support for Workforce", group.pal[6],
                                                                   ifelse(edge_node$from_group == "Support for Education", group.pal[7],
                                                                          ifelse(edge_node$from_group == "Civic and Social Service Organizations", group.pal[8],
                                                                                 ifelse(edge_node$from_group == "Government Administration", group.pal[9],
                                                                                        ifelse(edge_node$from_group == "Program", group.pal[10], group.pal[10]
                                                                                 )))))))))),
                        
                        'black',
                        ifelse(edge_node$from_group == "Program", "square", "dot"))

    colnames(node1) <- c("id", "label", "color.background", "color.border", "shape")
    
    
    # lnodes <- data.frame(id = 1:10, 
    #                      label = c("Elementary & Secondary Schools",
    #                                "Post-Secondary Education",              
    #                                "Training",
    #                                "Job placement & Career Advancement",
    #                                "Employer Services",
    #                                "Support for Workforce",
    #                                "Support for Education",
    #                                "Civic and Social Service Organizations",
    #                                "Government Administration",
    #                                "Program"), 
    #                      color = c(group.pal),
    #                      shape = rep("ellipse", 10)
    #)
    
  }else if(issue == "Opioid Addiction/Substance Abuse"){
    
    
    group.pal <- c('#CD5C5C', ' #FF7F50', ' #F0E68C', '#90EE90', '#ADD8E6', '#D8BFD8', '#FFB6C1', '#808080')
    
    
    node1 <- data.frame(edge_node$From, 
                        edge_node$name1, 
                        ifelse(edge_node$from_group == '92', group.pal[1],
                               ifelse(edge_node$from_group == '8133', group.pal[2],
                                      ifelse(edge_node$from_group == '8134', group.pal[3], 
                                             ifelse(edge_node$from_group == '54', group.pal[4],
                                                    ifelse(edge_node$from_group == '62', group.pal[5], 
                                                           ifelse(edge_node$from_group == '44', group.pal[6],
                                                                  ifelse(edge_node$from_group == '8131', group.pal[7], 
                                                                         ifelse(edge_node$from_group == 'Coalition', group.pal[8], group.pal[8])))))))),
                                                                                
                        "black",
                         ifelse(edge_node$from_group == "Coalition", "square", "dot"))
    
    colnames(node1) <- c("id", "label", "color.background", "color.border", "shape")
    
    # lnodes <- data.frame(id = 1:9, 
    #                      label = c("Public Adminstration",
    #                                "Social Advocacy Organizations", 
    #                                "Civic and Social Organizations", 
    #                                "Professional, Scientific, and Technical Services",
    #                                "Health Care and Social Assistance",
    #                                "Retail Trade",
    #                                "Religious Organizations",
    #                                "Coalition",
    #                                "No category"
    #                                ), 
    #                      color = c(group.pal),
    #                      shape = rep("ellipse", 9)
    # )
    
  }
    
    
  #Color code To
  if(issue == "Education/Workforce Development" ){
    
    group.pal <- c('#B22222', '#CD5C5C', '#FF7F50', '#F0E68C', '#90EE90', '#ADD8E6', '#B0C4DE',  '#D8BFD8', ' #FFB6C1', '#808080')
    
    
    node2 <- data.frame(edge_node$To, 
                        edge_node$name2, 
                        ifelse(edge_node$to_group == "Elementary & Secondary Schools", group.pal[1],
                               ifelse(edge_node$to_group == "Post-Secondary Education", group.pal[2],
                                      ifelse (edge_node$to_group == "Training", group.pal[3],
                                              ifelse(edge_node$to_group == "Job placement & Career Advancement", group.pal[4],
                                                     ifelse(edge_node$to_group == "Employer Services", group.pal[5],
                                                            ifelse(edge_node$to_group == "Support for Workforce", group.pal[6],
                                                                   ifelse(edge_node$to_group == "Support for Education", group.pal[7],
                                                                          ifelse(edge_node$to_group == "Civic and Social Service Organizations", group.pal[8],
                                                                                 ifelse(edge_node$to_group == "Government Administration", group.pal[9], 
                                                                                        ifelse(edge_node$to_group == "Program", group.pal[10], group.pal[10]
                                                                                 )))))))))),
                        
                        'black',
                        ifelse(edge_node$to_group == "Program", "square", "dot"))
    
    colnames(node2) <- c("id", "label", "color.background", "color.border", "shape")
    
    
    
    
  }else if(issue == "Opioid Addiction/Substance Abuse"){
    
    
    group.pal <- c('#CD5C5C', ' #FF7F50', ' #F0E68C', '#90EE90', '#ADD8E6', '#D8BFD8', '#FFB6C1', '#808080')
    
    
    node2 <- data.frame(edge_node$To, 
                        edge_node$name2, 
                        ifelse(edge_node$to_group == '92', group.pal[1],
                               ifelse(edge_node$to_group == '8133', group.pal[2],
                                      ifelse(edge_node$to_group == '8134', group.pal[3], 
                                             ifelse(edge_node$to_group == '54', group.pal[4],
                                                    ifelse(edge_node$to_group == '62', group.pal[5], 
                                                           ifelse(edge_node$to_group == '44', group.pal[6],
                                                                  ifelse(edge_node$to_group == '8131', group.pal[7], 
                                                                         ifelse(edge_node$to_group == 'Coalition', group.pal[8], group.pal[8])))))))),
                        
                        "black",
                        ifelse(edge_node$to_group == "Coalition", "square", "dot"))
    
    colnames(node2) <- c("id", "label", "color.background", "color.border", "shape")
    
    # lnodes <- data.frame(id = 1:9, 
    #                      label = c("Public Adminstration",
    #                                "Social Advocacy Organizations", 
    #                                "Civic and Social Organizations", 
    #                                "Professional, Scientific, and Technical Services",
    #                                "Health Care and Social Assistance",
    #                                "Retail Trade",
    #                                "Religious Organizations",
    #                                "Coalitions",
    #                                "No category"
    #                                ), 
    #                      color = c(group.pal),
    #                      shape = rep("ellipse", 9)
    # )
    
  }
  
  
  if(unconn == 1 && issue == "Opioid Addiction/Substance Abuse"){
    
    # all.oi.q <- "MATCH (a:organization)
    # WHERE  a.opioid_involved = '1'
    # RETURN a.blended_id AS id,
    # a.org_name   AS label"
    # 
    # all.oi <- cypher(graph, all.oi.q)
    
    all.oi <- all_oi_py()
    
    all.oi <- data.frame(all.oi, 
                         color.background = '#808080', 
                         color.border = "#808080",
                         shape = "dot"
    )
    
    node <- rbind(node1, node2, all.oi)
    
  } else if(unconn == 1 && issue == "Education/Workforce Development"){
    
    # all.oi.q <- "MATCH (a:organization)
    # WHERE   a.workforce_involved = '1'
    # RETURN  a.blended_id AS id,
    # a.org_name   AS label"
    # 
    # all.oi <- cypher(graph, all.oi.q)
    
    
    all.oi <- all_ed_py()
    
    all.oi <- data.frame(all.oi, 
                         color.background = '#808080', 
                         color.border = "#808080", 
                         shape = "dot"
    )
    
    node <- rbind(node1, node2, all.oi)
  
  }else if(unconn == 2){
    
    node <- rbind(node1, node2)
    
  }
  
    
  
  node <- node[!duplicated(node$id),]
  
   network <<- graph.data.frame(edge, vertices = node, directed = T)
  
   if(node_size == 1){
   
   #In Degree
   size <- data.frame(degree(network, mode = c("in")))
   
   size <- data.frame(label = rownames(size), 
                      value  = size[,1])

   }else if(node_size == 2){
     
   #Out Degree
     size <- data.frame(degree(network, mode = c("out")))
     
     size <- data.frame(label = rownames(size), 
                        value  = size[,1])  
     
    
   }else if(node_size == 3){
     
    #Degree
     size <- data.frame(degree(network, mode = c("all")))
     
     size <- data.frame(label = rownames(size), 
                        value  = size[,1])
     
   }else if(node_size == 4){
     
     #Centrality
     size <- data.frame(betweenness(network, v = V(network), directed = F))
     
     
     size <- data.frame(label = rownames(size), 
                        value  = size[,1])
     
     
   }else if (node_size == 5){
     
     #Page Rank
     size <- data.frame(page_rank(network)$vector)
     
     size <- data.frame(label = rownames(size), 
                        value  = size[,1])
     
   }else if(node_size == 6){
     
     size <- data.frame(vertex_attr(network, name = "name"))
     
     size <- data.frame(label = size[,1],
                        value = 1)
     
   }
  
   
   min.size <- unique(sort(size$value))[2]/2
   
   size$value[size$value == 0] <- min.size
   

   
   node <- merge(node, size, by.x = 'id', by.y = 'label')
   
   node$label <- as.character(node$label)
   
   for(i in 1:nrow(node)){
     
     node$label[i] <- simpleCap(tolower(node$label[i]))
     #progress$inc(1/n, detail = paste("Preparing Network...", i))
   }
   
   count <- paste0("Displaying ", nrow(node), " nodes")
   
   
   visNetwork(node, 
              edge, 
              height = "100%",
              width = "100%", 
              footer = list(text = count, style = ("float:right;"))
   ) %>%
     visOptions(highlightNearest = T) %>%
     visEdges(arrows = "to", color = list(color = 'darkgray', highlight = '#00BFFF') ) %>%
     visEdges(smooth = FALSE) %>%
     visIgraphLayout(layout ="layout_nicely") %>%
     visNodes(scaling = list(min = 10, max = 50))
   
}








