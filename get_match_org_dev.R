library(RNeo4j)
library(plyr)

graph = startGraph("http://156.56.32.125:7474/db/data/", username="neo4j", password='idp_savi')


get.match.org <- function(one.size, cats){
  
  
  #progress <- shiny::Progress$new()
  
  #on.exit(progress$close())
  
  #progress$set(message = "Mapping...", value = 0)
  
  #n <- length(cats)
  
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
    
    
    match.org.q <-
      paste0(
        "MATCH ",
        all.pattern,
        " WHERE ",
        all.where,
        "
        AND    a.opioid_involved =  'NA'
      
        RETURN a.org_name as org, ",
        all.return,
        ", toFloat(a.latitude) AS latitude, toFloat(a.longitude) AS longitude"
      )
    
    #AND    a.latitude        <> 'NULL'
    #OR     a.latitude        <> 'NA'
    
    match.org <-  cypher(graph, match.org.q)
    
    #match.org <- match.org[complete.cases(match.org),]
    
    #cols <- 2:(one.size+1)
    
    for(i in 2:(one.size+1)){
      
      match.org$category <- paste(match.org$category, match.org[,i], ', ', sep = "" )
      
    }
    
    #match.org$category <- paste(match.org[2:(one.size+1)], sep = ", ")
    
    match.org$category <- lapply(tolower(match.org$category), FUN = simpleCap)
    
    match.org$category = substr(match.org$category,1,nchar(match.org$category)-1)
    
    res.l[[m]] <- match.org
    
    #progress$inc(1/n, detail = paste("Doing part", m))
    
  } 
    res.out <- ldply(res.l, data.frame)
  
    return(res.out)
  
}


shr.dim.tab <- get.match.org(2, cats = cats)

class(shr.dim.tab)


orgs <- unique(shr.dim.tab$org)


orgC <- c()

for(i in 1:length(orgs)){
  
  orgC <- c(orgC, simpleCap(tolower(orgs[i])))
  
}



res.out <- get.match.org(2, cats = cats)

colnames(cats)<- "Col1"

cats.II <- data.frame(combn(cats$Col1, 2))


get.match.org(2, cats = cats.II)
