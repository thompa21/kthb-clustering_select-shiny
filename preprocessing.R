library(DBI)
library(odbc)
library(RODBC)

# setting location
#setwd("G:/ECE/KTHB/PI/Bibliometri/BibCap/Analyser/2018-08-07 Clustering_shiny")

# setting user info
source("env.R")
#user<- "tobias"
#userpw<- "zTx_sX52.Y"

bibclust_handle_odbc <- DBI::dbConnect(odbc::odbc(),Driver="ODBC Driver 17 for SQL Server",Server = "bibmet-prod.ug.kth.se",Database = "bibclust", UID = user, PWD = userpw) #dbConnect
bibmet_handle_odbc <- DBI::dbConnect(odbc::odbc(), Driver="ODBC Driver 17 for SQL Server",Server = "bibmet-prod.ug.kth.se",Database = "Bibmet_218", UID = user, PWD = userpw) #dbConnect

# Setting variables
clu_level<- 2

# functions
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
      sep="", collapse=" ")
}

authorCap <- function(x) {
  s <- strsplit(x, ", ")[[1]]
  paste(paste(toupper(substring(s[1], 1,1)), tolower(substring(s[1], 2)),
      sep="", collapse=" "),  toupper(s[2]), sep=", ")
}

# defining accessable clusterlevels
clu_level<- 1
clusterlevels<- c(1,2,3,4) # c(1,2,3,4)

query_droptest <- "
    IF ( OBJECT_ID('tempdb..##clusterlevels') IS NOT NULL )
    DROP TABLE ##clusterlevels
    "
dbSendQuery(bibmet_handle_odbc,query_droptest)

#getting cluster levels, inkl labels
query_levels<- paste0("
	SELECT t1.*,1 as level INTO ##clusterlevels
	FROM bibclust.dbo.lev",clu_level,"_labels_18q2 as t1
	")
dbSendQuery(bibmet_handle_odbc,query_levels)

clu_level<- 2
query_levels<- paste0("
	INSERT INTO ##clusterlevels SELECT t1.*,2 as level
	FROM bibclust.dbo.lev",clu_level,"_labels_18q2 as t1
	")
dbSendQuery(bibmet_handle_odbc,query_levels)

clu_level<- 3
query_levels<- paste0("
	INSERT INTO ##clusterlevels SELECT t1.*,3 as level
	FROM bibclust.dbo.lev",clu_level,"_labels_18q2 as t1
	")
dbSendQuery(bibmet_handle_odbc,query_levels)

clu_level<- 4
query_levels<- paste0("
	INSERT INTO ##clusterlevels SELECT t1.*,4 as level
	FROM bibclust.dbo.lev",clu_level,"_labels_18q2 as t1
	")
dbSendQuery(bibmet_handle_odbc,query_levels)

query_levels<- paste0("
	SELECT t1.label as 'Label',t1.level
	FROM ##clusterlevels as t1 order by Label
	")
levels_clu_pre<- dbGetQuery(bibmet_handle_odbc,query_levels)

# query_levels<- paste0("
# 	SELECT *
# 	FROM bibmet_318.dbo.Author
# 	")
# authors<- dbGetQuery(bibmet_handle_odbc,query_levels)

#query_levels<- paste0("exec selected_uts '2' ,'211' ,'2016'")
#selected_uts<- dbGetQuery(bibclust_handle_odbc,query_levels)
#View(selected_uts)

#View(levels_clu_pre)
#levels_clu2<-  sqlQuery(bibclust_handle_odbc,query_levels)
#levels_clu2$label<- as.factor(levels_clu2$label)

  
 