library(shiny)
require(httr)
library(ggplot2)
library(RODBC)
library(DBI)
library(odbc)
#library(igraph)
#library(networkD3)
require(dplyr)
library(plotly)
library(DT)
#library(utf8)

shinyServer(function(input, output, session) {
  
  user <- reactive({
    session$user
  })
  
  #Test Hämta label från inlästa lev1_labels_18q2 (levels_clu_pre)
  # och spara i en DT som användaren kan välja från (istället för en select med +40000 rader!)
  ilabels_pre <- reactive({
    print("ilabels_pre")
    levels_clu_pre[levels_clu_pre$level == input$clusterlevels[1],]
  })
  
  output$current_label <- reactive ({
    ilabels_pre()[rownames(ilabels_pre())[input$ilabels_rows_selected],1]
  })
  output$ilabels <-   DT::renderDataTable({
    DT::datatable(
      ilabels_pre()
      ,extensions = 'Responsive'
      #,selection = 'single'
      ,selection = list(mode = 'single', selected = c(1))
      ,rownames = TRUE
      ,filter = 'top'
      ,options = list(
        pagingType = "full",
        dom = 'tp'
        ,pageLength = 3
        #,scrollY = "200px"
        ,columnDefs = list(list(visible=FALSE, targets=c(0,2)))
        ,language = list(
          paginate = list(previous = '<', `next` = '>')
        )
        ,initComplete = JS(
          "function(settings, json) {"
          ,"$(this.api().table().header()).css({'background-color': '#222d32', 'color': '#fff'});"
          ,"$('.paginate-button').css({'background-color': '#222d32', 'color': '#fff'});"
          ,"}")
        ,rowCallback = JS("function(r,d) {$(r).attr('height', '80px')}")
        )
    )
    
  })
  
  output$username <- renderText( paste("Session User: ", {session$user}))
  output$groups <- renderText({session$groups})
  output$token <- renderText({session$token})
  
  output$userinfo <- reactive({
    # Find the names of all the keys in clientData
    cnames <- names(session$clientData)

    # Apply a function to all keys, to get corresponding values
    allvalues <- lapply(cnames, function(name) {
      item <- session$clientData[[name]]
      if (is.list(item)) {
        list_to_string(item, name)
      } else {
        paste(name, item, sep=" = ")
      }
    })
    paste(allvalues, collapse = "\n")
  })
  
  isManager <- reactive({
    if (user() == "kthb"){
      return(TRUE)
    } else{
      return(FALSE)
    }
  })
  
  levels_clu <- reactive({
    # Getting labels for chosen level
    #----------------------------------------
    #getting cluster levels, inkl labels
    query_levels<- paste0("
                        	SELECT t1.*
                        	FROM lev",input$clusterlevels,"_labels_18q2 as t1
                        	")
    dbGetQuery(bibclust_handle_odbc,query_levels)
  })
  
  # Pushing labels to UI
  #_--------------------------------
  output$ui.A = renderUI({
    #selectizeInput("xclust", label = 'Labels', choices = as.factor(levels_clu()$label)) #,selected = 1)
  })
  
  # Retrieve selected cluster
  #--------------------------------------
  selected_data <- eventReactive(input$update, {
  #selected_data <-reactive({
    # changed from "reactive" to eventReactive to create response only on update button
    #selected_cluster<- as.numeric(levels_clu()[levels_clu()$label == input$xclust[1],][1])
    print(ilabels_pre()[rownames(ilabels_pre())[input$ilabels_rows_selected],1])
    #print(levels_clu()[levels_clu()$label == levels_clu_pre[rownames(levels_clu_pre)[input$ilabels_rows_selected],1]])
    selected_cluster<- as.numeric(levels_clu()[levels_clu()$label == ilabels_pre()[rownames(ilabels_pre())[input$ilabels_rows_selected],1],][1])
    #selected_cluster<-levels_clu_pre[rownames(levels_clu_pre)[input$ilabels_rows_selected],1]
    query_selection <- paste0("
                              SELECT t1.*
                              FROM R_P_cit_clusterinfo_18q2 as t1
                              WHERE (t1.cluster_lev",input$clusterlevels,"_RC = ",selected_cluster,") 
                              AND (t1.Publication_year >=", as.numeric(input$starty),")")
    datamessage<- input$starty[1]
    print(datamessage)
    selected_data_sql<- dbGetQuery(bibclust_handle_odbc,query_selection)
    selected_data_sql$label<- ilabels_pre()[rownames(ilabels_pre())[input$ilabels_rows_selected],1] #input$xclust[1]
    
    #preprocessing
    
    # Upload retrieved UTs to temp table
    #----------------------------------------
    
    selected_ids<- as.data.frame(selected_data_sql$ut)	# Get UTs of selected data
    names(selected_ids)<- c("UT")
    query_droptest <- "
    IF ( OBJECT_ID('tempdb..##selected_uts') IS NOT NULL )
    DROP TABLE ##selected_uts
    "
    dbSendQuery(bibmet_handle_odbc,query_droptest)
    print("dbWriteTable start")
    print(Sys.time())
    dbWriteTable(bibmet_handle_odbc, name = "##selected_uts", value = selected_ids, row.names = FALSE, TEMPORARY=FALSE)
    print("dbWriteTable end")
    print(Sys.time())
    # choose columns to display
    selected_data_sql[,c("ut","Publication_year",paste0("cluster_lev",input$clusterlevels,"_RC"),"label")]
  })
  
  #---------------------------------------
  
  # Output of selected data
  output$selectiontable <-   DT::renderDataTable({
    DT::datatable(selected_data(), 
                  options = list(orderClasses = TRUE, lengthMenu = c(10, 50, 200), pageLength = 50))
    # sorted columns are colored now because CSS are attached to them
  })
  
  output$datamessage <- renderPrint({
    #input$xclust[2]
  })
  
  # Getting biblioinfo for selected publications
  #
  # Currently extremely slow to use - hangs app
  #
  #------------------------------------------------
  biblio_data<- eventReactive(input$update,{
    # changed from "reactive" to eventReactive to create response only on update button
    
    #input$xclust  
    query_biblio_temp <- paste0("
                                SELECT t1.Title,
                                t1.Year,
                                t1.Authors,
                                t1.[Journal/Host publication] as Journal
                                FROM v_Bibliodata as t1
                                INNER JOIN ##selected_uts as t2 on (t1.UT = t2.UT)")
    #biblioinfo_sql<- dbGetQuery(bibmet_handle_odbc,query_biblio_temp)
  })
  
  # Output table of publications
  #output$publicationstable <-   DT::renderDataTable({
  #DT::datatable(biblio_data())
  #})
  
  
  # Calculate and plot publication count per year
  #-----------------------------------------------
  publ_time<- eventReactive(input$update,{
  #publ_time<- reactive({
      aggregate(ut ~ Publication_year , data = selected_data(), FUN = length)
  })
  
  output$plot_time <- renderPlot({
    ggplot(data=publ_time(), aes(x=Publication_year, y=ut)) +
      geom_line() + geom_point() +
      xlab("Year") +
      ylab("Count")
  })
  
  
  # Getting addresses for selected publications
  #------------------------------------------------
  address_data<- eventReactive(input$update,{
  #address_data<- reactive({
    #input$xclust
    #if (isManager()){
    query_join_temp <- paste0("
                             SELECT t1.UT, t1.Country_name,t1.City,t1.Unified_org_id,t1.Org_divided,t1.Name_eng,t1.Org_type_code
                             FROM v_BestResAddr as t1
                            INNER JOIN ##selected_uts as t2 on (t1.UT = t2.ut)")
    
    query_join_temp <- paste0("exec get_addresses '", input$clusterlevels , "' ,'" , as.numeric(levels_clu()[levels_clu()$label == ilabels_pre()[rownames(ilabels_pre())[input$ilabels_rows_selected],1],][1]) , "' ,'" , as.numeric(input$starty) , "'")
    print(query_join_temp)
    #} else{
      # query_join_temp <- paste0("
      #                         SELECT 't1.Uz', 't1.Country_name','t1.City','t1.Unified_org_id','t1.Org_divided','t1.Name_eng','t1.Org_type_code'
      #                         ")
    #}
    print("addresses_sql start")
    print(Sys.time())
    #addresses_sql<- dbGetQuery(bibmet_handle_odbc,query_join_temp)
    addresses_sql<- dbGetQuery(bibclust_handle_odbc,query_join_temp)
    print("addresses_sql end")
    print(Sys.time())
    #pre-processing
    #addresses_sql$Country_name<- iconv(addresses_sql$Country_name, from="latin1", to='UTF-8')	# Fix encoding issue with e.g. Côte d'ivoire
    #addresses_sql$City<- iconv(addresses_sql$City, to='UTF-8')
    #addresses_sql$Country_name<- as.factor(addresses_sql$Country_name)
    #addresses_sql$City<- sapply(addresses_sql$City,simpleCap)
    #addresses_sql$City<- as.factor(addresses_sql$City)
    addr_display<- c("UT","Unified_org_id","Org_divided","Name_eng","City","Country_name","Org_type_code")	#select field to show
    addresses_sql[,addr_display]
  })
  
  # Output table of addresses
  output$addresstable <-   DT::renderDataTable(
    {
      DT::datatable(address_data(),
                    filter = 'top'
                    # ,options = list(
                    #   autoWidth = TRUE,
                    #   columnDefs = list(list(
                    #     width = '70px', targets = c(2,3,7)
                    #     ))
                    # )
                    ,extensions = 'Responsive'
                  )
    }
  )
  
  # Calculating and plotting publication counts per country
  # ---------------------------------------------------------
  publ_country<- eventReactive(input$update,{
  #publ_country<- reactive({
    # count unique country occurrences
    countries_unique<- distinct(address_data(), UT, Country_name)
    country_counts<- aggregate(UT ~ Country_name , data = countries_unique, FUN = length)
    country_counts[order(-country_counts$UT),]
  })
  
  # Barplot
  n_countries<- 20
  output$plot_countrycount<-renderPlot({
    #s = input$publ_country_rows_selected
    ggplot(data=publ_country()[1:n_countries,], 
         aes(x=reorder(Country_name, -UT), y=UT)) + 
      geom_bar(stat="identity") + 
      xlab("Country") +
      ylab("Publication count") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  }
  #,height = 400
  )	
  
  # Outout table of publications counts per country
  output$publ_country <- DT::renderDataTable({
    DT::datatable(publ_country(),
                  options=list(pageLength=20)
                  ,extensions = 'Responsive'
                  )
  })
  
  
  # Getting author info for selected publications
  #------------------------------------------------
  author_data<- eventReactive(input$update,{
  #author_data<- reactive({
    
    #input$xclust
    query_authors <- paste0("
                            SELECT t1.Author_id,
                            t1.UT,
                            t1.Name_eng,
                            t1.City,
                            t1.Country_name,
                            t3.Name
                            FROM ##selected_uts as t2 
                            INNER JOIN v_BestResAddrAuthorid as t1 on (t1.UT = t2.ut)
                            Inner join Author as t3 on (t1.Author_id = t3.Author_id)")
    
    authorinfo<- dbGetQuery(bibmet_handle_odbc,query_authors)
    #authorinfo<- dbGetQuery(bibclust_handle_odbc,query_authors)
    #pre-processing
    print("author_data")
    #authorinfo$Country_name<- iconv(authorinfo$Country_name, from="latin1", to='UTF-8')	# Fix encoding issue with e.g. Côte d'ivoire
    #authorinfo$Name<- sapply(authorinfo$Name,authorCap)
    #authorinfo$City<- sapply(authorinfo$City,simpleCap)
    authorinfo
  })
  
  # Getting author info for selected publications
  #------------------------------------------------
  author_count_test<- eventReactive(input$update,{
  #author_count_test<- reactive({
    
    #input$xclust
    query_authors_test <- paste0("
                            SELECT distinct
                            t1.UT,
                            t3.Name,
                            t1.Name_eng,
                            t1.Country_name
                            FROM ##selected_uts as t2 
                            INNER JOIN v_BestResAddrAuthorid as t1 on (t1.UT = t2.ut)
                            Inner join Author as t3 on (t1.Author_id = t3.Author_id)")
    
    query_authors_test <- paste0("exec get_authors '", input$clusterlevels , "' ,'" , as.numeric(levels_clu()[levels_clu()$label == ilabels_pre()[rownames(ilabels_pre())[input$ilabels_rows_selected],1],][1]) , "' ,'" , as.numeric(input$starty) , "'")
    authorcount<- dbGetQuery(bibclust_handle_odbc,query_authors_test)
    #authorcount<- dbGetQuery(bibmet_handle_odbc,query_authors_test)
    #pre-processing
    #print(authorcount)
    #authorcount$Country_name<- iconv(authorcount$Country_name, from="latin1", to='UTF-8')	# Fix encoding issue with e.g. Côte d'ivoire
    #authorcount$Name<- sapply(authorcount$Name,authorCap)
    #authorcount$City<- sapply(authorcount$City,simpleCap)
    authorcount
  })
  
  # Output table of authorinfo
  output$authortable <-   DT::renderDataTable({
    DT::datatable(author_data()
                  ,extensions = 'Responsive'
                  )		#author_data()
  })
  
  # Calculating and plotting publication counts per author name
  # ---------------------------------------------------------
  publ_authors<- eventReactive(input$update,{
  #publ_authors<- reactive({  
    # count unique country occurrence
    
    print("authors_unique start")
    print(Sys.time())
    #authors_unique<- distinct(author_data(), UT, Name, Name_eng, Country_name)
    #authors_unique<- distinct(author_count_test(), UT, Name, Name_eng, Country_name)
    authors_unique<- author_count_test()
    print("authors_unique end")
    print(Sys.time())
    author_counts<- aggregate(UT ~ Name + Name_eng + Country_name, data = authors_unique, FUN = length)
    #author_counts<- aggregate(UT ~ Name + Name_eng + Country_name, data = author_count_test(), FUN = length)
    print("aggregate end")
    print(Sys.time())
    #print(author_count_test())
    
    author_counts[order(-author_counts$UT),]
    #print(author_counts)
  })
  
  # Barplot
  n_authors<- 25
  
  output$plot_authorcount<-renderPlot({
    ggplot(data=publ_authors()[1:n_authors,], 
    #ggplot(data=author_count_test()[1:n_authors,], 
           aes(x=reorder(Name, -UT), y=UT)) + 
       geom_bar(stat="identity") + 
      xlab("Author") +
      ylab("Publication count") +
      #theme(axis.text.x=element_text(angle=65,hjust=1,vjust=0.5))
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  }
  #,height = 450
  )	
  
  output$publ_author <- DT::renderDataTable({
    DT::datatable(publ_authors(), colnames = c('Organization' = 'Name_eng'),
                  options=list(pageLength=20)
                  ,extensions = 'Responsive'
                  )
  })
  
  
  # Calculating and plotting publication counts per org and country
  # ---------------------------------------------------------
  publ_orgs<- eventReactive(input$update,{
  #publ_orgs<- reactive({
    # count unique country occurrences
    print("org_unique start")
    print(Sys.time())
    org_unique<- distinct(author_data(), UT, Name_eng, Country_name)
    print("org_unique end")
    print(Sys.time())
    org_counts<- aggregate(UT ~ Name_eng + Country_name, data = org_unique, FUN = length)
    org_counts[order(-org_counts$UT),]
  })
  
  
  output$publ_org <- DT::renderDataTable({
     DT::datatable(publ_orgs(), colnames = c('Organization' = 'Name_eng'),
                   options=list(pageLength=20)
                   ,extensions = 'Responsive'
                   )
  })
  
  
  # Calculating and plotting publication counts per org and country
  # ---------------------------------------------------------
  publ_cities<- eventReactive(input$update,{
  #publ_cities<- reactive({
    # count unique country occurrences
    cities_unique<- distinct(author_data(), UT, City, Country_name)
    cities_counts<- aggregate(UT ~ City + Country_name, data = cities_unique, FUN = length)
    cities_counts[order(-cities_counts$UT),]
  })
  
   output$publ_cities <- DT::renderDataTable({
     DT::datatable(publ_cities(),
                   options=list(pageLength=20)
                   ,extensions = 'Responsive'
                   )
   })
  
  session$onSessionEnded(function() {
    #odbcClose(bibclust_handle) 
    #dbDisconnect(bibclust_handle_odbc)
    #dbDisconnect(bibmet_handle_odbc)
    print("session end")
  })
  
})

