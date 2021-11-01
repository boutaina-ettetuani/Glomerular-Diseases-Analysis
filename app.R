

library(BiocManager)
options(repos = BiocManager::repositories())
options(rsconnect.http.trace = TRUE)
options(rsconnect.http.verbose = TRUE)
options(shiny.fullstacktrace = TRUE)

#library(AnnotationDbi)
library(Biobase)
library(BiocGenerics)
library(BiocParallel)
library(DO.db)
library(DOSE)
library(GO.db)
library(GOSemSim)
#setwd("../GenePPI")
library(readr)
gene_PrbId <- read_csv("../GenePPI/input/gene_PrbId.csv",show_col_types = FALSE)
library(googlesheets4)
library(readr)
library(shiny)
library(shinydashboard)
library(shinyauthr)
library(dplyr)
library(shinyjs)
library(DT)
library(shinyWidgets)
# sample logins dataframe with passwords hashed by sodium package
user_base <- tibble(
    user = c("user1", "user2"),
    password = sapply(c("pass1s", "pass2s"), sodium::password_store), 
    permissions = c("admin", "standard"),
    name = c("User One", "User Two")
)

ui <- dashboardPage(
    
    # put the shinyauthr logout ui module in here
    dashboardHeader(
        title = "Gene PPI",
        tags$li(class = "dropdown", style = "padding: 8px;", shinyauthr::logoutUI("logout"))
    ),
    
    # setup a sidebar menu to be rendered server-side
    dashboardSidebar(
        collapsed = TRUE, sidebarMenuOutput("sidebar")
    ),
    
    
    dashboardBody(
        shinyjs::useShinyjs(),
        
        # put the shinyauthr login ui module here
        shinyauthr::loginUI("login"),
        
        # setup any tab pages you want after login here with uiOutputs
        tabItems(
            tabItem("tab0",uiOutput("tab0_ui")),
            tabItem("tab3", uiOutput("tab3_ui")),
            tabItem("tab4", uiOutput("tab4_ui")),
            tabItem("sub_1", uiOutput("sub_1_ui")),
            tabItem("sub_2", uiOutput("sub_2_ui")),
            tabItem("sub_3", uiOutput("sub_3_ui")),
            tabItem("tab5", uiOutput("tab5_ui"))
        )
        
    )
)

server <- function(input, output, session) {
    library(readr)
    
    # login status and info will be managed by shinyauthr module and stores here
    credentials <- callModule(shinyauthr::login, "login", 
                              data = user_base,
                              user_col = user,
                              pwd_col = password,
                              sodium_hashed = TRUE,
                              log_out = reactive(logout_init()))
    
    # logout status managed by shinyauthr module and stored here
    logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
    
    # this opens or closes the sidebar on login/logout
    observe({
        if(credentials()$user_auth) {
            shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
        } else {
            shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        }
    })
    
    # only when credentials()$user_auth is TRUE, render your desired sidebar menu
    output$sidebar <- renderMenu({
        req(credentials()$user_auth)
        sidebarMenu(
            id = "tabs",
            menuItem("Welcome", tabName = "tab0"),
            menuItem("Start Analysis", tabName = "tab3"),
            menuItem("Tutorial", tabName = "tab4",menuSubItem("Gene Prediction", tabName = "sub_1"), 
                     menuSubItem("Gene Prioritization", tabName = "sub_2"),menuSubItem("Gene Interaction", tabName = "sub_3")),
            menuItem("Contact Us", tabName = "tab5")
            # /menuItem 
        )
    })
    
    
    
    # tab 0 UI and output ----------------------------------------
    output$tab0_ui <- renderUI({
        req(credentials()$user_auth)
        DT::DTOutput("table0")
    })
    
    
    output$tab0_ui <- renderUI({
        req(credentials()$user_auth)
        #includeHTML("home.html")
        tabPanel("Home",
                 includeHTML("home.html"),
                 tags$script(src = "plugins/scripts.js"),
                 tags$head(
                     tags$link(rel = "stylesheet", 
                               type = "text/css", 
                               href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                     tags$link(rel = "icon", 
                               type = "image/png", 
                               href = "images/logo_icon.png")
                 )
        )
        
    })
    
    
    
    
    # sub_1 UI and output ----------------------------------------
    
    output$sub_1_ui <- renderUI({
        
        req(credentials()$user_auth)
        #includeHTML("home.html")
        tabPanel("Home",
                 includeHTML("prediction.html"),#../bmc_bioinformatics_version_3.html
                 tags$script(src = "plugins/scripts.js"),
                 tags$head(
                     tags$link(rel = "stylesheet", 
                               type = "text/css", 
                               href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                     tags$link(rel = "icon", 
                               type = "image/png", 
                               href = "images/logo_icon.png")
                 )
        )
        
    })
    
    output$sub_2_ui <- renderUI({
        
        req(credentials()$user_auth)
        #includeHTML("home.html")
        tabPanel("Home",
                 includeHTML("prioritization.html"),#../bmc_bioinformatics_version_3.html
                 tags$script(src = "plugins/scripts.js"),
                 tags$head(
                     tags$link(rel = "stylesheet", 
                               type = "text/css", 
                               href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                     tags$link(rel = "icon", 
                               type = "image/png", 
                               href = "images/logo_icon.png")
                 )
        )
        
    })
    
    output$sub_3_ui <- renderUI({
        
        req(credentials()$user_auth)
        #includeHTML("home.html")
        tabPanel("Home",
                 includeHTML("interaction.html"),#../bmc_bioinformatics_version_3.html
                 tags$script(src = "plugins/scripts.js"),
                 tags$head(
                     tags$link(rel = "stylesheet", 
                               type = "text/css", 
                               href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                     tags$link(rel = "icon", 
                               type = "image/png", 
                               href = "images/logo_icon.png")
                 )
        )
        
    })
    
    
    
    
    output$tab5_ui <- renderUI({
        req(credentials()$user_auth)
        #includeHTML("home.html")
        tabPanel("Home",
                 includeHTML("contact.html"),
                 tags$script(src = "plugins/scripts.js"),
                 tags$head(
                     tags$link(rel = "stylesheet", 
                               type = "text/css", 
                               href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                     tags$link(rel = "icon", 
                               type = "image/png", 
                               href = "images/logo_icon.png")
                 )
        )
        
    })
    
    
    
    
    # tab 3 UI and output ----------------------------------------
    
    output$tab3_ui <- renderUI({
        req(credentials()$user_auth)
        
        DT::DTOutput("table3")
        tabsetPanel(
            
            tabPanel(title = "Gene Prediction",
                     fluidPage(
                         sidebarLayout(
                             sidebarPanel(
                                 # Input: Select a file ----
                                 fileInput("file1", "Upload your Expression results (CSV) File"),
                                 
                                 
                                 textInput("caption", "Fix a P-value", ""),
                                 tags$hr(),
                                 verbatimTextOutput("P_adjust"),
                                 # Horizontal line ----
                                 
                                 # actionButton("action", label = "Start Analysis!",color = "btn action-button"), #class = "btn-success"
                                 actionButton("action", label = "Start Analysis", icon = icon("play-circle")),
                                 # Horizontal line ----
                                 tags$hr(),
                                 sliderInput("bw_adjust", label = "Threshold adjustment",
                                             min = 10, max = 100, value = 1, step = 0.2),
                                 
                                 
                             ),
                             mainPanel(
                                 tabsetPanel(
                                     id = 'dataset',
                                     tabPanel("Selected Expression table", DT::dataTableOutput("slide1")),
                                     tabPanel("semantic similarity visualisation", DT::dataTableOutput("slide2"),plotOutput("plot2")),
                                     tabPanel("Gene Prediction Results", DT::dataTableOutput("slide3"),DT::dataTableOutput("slide31")),
                                     tabPanel("Gene Prediction visualisation", DT::dataTableOutput("slide4"),plotOutput("plot4"))
                                     
                                 )
                             )
                         )
                     )),
            
            tabPanel(title = "Gene Prioritization",
                     fluidPage(
                         sidebarLayout(
                             sidebarPanel(
                                 
                                 # Horizontal line ----
                                 #tags$hr(),
                                 #numericInput("captionclus", "Number  of Clusters", value = 0, min = 0, max = 20),
                                 # Horizontal line ----
                                 
                                 #tags$hr(),
                                 #actionButton("actionclus", label = "Find!",color = "btn action-button"),
                                 #textInput("captionclus", "Number of Clusters", ""),
                                 #tags$hr(),
                                 # actionButton("action", label = "Start Analysis!",color = "btn action-button"), #class = "btn-success"
                                 #actionButton("actionclust", label = "Start Analysis", icon = icon("play-circle")),
                                 fileInput("file111", "Upload your Expression results (CSV) File"),
                                 # Input: Select a file ----
                                 fileInput("file2", "Upload Tissues (CSV) File",multiple = TRUE),
                                 fileInput("file21", "Upload Biological Proccess (CSV) File",multiple = TRUE),
                                 fileInput("file22", "Upload GWAS (CSV) File",multiple = TRUE),
                                 fileInput("file23", "Upload TFs (CSV) File",multiple = TRUE),
                                 # Horizontal line ----
                                 tags$hr(),
                                 actionButton("action2", label = "Start Analysis!",icon = icon("play-circle")),
                                 tags$hr(),
                                 textInput("captionclus", "Fix a number  of Clusters", ""),
                                 tags$hr(),
                                 verbatimTextOutput("captionclus_adjust"),
                                 actionButton("actionvalidclust", label = "Fix the number  of Clusters", icon = icon("play-circle")),
                                 # Horizontal line ----
                                 #tags$hr(),
                                 # Input: Checkbox if file has header ----
                                 #selectInput("col2", "Select a column", character(0)),
                                 # Horizontal line ----
                                 #tags$hr(),
                                 #textInput("caption2", "Tissue_name", ""),
                                 #tags$hr(),
                                 #verbatimTextOutput("Tissue_name"),
                                 # Horizontal line ----
                                 tags$hr(),
                                 multiInput(
                                     inputId = "id_Tissues", label = "Select Tissues :",
                                     choices = c("Kidney",	"Kidney cancer cell",	"Immune system",	"Urine",	"Blood",
                                                 "Blood vessel",	"Blood plasma",	"Hematopoietic stem cell",	"Parenchyma",
                                                 "Uroepithelium",	"HEK 293 EBNA cell",	"HEK 293ET cell",	"HK 2 cell", 
                                                 "Heart", "Intestine", "Leukocyte", "Stomach","Bone marrow","Muscle","Hepatoma cell",
                                                 "Ovary","Lung","Pancreas","Rhizome","Neuron","Breast cancer cell","Prostate cancer cell line","Vasculature",
                                                 "Arterial blood","Knee","Eye","Mesenchyme","Cancer stem cell","Vertebra"),
                                     selected = "", width = "400px",
                                     #options = list(
                                     # enable_search = FALSE,
                                     #  non_selected_header = "Choose between:",
                                     #  selected_header = "You have selected:"
                                     # ),
                                 ),
                                 
                                 verbatimTextOutput(outputId = "res_Tissues"),
                                 tags$hr(),
                                 actionButton("actiontis", label = "Update Tissues!",icon = icon("play-circle")),
                                 
                                 # actionButton("action", label = "Start Analysis!",color = "btn action-button"), #class = "btn-success"
                                 # Input: Select a file ----
                                 
                                 
                                 
                                 
                                 
                             ),
                             mainPanel(
                                 tabsetPanel(
                                     id = 'priori',
                                     tabPanel("Data Tables visualisation", DT::dataTableOutput("slide21"), DT::dataTableOutput("slide211"),
                                              DT::dataTableOutput("slide212"),DT::dataTableOutput("slide213"),
                                              plotOutput("plot21")),
                                     tabPanel("Clustering Analysis", DT::dataTableOutput("slide91"),DT::dataTableOutput("slide911"), DT::dataTableOutput("slide9111"),DT::dataTableOutput("slide91111"), plotOutput("slide991"), plotOutput("slide992")),
                                     
                                     tabPanel("Classification visualisation", DT::dataTableOutput("slide23"), DT::dataTableOutput("slide233"),
                                              plotOutput("plot23"),plotOutput("plot233"),plotOutput("plot2333"),plotOutput("plot23333")),
                                     tabPanel("Logistic Regression  Results", DT::dataTableOutput("slide222"),DT::dataTableOutput("slide22"),plotOutput("plot22")),
                                     tabPanel("Logistic Regression visualisation", DT::dataTableOutput("slide24"),plotOutput("plot24"))
                                     
                                     
                                 ))))),tabPanel(title = "Gene Interaction",fluidPage(
                                     sidebarLayout(
                                         sidebarPanel(
                                             
                                             # Horizontal line ----
                                             tags$hr(),
                                             multiInput(
                                                 inputId = "id_Genes", label = "Select Genes :",
                                                 
                                                 choices = unique(gene_PrbId$SYMBOL),#subset_priorized_genes$SYMBOL
                                                 selected = "", width = "400px",
                                                 #options = list(
                                                 # enable_search = FALSE,
                                                 #  non_selected_header = "Choose between:",
                                                 #  selected_header = "You have selected:"
                                                 # )
                                             ),
                                             verbatimTextOutput(outputId = "res_Genes"),
                                             # Horizontal line ----
                                             tags$hr(),
                                             actionButton("action3", label = "Find!",color = "btn action-button")
                                             
                                             
                                         ),
                                         mainPanel(
                                             tabsetPanel(
                                                 id = 'interac',
                                                 tabPanel("MTGI  Results", DT::dataTableOutput("slide441"),verbatimTextOutput("slide442"),DT::dataTableOutput("slide4411"))
                                                 #tabPanel("MTGI   Plot", DT::dataTableOutput("slide4441"),  plotOutput("plot4442"))
                                                 
                                                 
                                             )))))
            
        )
        
    })
    
    
    
    ########################################################
    ###Gene Prediction############################
    
    output$home_img <- renderImage({
        
        list(src = "www/header_img.png",
             width = "100%",
             height = 330)
        
    }, deleteFile = F)
    
    data1 <- reactive({
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)
        read.csv(inFile$datapath)
    })
    
    
    
    
    observeEvent(input$action,
                 output$slide1 <- DT::renderDT({
                     
                     DT::datatable(data1(), options = list(scrollX = TRUE))
                     
                     #req(data())
                 })
                 
                 
    )
    
    output$P_adjust <- renderText({  
        input$P_adjust
        #writeLines(input$caption, "pvalue.txt")
    })
    
    
    observeEvent(input$action,  
                 output$slide2 <- DT::renderDT({
                     #library(readr)
                     #gene_PrbId <- read_csv("../GenePPI/input/gene_PrbId.csv",show_col_types = FALSE)
                     library(GOSemSim)
                     library(org.Hs.eg.db)
                     hsGO_all <- godata('org.Hs.eg.db', ont=c("BP", "CC", "MF"))
                     #mgeneSim_output =mgeneSim(c("1363","189","54993"),semData=hsGO_all, measure="Wang",combine = "BMA" ,verbose=FALSE)# plot squared correlations
                     mgeneSim_output =mgeneSim(data1()[,"ENTREZID"],semData=hsGO_all, measure="Wang",combine = "BMA" ,verbose=FALSE)# plot squared correlations
                     #mgeneSim_output =mgeneSim(gene_PrbId$ENTREZID,semData=hsGO_all, measure="Wang",combine = "BMA" ,verbose=FALSE)# plot squared correlations
                     
                     
                     matrix <- mgeneSim_output
                     #pvalues = as.double(input$P_adjust)
                     pvalues = 0.0001
                     fgh = nrow(matrix)
                     
                     
                     loop.for.function <- function(){
                         
                         for( i in 1:fgh) {
                             j = i+1
                             for( j in 2:fgh) {
                                 #print(mgeneSim_0_000001[c(i,j),] )
                                 batch2009 <- matrix[i,]
                                 batch2015 <- matrix[j,]
                                 diff_in_means <- mean((batch2009 + batch2015)/2)
                                 s1 <- sd(batch2009)
                                 s2 <- sd(batch2015) 
                                 t_entropy <- sqrt(1/2*(((s1^2/s2^2)+(s2^2/s1^2)))*diff_in_means^2)
                                 ffff = matrix[i,]> t_entropy
                                 gggg_table = subset(matrix[i,], ffff)
                                 outputdf <- data.frame(Row  = i, Similarity_score = t_entropy, Selected_score = gggg_table)
                                 selection_based_simila <- (fgh*pvalues) + (length(outputdf$Row))* outputdf$Similarity_score
                                 
                                 selection_based_selection <- (fgh*pvalues) + (length(outputdf$Row))* outputdf$Selected_score 
                                 
                                 outputselection <- data.frame(Row  = i, Similarity_score = t_entropy, Selected_score = gggg_table,
                                                               FreqRow = length(outputdf$Row), selection_based_simila = selection_based_simila,
                                                               selection_based_selection = selection_based_selection)
                                 #outputdf5 <- data.frame(Row  = i, Similarity_score = t_entropy, Selected_score = gggg_table, selection_based_simila = selection_based_simila, selection_based_selection = selection_based_selection)
                                 
                                 mat2 = outputselection
                                 form2 = paste('../GenePPI/test/outputselection', i, 'csv', sep = '.')
                                 write.csv(mat2, file = form2)
                             }
                         }}
                     loop.for.function()
                     
                     
                 })
    )
    observeEvent(input$action,
                 output$plot2 <- renderPlot({ 
                     library(GOSemSim)
                     library(org.Hs.eg.db)
                     hsGO_all <- godata('org.Hs.eg.db', ont=c("BP", "CC", "MF"))
                     #mgeneSim_output =mgeneSim(c("1363","189","54993"),semData=hsGO_all, measure="Wang",combine = "BMA" ,verbose=FALSE)# plot squared correlations
                     mgeneSim_output =mgeneSim(data1()[,"ENTREZID"],semData=hsGO_all, measure="Wang",combine = "BMA" ,verbose=FALSE)# plot squared correlations
                     
                     library(variancePartition)
                     plotCorrMatrix( mgeneSim_output, dendrogram="none" )
                     
                 })  )
    
    
    
    output$slide31 <- DT::renderDT({ 
        ## Full path to csv filenames
        
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        gene_prediction_DT <- dataset_selected_0_000001[,c(1,5,7)]
        DT::datatable(gene_prediction_DT, options = list(scrollX = TRUE))
        
        # print(dataset_selected_0_000001)    #DT::datatable(dplyr::dataset_selected_0_000001, options = list(scrollX = TRUE))
        
        
    })
    
    output$plot4 <- renderPlot({ 
        ## Full path to csv filenames
        #write.csv( dataset_selected_0_000001, "../GenePPI/dataset_selected_0_000001.csv")
        
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        gene_prediction_DT <- dataset_selected_0_000001[,c(1,5,7)]
        
        p_0_000001 <-  ggplot(dataset_selected_0_000001,inline = F, aes(x = dataset_selected_0_000001$Row,
                                                                        y = dataset_selected_0_000001$selection_based_selection)) +
            geom_line(colour = dataset_selected_0_000001$ENTREZID,size=1) + #colour = dataset_litt$X   color = 'red'  linetype = "dotdash" alpha=0.1 size=1.3
            theme_classic() +
            labs(
                x = "Number of genes",
                y = "CombSC",
                title = paste(""
                )
            )
        p_0_000001 + geom_hline(yintercept = input$bw_adjust, color = 'red' )
        
    })
    output$slide4 <- DT::renderDT({ 
        ## Full path to csv filenames
        
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        library(dplyr)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
        
        gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
        gene_prediction_DT2 <- gene_prediction_Results[,c(1,5,7)]
    })
    
    
    
    
    
    ########################################################
    ###Gene Prioritization############################
    #output$res <- renderPrint({
    #input$Tid
    # })
    output$captionclus_adjust <- renderText({  
        # writeLines(input$captionclus, "captionclus.txt")
        input$captionclus
    })
    
    
    output$res_Tissues <- renderPrint({
        input$id_Tissues
        
    })
    
    
    data111 <- reactive({
        inFile <- input$file111
        if (is.null(inFile)) return(NULL)
        read.csv(inFile$datapath)
    })
    data2 <- reactive({
        inFile2 <- input$file2
        if (is.null(inFile2)) return(NULL)
        read.csv(inFile2$datapath)
    })
    
    observeEvent(data2(), {
        updateSelectInput(session, "col", choices = names(data2()))
    })
    
    
    data21 <- reactive({
        inFile21 <- input$file21
        if (is.null(inFile21)) return(NULL)
        read.csv(inFile21$datapath)
    })
    
    data22 <- reactive({
        inFile22 <- input$file22
        if (is.null(inFile22)) return(NULL)
        read.csv(inFile22$datapath)
    })
    data23 <- reactive({
        inFile23 <- input$file23
        if (is.null(inFile23)) return(NULL)
        read.csv(inFile23$datapath)
    })
    
    
    observeEvent(input$action2,
                 output$slide991 <- renderPlot({
                     ##########################################################
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     
                     
                     #################################################################
                     library(readr)
                     
                     my_data <- gene_prediction_Results
                     #names(my_data)[1] <-"ENTREZID"
                     
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in%  gene_prediction_Results$ENTREZID)
                     my_data2 <- merge(my_data, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     
                     #DT::datatable(my_data2, options = list(scrollX = TRUE))
                     my_data <- my_data %>%
                         na.omit() %>%          # Remove missing values (NA)
                         scale() 
                     # Compute
                     library("NbClust")
                     res.nbclust <- my_data %>%
                         scale() %>%
                         NbClust(distance = "euclidean",
                                 min.nc = 2, max.nc = 10, 
                                 method = "complete", index ="all") 
                     
                     # Visualize
                     library(factoextra)
                     fviz_nbclust(res.nbclust, ggtheme = theme_minimal())
                     
                 })
                 
    )
    
    
    
    observeEvent(input$actionvalidclust,
                 output$slide992 <- renderPlot({
                     ###################################################################################
                     
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     ####################################################################################
                     
                     
                     
                     library(readr)
                     my_data <- gene_prediction_Results
                     #names(my_data)[1] <-"ENTREZID"
                     
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in%  my_data$ENTREZID)
                     my_data2 <- merge(my_data, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     my_data2 <- merge(aaaaaaaa[, c("SYMBOL", "ENTREZID")], my_data, by="ENTREZID",all.x=TRUE)
                     my_data3 <-  my_data2[,c("SYMBOL","ENTREZID","selection_based_simila","selection_based_selection")]
                     ##########################################################################
                     
                     #####################################################################################################
                     my_dataclus  <- my_data2[,c("selection_based_simila","selection_based_selection")]
                     names <- make.unique(as.character(my_data3$SYMBOL))
                     row.names(my_dataclus) <- names  
                     ##########################################################################
                     
                     
                     #DT::datatable(my_data2, options = list(scrollX = TRUE))
                     my_data <- my_dataclus %>%
                         na.omit() %>%          # Remove missing values (NA)
                         scale() 
                     
                     # Compute hierarchical clustering
                     res.hc <- my_data %>%
                         scale() %>%                    # Scale the data
                         dist(method = "euclidean") %>% # Compute dissimilarity matrix
                         hclust(method = "ward.D2")     # Compute hierachical clustering
                     
                     #captionclus = as.double(read_lines( "../GenePPI/captionclus.txt"))
                     
                     fviz_dend(res.hc, k = as.double(input$captionclus),
                               cex = 0.7,                     # Label size
                               palette = "jco",               # Color palette see ?ggpubr::ggpar
                               rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                               rect_border = "jco",           # Rectangle color
                               labels_track_height = 0.8      # Augment the room for labels
                     )
                     
                     #fviz_dend(res.hc, k = 4, # Cut in four groups
                     #     cex = 0.5, # label size
                     #     k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                     #     color_labels_by_k = TRUE, # color labels by groups
                     #     rect = TRUE # Add rectangle around groups
                     #  )
                     
                 }))
    
    
    observeEvent(input$actionvalidclust,
                 output$slide911 <- DT::renderDT({
                     
                     
                     
                     ###############################################################################################
                     
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     
                     ###############################################################################################
                     library(readr)
                     library(tidyr)
                     library("cluster")
                     library("factoextra")
                     library("magrittr")
                     
                     my_data <- gene_prediction_Results
                     #names(my_data)[1] <-"ENTREZID"
                     
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in%  my_data$ENTREZID)
                     my_data2 <- merge(my_data, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     my_data2 <- merge(aaaaaaaa[, c("SYMBOL", "ENTREZID")], my_data, by="ENTREZID",all.x=TRUE)
                     my_data3 <-  my_data2[,c("SYMBOL","ENTREZID","selection_based_simila","selection_based_selection")]
                     
                     ##########################################################################
                     
                     #####################################################################################################
                     my_dataclus  <- my_data2[,c("selection_based_simila","selection_based_selection")]
                     names <- make.unique(as.character(my_data3$SYMBOL))
                     #names <- make.names(as.character(my_data3$SYMBOL), unique=FALSE)
                     row.names(my_dataclus) <- names  
                     ##########################################################################
                     #DT::datatable(my_data2, options = list(scrollX = TRUE))
                     my_data <- my_dataclus %>%
                         na.omit() %>%          # Remove missing values (NA)
                         scale() 
                     # Compute
                     res.hc <- my_data %>%
                         scale() %>%                    # Scale the data
                         dist(method = "euclidean") %>% # Compute dissimilarity matrix
                         hclust(method = "ward.D2")     # Compute hierachical clustering
                     ##############
                     #captionclus = as.double(read_lines( "../GenePPI/captionclus.txt"))
                     
                     clus4 = cutree(res.hc,input$captionclus )
                     testDF <- data.frame(clus4)
                     testVect <- structure(clus4)
                     clusters2 = data.frame(SYMBOL2=names(testVect), clustid=testVect, row.names=NULL)
                     
                     db <- gsub("\\..*","",clusters2$SYMBOL2)
                     db
                     tools::file_path_sans_ext(clusters2$SYMBOL2)
                     clusters <- clusters2 %>% 
                         mutate(SYMBOL = db)
                     
                     mylist<-merge(my_data2, clusters[,c("SYMBOL","clustid")], by="SYMBOL",all.x=TRUE)
                     lll <- split(mylist, as.factor(mylist$clustid))
                     mylist2 <- subset(mylist[,c("SYMBOL","clustid")])
                     mylist22 <- subset(unique(mylist[,c("SYMBOL","clustid")]))
                     # mylist22[order(mylist22$clustid, decreasing = FALSE), ]
                     
                     mydataorderclustid <-mylist22[order(mylist22$clustid,decreasing = TRUE), ]
                     #print(mylist22)
                     #DT::datatable(as.data.frame(mydataorderclustid[1:length(mydataorderclustid$SYMBOL),]), options = list(scrollX = TRUE))
                     DT::datatable(as.data.frame(mydataorderclustid), options = list(scrollX = TRUE))
                     
                     #head(lll)
                     #cd <- enframe(lll)
                     #xxxxx <- cd$value[1:length(cd$name)]
                     #library(kableExtra)
                     #knitr::kable(head(lll, "pipe"))
                 }))
    
    
    observeEvent(input$action2,
                 output$slide21 <- DT::renderDT({
                     DT::datatable(req(data2()), options = list(scrollX = TRUE))
                     #DT::datatable(dplyr::dataset_selected_0_000001, options = list(scrollX = TRUE))
                     
                     
                 }))
    
    observeEvent(input$action2,
                 output$slide211 <- DT::renderDT({
                     DT::datatable(req(data21()), options = list(scrollX = TRUE))
                     
                     #req(data21())
                 }))
    
    observeEvent(input$action2,
                 output$slide212 <- DT::renderDT({
                     DT::datatable(req(data22()), options = list(scrollX = TRUE))
                     
                     #req(data22())
                 }))
    
    observeEvent(input$action2,
                 output$slide213 <- DT::renderDT({
                     
                     DT::datatable(req(data23()), options = list(scrollX = TRUE))
                     
                     #req(data23())
                 }))
    
    
    #######################################################################################################################################
    #######################################################################################################################################
    #actiontis
    #actionvalidclust
    observeEvent(input$actiontis,
                 output$slide222 <- DT::renderDT({
                     
                     
                     #################################################################################################
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     
                     ######################################################################################################3
                     # Loading
                     library("tibble")
                     library(readr)
                     library(tidyverse)
                     #write.csv(gene_PrbId,file="../docglom21/GenePPI/gene_PrbId.csv")
                     #gene_PrbId <- read_csv("../GenePPI/gene_PrbId.csv")
                     
                     #gene_predicted <-subset(input,input$ENTREZID %in%  gene_prediction_Results$X)
                     gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     
                     GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                     #selection_seuil_0_0001 <- read_csv("../docglom21/GenePPI/selection_seuil_0_0001.csv")
                     selection_seuil_0_0001 <- as.data.frame(list(ENTREZID = gene_prediction_Results$ENTREZID,selection_based_simila =gene_prediction_Results$selection_based_simila,selection_based_selection = gene_prediction_Results$selection_based_selection))
                     tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
                     #cl4 <-subset(selection_seuil_0_0001,selection_seuil_0_0001$ENTREZID %in% gene_predicted$ENTREZID)
                     
                     cl4 <- gene_predicted$SYMBOL
                     #######################################################################################
                     #################################Tissus
                     tissues_selected = as.character(input$id_Tissues) 
                     #tissus_all_clus1 <- subset(data2()[,c("SYMBOL",tissues_selected)],data2()[,"SYMBOL"] %in%  cl4)
                     tissus_all_clus1 <- subset(tissus[,c("SYMBOL",tissues_selected)],tissus$SYMBOL %in%  cl4)
                     
                     means_tiss <- tissus_all_clus1 %>%
                         na.omit()
                     
                     tissus_all_clus1$mean <-rowMeans(tissus_all_clus1[,tissues_selected], na.rm=TRUE)
                     ##########################################################################################################
                     #####################################BP         ###############means
                     df_BP_values_na <- data21() %>% na.omit() 
                     df_BP_values_na_sele <- subset(df_BP_values_na,df_BP_values_na$geneSymbol %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     df_BP_values_na_sele_na_norm <- as.data.frame(lapply(df_BP_values_na_sele[,6:7], min_max_norm))
                     df_BP_values_na_sele_norm_df <- df_BP_values_na_sele %>% 
                         mutate(zscore = df_BP_values_na_sele_na_norm)
                     res.by <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, mean)
                     
                     #res.by
                     library(taRifx)
                     vvv <- as.data.frame(res.by)
                     BP_db <- vvv %>% 
                         rename(
                             SYMBOL = IDX1,
                             mean_BP_DF = value )
                     res.by_BP_nbr <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, length)
                     res.by_BP_nbr
                     vvv_res.by_BP_nbr <- as.data.frame(res.by_BP_nbr)
                     
                     names(vvv_res.by_BP_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_BP_nbr)[2] <-"nbr_BP"
                     nbr_BP <- vvv_res.by_BP_nbr
                     
                     nbr_BP_DES_sel <- by(df_BP_values_na_sele_norm_df$Description, df_BP_values_na_sele$geneSymbol, paste)
                     nbr_BP_DES_df <- as.data.frame(nbr_BP_DES_sel)
                     names(nbr_BP_DES_df)[1] <-"SYMBOL"
                     names(nbr_BP_DES_df)[2] <-"DES_BP"
                     
                     nbr_BP_DES <- nbr_BP_DES_df 
                     ##########################################################################################################
                     ggg1_k <-  merge(tissus_all_clus1, BP_db[, c("SYMBOL", "mean_BP_DF")], by="SYMBOL",all.x=TRUE)
                     ggg12_k <-  merge(ggg1_k, nbr_BP[, c("SYMBOL", "nbr_BP")], by="SYMBOL",all.x=TRUE)
                     ggg13_k <-  merge(ggg12_k, nbr_BP_DES[, c("SYMBOL", "DES_BP")], by="SYMBOL",all.x=TRUE)
                     ############################################################################################################
                     ########################################################################
                     ##################################Expression                ####means
                     min_max_norm <- function(x) {
                         #(x - min(x)) / (max(x) - min(x))
                         (x - min(x)) / sd(x)}
                     EXP_norm0 <- as.data.frame(lapply(selection_seuil_0_0001[2:3], min_max_norm))
                     EXP_norm1 <- selection_seuil_0_0001 %>%   mutate(zscore = EXP_norm0$selection_based_selection)
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     EXP_df <- merge(EXP_norm1, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     res.by_EXP <- by(EXP_df$zscore, EXP_df$SYMBOL, mean)
                     
                     
                     vvv_EXP <- as.data.frame(res.by_EXP)
                     
                     names(vvv_EXP)[1] <-"SYMBOL"
                     names(vvv_EXP)[2] <-"mean_EXP_df"
                     
                     EXP_db <- vvv_EXP 
                     
                     
                     ##########################################################################################################
                     ggg2_k <-  merge(ggg13_k, EXP_db[, c("SYMBOL", "mean_EXP_df")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     TFs <-  vroom::vroom("../GenePPI/input/TFs.csv",  locale = vroom::locale(encoding = "CP1252"))
                     ####################TFs
                     ghgwas = as.data.frame(list(SYMBOL= TFs$SYMBOL,
                                                 TF= TFs$TF,
                                                 Zscore= TFs$`Z-score`,
                                                 Fisher_score= TFs$`Fisher score `))
                     
                     TFs_na <- ghgwas %>% na.omit() 
                     TFs_na_sele <- subset(TFs_na,TFs_na$SYMBOL %in% cl4)
                     res.by_TFs <- by(TFs_na_sele$Fisher_score, TFs_na_sele$SYMBOL, mean)
                     res.by_TFs
                     vvv_TFs <- as.data.frame(res.by_TFs)
                     
                     names(vvv_TFs)[1] <-"SYMBOL"
                     names(vvv_TFs)[2] <-"mean_TFs_df"
                     EXP_TFs <- vvv_TFs
                     
                     res.by_TFs_nbr <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, length)
                     vvv_res.by_TFs_nbr <- as.data.frame(res.by_TFs_nbr)
                     
                     
                     names(vvv_res.by_TFs_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_TFs_nbr)[2] <-"nbr_TFs"
                     
                     nbr_TFs <- vvv_res.by_TFs_nbr
                     nbr_TFs_DES_sel <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, paste)
                     nbr_TFs_DES_df <- as.data.frame(nbr_TFs_DES_sel)
                     names(nbr_TFs_DES_df)[1] <-"SYMBOL"
                     names(nbr_TFs_DES_df)[2] <-"DES_TFs"
                     
                     nbr_TFs_DES <- nbr_TFs_DES_df 
                     ##########################################################################################################
                     ggg3_k <-  merge(ggg2_k, EXP_TFs[, c("SYMBOL", "mean_TFs_df")], by="SYMBOL",all.x=TRUE)
                     ggg31_k <-  merge(ggg3_k, nbr_TFs[, c("SYMBOL", "nbr_TFs")], by="SYMBOL",all.x=TRUE)
                     ggg32_k <-  merge(ggg31_k, nbr_TFs_DES[, c("SYMBOL", "DES_TFs")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ########################################################################
                     #########################GWAS
                     selection_seuil_GWAs_na <- GWAS %>% na.omit() 
                     GWAs_na_sele <- subset(selection_seuil_GWAs_na,selection_seuil_GWAs_na$GeneID %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     selection_seuil_GWAs_na_norm <- as.data.frame(lapply(GWAs_na_sele[,10], min_max_norm))
                     selection_seuil_GWAs_na_norm_df <- GWAs_na_sele %>% 
                         mutate(zscore = selection_seuil_GWAs_na_norm)
                     
                     mean_norm_GWAS <- mean(selection_seuil_GWAs_na_norm_df$zscore$P)
                     
                     res.by_GWAS <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$GeneID, mean)
                     res.by_GWAS
                     
                     vvv_GWAS <- as.data.frame(res.by_GWAS)
                     names(vvv_GWAS)[1] <-"SYMBOL"
                     names(vvv_GWAS)[2] <-"mean_GWAS_df"
                     
                     EXP_GWAS <- vvv_GWAS
                     
                     res.by_GWAS_nbr <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$GeneID, length)
                     res.by_GWAS_nbr
                     vvv_res.by_GWAS_nbr <- as.data.frame(res.by_GWAS_nbr)
                     names(vvv_res.by_GWAS_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_GWAS_nbr)[2] <-"nbr_GWAS"
                     
                     nbr_GWAS <- vvv_res.by_GWAS_nbr 
                     nbr_GWAS_DES_sel <- by(selection_seuil_GWAs_na_norm_df$`Trait`, selection_seuil_GWAs_na_norm_df$GeneID, paste)
                     nbr_GWAS_DES_df <- as.data.frame(nbr_GWAS_DES_sel)
                     names(nbr_GWAS_DES_df)[1] <-"SYMBOL"
                     names(nbr_GWAS_DES_df)[2] <-"DES_GWAS"
                     
                     nbr_GWAS_DES <- nbr_GWAS_DES_df 
                     ##########################################################################################################
                     ggg4_k <-  merge(ggg32_k, EXP_GWAS[, c("SYMBOL", "mean_GWAS_df")], by="SYMBOL",all.x=TRUE)
                     ggg41_k <-  merge(ggg4_k, nbr_GWAS[, c("SYMBOL", "nbr_GWAS")], by="SYMBOL",all.x=TRUE)
                     ggg42_k <-  merge(ggg41_k, nbr_GWAS_DES[, c("SYMBOL", "DES_GWAS")], by="SYMBOL",all.x=TRUE)
                     #View(ggg42)
                     #########################################################################
                     ########################################################################
                     ##########################################function()
                     #prior = (result.mean_tissus + result.mean_expr)/2 + 
                     #ggg42[,c(6:8,10:13,15:16)][is.na(ggg42[,c(6:8,10:13,15:16)])] = 0
                     ggg42_k[is.na(ggg42_k)] = 0
                     ggg42fin_kidney_cl4 <- ggg42_k %>% rowwise() %>% mutate(Priori = ((mean+mean_EXP_df)/2) + (mean_BP_DF*nbr_BP) + (mean_TFs_df*nbr_TFs) + (mean_GWAS_df*nbr_GWAS))
                     #View(ggg42fin_kidney)
                     ###write.csv(as.matrix(ggg42fin_kidney_cl4), "../docglom21/GenePPI/ggg42fin_kidney_cl4.csv")#, row.names=FALSE
                     #write.xlsx(ggg42fin_kidney_cl4, "../docglom21/GenePPI/ggg42fin_kidney_cl4.xlsx")
                     #print(ggg42fin_kidney_cl4)
                     priori_scores <- ggg42fin_kidney_cl4[order(ggg42fin_kidney_cl4$Priori, decreasing = TRUE), ]
                     #DT::datatable(dplyr::printer, options = list(scrollX = TRUE))
                     
                     subset_priorized_genes <- subset(priori_scores[,c("SYMBOL","Priori")])
                     #DT::datatable(priori_scores, options = list(scrollX = TRUE))
                     #print(printer)
                     library(readr)
                     library(tidyr)
                     library("cluster")
                     library("factoextra")
                     library("magrittr")
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     my_data <- gene_prediction_Results
                     
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in%  my_data$ENTREZID)
                     my_data2 <- merge(my_data, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     my_data2 <- merge(aaaaaaaa[, c("SYMBOL", "ENTREZID")], my_data, by="ENTREZID",all.x=TRUE)
                     my_data3 <-  my_data2[,c("SYMBOL","ENTREZID","selection_based_simila","selection_based_selection")]
                     
                     ##########################################################################
                     
                     #####################################################################################################
                     my_dataclus  <- my_data2[,c("selection_based_simila","selection_based_selection")]
                     names <- make.unique(as.character(my_data3$SYMBOL))
                     #names <- make.names(as.character(my_data3$SYMBOL), unique=FALSE)
                     row.names(my_dataclus) <- names  
                     ##########################################################################
                     #DT::datatable(my_data2, options = list(scrollX = TRUE))
                     my_data <- my_dataclus %>%
                         na.omit() %>%          # Remove missing values (NA)
                         scale() 
                     # Compute
                     res.hc <- my_data %>%
                         scale() %>%                    # Scale the data
                         dist(method = "euclidean") %>% # Compute dissimilarity matrix
                         hclust(method = "ward.D2")     # Compute hierachical clustering
                     ##############
                     #captionclus = as.double(read_lines( "../docglom21/GenePPI/captionclus.txt"))
                     
                     clus4 = cutree(res.hc, input$captionclus)
                     testDF <- data.frame(clus4)
                     testVect <- structure(clus4)
                     clusters2 = data.frame(SYMBOL2=names(testVect), clustid=testVect, row.names=NULL)
                     
                     db <- gsub("\\..*","",clusters2$SYMBOL2)
                     db
                     tools::file_path_sans_ext(clusters2$SYMBOL2)
                     clusters <- clusters2 %>% 
                         mutate(SYMBOL = db)
                     
                     mylist<-merge(my_data2, clusters[,c("SYMBOL","clustid")], by="SYMBOL",all.x=TRUE)
                     lll <- split(mylist, as.factor(mylist$clustid))
                     mylist2 <- subset(mylist[,c("SYMBOL","clustid")])
                     mylist22 <- subset(unique(mylist[,c("SYMBOL","clustid")]))
                     # mylist22[order(mylist22$clustid, decreasing = FALSE), ]
                     
                     mydataorderclustid <-mylist22[order(mylist22$clustid,decreasing = TRUE), ]
                     #DT::datatable(as.data.frame(mydataorderclustid), options = list(scrollX = TRUE))
                     #library(readr)
                     #subset_priorized_genes <- read_csv("../docglom21/GenePPI/subset_priorized_genes.csv")
                     
                     gene_int_clus <- merge(subset_priorized_genes, mydataorderclustid[, c("SYMBOL", "clustid")], by="SYMBOL",all.x=TRUE)
                     #gene_int_clus[order(gene_int_clus$clustid,gene_int_clus$Priori,decreasing = TRUE), ]
                     gene_int_clus_ord <- gene_int_clus[order(gene_int_clus$clustid,-gene_int_clus$Priori), ]
                     DT::datatable(as.data.frame(gene_int_clus_ord), options = list(scrollX = TRUE))
                     
                     
                 }))
    
    ###################################################################################################################################
    observeEvent(input$actiontis,
                 output$slide22 <- DT::renderDT({
                     ##############################################################################
                     
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     
                     ###############################################################################
                     # Loading
                     library("tibble")
                     library(readr)
                     library(tidyverse)
                     #write.csv(gene_PrbId,file="../docglom21/GenePPI/gene_PrbId.csv")
                     
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     #gene_predicted <-subset(input,input$ENTREZID %in%  gene_prediction_Results$X)
                     gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     
                     GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                     #selection_seuil_0_0001 <- read_csv("../docglom21/GenePPI/selection_seuil_0_0001.csv")
                     selection_seuil_0_0001 <- as.data.frame(list(ENTREZID = gene_prediction_Results$ENTREZID,selection_based_simila =gene_prediction_Results$selection_based_simila,selection_based_selection = gene_prediction_Results$selection_based_selection))
                     tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
                     #cl4 <-subset(selection_seuil_0_0001,selection_seuil_0_0001$ENTREZID %in% gene_predicted$ENTREZID)
                     
                     cl4 <- gene_predicted$SYMBOL
                     #######################################################################################
                     #################################Tissus
                     tissues_selected = input$id_Tissues
                     #read_csv( "../docglom21/GenePPI/id_Tissues.csv",show_col_types = FALSE)
                     #tissus_all_clus1 <- subset(data2()[,c("SYMBOL",tissues_selected)],data2()[,"SYMBOL"] %in%  cl4)
                     
                     tissus_all_clus1 <- subset(tissus[,c("SYMBOL",tissues_selected)],tissus$SYMBOL %in%  cl4)
                     #write.csv(tissus_all_clus1,file="../docglom21/Priorization/tissus_all_clus1.csv")
                     #tissus_all_clus1[is.na(tissus_all_clus1)] = 0
                     
                     #means_tiss$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #tissus_all_clus1$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #colMeans(df, na.rm=TRUE)
                     means_tiss <- tissus_all_clus1 %>%
                         na.omit()
                     
                     tissus_all_clus1$mean <-rowMeans(tissus_all_clus1[,tissues_selected], na.rm=TRUE)
                     #tissus_all_clus1
                     #tissus_all_clus1[,2:3]
                     #df = subset(nbr_Tiss, select = -IDX1.1 )
                     ##########################################################################################################
                     #####################################BP         ###############means
                     df_BP_values_na <- data21() %>% na.omit() 
                     df_BP_values_na_sele <- subset(df_BP_values_na,df_BP_values_na$geneSymbol %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     df_BP_values_na_sele_na_norm <- as.data.frame(lapply(df_BP_values_na_sele[,6:7], min_max_norm))
                     df_BP_values_na_sele_norm_df <- df_BP_values_na_sele %>% 
                         mutate(zscore = df_BP_values_na_sele_na_norm)
                     res.by <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, mean)
                     #res.by
                     vvv <- as.data.frame(res.by)
                     names(vvv)[1] <-"SYMBOL"
                     names(vvv)[2] <-"mean_BP_DF"
                     BP_db <- vvv
                     
                     res.by_BP_nbr <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, length)
                     res.by_BP_nbr
                     vvv_res.by_BP_nbr <- as.data.frame(res.by_BP_nbr)
                     
                     names(vvv_res.by_BP_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_BP_nbr)[2] <-"nbr_BP"
                     nbr_BP <- vvv_res.by_BP_nbr 
                     
                     nbr_BP_DES_sel <- by(df_BP_values_na_sele_norm_df$Description, df_BP_values_na_sele$geneSymbol, paste)
                     nbr_BP_DES_df <- as.data.frame(nbr_BP_DES_sel)
                     names(nbr_BP_DES_df)[1] <-"SYMBOL"
                     names(nbr_BP_DES_df)[2] <-"DES_BP"
                     
                     nbr_BP_DES <- nbr_BP_DES_df 
                     ##########################################################################################################
                     ggg1_k <-  merge(tissus_all_clus1, BP_db[, c("SYMBOL", "mean_BP_DF")], by="SYMBOL",all.x=TRUE)
                     ggg12_k <-  merge(ggg1_k, nbr_BP[, c("SYMBOL", "nbr_BP")], by="SYMBOL",all.x=TRUE)
                     ggg13_k <-  merge(ggg12_k, nbr_BP_DES[, c("SYMBOL", "DES_BP")], by="SYMBOL",all.x=TRUE)
                     ############################################################################################################
                     ########################################################################
                     ##################################Expression                ####means
                     min_max_norm <- function(x) {
                         #(x - min(x)) / (max(x) - min(x))
                         (x - min(x)) / sd(x)}
                     EXP_norm0 <- as.data.frame(lapply(selection_seuil_0_0001[2:3], min_max_norm))
                     EXP_norm1 <- selection_seuil_0_0001 %>%   mutate(zscore = EXP_norm0$selection_based_selection)
                     #EXP_df <- EXP_norm1 %>%   mutate(SYMBOL = selection_seuil_0_0001_norm1$SYMBOL)
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     EXP_df <- merge(EXP_norm1, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     res.by_EXP <- by(EXP_df$zscore, EXP_df$SYMBOL, mean)
                     vvv_EXP <- as.data.frame(res.by_EXP)
                     names(vvv_EXP)[1] <-"SYMBOL"
                     names(vvv_EXP)[2] <-"mean_EXP_df"
                     
                     EXP_db <- vvv_EXP
                     
                     
                     ##########################################################################################################
                     ggg2_k <-  merge(ggg13_k, EXP_db[, c("SYMBOL", "mean_EXP_df")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ####################TFs
                     TFs <-  vroom::vroom("../GenePPI/input/TFs.csv",  locale = vroom::locale(encoding = "CP1252"))
                     
                     ghgwas = as.data.frame(list(SYMBOL= TFs$SYMBOL,
                                                 TF= TFs$TF,
                                                 Zscore= TFs$`Z-score`,
                                                 Fisher_score= TFs$`Fisher score `))
                     
                     TFs_na <- ghgwas %>% na.omit() 
                     TFs_na_sele <- subset(TFs_na,TFs_na$SYMBOL %in% cl4)
                     res.by_TFs <- by(TFs_na_sele$Fisher_score, TFs_na_sele$SYMBOL, mean)
                     res.by_TFs
                     vvv_TFs <- as.data.frame(res.by_TFs)
                     names(vvv_TFs)[1] <-"SYMBOL"
                     names(vvv_TFs)[2] <-"mean_TFs_df"
                     EXP_TFs <- vvv_TFs
                     res.by_TFs_nbr <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, length)
                     vvv_res.by_TFs_nbr <- as.data.frame(res.by_TFs_nbr)
                     
                     
                     names(vvv_res.by_TFs_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_TFs_nbr)[2] <-"nbr_TFs"
                     
                     nbr_TFs <- vvv_res.by_TFs_nbr
                     
                     nbr_TFs_DES_sel <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, paste)
                     nbr_TFs_DES_df <- as.data.frame(nbr_TFs_DES_sel)
                     names(nbr_TFs_DES_df)[1] <-"SYMBOL"
                     names(nbr_TFs_DES_df)[2] <-"DES_TFs"
                     
                     nbr_TFs_DES <- nbr_TFs_DES_df 
                     ##########################################################################################################
                     ggg3_k <-  merge(ggg2_k, EXP_TFs[, c("SYMBOL", "mean_TFs_df")], by="SYMBOL",all.x=TRUE)
                     ggg31_k <-  merge(ggg3_k, nbr_TFs[, c("SYMBOL", "nbr_TFs")], by="SYMBOL",all.x=TRUE)
                     ggg32_k <-  merge(ggg31_k, nbr_TFs_DES[, c("SYMBOL", "DES_TFs")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ########################################################################
                     #########################GWAS
                     selection_seuil_GWAs_na <- GWAS %>% na.omit() 
                     GWAs_na_sele <- subset(selection_seuil_GWAs_na,selection_seuil_GWAs_na$`GeneID` %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     selection_seuil_GWAs_na_norm <- as.data.frame(lapply(GWAs_na_sele[,10], min_max_norm))
                     selection_seuil_GWAs_na_norm_df <- GWAs_na_sele %>% 
                         mutate(zscore = selection_seuil_GWAs_na_norm)
                     
                     #mean_norm_GWAS <- mean(selection_seuil_GWAs_na_norm_df$zscore$P)
                     
                     res.by_GWAS <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$GeneID, mean)
                     res.by_GWAS
                     library(taRifx)
                     vvv_GWAS <- as.data.frame(res.by_GWAS)
                     names(vvv_GWAS)[1] <-"SYMBOL"
                     names(vvv_GWAS)[2] <-"mean_GWAS_df"
                     
                     EXP_GWAS <- vvv_GWAS 
                     
                     res.by_GWAS_nbr <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, length)
                     res.by_GWAS_nbr
                     vvv_res.by_GWAS_nbr <- as.data.frame(res.by_GWAS_nbr)
                     names(vvv_res.by_GWAS_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_GWAS_nbr)[2] <-"nbr_GWAS"
                     
                     nbr_GWAS <- vvv_res.by_GWAS_nbr 
                     nbr_GWAS_DES_sel <- by(selection_seuil_GWAs_na_norm_df$`Trait`, selection_seuil_GWAs_na_norm_df$`GeneID`, paste)
                     nbr_GWAS_DES_df <- as.data.frame(nbr_GWAS_DES_sel)
                     names(nbr_GWAS_DES_df)[1] <-"SYMBOL"
                     names(nbr_GWAS_DES_df)[2] <-"DES_GWAS"
                     
                     nbr_GWAS_DES <- nbr_GWAS_DES_df 
                     ##########################################################################################################
                     ggg4_k <-  merge(ggg32_k, EXP_GWAS[, c("SYMBOL", "mean_GWAS_df")], by="SYMBOL",all.x=TRUE)
                     ggg41_k <-  merge(ggg4_k, nbr_GWAS[, c("SYMBOL", "nbr_GWAS")], by="SYMBOL",all.x=TRUE)
                     ggg42_k <-  merge(ggg41_k, nbr_GWAS_DES[, c("SYMBOL", "DES_GWAS")], by="SYMBOL",all.x=TRUE)
                     #View(ggg42)
                     #########################################################################
                     ########################################################################
                     ##########################################function()
                     #prior = (result.mean_tissus + result.mean_expr)/2 + 
                     #ggg42[,c(6:8,10:13,15:16)][is.na(ggg42[,c(6:8,10:13,15:16)])] = 0
                     ggg42_k[is.na(ggg42_k)] = 0
                     ggg42fin_kidney_cl4 <- ggg42_k %>% rowwise() %>% mutate(Priori = ((mean+mean_EXP_df)/2) + (mean_BP_DF*nbr_BP) + (mean_TFs_df*nbr_TFs) + (mean_GWAS_df*nbr_GWAS))
                     #View(ggg42fin_kidney)
                     ###write.csv(as.matrix(ggg42fin_kidney_cl4), "../docglom21/GenePPI/ggg42fin_kidney_cl4.csv")#, row.names=FALSE
                     #write.xlsx(ggg42fin_kidney_cl4, "../docglom21/GenePPI/ggg42fin_kidney_cl4.xlsx")
                     #print(ggg42fin_kidney_cl4)
                     priori_scores <- ggg42fin_kidney_cl4[order(ggg42fin_kidney_cl4$Priori, decreasing = TRUE), ]
                     #DT::datatable(dplyr::printer, options = list(scrollX = TRUE))
                     
                     subset_priorized_genes <- subset(priori_scores[,c("SYMBOL","Priori")])
                     
                     #write.csv(subset_priorized_genes, "../docglom21/GenePPI/subset_priorized_genes.csv", row.names=FALSE)#, row.names=FALSE
                     DT::datatable(priori_scores, options = list(scrollX = TRUE))
                     #print(printer)
                     
                 }))
    #######################################################################################################################################
    #######################################################################################################################################
    observeEvent(input$actiontis,
                 output$plot24 <- renderPlot({ 
                     
                     
                     #################################################################################
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     
                     ####################################################################################
                     # Loading
                     library("tibble")
                     library(readr)
                     library(tidyverse)
                     #write.csv(gene_PrbId,file="../GenePPI/gene_PrbId.csv")
                     
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     #gene_predicted <-subset(input,input$ENTREZID %in%  gene_prediction_Results$ENTREZID)
                     gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                     
                     #selection_seuil_0_0001 <- read_csv("../GenePPI/selection_seuil_0_0001.csv")
                     selection_seuil_0_0001 <- as.data.frame(list(ENTREZID = gene_prediction_Results$ENTREZID,selection_based_simila =gene_prediction_Results$selection_based_simila,selection_based_selection = gene_prediction_Results$selection_based_selection))
                     
                     tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
                     #cl4 <-subset(selection_seuil_0_0001,selection_seuil_0_0001$ENTREZID %in% gene_predicted$ENTREZID)
                     
                     cl4 <- gene_predicted$SYMBOL
                     #######################################################################################
                     #################################Tissus
                     tissues_selected = input$id_Tissues
                     #read_csv( "../GenePPI/id_Tissues.csv",show_col_types = FALSE)
                     #tissus_all_clus1 <- subset(data2()[,c("SYMBOL",tissues_selected)],data2()[,"SYMBOL"] %in%  cl4)
                     
                     tissus_all_clus1 <- subset(tissus[,c("SYMBOL",tissues_selected)],tissus$SYMBOL %in%  cl4)
                     #write.csv(tissus_all_clus1,file="../docglom21/Priorization/tissus_all_clus1.csv")
                     #tissus_all_clus1[is.na(tissus_all_clus1)] = 0
                     
                     #means_tiss$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #tissus_all_clus1$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #colMeans(df, na.rm=TRUE)
                     means_tiss <- tissus_all_clus1 %>%
                         na.omit()
                     
                     tissus_all_clus1$mean <-rowMeans(tissus_all_clus1[,tissues_selected], na.rm=TRUE)
                     #tissus_all_clus1
                     #tissus_all_clus1[,2:3]
                     #df = subset(nbr_Tiss, select = -IDX1.1 )
                     ##########################################################################################################
                     #####################################BP         ###############means
                     df_BP_values_na <- data21() %>% na.omit() 
                     df_BP_values_na_sele <- subset(df_BP_values_na,df_BP_values_na$geneSymbol %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     df_BP_values_na_sele_na_norm <- as.data.frame(lapply(df_BP_values_na_sele[,6:7], min_max_norm))
                     df_BP_values_na_sele_norm_df <- df_BP_values_na_sele %>% 
                         mutate(zscore = df_BP_values_na_sele_na_norm)
                     res.by <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, mean)
                     #res.by
                     vvv <- as.data.frame(res.by)
                     names(vvv)[1] <-"SYMBOL"
                     names(vvv)[2] <-"mean_BP_DF"
                     BP_db <- vvv
                     res.by_BP_nbr <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, length)
                     res.by_BP_nbr
                     vvv_res.by_BP_nbr <- as.data.frame(res.by_BP_nbr)
                     names(vvv_res.by_BP_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_BP_nbr)[2] <-"nbr_BP"
                     
                     nbr_BP <- vvv_res.by_BP_nbr
                     
                     nbr_BP_DES_sel <- by(df_BP_values_na_sele_norm_df$Description, df_BP_values_na_sele$geneSymbol, paste)
                     nbr_BP_DES_df <- as.data.frame(nbr_BP_DES_sel)
                     names(nbr_BP_DES_df)[1] <-"SYMBOL"
                     names(nbr_BP_DES_df)[2] <-"DES_BP"
                     
                     nbr_BP_DES <- nbr_BP_DES_df 
                     ##########################################################################################################
                     ggg1_k <-  merge(tissus_all_clus1, BP_db[, c("SYMBOL", "mean_BP_DF")], by="SYMBOL",all.x=TRUE)
                     ggg12_k <-  merge(ggg1_k, nbr_BP[, c("SYMBOL", "nbr_BP")], by="SYMBOL",all.x=TRUE)
                     ggg13_k <-  merge(ggg12_k, nbr_BP_DES[, c("SYMBOL", "DES_BP")], by="SYMBOL",all.x=TRUE)
                     ############################################################################################################
                     ########################################################################
                     ##################################Expression                ####means
                     min_max_norm <- function(x) {
                         #(x - min(x)) / (max(x) - min(x))
                         (x - min(x)) / sd(x)}
                     EXP_norm0 <- as.data.frame(lapply(selection_seuil_0_0001[2:3], min_max_norm))
                     EXP_norm1 <- selection_seuil_0_0001 %>%   mutate(zscore = EXP_norm0$selection_based_selection)
                     #EXP_df <- EXP_norm1 %>%   mutate(SYMBOL = selection_seuil_0_0001_norm1$SYMBOL)
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     EXP_df <- merge(EXP_norm1, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     res.by_EXP <- by(EXP_df$zscore, EXP_df$SYMBOL, mean)
                     vvv_EXP <- as.data.frame(res.by_EXP)
                     names(vvv_EXP)[1] <-"SYMBOL"
                     names(vvv_EXP)[2] <-"mean_EXP_df"
                     
                     EXP_db <- vvv_EXP
                     
                     
                     ##########################################################################################################
                     ggg2_k <-  merge(ggg13_k, EXP_db[, c("SYMBOL", "mean_EXP_df")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ####################TFs
                     TFs <-  vroom::vroom("../GenePPI/input/TFs.csv",  locale = vroom::locale(encoding = "CP1252"))
                     
                     ghgwas = as.data.frame(list(SYMBOL= TFs$SYMBOL,
                                                 TF= TFs$TF,
                                                 Zscore= TFs$`Z-score`,
                                                 Fisher_score= TFs$`Fisher score `))
                     
                     TFs_na <- ghgwas %>% na.omit() 
                     TFs_na_sele <- subset(TFs_na,TFs_na$SYMBOL %in% cl4)
                     res.by_TFs <- by(TFs_na_sele$Fisher_score, TFs_na_sele$SYMBOL, mean)
                     res.by_TFs
                     
                     vvv_TFs <- as.data.frame(res.by_TFs)
                     names(vvv_TFs)[1] <-"SYMBOL"
                     names(vvv_TFs)[2] <-"mean_TFs_df"
                     EXP_TFs <- vvv_TFs
                     res.by_TFs_nbr <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, length)
                     vvv_res.by_TFs_nbr <- as.data.frame(res.by_TFs_nbr)
                     
                     
                     names(vvv_res.by_TFs_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_TFs_nbr)[2] <-"nbr_TFs"
                     
                     nbr_TFs <- vvv_res.by_TFs_nbr
                     
                     nbr_TFs_DES_sel <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, paste)
                     nbr_TFs_DES_df <- as.data.frame(nbr_TFs_DES_sel)
                     
                     names(nbr_TFs_DES_df)[1] <-"SYMBOL"
                     names(nbr_TFs_DES_df)[2] <-"DES_TFs"
                     
                     nbr_TFs_DES <- nbr_TFs_DES_df 
                     ##########################################################################################################
                     ggg3_k <-  merge(ggg2_k, EXP_TFs[, c("SYMBOL", "mean_TFs_df")], by="SYMBOL",all.x=TRUE)
                     ggg31_k <-  merge(ggg3_k, nbr_TFs[, c("SYMBOL", "nbr_TFs")], by="SYMBOL",all.x=TRUE)
                     ggg32_k <-  merge(ggg31_k, nbr_TFs_DES[, c("SYMBOL", "DES_TFs")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ########################################################################
                     #########################GWAS
                     GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                     
                     selection_seuil_GWAs_na <- GWAS %>% na.omit() 
                     GWAs_na_sele <- subset(selection_seuil_GWAs_na,selection_seuil_GWAs_na$`GeneID` %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     selection_seuil_GWAs_na_norm <- as.data.frame(lapply(GWAs_na_sele[,10], min_max_norm))
                     selection_seuil_GWAs_na_norm_df <- GWAs_na_sele %>% 
                         mutate(zscore = selection_seuil_GWAs_na_norm)
                     
                     mean_norm_GWAS <- mean(selection_seuil_GWAs_na_norm_df$zscore$P)
                     
                     res.by_GWAS <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, mean)
                     res.by_GWAS
                     library(taRifx)
                     vvv_GWAS <- as.data.frame(res.by_GWAS)
                     names(vvv_GWAS)[1] <-"SYMBOL"
                     names(vvv_GWAS)[2] <-"mean_GWAS_df"
                     
                     EXP_GWAS <- vvv_GWAS 
                     
                     res.by_GWAS_nbr <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, length)
                     res.by_GWAS_nbr
                     vvv_res.by_GWAS_nbr <- as.data.frame(res.by_GWAS_nbr)
                     names(vvv_res.by_GWAS_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_GWAS_nbr)[2] <-"nbr_GWAS"
                     
                     nbr_GWAS <- vvv_res.by_GWAS_nbr 
                     nbr_GWAS_DES_sel <- by(selection_seuil_GWAs_na_norm_df$`Trait`, selection_seuil_GWAs_na_norm_df$`GeneID`, paste)
                     nbr_GWAS_DES_df <- as.data.frame(nbr_GWAS_DES_sel)
                     names(nbr_GWAS_DES_df)[1] <-"SYMBOL"
                     names(nbr_GWAS_DES_df)[2] <-"DES_GWAS"
                     nbr_GWAS_DES <- nbr_GWAS_DES_df
                     ##########################################################################################################
                     ggg4_k <-  merge(ggg32_k, EXP_GWAS[, c("SYMBOL", "mean_GWAS_df")], by="SYMBOL",all.x=TRUE)
                     ggg41_k <-  merge(ggg4_k, nbr_GWAS[, c("SYMBOL", "nbr_GWAS")], by="SYMBOL",all.x=TRUE)
                     ggg42_k <-  merge(ggg41_k, nbr_GWAS_DES[, c("SYMBOL", "DES_GWAS")], by="SYMBOL",all.x=TRUE)
                     #View(ggg42)
                     #########################################################################
                     ########################################################################
                     ##########################################function()
                     #prior = (result.mean_tissus + result.mean_expr)/2 + 
                     #ggg42[,c(6:8,10:13,15:16)][is.na(ggg42[,c(6:8,10:13,15:16)])] = 0
                     ggg42_k[is.na(ggg42_k)] = 0
                     ggg42fin_kidney_cl4 <- ggg42_k %>% rowwise() %>% mutate(Priori = ((mean+mean_EXP_df)/2) + (mean_BP_DF*nbr_BP) + (mean_TFs_df*nbr_TFs) + (mean_GWAS_df*nbr_GWAS))
                     #View(ggg42fin_kidney)
                     ###write.csv(as.matrix(ggg42fin_kidney_cl4), "../GenePPI/ggg42fin_kidney_cl4.csv")#, row.names=FALSE
                     #write.xlsx(ggg42fin_kidney_cl4, "../GenePPI/ggg42fin_kidney_cl4.xlsx")
                     #print(ggg42fin_kidney_cl4)
                     priori_scores <- ggg42fin_kidney_cl4[order(ggg42fin_kidney_cl4$Priori, decreasing = TRUE), ]
                     #DT::datatable(dplyr::printer, options = list(scrollX = TRUE))
                     
                     subset_priorized_genes <- subset(priori_scores[,c("SYMBOL","Priori")])
                     #DT::datatable(priori_scores, options = list(scrollX = TRUE))
                     #print(printer)
                     library(readr)
                     library(tidyr)
                     library("cluster")
                     library("factoextra")
                     library("magrittr")
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     my_data <- gene_prediction_Results
                     #names(my_data)[1] <-"ENTREZID"
                     
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in%  my_data$ENTREZID)
                     my_data2 <- merge(my_data, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     my_data2 <- merge(aaaaaaaa[, c("SYMBOL", "ENTREZID")], my_data, by="ENTREZID",all.x=TRUE)
                     my_data3 <-  my_data2[,c("SYMBOL","ENTREZID","selection_based_simila","selection_based_selection")]
                     
                     ##########################################################################
                     
                     #####################################################################################################
                     my_dataclus  <- my_data2[,c("selection_based_simila","selection_based_selection")]
                     names <- make.unique(as.character(my_data3$SYMBOL))
                     #names <- make.names(as.character(my_data3$SYMBOL), unique=FALSE)
                     row.names(my_dataclus) <- names  
                     ##########################################################################
                     #DT::datatable(my_data2, options = list(scrollX = TRUE))
                     my_data <- my_dataclus %>%
                         na.omit() %>%          # Remove missing values (NA)
                         scale() 
                     # Compute
                     res.hc <- my_data %>%
                         scale() %>%                    # Scale the data
                         dist(method = "euclidean") %>% # Compute dissimilarity matrix
                         hclust(method = "ward.D2")     # Compute hierachical clustering
                     ##############
                     #captionclus = as.double(read_lines( "../GenePPI/captionclus.txt"))
                     
                     clus4 = cutree(res.hc, input$captionclus)
                     testDF <- data.frame(clus4)
                     testVect <- structure(clus4)
                     clusters2 = data.frame(SYMBOL2=names(testVect), clustid=testVect, row.names=NULL)
                     
                     db <- gsub("\\..*","",clusters2$SYMBOL2)
                     db
                     tools::file_path_sans_ext(clusters2$SYMBOL2)
                     clusters <- clusters2 %>% 
                         mutate(SYMBOL = db)
                     
                     mylist<-merge(my_data2, clusters[,c("SYMBOL","clustid")], by="SYMBOL",all.x=TRUE)
                     lll <- split(mylist, as.factor(mylist$clustid))
                     mylist2 <- subset(mylist[,c("SYMBOL","clustid")])
                     mylist22 <- subset(unique(mylist[,c("SYMBOL","clustid")]))
                     # mylist22[order(mylist22$clustid, decreasing = FALSE), ]
                     
                     mydataorderclustid <-mylist22[order(mylist22$clustid,decreasing = TRUE), ]
                     #DT::datatable(as.data.frame(mydataorderclustid), options = list(scrollX = TRUE))
                     #library(readr)
                     #subset_priorized_genes <- read_csv("../GenePPI/subset_priorized_genes.csv")
                     
                     gene_int_clus <- merge(subset_priorized_genes, mydataorderclustid[, c("SYMBOL", "clustid")], by="SYMBOL",all.x=TRUE)
                     #gene_int_clus[order(gene_int_clus$clustid,gene_int_clus$Priori,decreasing = TRUE), ]
                     gene_int_clus_ord <- gene_int_clus[order(gene_int_clus$clustid,-gene_int_clus$Priori), ]
                     ##################
                     library(ggplot2)
                     
                     #tissues_selected = read_csv( "../GenePPI/id_Tissues.csv",show_col_types = FALSE)
                     
                     
                     #ggplot(priori_scores, aes(x =Priori , y = mean, colour=factor(tissues_selected$x[1]))) +
                     #  geom_point() +
                     #  stat_smooth(method = "loess", se = F) + 
                     # stat_smooth(aes(group = 1), method = "lm", se = F)
                     subset_priorized_genes_mean <- subset(priori_scores[,c("SYMBOL","mean","Priori")])
                     gene_int_clus_mean <- merge(subset_priorized_genes_mean, mydataorderclustid[, c("SYMBOL", "clustid")], by="SYMBOL",all.x=TRUE)
                     
                     
                     
                     ggplot(gene_int_clus_mean, aes(x =Priori , y = mean, colour=factor(clustid))) +
                         geom_point() +
                         geom_smooth(se = F, span = 0.7)
                     
                     
                     
                 }))
    
    
    
    output$plot23<- renderPlot({ 
        
        #######################################################################################
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        library(dplyr)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
        
        gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
        
        
        
        
        #########################################################################################
        library(readr)
        library(ggplot2)
        #library(AnnotationDbi)
        #library(hgu133a.db)
        
        #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
        #c3g_genes_entrezid = c("8728" ,"718","719","731","4179","629" ,"1675","3075","3078" ,"3080" ,"10878", "81494", "3426" , "1378") 
        library(clusterProfiler)
        library(enrichplot)
        enrichGO <- enrichGO(gene_prediction_Results$ENTREZID, 
                             OrgDb = "org.Hs.eg.db",
                             ont="BP",##all
                             readable=TRUE)
        
        #write.csv(ego@result[["Description"]], "../GenePPI/ego_Description.csv", row.names=FALSE)
        #dotplot(ego)
        goplot(enrichGO)
    })
    
    output$slide23 <- DT::renderDT({
        #######################################################################
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        library(dplyr)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
        
        gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
        
        
        
        ######################################################################
        library(clusterProfiler)
        library(enrichplot)
        #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
        
        enrichGO <- enrichGO(gene_prediction_Results$ENTREZID, 
                             OrgDb = "org.Hs.eg.db",
                             ont="BP",##all
                             readable=TRUE)
        DT::datatable(as.data.frame(enrichGO@result[["Description"]]), options = list(scrollX = TRUE))
        
        
        #print(ego_Description_Results)
    })
    
    
    output$plot233<- renderPlot({ 
        ###############################################################################
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        library(dplyr)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
        
        gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
        
        
        ###############################################################################
        library(readr)
        library(ggplot2)
        #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
        library(ReactomePA)
        c3g_genes_entrezid = c("8728" ,"718","719","731","4179","629" ,"1675","3075","3078" ,"3080" ,"10878", "81494", "3426" , "1378") 
        #x_litt <- enrichPathway(gene=c(gene_prediction_Results$ENTREZID,c3g_genes_entrezid),pvalueCutoff=0.05, readable=T)
        x_litt <- enrichPathway(gene=gene_prediction_Results$ENTREZID,pvalueCutoff=0.05, readable=T)
        
        #cnetplot(x_litt, circular = TRUE, colorEdge = TRUE)
        #heatplot(x_litt)
        
    })
    
    
    
    output$plot2333<- renderPlot({ 
        library(readr)
        library(ggplot2)
        GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
        
        DF3 <- data.frame(chr=data22()[,'CHR'],
                          traits=data22()[,'Trait'],
                          genes= data22()[,'GeneID'])
        E <-  ggplot(data = DF3, aes(x = genes)) +
            geom_bar(aes(fill = traits))
        E+ coord_flip() +theme_minimal() 
        E <- E+ coord_flip() +theme_minimal() 
        E
        
        
    })
    
    output$plot23333<- renderPlot({ 
        ##############################################################################################
        
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        library(dplyr)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
        
        gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
        
        ###################################################################################################
        library(readr)
        library(ggplot2)
        
        tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
        #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
        gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
        cl4 <- gene_predicted$SYMBOL
        tissus_all_clus1 <- subset(data2(),data2()[,"SYMBOL"] %in%  cl4)
        
        #tissus_all_clus1 <- subset(tissus[],tissus$SYMBOL %in%  cl4)
        tissus_all_clusallmeans <- colMeans(tissus_all_clus1[,2:14], na.rm=TRUE)
        
        tissus_all_clusallmeans_df <- as.data.frame(tissus_all_clusallmeans)
        ##
        write.csv( tissus_all_clusallmeans_df, "../GenePPI/tissus_all_clusallmeans_df.csv")
        
        tissus_all_clusallmeans_df <- read_csv("../GenePPI/tissus_all_clusallmeans_df.csv",show_col_types = FALSE)
        ###
        
        names(tissus_all_clusallmeans_df)[1]<-paste("tissus")
        names(tissus_all_clusallmeans_df)[2]<-paste("means")
        
        DF <- data.frame(means=tissus_all_clusallmeans_df$means,
                         tissus= tissus_all_clusallmeans_df$tissus)
        
        pb = ggplot(data = DF, aes(x = tissus))
        pb = pb + geom_bar(aes(fill = means))
        pb.polar = pb + coord_polar() +theme_minimal() +
            theme(axis.text.x = element_text(angle = 0, hjust = 1),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank()) +
            xlab("") + ylab("")
        
        pb.polar
        
        
    })
    ########################################################
    ###Gene Interaction############################
    #output$res <- renderPrint({
    #input$Tid
    # })
    output$res_Genes <- renderPrint({
        input$id_Genes
        #write.csv(input$id_Genes, "../GenePPI/id_Genes.csv")
        
    })
    
    
    observeEvent(input$actionvalidclust,
                 output$slide4411<- DT::renderDT({
                     ########################################################################
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     ################################################################################
                     # Loading
                     library("tibble")
                     library(readr)
                     library(tidyverse)
                     #write.csv(gene_PrbId,file="../GenePPI/gene_PrbId.csv")
                     id_Tissues = as.character(input$id_Tissues)
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     #gene_predicted <-subset(input,input$ENTREZID %in%  gene_prediction_Results$ENTREZID)
                     gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                     
                     #selection_seuil_0_0001 <- read_csv("../GenePPI/selection_seuil_0_0001.csv")
                     selection_seuil_0_0001 <- as.data.frame(list(ENTREZID = gene_prediction_Results$ENTREZID,selection_based_simila =gene_prediction_Results$selection_based_simila,selection_based_selection = gene_prediction_Results$selection_based_selection))
                     
                     tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
                     #cl4 <-subset(selection_seuil_0_0001,selection_seuil_0_0001$ENTREZID %in% gene_predicted$ENTREZID)
                     
                     cl4 <- gene_predicted$SYMBOL
                     #######################################################################################
                     #################################Tissus
                     tissues_selected = input$id_Tissues
                     #read_csv( "../GenePPI/id_Tissues.csv",show_col_types = FALSE)
                     #tissus_all_clus1 <- subset(data2()[,c("SYMBOL",tissues_selected)],data2()[,"SYMBOL"] %in%  cl4)
                     
                     tissus_all_clus1 <- subset(tissus[,c("SYMBOL",tissues_selected)],tissus$SYMBOL %in%  cl4)
                     #write.csv(tissus_all_clus1,file="../docglom21/Priorization/tissus_all_clus1.csv")
                     #tissus_all_clus1[is.na(tissus_all_clus1)] = 0
                     
                     #means_tiss$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #tissus_all_clus1$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #colMeans(df, na.rm=TRUE)
                     means_tiss <- tissus_all_clus1 %>%
                         na.omit()
                     
                     tissus_all_clus1$mean <-rowMeans(tissus_all_clus1[,tissues_selected], na.rm=TRUE)
                     #tissus_all_clus1
                     #tissus_all_clus1[,2:3]
                     #df = subset(nbr_Tiss, select = -IDX1.1 )
                     ##########################################################################################################
                     #####################################BP         ###############means
                     df_BP_values_na <- data21() %>% na.omit() 
                     df_BP_values_na_sele <- subset(df_BP_values_na,df_BP_values_na$geneSymbol %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     df_BP_values_na_sele_na_norm <- as.data.frame(lapply(df_BP_values_na_sele[,6:7], min_max_norm))
                     df_BP_values_na_sele_norm_df <- df_BP_values_na_sele %>% 
                         mutate(zscore = df_BP_values_na_sele_na_norm)
                     res.by <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, mean)
                     #res.by
                     vvv <- as.data.frame(res.by)
                     names(vvv)[1] <-"SYMBOL"
                     names(vvv)[2] <-"mean_BP_DF"
                     BP_db <- vvv
                     res.by_BP_nbr <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, length)
                     res.by_BP_nbr
                     vvv_res.by_BP_nbr <- as.data.frame(res.by_BP_nbr)
                     names(vvv_res.by_BP_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_BP_nbr)[2] <-"nbr_BP"
                     
                     nbr_BP <- vvv_res.by_BP_nbr 
                     
                     nbr_BP_DES_sel <- by(df_BP_values_na_sele_norm_df$Description, df_BP_values_na_sele$geneSymbol, paste)
                     nbr_BP_DES_df <- as.data.frame(nbr_BP_DES_sel)
                     names(nbr_BP_DES_df)[1] <-"SYMBOL"
                     names(nbr_BP_DES_df)[2] <-"DES_BP"
                     
                     nbr_BP_DES <- nbr_BP_DES_df 
                     ##########################################################################################################
                     ggg1_k <-  merge(tissus_all_clus1, BP_db[, c("SYMBOL", "mean_BP_DF")], by="SYMBOL",all.x=TRUE)
                     ggg12_k <-  merge(ggg1_k, nbr_BP[, c("SYMBOL", "nbr_BP")], by="SYMBOL",all.x=TRUE)
                     ggg13_k <-  merge(ggg12_k, nbr_BP_DES[, c("SYMBOL", "DES_BP")], by="SYMBOL",all.x=TRUE)
                     ############################################################################################################
                     ########################################################################
                     ##################################Expression                ####means
                     min_max_norm <- function(x) {
                         #(x - min(x)) / (max(x) - min(x))
                         (x - min(x)) / sd(x)}
                     EXP_norm0 <- as.data.frame(lapply(selection_seuil_0_0001[2:3], min_max_norm))
                     EXP_norm1 <- selection_seuil_0_0001 %>%   mutate(zscore = EXP_norm0$selection_based_selection)
                     #EXP_df <- EXP_norm1 %>%   mutate(SYMBOL = selection_seuil_0_0001_norm1$SYMBOL)
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     EXP_df <- merge(EXP_norm1, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     res.by_EXP <- by(EXP_df$zscore, EXP_df$SYMBOL, mean)
                     vvv_EXP <- as.data.frame(res.by_EXP)
                     names(vvv_EXP)[1] <-"SYMBOL"
                     names(vvv_EXP)[2] <-"mean_EXP_df"
                     
                     EXP_db <- vvv_EXP 
                     
                     
                     ##########################################################################################################
                     ggg2_k <-  merge(ggg13_k, EXP_db[, c("SYMBOL", "mean_EXP_df")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ####################TFs
                     TFs <-  vroom::vroom("../GenePPI/input/TFs.csv",  locale = vroom::locale(encoding = "CP1252"))
                     
                     ghgwas = as.data.frame(list(SYMBOL= TFs$SYMBOL,
                                                 TF= TFs$TF,
                                                 Zscore= TFs$`Z-score`,
                                                 Fisher_score= TFs$`Fisher score `))
                     
                     TFs_na <- ghgwas %>% na.omit() 
                     TFs_na_sele <- subset(TFs_na,TFs_na$SYMBOL %in% cl4)
                     res.by_TFs <- by(TFs_na_sele$Fisher_score, TFs_na_sele$SYMBOL, mean)
                     res.by_TFs
                     vvv_TFs <- as.data.frame(res.by_TFs)
                     names(vvv_TFs)[1] <-"SYMBOL"
                     names(vvv_TFs)[2] <-"mean_TFs_df"
                     EXP_TFs <- vvv_TFs
                     res.by_TFs_nbr <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, length)
                     vvv_res.by_TFs_nbr <- as.data.frame(res.by_TFs_nbr)
                     
                     
                     names(vvv_res.by_TFs_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_TFs_nbr)[2] <-"nbr_TFs"
                     
                     nbr_TFs <- vvv_res.by_TFs_nbr
                     
                     nbr_TFs_DES_sel <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, paste)
                     nbr_TFs_DES_df <- as.data.frame(nbr_TFs_DES_sel)
                     names(nbr_TFs_DES_df)[1] <-"SYMBOL"
                     names(nbr_TFs_DES_df)[2] <-"DES_TFs"
                     
                     nbr_TFs_DES <- nbr_TFs_DES_df 
                     ##########################################################################################################
                     ggg3_k <-  merge(ggg2_k, EXP_TFs[, c("SYMBOL", "mean_TFs_df")], by="SYMBOL",all.x=TRUE)
                     ggg31_k <-  merge(ggg3_k, nbr_TFs[, c("SYMBOL", "nbr_TFs")], by="SYMBOL",all.x=TRUE)
                     ggg32_k <-  merge(ggg31_k, nbr_TFs_DES[, c("SYMBOL", "DES_TFs")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ########################################################################
                     #########################GWAS
                     selection_seuil_GWAs_na <- GWAS %>% na.omit() 
                     GWAs_na_sele <- subset(selection_seuil_GWAs_na,selection_seuil_GWAs_na$`GeneID` %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     selection_seuil_GWAs_na_norm <- as.data.frame(lapply(GWAs_na_sele[,10], min_max_norm))
                     selection_seuil_GWAs_na_norm_df <- GWAs_na_sele %>% 
                         mutate(zscore = selection_seuil_GWAs_na_norm)
                     
                     mean_norm_GWAS <- mean(selection_seuil_GWAs_na_norm_df$zscore$P)
                     
                     res.by_GWAS <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, mean)
                     res.by_GWAS
                     library(taRifx)
                     vvv_GWAS <- as.data.frame(res.by_GWAS)
                     names(vvv_GWAS)[1] <-"SYMBOL"
                     names(vvv_GWAS)[2] <-"mean_GWAS_df"
                     
                     EXP_GWAS <- vvv_GWAS 
                     
                     res.by_GWAS_nbr <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, length)
                     res.by_GWAS_nbr
                     vvv_res.by_GWAS_nbr <- as.data.frame(res.by_GWAS_nbr)
                     names(vvv_res.by_GWAS_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_GWAS_nbr)[2] <-"nbr_GWAS"
                     
                     nbr_GWAS <- vvv_res.by_GWAS_nbr 
                     nbr_GWAS_DES_sel <- by(selection_seuil_GWAs_na_norm_df$`Trait`, selection_seuil_GWAs_na_norm_df$`GeneID`, paste)
                     nbr_GWAS_DES_df <- as.data.frame(nbr_GWAS_DES_sel)
                     names(nbr_GWAS_DES_df)[1] <-"SYMBOL"
                     names(nbr_GWAS_DES_df)[2] <-"DES_GWAS"
                     
                     nbr_GWAS_DES <- nbr_GWAS_DES_df
                     ##########################################################################################################
                     ggg4_k <-  merge(ggg32_k, EXP_GWAS[, c("SYMBOL", "mean_GWAS_df")], by="SYMBOL",all.x=TRUE)
                     ggg41_k <-  merge(ggg4_k, nbr_GWAS[, c("SYMBOL", "nbr_GWAS")], by="SYMBOL",all.x=TRUE)
                     ggg42_k <-  merge(ggg41_k, nbr_GWAS_DES[, c("SYMBOL", "DES_GWAS")], by="SYMBOL",all.x=TRUE)
                     #View(ggg42)
                     #########################################################################
                     ########################################################################
                     ##########################################function()
                     #prior = (result.mean_tissus + result.mean_expr)/2 + 
                     #ggg42[,c(6:8,10:13,15:16)][is.na(ggg42[,c(6:8,10:13,15:16)])] = 0
                     ggg42_k[is.na(ggg42_k)] = 0
                     ggg42fin_kidney_cl4 <- ggg42_k %>% rowwise() %>% mutate(Priori = ((mean+mean_EXP_df)/2) + (mean_BP_DF*nbr_BP) + (mean_TFs_df*nbr_TFs) + (mean_GWAS_df*nbr_GWAS))
                     #View(ggg42fin_kidney)
                     ###write.csv(as.matrix(ggg42fin_kidney_cl4), "../GenePPI/ggg42fin_kidney_cl4.csv")#, row.names=FALSE
                     #write.xlsx(ggg42fin_kidney_cl4, "../GenePPI/ggg42fin_kidney_cl4.xlsx")
                     #print(ggg42fin_kidney_cl4)
                     priori_scores <- ggg42fin_kidney_cl4[order(ggg42fin_kidney_cl4$Priori, decreasing = TRUE), ]
                     #DT::datatable(dplyr::printer, options = list(scrollX = TRUE))
                     
                     subset_priorized_genes <- subset(priori_scores[,c("SYMBOL","Priori")])
                     #DT::datatable(priori_scores, options = list(scrollX = TRUE))
                     #print(printer)
                     library(readr)
                     library(tidyr)
                     library("cluster")
                     library("factoextra")
                     library("magrittr")
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     my_data <- gene_prediction_Results
                     #names(my_data)[1] <-"ENTREZID"
                     
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in%  my_data$ENTREZID)
                     my_data2 <- merge(my_data, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     my_data2 <- merge(aaaaaaaa[, c("SYMBOL", "ENTREZID")], my_data, by="ENTREZID",all.x=TRUE)
                     my_data3 <-  my_data2[,c("SYMBOL","ENTREZID","selection_based_simila","selection_based_selection")]
                     
                     ##########################################################################
                     
                     #####################################################################################################
                     my_dataclus  <- my_data2[,c("selection_based_simila","selection_based_selection")]
                     names <- make.unique(as.character(my_data3$SYMBOL))
                     #names <- make.names(as.character(my_data3$SYMBOL), unique=FALSE)
                     row.names(my_dataclus) <- names  
                     ##########################################################################
                     #DT::datatable(my_data2, options = list(scrollX = TRUE))
                     my_data <- my_dataclus %>%
                         na.omit() %>%          # Remove missing values (NA)
                         scale() 
                     # Compute
                     res.hc <- my_data %>%
                         scale() %>%                    # Scale the data
                         dist(method = "euclidean") %>% # Compute dissimilarity matrix
                         hclust(method = "ward.D2")     # Compute hierachical clustering
                     ##############
                     #captionclus = as.double(read_lines( "../GenePPI/captionclus.txt"))
                     
                     clus4 = cutree(res.hc, input$captionclus)
                     testDF <- data.frame(clus4)
                     testVect <- structure(clus4)
                     clusters2 = data.frame(SYMBOL2=names(testVect), clustid=testVect, row.names=NULL)
                     
                     db <- gsub("\\..*","",clusters2$SYMBOL2)
                     db
                     tools::file_path_sans_ext(clusters2$SYMBOL2)
                     clusters <- clusters2 %>% 
                         mutate(SYMBOL = db)
                     
                     mylist<-merge(my_data2, clusters[,c("SYMBOL","clustid")], by="SYMBOL",all.x=TRUE)
                     lll <- split(mylist, as.factor(mylist$clustid))
                     mylist2 <- subset(mylist[,c("SYMBOL","clustid")])
                     mylist22 <- subset(unique(mylist[,c("SYMBOL","clustid")]))
                     # mylist22[order(mylist22$clustid, decreasing = FALSE), ]
                     
                     mydataorderclustid <-mylist22[order(mylist22$clustid,decreasing = TRUE), ]
                     #DT::datatable(as.data.frame(mydataorderclustid), options = list(scrollX = TRUE))
                     #library(readr)
                     #subset_priorized_genes <- read_csv("../GenePPI/subset_priorized_genes.csv")
                     
                     gene_int_clus <- merge(subset_priorized_genes, mydataorderclustid[, c("SYMBOL", "clustid")], by="SYMBOL",all.x=TRUE)
                     #gene_int_clus[order(gene_int_clus$clustid,gene_int_clus$Priori,decreasing = TRUE), ]
                     gene_int_clus_ord <- gene_int_clus[order(gene_int_clus$clustid,-gene_int_clus$Priori), ]
                     DT::datatable(as.data.frame(gene_int_clus_ord), options = list(scrollX = TRUE))
                     
                     
                 }))
    
    
    
    observeEvent(input$action3,
                 output$slide441 <- DT::renderDT({
                     
                     ################################################################################
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     ###############################################################################
                     #id_Genes = as.character(input$id_Genes)
                     
                     # Loading
                     library("tibble")
                     library(readr)
                     library(tidyverse)
                     #write.csv(gene_PrbId,file="../GenePPI/gene_PrbId.csv")
                     GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                     
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     #gene_predicted <-subset(input,input$ENTREZID %in%  gene_prediction_Results$ENTREZID)
                     gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     
                     #selection_seuil_0_0001 <- read_csv("../GenePPI/selection_seuil_0_0001.csv")
                     selection_seuil_0_0001 <- as.data.frame(list(ENTREZID = gene_prediction_Results$ENTREZID,selection_based_simila =gene_prediction_Results$selection_based_simila,selection_based_selection = gene_prediction_Results$selection_based_selection))
                     tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
                     #cl4 <-subset(selection_seuil_0_0001,selection_seuil_0_0001$ENTREZID %in% gene_predicted$ENTREZID)
                     
                     cl4 <- gene_predicted$SYMBOL
                     #######################################################################################
                     #################################Tissus
                     tissues_selected = input$id_Tissues
                     #read_csv( "../GenePPI/id_Tissues.csv",show_col_types = FALSE)
                     #tissus_all_clus1 <- subset(data2()[,c("SYMBOL",tissues_selected)],data2()[,"SYMBOL"] %in%  cl4)
                     
                     tissus_all_clus1 <- subset(tissus[,c("SYMBOL",tissues_selected)],tissus$SYMBOL %in%  cl4)
                     #write.csv(tissus_all_clus1,file="../docglom21/Priorization/tissus_all_clus1.csv")
                     #tissus_all_clus1[is.na(tissus_all_clus1)] = 0
                     
                     #means_tiss$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #tissus_all_clus1$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #colMeans(df, na.rm=TRUE)
                     means_tiss <- tissus_all_clus1 %>%
                         na.omit()
                     
                     tissus_all_clus1$mean <-rowMeans(tissus_all_clus1[,tissues_selected], na.rm=TRUE)
                     #tissus_all_clus1
                     #tissus_all_clus1[,2:3]
                     #df = subset(nbr_Tiss, select = -IDX1.1 )
                     ##########################################################################################################
                     #####################################BP         ###############means
                     df_BP_values_na <- data21() %>% na.omit() 
                     df_BP_values_na_sele <- subset(df_BP_values_na,df_BP_values_na$geneSymbol %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     df_BP_values_na_sele_na_norm <- as.data.frame(lapply(df_BP_values_na_sele[,6:7], min_max_norm))
                     df_BP_values_na_sele_norm_df <- df_BP_values_na_sele %>% 
                         mutate(zscore = df_BP_values_na_sele_na_norm)
                     res.by <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, mean)
                     #res.by
                     vvv <- as.data.frame(res.by)
                     names(vvv)[1] <-"SYMBOL"
                     names(vvv)[2] <-"mean_BP_DF"
                     BP_db <- vvv
                     res.by_BP_nbr <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, length)
                     res.by_BP_nbr
                     vvv_res.by_BP_nbr <- as.data.frame(res.by_BP_nbr)
                     names(vvv_res.by_BP_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_BP_nbr)[2] <-"nbr_BP"
                     
                     nbr_BP <- vvv_res.by_BP_nbr 
                     
                     nbr_BP_DES_sel <- by(df_BP_values_na_sele_norm_df$Description, df_BP_values_na_sele$geneSymbol, paste)
                     nbr_BP_DES_df <- as.data.frame(nbr_BP_DES_sel)
                     names(nbr_BP_DES_df)[1] <-"SYMBOL"
                     names(nbr_BP_DES_df)[2] <-"DES_BP"
                     
                     nbr_BP_DES <- nbr_BP_DES_df 
                     ##########################################################################################################
                     ggg1_k <-  merge(tissus_all_clus1, BP_db[, c("SYMBOL", "mean_BP_DF")], by="SYMBOL",all.x=TRUE)
                     ggg12_k <-  merge(ggg1_k, nbr_BP[, c("SYMBOL", "nbr_BP")], by="SYMBOL",all.x=TRUE)
                     ggg13_k <-  merge(ggg12_k, nbr_BP_DES[, c("SYMBOL", "DES_BP")], by="SYMBOL",all.x=TRUE)
                     ############################################################################################################
                     ########################################################################
                     ##################################Expression                ####means
                     min_max_norm <- function(x) {
                         #(x - min(x)) / (max(x) - min(x))
                         (x - min(x)) / sd(x)}
                     EXP_norm0 <- as.data.frame(lapply(selection_seuil_0_0001[2:3], min_max_norm))
                     EXP_norm1 <- selection_seuil_0_0001 %>%   mutate(zscore = EXP_norm0$selection_based_selection)
                     #EXP_df <- EXP_norm1 %>%   mutate(SYMBOL = selection_seuil_0_0001_norm1$SYMBOL)
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     EXP_df <- merge(EXP_norm1, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     res.by_EXP <- by(EXP_df$zscore, EXP_df$SYMBOL, mean)
                     vvv_EXP <- as.data.frame(res.by_EXP)
                     names(vvv_EXP)[1] <-"SYMBOL"
                     names(vvv_EXP)[2] <-"mean_EXP_df"
                     
                     EXP_db <- vvv_EXP
                     
                     
                     ##########################################################################################################
                     ggg2_k <-  merge(ggg13_k, EXP_db[, c("SYMBOL", "mean_EXP_df")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ####################TFs
                     TFs <-  vroom::vroom("../GenePPI/input/TFs.csv",  locale = vroom::locale(encoding = "CP1252"))
                     
                     ghgwas = as.data.frame(list(SYMBOL= TFs$SYMBOL,
                                                 TF= TFs$TF,
                                                 Zscore= TFs$`Z-score`,
                                                 Fisher_score= TFs$`Fisher score `))
                     
                     TFs_na <- ghgwas %>% na.omit() 
                     TFs_na_sele <- subset(TFs_na,TFs_na$SYMBOL %in% cl4)
                     res.by_TFs <- by(TFs_na_sele$Fisher_score, TFs_na_sele$SYMBOL, mean)
                     res.by_TFs
                     vvv_TFs <- as.data.frame(res.by_TFs)
                     names(vvv_TFs)[1] <-"SYMBOL"
                     names(vvv_TFs)[2] <-"mean_TFs_df"
                     EXP_TFs <- vvv_TFs
                     res.by_TFs_nbr <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, length)
                     vvv_res.by_TFs_nbr <- as.data.frame(res.by_TFs_nbr)
                     
                     
                     names(vvv_res.by_TFs_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_TFs_nbr)[2] <-"nbr_TFs"
                     
                     nbr_TFs <- vvv_res.by_TFs_nbr
                     nbr_TFs_DES_sel <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, paste)
                     nbr_TFs_DES_df <- as.data.frame(nbr_TFs_DES_sel)
                     names(nbr_TFs_DES_df)[1] <-"SYMBOL"
                     names(nbr_TFs_DES_df)[2] <-"DES_TFs"
                     
                     nbr_TFs_DES <- nbr_TFs_DES_df 
                     ##########################################################################################################
                     ggg3_k <-  merge(ggg2_k, EXP_TFs[, c("SYMBOL", "mean_TFs_df")], by="SYMBOL",all.x=TRUE)
                     ggg31_k <-  merge(ggg3_k, nbr_TFs[, c("SYMBOL", "nbr_TFs")], by="SYMBOL",all.x=TRUE)
                     ggg32_k <-  merge(ggg31_k, nbr_TFs_DES[, c("SYMBOL", "DES_TFs")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ########################################################################
                     #########################GWAS
                     selection_seuil_GWAs_na <- GWAS %>% na.omit() 
                     GWAs_na_sele <- subset(selection_seuil_GWAs_na,selection_seuil_GWAs_na$`GeneID` %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     selection_seuil_GWAs_na_norm <- as.data.frame(lapply(GWAs_na_sele[,10], min_max_norm))
                     selection_seuil_GWAs_na_norm_df <- GWAs_na_sele %>% 
                         mutate(zscore = selection_seuil_GWAs_na_norm)
                     
                     mean_norm_GWAS <- mean(selection_seuil_GWAs_na_norm_df$zscore$P)
                     
                     res.by_GWAS <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, mean)
                     res.by_GWAS
                     library(taRifx)
                     vvv_GWAS <- as.data.frame(res.by_GWAS)
                     names(vvv_GWAS)[1] <-"SYMBOL"
                     names(vvv_GWAS)[2] <-"mean_GWAS_df"
                     
                     EXP_GWAS <- vvv_GWAS 
                     
                     res.by_GWAS_nbr <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, length)
                     res.by_GWAS_nbr
                     vvv_res.by_GWAS_nbr <- as.data.frame(res.by_GWAS_nbr)
                     names(vvv_res.by_GWAS_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_GWAS_nbr)[2] <-"nbr_GWAS"
                     
                     nbr_GWAS <- vvv_res.by_GWAS_nbr 
                     nbr_GWAS_DES_sel <- by(selection_seuil_GWAs_na_norm_df$`Trait`, selection_seuil_GWAs_na_norm_df$`GeneID`, paste)
                     nbr_GWAS_DES_df <- as.data.frame(nbr_GWAS_DES_sel)
                     names(nbr_GWAS_DES_df)[1] <-"SYMBOL"
                     names(nbr_GWAS_DES_df)[2] <-"DES_GWAS"
                     
                     nbr_GWAS_DES <- nbr_GWAS_DES_df
                     ##########################################################################################################
                     ggg4_k <-  merge(ggg32_k, EXP_GWAS[, c("SYMBOL", "mean_GWAS_df")], by="SYMBOL",all.x=TRUE)
                     ggg41_k <-  merge(ggg4_k, nbr_GWAS[, c("SYMBOL", "nbr_GWAS")], by="SYMBOL",all.x=TRUE)
                     ggg42_k <-  merge(ggg41_k, nbr_GWAS_DES[, c("SYMBOL", "DES_GWAS")], by="SYMBOL",all.x=TRUE)
                     #View(ggg42)
                     #########################################################################
                     ########################################################################
                     ##########################################function()
                     #prior = (result.mean_tissus + result.mean_expr)/2 + 
                     #ggg42[,c(6:8,10:13,15:16)][is.na(ggg42[,c(6:8,10:13,15:16)])] = 0
                     ggg42_k[is.na(ggg42_k)] = 0
                     ggg42fin_kidney_cl4 <- ggg42_k %>% rowwise() %>% mutate(Priori = ((mean+mean_EXP_df)/2) + (mean_BP_DF*nbr_BP) + (mean_TFs_df*nbr_TFs) + (mean_GWAS_df*nbr_GWAS))
                     #View(ggg42fin_kidney)
                     ###write.csv(as.matrix(ggg42fin_kidney_cl4), "../GenePPI/ggg42fin_kidney_cl4.csv")#, row.names=FALSE
                     #write.xlsx(ggg42fin_kidney_cl4, "../GenePPI/ggg42fin_kidney_cl4.xlsx")
                     #print(ggg42fin_kidney_cl4)
                     priori_scores <- ggg42fin_kidney_cl4[order(ggg42fin_kidney_cl4$Priori, decreasing = TRUE), ]
                     #DT::datatable(dplyr::printer, options = list(scrollX = TRUE))
                     
                     subset_priorized_genes <- subset(priori_scores[,c("SYMBOL","Priori")])
                     
                     
                     
                     
                     
                     
                     
                     
                     #print(myselectionread)
                     #req(data2())
                     
                     #subset_myselection_genes <- subset(subset_priorized_genes[,c("SYMBOL","Priori")],subset_priorized_genes$SYMBOL %in%  myselection_genes$x)
                     subset_myselection_genes <- subset(subset_priorized_genes[,c("SYMBOL","Priori")],subset_priorized_genes$SYMBOL %in% input$id_Genes)
                     
                     #file.remove("../GenePPI/id_Genes.csv")
                     #DT::datatable(subset_myselection_genes, options = list(scrollX = TRUE))
                     
                     DT::datatable(subset_myselection_genes, options = list(scrollX = TRUE))
                     
                 }))
    
    observeEvent(input$action3,
                 observeEvent(input$action3,
                              output$slide442 <- renderText({
                                  
                                  #################################################################################################
                                  library(tidyverse)
                                  
                                  fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                                  pattern = "\\.csv$",
                                                                  full.names = TRUE)
                                  dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                                      map_dfr(read.csv, header=TRUE, fill=TRUE)
                                  
                                  dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                                  library(dplyr)
                                  names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                                  #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                                  
                                  gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                                  
                                  
                                  #id_Genes = as.character(input$id_Genes)
                                  
                                  # Loading
                                  library("tibble")
                                  library(readr)
                                  library(tidyverse)
                                  #write.csv(gene_PrbId,file="../GenePPI/gene_PrbId.csv")
                                  GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                                  
                                  #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                                  #gene_predicted <-subset(input,input$ENTREZID %in%  gene_prediction_Results$ENTREZID)
                                  gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                                  
                                  #selection_seuil_0_0001 <- read_csv("../GenePPI/selection_seuil_0_0001.csv")
                                  selection_seuil_0_0001 <- as.data.frame(list(ENTREZID = gene_prediction_Results$ENTREZID,selection_based_simila =gene_prediction_Results$selection_based_simila,selection_based_selection = gene_prediction_Results$selection_based_selection))
                                  tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
                                  #cl4 <-subset(selection_seuil_0_0001,selection_seuil_0_0001$ENTREZID %in% gene_predicted$ENTREZID)
                                  
                                  cl4 <- gene_predicted$SYMBOL
                                  #######################################################################################
                                  #################################Tissus
                                  tissues_selected = input$id_Tissues
                                  #read_csv( "../GenePPI/id_Tissues.csv",show_col_types = FALSE)
                                  #tissus_all_clus1 <- subset(data2()[,c("SYMBOL",tissues_selected)],data2()[,"SYMBOL"] %in%  cl4)
                                  
                                  tissus_all_clus1 <- subset(tissus[,c("SYMBOL",tissues_selected)],tissus$SYMBOL %in%  cl4)
                                  #write.csv(tissus_all_clus1,file="../docglom21/Priorization/tissus_all_clus1.csv")
                                  #tissus_all_clus1[is.na(tissus_all_clus1)] = 0
                                  
                                  #means_tiss$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                                  #tissus_all_clus1$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                                  #colMeans(df, na.rm=TRUE)
                                  means_tiss <- tissus_all_clus1 %>%
                                      na.omit()
                                  
                                  tissus_all_clus1$mean <-rowMeans(tissus_all_clus1[,tissues_selected], na.rm=TRUE)
                                  #tissus_all_clus1
                                  #tissus_all_clus1[,2:3]
                                  #df = subset(nbr_Tiss, select = -IDX1.1 )
                                  ##########################################################################################################
                                  #####################################BP         ###############means
                                  df_BP_values_na <- data21() %>% na.omit() 
                                  df_BP_values_na_sele <- subset(df_BP_values_na,df_BP_values_na$geneSymbol %in% cl4)
                                  min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                                  df_BP_values_na_sele_na_norm <- as.data.frame(lapply(df_BP_values_na_sele[,6:7], min_max_norm))
                                  df_BP_values_na_sele_norm_df <- df_BP_values_na_sele %>% 
                                      mutate(zscore = df_BP_values_na_sele_na_norm)
                                  res.by <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, mean)
                                  #res.by
                                  vvv <- as.data.frame(res.by)
                                  names(vvv)[1] <-"SYMBOL"
                                  names(vvv)[2] <-"mean_BP_DF"
                                  BP_db <- vvv
                                  res.by_BP_nbr <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, length)
                                  res.by_BP_nbr
                                  vvv_res.by_BP_nbr <- as.data.frame(res.by_BP_nbr)
                                  names(vvv_res.by_BP_nbr)[1] <-"SYMBOL"
                                  names(vvv_res.by_BP_nbr)[2] <-"nbr_BP"
                                  
                                  nbr_BP <- vvv_res.by_BP_nbr 
                                  
                                  nbr_BP_DES_sel <- by(df_BP_values_na_sele_norm_df$Description, df_BP_values_na_sele$geneSymbol, paste)
                                  nbr_BP_DES_df <- as.data.frame(nbr_BP_DES_sel)
                                  names(nbr_BP_DES_df)[1] <-"SYMBOL"
                                  names(nbr_BP_DES_df)[2] <-"DES_BP"
                                  
                                  nbr_BP_DES <- nbr_BP_DES_df 
                                  ##########################################################################################################
                                  ggg1_k <-  merge(tissus_all_clus1, BP_db[, c("SYMBOL", "mean_BP_DF")], by="SYMBOL",all.x=TRUE)
                                  ggg12_k <-  merge(ggg1_k, nbr_BP[, c("SYMBOL", "nbr_BP")], by="SYMBOL",all.x=TRUE)
                                  ggg13_k <-  merge(ggg12_k, nbr_BP_DES[, c("SYMBOL", "DES_BP")], by="SYMBOL",all.x=TRUE)
                                  ############################################################################################################
                                  ########################################################################
                                  ##################################Expression                ####means
                                  min_max_norm <- function(x) {
                                      #(x - min(x)) / (max(x) - min(x))
                                      (x - min(x)) / sd(x)}
                                  EXP_norm0 <- as.data.frame(lapply(selection_seuil_0_0001[2:3], min_max_norm))
                                  EXP_norm1 <- selection_seuil_0_0001 %>%   mutate(zscore = EXP_norm0$selection_based_selection)
                                  #EXP_df <- EXP_norm1 %>%   mutate(SYMBOL = selection_seuil_0_0001_norm1$SYMBOL)
                                  aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                                  EXP_df <- merge(EXP_norm1, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                                  res.by_EXP <- by(EXP_df$zscore, EXP_df$SYMBOL, mean)
                                  vvv_EXP <- as.data.frame(res.by_EXP)
                                  names(vvv_EXP)[1] <-"SYMBOL"
                                  names(vvv_EXP)[2] <-"mean_EXP_df"
                                  
                                  EXP_db <- vvv_EXP 
                                  
                                  
                                  ##########################################################################################################
                                  ggg2_k <-  merge(ggg13_k, EXP_db[, c("SYMBOL", "mean_EXP_df")], by="SYMBOL",all.x=TRUE)
                                  ########################################################################
                                  ####################TFs
                                  TFs <-  vroom::vroom("../GenePPI/input/TFs.csv",  locale = vroom::locale(encoding = "CP1252"))
                                  
                                  ghgwas = as.data.frame(list(SYMBOL= TFs$SYMBOL,
                                                              TF= TFs$TF,
                                                              Zscore= TFs$`Z-score`,
                                                              Fisher_score= TFs$`Fisher score `))
                                  
                                  TFs_na <- ghgwas %>% na.omit() 
                                  TFs_na_sele <- subset(TFs_na,TFs_na$SYMBOL %in% cl4)
                                  res.by_TFs <- by(TFs_na_sele$Fisher_score, TFs_na_sele$SYMBOL, mean)
                                  res.by_TFs
                                  vvv_TFs <- as.data.frame(res.by_TFs)
                                  names(vvv_TFs)[1] <-"SYMBOL"
                                  names(vvv_TFs)[2] <-"mean_TFs_df"
                                  EXP_TFs <- vvv_TFs
                                  res.by_TFs_nbr <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, length)
                                  vvv_res.by_TFs_nbr <- as.data.frame(res.by_TFs_nbr)
                                  
                                  
                                  names(vvv_res.by_TFs_nbr)[1] <-"SYMBOL"
                                  names(vvv_res.by_TFs_nbr)[2] <-"nbr_TFs"
                                  
                                  nbr_TFs <- vvv_res.by_TFs_nbr
                                  nbr_TFs_DES_sel <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, paste)
                                  nbr_TFs_DES_df <- as.data.frame(nbr_TFs_DES_sel)
                                  names(nbr_TFs_DES_df)[1] <-"SYMBOL"
                                  names(nbr_TFs_DES_df)[2] <-"DES_TFs"
                                  
                                  nbr_TFs_DES <- nbr_TFs_DES_df 
                                  ##########################################################################################################
                                  ggg3_k <-  merge(ggg2_k, EXP_TFs[, c("SYMBOL", "mean_TFs_df")], by="SYMBOL",all.x=TRUE)
                                  ggg31_k <-  merge(ggg3_k, nbr_TFs[, c("SYMBOL", "nbr_TFs")], by="SYMBOL",all.x=TRUE)
                                  ggg32_k <-  merge(ggg31_k, nbr_TFs_DES[, c("SYMBOL", "DES_TFs")], by="SYMBOL",all.x=TRUE)
                                  ########################################################################
                                  ########################################################################
                                  #########################GWAS
                                  selection_seuil_GWAs_na <- GWAS %>% na.omit() 
                                  GWAs_na_sele <- subset(selection_seuil_GWAs_na,selection_seuil_GWAs_na$`GeneID` %in% cl4)
                                  min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                                  selection_seuil_GWAs_na_norm <- as.data.frame(lapply(GWAs_na_sele[,10], min_max_norm))
                                  selection_seuil_GWAs_na_norm_df <- GWAs_na_sele %>% 
                                      mutate(zscore = selection_seuil_GWAs_na_norm)
                                  
                                  mean_norm_GWAS <- mean(selection_seuil_GWAs_na_norm_df$zscore$P)
                                  
                                  res.by_GWAS <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, mean)
                                  res.by_GWAS
                                  vvv_GWAS <- as.data.frame(res.by_GWAS)
                                  names(vvv_GWAS)[1] <-"SYMBOL"
                                  names(vvv_GWAS)[2] <-"mean_GWAS_df"
                                  
                                  EXP_GWAS <- vvv_GWAS 
                                  
                                  res.by_GWAS_nbr <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, length)
                                  res.by_GWAS_nbr
                                  vvv_res.by_GWAS_nbr <- as.data.frame(res.by_GWAS_nbr)
                                  names(vvv_res.by_GWAS_nbr)[1] <-"SYMBOL"
                                  names(vvv_res.by_GWAS_nbr)[2] <-"nbr_GWAS"
                                  
                                  nbr_GWAS <- vvv_res.by_GWAS_nbr 
                                  nbr_GWAS_DES_sel <- by(selection_seuil_GWAs_na_norm_df$`Trait`, selection_seuil_GWAs_na_norm_df$`GeneID`, paste)
                                  nbr_GWAS_DES_df <- as.data.frame(nbr_GWAS_DES_sel)
                                  names(nbr_GWAS_DES_df)[1] <-"SYMBOL"
                                  names(nbr_GWAS_DES_df)[2] <-"DES_GWAS"
                                  
                                  nbr_GWAS_DES <- nbr_GWAS_DES_df
                                  ##########################################################################################################
                                  ggg4_k <-  merge(ggg32_k, EXP_GWAS[, c("SYMBOL", "mean_GWAS_df")], by="SYMBOL",all.x=TRUE)
                                  ggg41_k <-  merge(ggg4_k, nbr_GWAS[, c("SYMBOL", "nbr_GWAS")], by="SYMBOL",all.x=TRUE)
                                  ggg42_k <-  merge(ggg41_k, nbr_GWAS_DES[, c("SYMBOL", "DES_GWAS")], by="SYMBOL",all.x=TRUE)
                                  #View(ggg42)
                                  #########################################################################
                                  ########################################################################
                                  ##########################################function()
                                  #prior = (result.mean_tissus + result.mean_expr)/2 + 
                                  #ggg42[,c(6:8,10:13,15:16)][is.na(ggg42[,c(6:8,10:13,15:16)])] = 0
                                  ggg42_k[is.na(ggg42_k)] = 0
                                  ggg42fin_kidney_cl4 <- ggg42_k %>% rowwise() %>% mutate(Priori = ((mean+mean_EXP_df)/2) + (mean_BP_DF*nbr_BP) + (mean_TFs_df*nbr_TFs) + (mean_GWAS_df*nbr_GWAS))
                                  #View(ggg42fin_kidney)
                                  ###write.csv(as.matrix(ggg42fin_kidney_cl4), "../GenePPI/ggg42fin_kidney_cl4.csv")#, row.names=FALSE
                                  #write.xlsx(ggg42fin_kidney_cl4, "../GenePPI/ggg42fin_kidney_cl4.xlsx")
                                  #print(ggg42fin_kidney_cl4)
                                  priori_scores <- ggg42fin_kidney_cl4[order(ggg42fin_kidney_cl4$Priori, decreasing = TRUE), ]
                                  #DT::datatable(dplyr::printer, options = list(scrollX = TRUE))
                                  
                                  subset_priorized_genes <- subset(priori_scores[,c("SYMBOL","Priori")])
                                  
                                  #subset_myselection_genes <- subset(subset_priorized_genes[,c("SYMBOL","Priori")],subset_priorized_genes$SYMBOL %in%  myselection_genes$x)
                                  subset_myselection_genes <- subset(subset_priorized_genes[,c("SYMBOL","Priori")],subset_priorized_genes$SYMBOL %in%  input$id_Genes)
                                  
                                  #file.remove("../GenePPI/id_Genes.csv")
                                  #DT::datatable(subset_myselection_genes, options = list(scrollX = TRUE))
                                  res_intera <-  log10((subset_myselection_genes$Priori[1])+log10(subset_myselection_genes$Priori[2]))/(2+log10(max(subset_myselection_genes$Priori)))
                                  
                                  interaction_score <- cbind(subset_myselection_genes, res_intera)
                                  #print(paste0("You've selected the genes named ", input$id_Genes[], ". It's mean value is: ", res_intera))
                                  
                                  #paste0("You've selected the genes named ", res_intera)
                                  # DT::datatable(res_intera, options = list(scrollX = TRUE))
                                  print(paste0("Your interaction score based MGTI model is ", res_intera))
                                  
                              }))
    )
    
    
    
    
}

shiny::shinyApp(ui, server)

library(BiocManager)
options(repos = BiocManager::repositories())
options(rsconnect.http.trace = TRUE)
options(rsconnect.http.verbose = TRUE)
options(shiny.fullstacktrace = TRUE)

#library(AnnotationDbi)
library(Biobase)
library(BiocGenerics)
library(BiocParallel)
library(DO.db)
library(DOSE)
library(GO.db)
library(GOSemSim)
#setwd("../GenePPI")
library(readr)
gene_PrbId <- read_csv("../GenePPI/input/gene_PrbId.csv",show_col_types = FALSE)
library(googlesheets4)
library(readr)
library(shiny)
library(shinydashboard)
library(shinyauthr)
library(dplyr)
library(shinyjs)
library(DT)
library(shinyWidgets)
# sample logins dataframe with passwords hashed by sodium package
user_base <- tibble(
    user = c("user1", "user2"),
    password = sapply(c("pass1s", "pass2s"), sodium::password_store), 
    permissions = c("admin", "standard"),
    name = c("User One", "User Two")
)

ui <- dashboardPage(
    
    # put the shinyauthr logout ui module in here
    dashboardHeader(
        title = "Gene PPI",
        tags$li(class = "dropdown", style = "padding: 8px;", shinyauthr::logoutUI("logout"))
    ),
    
    # setup a sidebar menu to be rendered server-side
    dashboardSidebar(
        collapsed = TRUE, sidebarMenuOutput("sidebar")
    ),
    
    
    dashboardBody(
        shinyjs::useShinyjs(),
        
        # put the shinyauthr login ui module here
        shinyauthr::loginUI("login"),
        
        # setup any tab pages you want after login here with uiOutputs
        tabItems(
            tabItem("tab0",uiOutput("tab0_ui")),
            tabItem("tab3", uiOutput("tab3_ui")),
            tabItem("tab4", uiOutput("tab4_ui")),
            tabItem("sub_1", uiOutput("sub_1_ui")),
            tabItem("sub_2", uiOutput("sub_2_ui")),
            tabItem("sub_3", uiOutput("sub_3_ui")),
            tabItem("tab5", uiOutput("tab5_ui"))
        )
        
    )
)

server <- function(input, output, session) {
    library(readr)
    
    # login status and info will be managed by shinyauthr module and stores here
    credentials <- callModule(shinyauthr::login, "login", 
                              data = user_base,
                              user_col = user,
                              pwd_col = password,
                              sodium_hashed = TRUE,
                              log_out = reactive(logout_init()))
    
    # logout status managed by shinyauthr module and stored here
    logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
    
    # this opens or closes the sidebar on login/logout
    observe({
        if(credentials()$user_auth) {
            shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
        } else {
            shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        }
    })
    
    # only when credentials()$user_auth is TRUE, render your desired sidebar menu
    output$sidebar <- renderMenu({
        req(credentials()$user_auth)
        sidebarMenu(
            id = "tabs",
            menuItem("Welcome", tabName = "tab0"),
            menuItem("Start Analysis", tabName = "tab3"),
            menuItem("Tutorial", tabName = "tab4",menuSubItem("Gene Prediction", tabName = "sub_1"), 
                     menuSubItem("Gene Prioritization", tabName = "sub_2"),menuSubItem("Gene Interaction", tabName = "sub_3")),
            menuItem("Contact Us", tabName = "tab5")
            # /menuItem 
        )
    })
    
    
    
    # tab 0 UI and output ----------------------------------------
    output$tab0_ui <- renderUI({
        req(credentials()$user_auth)
        DT::DTOutput("table0")
    })
    
    
    output$tab0_ui <- renderUI({
        req(credentials()$user_auth)
        #includeHTML("home.html")
        tabPanel("Home",
                 includeHTML("home.html"),
                 tags$script(src = "plugins/scripts.js"),
                 tags$head(
                     tags$link(rel = "stylesheet", 
                               type = "text/css", 
                               href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                     tags$link(rel = "icon", 
                               type = "image/png", 
                               href = "images/logo_icon.png")
                 )
        )
        
    })
    
    
    
    
    # sub_1 UI and output ----------------------------------------
    
    output$sub_1_ui <- renderUI({
        
        req(credentials()$user_auth)
        #includeHTML("home.html")
        tabPanel("Home",
                 includeHTML("prediction.html"),#../bmc_bioinformatics_version_3.html
                 tags$script(src = "plugins/scripts.js"),
                 tags$head(
                     tags$link(rel = "stylesheet", 
                               type = "text/css", 
                               href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                     tags$link(rel = "icon", 
                               type = "image/png", 
                               href = "images/logo_icon.png")
                 )
        )
        
    })
    
    output$sub_2_ui <- renderUI({
        
        req(credentials()$user_auth)
        #includeHTML("home.html")
        tabPanel("Home",
                 includeHTML("prioritization.html"),#../bmc_bioinformatics_version_3.html
                 tags$script(src = "plugins/scripts.js"),
                 tags$head(
                     tags$link(rel = "stylesheet", 
                               type = "text/css", 
                               href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                     tags$link(rel = "icon", 
                               type = "image/png", 
                               href = "images/logo_icon.png")
                 )
        )
        
    })
    
    output$sub_3_ui <- renderUI({
        
        req(credentials()$user_auth)
        #includeHTML("home.html")
        tabPanel("Home",
                 includeHTML("interaction.html"),#../bmc_bioinformatics_version_3.html
                 tags$script(src = "plugins/scripts.js"),
                 tags$head(
                     tags$link(rel = "stylesheet", 
                               type = "text/css", 
                               href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                     tags$link(rel = "icon", 
                               type = "image/png", 
                               href = "images/logo_icon.png")
                 )
        )
        
    })
    
    
    
    
    output$tab5_ui <- renderUI({
        req(credentials()$user_auth)
        #includeHTML("home.html")
        tabPanel("Home",
                 includeHTML("contact.html"),
                 tags$script(src = "plugins/scripts.js"),
                 tags$head(
                     tags$link(rel = "stylesheet", 
                               type = "text/css", 
                               href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                     tags$link(rel = "icon", 
                               type = "image/png", 
                               href = "images/logo_icon.png")
                 )
        )
        
    })
    
    
    
    
    # tab 3 UI and output ----------------------------------------
    
    output$tab3_ui <- renderUI({
        req(credentials()$user_auth)
        
        DT::DTOutput("table3")
        tabsetPanel(
            
            tabPanel(title = "Gene Prediction",
                     fluidPage(
                         sidebarLayout(
                             sidebarPanel(
                                 # Input: Select a file ----
                                 fileInput("file1", "Upload your Expression results (CSV) File"),
                                 
                                 
                                 textInput("caption", "Fix a P-value", ""),
                                 tags$hr(),
                                 verbatimTextOutput("P_adjust"),
                                 # Horizontal line ----
                                 
                                 # actionButton("action", label = "Start Analysis!",color = "btn action-button"), #class = "btn-success"
                                 actionButton("action", label = "Start Analysis", icon = icon("play-circle")),
                                 # Horizontal line ----
                                 tags$hr(),
                                 sliderInput("bw_adjust", label = "Threshold adjustment",
                                             min = 10, max = 100, value = 1, step = 0.2),
                                 
                                 
                             ),
                             mainPanel(
                                 tabsetPanel(
                                     id = 'dataset',
                                     tabPanel("Selected Expression table", DT::dataTableOutput("slide1")),
                                     tabPanel("semantic similarity visualisation", DT::dataTableOutput("slide2"),plotOutput("plot2")),
                                     tabPanel("Gene Prediction Results", DT::dataTableOutput("slide3"),DT::dataTableOutput("slide31")),
                                     tabPanel("Gene Prediction visualisation", DT::dataTableOutput("slide4"),plotOutput("plot4"))
                                     
                                 )
                             )
                         )
                     )),
            
            tabPanel(title = "Gene Prioritization",
                     fluidPage(
                         sidebarLayout(
                             sidebarPanel(
                                 
                                 # Horizontal line ----
                                 #tags$hr(),
                                 #numericInput("captionclus", "Number  of Clusters", value = 0, min = 0, max = 20),
                                 # Horizontal line ----
                                 
                                 #tags$hr(),
                                 #actionButton("actionclus", label = "Find!",color = "btn action-button"),
                                 #textInput("captionclus", "Number of Clusters", ""),
                                 #tags$hr(),
                                 # actionButton("action", label = "Start Analysis!",color = "btn action-button"), #class = "btn-success"
                                 #actionButton("actionclust", label = "Start Analysis", icon = icon("play-circle")),
                                 fileInput("file111", "Upload your Expression results (CSV) File"),
                                 # Input: Select a file ----
                                 fileInput("file2", "Upload Tissues (CSV) File",multiple = TRUE),
                                 fileInput("file21", "Upload Biological Proccess (CSV) File",multiple = TRUE),
                                 fileInput("file22", "Upload GWAS (CSV) File",multiple = TRUE),
                                 fileInput("file23", "Upload TFs (CSV) File",multiple = TRUE),
                                 # Horizontal line ----
                                 tags$hr(),
                                 actionButton("action2", label = "Start Analysis!",icon = icon("play-circle")),
                                 tags$hr(),
                                 textInput("captionclus", "Fix a number  of Clusters", ""),
                                 tags$hr(),
                                 verbatimTextOutput("captionclus_adjust"),
                                 actionButton("actionvalidclust", label = "Fix the number  of Clusters", icon = icon("play-circle")),
                                 # Horizontal line ----
                                 #tags$hr(),
                                 # Input: Checkbox if file has header ----
                                 #selectInput("col2", "Select a column", character(0)),
                                 # Horizontal line ----
                                 #tags$hr(),
                                 #textInput("caption2", "Tissue_name", ""),
                                 #tags$hr(),
                                 #verbatimTextOutput("Tissue_name"),
                                 # Horizontal line ----
                                 tags$hr(),
                                 multiInput(
                                     inputId = "id_Tissues", label = "Select Tissues :",
                                     choices = c("Kidney",	"Kidney cancer cell",	"Immune system",	"Urine",	"Blood",
                                                 "Blood vessel",	"Blood plasma",	"Hematopoietic stem cell",	"Parenchyma",
                                                 "Uroepithelium",	"HEK 293 EBNA cell",	"HEK 293ET cell",	"HK 2 cell", 
                                                 "Heart", "Intestine", "Leukocyte", "Stomach","Bone marrow","Muscle","Hepatoma cell",
                                                 "Ovary","Lung","Pancreas","Rhizome","Neuron","Breast cancer cell","Prostate cancer cell line","Vasculature",
                                                 "Arterial blood","Knee","Eye","Mesenchyme","Cancer stem cell","Vertebra"),
                                     selected = "", width = "400px",
                                     #options = list(
                                     # enable_search = FALSE,
                                     #  non_selected_header = "Choose between:",
                                     #  selected_header = "You have selected:"
                                     # ),
                                 ),
                                 
                                 verbatimTextOutput(outputId = "res_Tissues"),
                                 tags$hr(),
                                 actionButton("actiontis", label = "Update Tissues!",icon = icon("play-circle")),
                                 
                                 # actionButton("action", label = "Start Analysis!",color = "btn action-button"), #class = "btn-success"
                                 # Input: Select a file ----
                                 
                                 
                                 
                                 
                                 
                             ),
                             mainPanel(
                                 tabsetPanel(
                                     id = 'priori',
                                     tabPanel("Data Tables visualisation", DT::dataTableOutput("slide21"), DT::dataTableOutput("slide211"),
                                              DT::dataTableOutput("slide212"),DT::dataTableOutput("slide213"),
                                              plotOutput("plot21")),
                                     tabPanel("Clustering Analysis", DT::dataTableOutput("slide91"),DT::dataTableOutput("slide911"), DT::dataTableOutput("slide9111"),DT::dataTableOutput("slide91111"), plotOutput("slide991"), plotOutput("slide992")),
                                     
                                     tabPanel("Classification visualisation", DT::dataTableOutput("slide23"), DT::dataTableOutput("slide233"),
                                              plotOutput("plot23"),plotOutput("plot233"),plotOutput("plot2333"),plotOutput("plot23333")),
                                     tabPanel("Logistic Regression  Results", DT::dataTableOutput("slide222"),DT::dataTableOutput("slide22"),plotOutput("plot22")),
                                     tabPanel("Logistic Regression visualisation", DT::dataTableOutput("slide24"),plotOutput("plot24"))
                                     
                                     
                                 ))))),tabPanel(title = "Gene Interaction",fluidPage(
                                     sidebarLayout(
                                         sidebarPanel(
                                             
                                             # Horizontal line ----
                                             tags$hr(),
                                             multiInput(
                                                 inputId = "id_Genes", label = "Select Genes :",
                                                 
                                                 choices = unique(gene_PrbId$SYMBOL),#subset_priorized_genes$SYMBOL
                                                 selected = "", width = "400px",
                                                 #options = list(
                                                 # enable_search = FALSE,
                                                 #  non_selected_header = "Choose between:",
                                                 #  selected_header = "You have selected:"
                                                 # )
                                             ),
                                             verbatimTextOutput(outputId = "res_Genes"),
                                             # Horizontal line ----
                                             tags$hr(),
                                             actionButton("action3", label = "Find!",color = "btn action-button")
                                             
                                             
                                         ),
                                         mainPanel(
                                             tabsetPanel(
                                                 id = 'interac',
                                                 tabPanel("MTGI  Results", DT::dataTableOutput("slide441"),verbatimTextOutput("slide442"),DT::dataTableOutput("slide4411"))
                                                 #tabPanel("MTGI   Plot", DT::dataTableOutput("slide4441"),  plotOutput("plot4442"))
                                                 
                                                 
                                             )))))
            
        )
        
    })
    
    
    
    ########################################################
    ###Gene Prediction############################
    
    output$home_img <- renderImage({
        
        list(src = "www/header_img.png",
             width = "100%",
             height = 330)
        
    }, deleteFile = F)
    
    data1 <- reactive({
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)
        read.csv(inFile$datapath)
    })
    
    
    
    
    observeEvent(input$action,
                 output$slide1 <- DT::renderDT({
                     
                     DT::datatable(data1(), options = list(scrollX = TRUE))
                     
                     #req(data())
                 })
                 
                 
    )
    
    output$P_adjust <- renderText({  
        input$P_adjust
        #writeLines(input$caption, "pvalue.txt")
    })
    
    
    observeEvent(input$action,  
                 output$slide2 <- DT::renderDT({
                     #library(readr)
                     #gene_PrbId <- read_csv("../GenePPI/input/gene_PrbId.csv",show_col_types = FALSE)
                     library(GOSemSim)
                     library(org.Hs.eg.db)
                     hsGO_all <- godata('org.Hs.eg.db', ont=c("BP", "CC", "MF"))
                     #mgeneSim_output =mgeneSim(c("1363","189","54993"),semData=hsGO_all, measure="Wang",combine = "BMA" ,verbose=FALSE)# plot squared correlations
                     mgeneSim_output =mgeneSim(data1()[,"ENTREZID"],semData=hsGO_all, measure="Wang",combine = "BMA" ,verbose=FALSE)# plot squared correlations
                     #mgeneSim_output =mgeneSim(gene_PrbId$ENTREZID,semData=hsGO_all, measure="Wang",combine = "BMA" ,verbose=FALSE)# plot squared correlations
                     
                     
                     matrix <- mgeneSim_output
                     #pvalues = as.double(input$P_adjust)
                     pvalues = 0.0001
                     fgh = nrow(matrix)
                     
                     
                     loop.for.function <- function(){
                         
                         for( i in 1:fgh) {
                             j = i+1
                             for( j in 2:fgh) {
                                 #print(mgeneSim_0_000001[c(i,j),] )
                                 batch2009 <- matrix[i,]
                                 batch2015 <- matrix[j,]
                                 diff_in_means <- mean((batch2009 + batch2015)/2)
                                 s1 <- sd(batch2009)
                                 s2 <- sd(batch2015) 
                                 t_entropy <- sqrt(1/2*(((s1^2/s2^2)+(s2^2/s1^2)))*diff_in_means^2)
                                 ffff = matrix[i,]> t_entropy
                                 gggg_table = subset(matrix[i,], ffff)
                                 outputdf <- data.frame(Row  = i, Similarity_score = t_entropy, Selected_score = gggg_table)
                                 selection_based_simila <- (fgh*pvalues) + (length(outputdf$Row))* outputdf$Similarity_score
                                 
                                 selection_based_selection <- (fgh*pvalues) + (length(outputdf$Row))* outputdf$Selected_score 
                                 
                                 outputselection <- data.frame(Row  = i, Similarity_score = t_entropy, Selected_score = gggg_table,
                                                               FreqRow = length(outputdf$Row), selection_based_simila = selection_based_simila,
                                                               selection_based_selection = selection_based_selection)
                                 #outputdf5 <- data.frame(Row  = i, Similarity_score = t_entropy, Selected_score = gggg_table, selection_based_simila = selection_based_simila, selection_based_selection = selection_based_selection)
                                 
                                 mat2 = outputselection
                                 form2 = paste('../GenePPI/test/outputselection', i, 'csv', sep = '.')
                                 write.csv(mat2, file = form2)
                             }
                         }}
                     loop.for.function()
                     
                     
                 })
    )
    observeEvent(input$action,
                 output$plot2 <- renderPlot({ 
                     library(GOSemSim)
                     library(org.Hs.eg.db)
                     hsGO_all <- godata('org.Hs.eg.db', ont=c("BP", "CC", "MF"))
                     #mgeneSim_output =mgeneSim(c("1363","189","54993"),semData=hsGO_all, measure="Wang",combine = "BMA" ,verbose=FALSE)# plot squared correlations
                     mgeneSim_output =mgeneSim(data1()[,"ENTREZID"],semData=hsGO_all, measure="Wang",combine = "BMA" ,verbose=FALSE)# plot squared correlations
                     
                     library(variancePartition)
                     plotCorrMatrix( mgeneSim_output, dendrogram="none" )
                     
                 })  )
    
    
    
    output$slide31 <- DT::renderDT({ 
        ## Full path to csv filenames
        
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        gene_prediction_DT <- dataset_selected_0_000001[,c(1,5,7)]
        DT::datatable(gene_prediction_DT, options = list(scrollX = TRUE))
        
        # print(dataset_selected_0_000001)    #DT::datatable(dplyr::dataset_selected_0_000001, options = list(scrollX = TRUE))
        
        
    })
    
    output$plot4 <- renderPlot({ 
        ## Full path to csv filenames
        #write.csv( dataset_selected_0_000001, "../GenePPI/dataset_selected_0_000001.csv")
        
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        gene_prediction_DT <- dataset_selected_0_000001[,c(1,5,7)]
        
        p_0_000001 <-  ggplot(dataset_selected_0_000001,inline = F, aes(x = dataset_selected_0_000001$Row,
                                                                        y = dataset_selected_0_000001$selection_based_selection)) +
            geom_line(colour = dataset_selected_0_000001$ENTREZID,size=1) + #colour = dataset_litt$X   color = 'red'  linetype = "dotdash" alpha=0.1 size=1.3
            theme_classic() +
            labs(
                x = "Number of genes",
                y = "CombSC",
                title = paste(""
                )
            )
        p_0_000001 + geom_hline(yintercept = input$bw_adjust, color = 'red' )
        
    })
    output$slide4 <- DT::renderDT({ 
        ## Full path to csv filenames
        
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        library(dplyr)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
        
        gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
        gene_prediction_DT2 <- gene_prediction_Results[,c(1,5,7)]
    })
    
    
    
    
    
    ########################################################
    ###Gene Prioritization############################
    #output$res <- renderPrint({
    #input$Tid
    # })
    output$captionclus_adjust <- renderText({  
        # writeLines(input$captionclus, "captionclus.txt")
        input$captionclus
    })
    
    
    output$res_Tissues <- renderPrint({
        input$id_Tissues
        
    })
    
    
    data111 <- reactive({
        inFile <- input$file111
        if (is.null(inFile)) return(NULL)
        read.csv(inFile$datapath)
    })
    data2 <- reactive({
        inFile2 <- input$file2
        if (is.null(inFile2)) return(NULL)
        read.csv(inFile2$datapath)
    })
    
    observeEvent(data2(), {
        updateSelectInput(session, "col", choices = names(data2()))
    })
    
    
    data21 <- reactive({
        inFile21 <- input$file21
        if (is.null(inFile21)) return(NULL)
        read.csv(inFile21$datapath)
    })
    
    data22 <- reactive({
        inFile22 <- input$file22
        if (is.null(inFile22)) return(NULL)
        read.csv(inFile22$datapath)
    })
    data23 <- reactive({
        inFile23 <- input$file23
        if (is.null(inFile23)) return(NULL)
        read.csv(inFile23$datapath)
    })
    
    
    observeEvent(input$action2,
                 output$slide991 <- renderPlot({
                     ##########################################################
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     
                     
                     #################################################################
                     library(readr)
                     
                     my_data <- gene_prediction_Results
                     #names(my_data)[1] <-"ENTREZID"
                     
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in%  gene_prediction_Results$ENTREZID)
                     my_data2 <- merge(my_data, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     
                     #DT::datatable(my_data2, options = list(scrollX = TRUE))
                     my_data <- my_data %>%
                         na.omit() %>%          # Remove missing values (NA)
                         scale() 
                     # Compute
                     library("NbClust")
                     res.nbclust <- my_data %>%
                         scale() %>%
                         NbClust(distance = "euclidean",
                                 min.nc = 2, max.nc = 10, 
                                 method = "complete", index ="all") 
                     
                     # Visualize
                     library(factoextra)
                     fviz_nbclust(res.nbclust, ggtheme = theme_minimal())
                     
                 })
                 
    )
    
    
    
    observeEvent(input$actionvalidclust,
                 output$slide992 <- renderPlot({
                     ###################################################################################
                     
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     ####################################################################################
                     
                     
                     
                     library(readr)
                     my_data <- gene_prediction_Results
                     #names(my_data)[1] <-"ENTREZID"
                     
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in%  my_data$ENTREZID)
                     my_data2 <- merge(my_data, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     my_data2 <- merge(aaaaaaaa[, c("SYMBOL", "ENTREZID")], my_data, by="ENTREZID",all.x=TRUE)
                     my_data3 <-  my_data2[,c("SYMBOL","ENTREZID","selection_based_simila","selection_based_selection")]
                     ##########################################################################
                     
                     #####################################################################################################
                     my_dataclus  <- my_data2[,c("selection_based_simila","selection_based_selection")]
                     names <- make.unique(as.character(my_data3$SYMBOL))
                     row.names(my_dataclus) <- names  
                     ##########################################################################
                     
                     
                     #DT::datatable(my_data2, options = list(scrollX = TRUE))
                     my_data <- my_dataclus %>%
                         na.omit() %>%          # Remove missing values (NA)
                         scale() 
                     
                     # Compute hierarchical clustering
                     res.hc <- my_data %>%
                         scale() %>%                    # Scale the data
                         dist(method = "euclidean") %>% # Compute dissimilarity matrix
                         hclust(method = "ward.D2")     # Compute hierachical clustering
                     
                     #captionclus = as.double(read_lines( "../GenePPI/captionclus.txt"))
                     
                     fviz_dend(res.hc, k = as.double(input$captionclus),
                               cex = 0.7,                     # Label size
                               palette = "jco",               # Color palette see ?ggpubr::ggpar
                               rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                               rect_border = "jco",           # Rectangle color
                               labels_track_height = 0.8      # Augment the room for labels
                     )
                     
                     #fviz_dend(res.hc, k = 4, # Cut in four groups
                     #     cex = 0.5, # label size
                     #     k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                     #     color_labels_by_k = TRUE, # color labels by groups
                     #     rect = TRUE # Add rectangle around groups
                     #  )
                     
                 }))
    
    
    observeEvent(input$actionvalidclust,
                 output$slide911 <- DT::renderDT({
                     
                     
                     
                     ###############################################################################################
                     
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     
                     ###############################################################################################
                     library(readr)
                     library(tidyr)
                     library("cluster")
                     library("factoextra")
                     library("magrittr")
                     
                     my_data <- gene_prediction_Results
                     #names(my_data)[1] <-"ENTREZID"
                     
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in%  my_data$ENTREZID)
                     my_data2 <- merge(my_data, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     my_data2 <- merge(aaaaaaaa[, c("SYMBOL", "ENTREZID")], my_data, by="ENTREZID",all.x=TRUE)
                     my_data3 <-  my_data2[,c("SYMBOL","ENTREZID","selection_based_simila","selection_based_selection")]
                     
                     ##########################################################################
                     
                     #####################################################################################################
                     my_dataclus  <- my_data2[,c("selection_based_simila","selection_based_selection")]
                     names <- make.unique(as.character(my_data3$SYMBOL))
                     #names <- make.names(as.character(my_data3$SYMBOL), unique=FALSE)
                     row.names(my_dataclus) <- names  
                     ##########################################################################
                     #DT::datatable(my_data2, options = list(scrollX = TRUE))
                     my_data <- my_dataclus %>%
                         na.omit() %>%          # Remove missing values (NA)
                         scale() 
                     # Compute
                     res.hc <- my_data %>%
                         scale() %>%                    # Scale the data
                         dist(method = "euclidean") %>% # Compute dissimilarity matrix
                         hclust(method = "ward.D2")     # Compute hierachical clustering
                     ##############
                     #captionclus = as.double(read_lines( "../GenePPI/captionclus.txt"))
                     
                     clus4 = cutree(res.hc,input$captionclus )
                     testDF <- data.frame(clus4)
                     testVect <- structure(clus4)
                     clusters2 = data.frame(SYMBOL2=names(testVect), clustid=testVect, row.names=NULL)
                     
                     db <- gsub("\\..*","",clusters2$SYMBOL2)
                     db
                     tools::file_path_sans_ext(clusters2$SYMBOL2)
                     clusters <- clusters2 %>% 
                         mutate(SYMBOL = db)
                     
                     mylist<-merge(my_data2, clusters[,c("SYMBOL","clustid")], by="SYMBOL",all.x=TRUE)
                     lll <- split(mylist, as.factor(mylist$clustid))
                     mylist2 <- subset(mylist[,c("SYMBOL","clustid")])
                     mylist22 <- subset(unique(mylist[,c("SYMBOL","clustid")]))
                     # mylist22[order(mylist22$clustid, decreasing = FALSE), ]
                     
                     mydataorderclustid <-mylist22[order(mylist22$clustid,decreasing = TRUE), ]
                     #print(mylist22)
                     #DT::datatable(as.data.frame(mydataorderclustid[1:length(mydataorderclustid$SYMBOL),]), options = list(scrollX = TRUE))
                     DT::datatable(as.data.frame(mydataorderclustid), options = list(scrollX = TRUE))
                     
                     #head(lll)
                     #cd <- enframe(lll)
                     #xxxxx <- cd$value[1:length(cd$name)]
                     #library(kableExtra)
                     #knitr::kable(head(lll, "pipe"))
                 }))
    
    
    observeEvent(input$action2,
                 output$slide21 <- DT::renderDT({
                     DT::datatable(req(data2()), options = list(scrollX = TRUE))
                     #DT::datatable(dplyr::dataset_selected_0_000001, options = list(scrollX = TRUE))
                     
                     
                 }))
    
    observeEvent(input$action2,
                 output$slide211 <- DT::renderDT({
                     DT::datatable(req(data21()), options = list(scrollX = TRUE))
                     
                     #req(data21())
                 }))
    
    observeEvent(input$action2,
                 output$slide212 <- DT::renderDT({
                     DT::datatable(req(data22()), options = list(scrollX = TRUE))
                     
                     #req(data22())
                 }))
    
    observeEvent(input$action2,
                 output$slide213 <- DT::renderDT({
                     
                     DT::datatable(req(data23()), options = list(scrollX = TRUE))
                     
                     #req(data23())
                 }))
    
    
    #######################################################################################################################################
    #######################################################################################################################################
    #actiontis
    #actionvalidclust
    observeEvent(input$actiontis,
                 output$slide222 <- DT::renderDT({
                     
                     
                     #################################################################################################
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     
                     ######################################################################################################3
                     # Loading
                     library("tibble")
                     library(readr)
                     library(tidyverse)
                     #write.csv(gene_PrbId,file="../docglom21/GenePPI/gene_PrbId.csv")
                     #gene_PrbId <- read_csv("../GenePPI/gene_PrbId.csv")
                     
                     #gene_predicted <-subset(input,input$ENTREZID %in%  gene_prediction_Results$X)
                     gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     
                     GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                     #selection_seuil_0_0001 <- read_csv("../docglom21/GenePPI/selection_seuil_0_0001.csv")
                     selection_seuil_0_0001 <- as.data.frame(list(ENTREZID = gene_prediction_Results$ENTREZID,selection_based_simila =gene_prediction_Results$selection_based_simila,selection_based_selection = gene_prediction_Results$selection_based_selection))
                     tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
                     #cl4 <-subset(selection_seuil_0_0001,selection_seuil_0_0001$ENTREZID %in% gene_predicted$ENTREZID)
                     
                     cl4 <- gene_predicted$SYMBOL
                     #######################################################################################
                     #################################Tissus
                     tissues_selected = as.character(input$id_Tissues) 
                     #tissus_all_clus1 <- subset(data2()[,c("SYMBOL",tissues_selected)],data2()[,"SYMBOL"] %in%  cl4)
                     tissus_all_clus1 <- subset(tissus[,c("SYMBOL",tissues_selected)],tissus$SYMBOL %in%  cl4)
                     
                     means_tiss <- tissus_all_clus1 %>%
                         na.omit()
                     
                     tissus_all_clus1$mean <-rowMeans(tissus_all_clus1[,tissues_selected], na.rm=TRUE)
                     ##########################################################################################################
                     #####################################BP         ###############means
                     df_BP_values_na <- data21() %>% na.omit() 
                     df_BP_values_na_sele <- subset(df_BP_values_na,df_BP_values_na$geneSymbol %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     df_BP_values_na_sele_na_norm <- as.data.frame(lapply(df_BP_values_na_sele[,6:7], min_max_norm))
                     df_BP_values_na_sele_norm_df <- df_BP_values_na_sele %>% 
                         mutate(zscore = df_BP_values_na_sele_na_norm)
                     res.by <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, mean)
                     
                     #res.by
                     library(taRifx)
                     vvv <- as.data.frame(res.by)
                     BP_db <- vvv %>% 
                         rename(
                             SYMBOL = IDX1,
                             mean_BP_DF = value )
                     res.by_BP_nbr <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, length)
                     res.by_BP_nbr
                     vvv_res.by_BP_nbr <- as.data.frame(res.by_BP_nbr)
                     
                     names(vvv_res.by_BP_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_BP_nbr)[2] <-"nbr_BP"
                     nbr_BP <- vvv_res.by_BP_nbr
                     
                     nbr_BP_DES_sel <- by(df_BP_values_na_sele_norm_df$Description, df_BP_values_na_sele$geneSymbol, paste)
                     nbr_BP_DES_df <- as.data.frame(nbr_BP_DES_sel)
                     names(nbr_BP_DES_df)[1] <-"SYMBOL"
                     names(nbr_BP_DES_df)[2] <-"DES_BP"
                     
                     nbr_BP_DES <- nbr_BP_DES_df 
                     ##########################################################################################################
                     ggg1_k <-  merge(tissus_all_clus1, BP_db[, c("SYMBOL", "mean_BP_DF")], by="SYMBOL",all.x=TRUE)
                     ggg12_k <-  merge(ggg1_k, nbr_BP[, c("SYMBOL", "nbr_BP")], by="SYMBOL",all.x=TRUE)
                     ggg13_k <-  merge(ggg12_k, nbr_BP_DES[, c("SYMBOL", "DES_BP")], by="SYMBOL",all.x=TRUE)
                     ############################################################################################################
                     ########################################################################
                     ##################################Expression                ####means
                     min_max_norm <- function(x) {
                         #(x - min(x)) / (max(x) - min(x))
                         (x - min(x)) / sd(x)}
                     EXP_norm0 <- as.data.frame(lapply(selection_seuil_0_0001[2:3], min_max_norm))
                     EXP_norm1 <- selection_seuil_0_0001 %>%   mutate(zscore = EXP_norm0$selection_based_selection)
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     EXP_df <- merge(EXP_norm1, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     res.by_EXP <- by(EXP_df$zscore, EXP_df$SYMBOL, mean)
                     
                     
                     vvv_EXP <- as.data.frame(res.by_EXP)
                     
                     names(vvv_EXP)[1] <-"SYMBOL"
                     names(vvv_EXP)[2] <-"mean_EXP_df"
                     
                     EXP_db <- vvv_EXP 
                     
                     
                     ##########################################################################################################
                     ggg2_k <-  merge(ggg13_k, EXP_db[, c("SYMBOL", "mean_EXP_df")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     TFs <-  vroom::vroom("../GenePPI/input/TFs.csv",  locale = vroom::locale(encoding = "CP1252"))
                     ####################TFs
                     ghgwas = as.data.frame(list(SYMBOL= TFs$SYMBOL,
                                                 TF= TFs$TF,
                                                 Zscore= TFs$`Z-score`,
                                                 Fisher_score= TFs$`Fisher score `))
                     
                     TFs_na <- ghgwas %>% na.omit() 
                     TFs_na_sele <- subset(TFs_na,TFs_na$SYMBOL %in% cl4)
                     res.by_TFs <- by(TFs_na_sele$Fisher_score, TFs_na_sele$SYMBOL, mean)
                     res.by_TFs
                     vvv_TFs <- as.data.frame(res.by_TFs)
                     
                     names(vvv_TFs)[1] <-"SYMBOL"
                     names(vvv_TFs)[2] <-"mean_TFs_df"
                     EXP_TFs <- vvv_TFs
                     
                     res.by_TFs_nbr <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, length)
                     vvv_res.by_TFs_nbr <- as.data.frame(res.by_TFs_nbr)
                     
                     
                     names(vvv_res.by_TFs_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_TFs_nbr)[2] <-"nbr_TFs"
                     
                     nbr_TFs <- vvv_res.by_TFs_nbr
                     nbr_TFs_DES_sel <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, paste)
                     nbr_TFs_DES_df <- as.data.frame(nbr_TFs_DES_sel)
                     names(nbr_TFs_DES_df)[1] <-"SYMBOL"
                     names(nbr_TFs_DES_df)[2] <-"DES_TFs"
                     
                     nbr_TFs_DES <- nbr_TFs_DES_df 
                     ##########################################################################################################
                     ggg3_k <-  merge(ggg2_k, EXP_TFs[, c("SYMBOL", "mean_TFs_df")], by="SYMBOL",all.x=TRUE)
                     ggg31_k <-  merge(ggg3_k, nbr_TFs[, c("SYMBOL", "nbr_TFs")], by="SYMBOL",all.x=TRUE)
                     ggg32_k <-  merge(ggg31_k, nbr_TFs_DES[, c("SYMBOL", "DES_TFs")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ########################################################################
                     #########################GWAS
                     selection_seuil_GWAs_na <- GWAS %>% na.omit() 
                     GWAs_na_sele <- subset(selection_seuil_GWAs_na,selection_seuil_GWAs_na$GeneID %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     selection_seuil_GWAs_na_norm <- as.data.frame(lapply(GWAs_na_sele[,10], min_max_norm))
                     selection_seuil_GWAs_na_norm_df <- GWAs_na_sele %>% 
                         mutate(zscore = selection_seuil_GWAs_na_norm)
                     
                     mean_norm_GWAS <- mean(selection_seuil_GWAs_na_norm_df$zscore$P)
                     
                     res.by_GWAS <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$GeneID, mean)
                     res.by_GWAS
                     
                     vvv_GWAS <- as.data.frame(res.by_GWAS)
                     names(vvv_GWAS)[1] <-"SYMBOL"
                     names(vvv_GWAS)[2] <-"mean_GWAS_df"
                     
                     EXP_GWAS <- vvv_GWAS
                     
                     res.by_GWAS_nbr <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$GeneID, length)
                     res.by_GWAS_nbr
                     vvv_res.by_GWAS_nbr <- as.data.frame(res.by_GWAS_nbr)
                     names(vvv_res.by_GWAS_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_GWAS_nbr)[2] <-"nbr_GWAS"
                     
                     nbr_GWAS <- vvv_res.by_GWAS_nbr 
                     nbr_GWAS_DES_sel <- by(selection_seuil_GWAs_na_norm_df$`Trait`, selection_seuil_GWAs_na_norm_df$GeneID, paste)
                     nbr_GWAS_DES_df <- as.data.frame(nbr_GWAS_DES_sel)
                     names(nbr_GWAS_DES_df)[1] <-"SYMBOL"
                     names(nbr_GWAS_DES_df)[2] <-"DES_GWAS"
                     
                     nbr_GWAS_DES <- nbr_GWAS_DES_df 
                     ##########################################################################################################
                     ggg4_k <-  merge(ggg32_k, EXP_GWAS[, c("SYMBOL", "mean_GWAS_df")], by="SYMBOL",all.x=TRUE)
                     ggg41_k <-  merge(ggg4_k, nbr_GWAS[, c("SYMBOL", "nbr_GWAS")], by="SYMBOL",all.x=TRUE)
                     ggg42_k <-  merge(ggg41_k, nbr_GWAS_DES[, c("SYMBOL", "DES_GWAS")], by="SYMBOL",all.x=TRUE)
                     #View(ggg42)
                     #########################################################################
                     ########################################################################
                     ##########################################function()
                     #prior = (result.mean_tissus + result.mean_expr)/2 + 
                     #ggg42[,c(6:8,10:13,15:16)][is.na(ggg42[,c(6:8,10:13,15:16)])] = 0
                     ggg42_k[is.na(ggg42_k)] = 0
                     ggg42fin_kidney_cl4 <- ggg42_k %>% rowwise() %>% mutate(Priori = ((mean+mean_EXP_df)/2) + (mean_BP_DF*nbr_BP) + (mean_TFs_df*nbr_TFs) + (mean_GWAS_df*nbr_GWAS))
                     #View(ggg42fin_kidney)
                     ###write.csv(as.matrix(ggg42fin_kidney_cl4), "../docglom21/GenePPI/ggg42fin_kidney_cl4.csv")#, row.names=FALSE
                     #write.xlsx(ggg42fin_kidney_cl4, "../docglom21/GenePPI/ggg42fin_kidney_cl4.xlsx")
                     #print(ggg42fin_kidney_cl4)
                     priori_scores <- ggg42fin_kidney_cl4[order(ggg42fin_kidney_cl4$Priori, decreasing = TRUE), ]
                     #DT::datatable(dplyr::printer, options = list(scrollX = TRUE))
                     
                     subset_priorized_genes <- subset(priori_scores[,c("SYMBOL","Priori")])
                     #DT::datatable(priori_scores, options = list(scrollX = TRUE))
                     #print(printer)
                     library(readr)
                     library(tidyr)
                     library("cluster")
                     library("factoextra")
                     library("magrittr")
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     my_data <- gene_prediction_Results
                     
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in%  my_data$ENTREZID)
                     my_data2 <- merge(my_data, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     my_data2 <- merge(aaaaaaaa[, c("SYMBOL", "ENTREZID")], my_data, by="ENTREZID",all.x=TRUE)
                     my_data3 <-  my_data2[,c("SYMBOL","ENTREZID","selection_based_simila","selection_based_selection")]
                     
                     ##########################################################################
                     
                     #####################################################################################################
                     my_dataclus  <- my_data2[,c("selection_based_simila","selection_based_selection")]
                     names <- make.unique(as.character(my_data3$SYMBOL))
                     #names <- make.names(as.character(my_data3$SYMBOL), unique=FALSE)
                     row.names(my_dataclus) <- names  
                     ##########################################################################
                     #DT::datatable(my_data2, options = list(scrollX = TRUE))
                     my_data <- my_dataclus %>%
                         na.omit() %>%          # Remove missing values (NA)
                         scale() 
                     # Compute
                     res.hc <- my_data %>%
                         scale() %>%                    # Scale the data
                         dist(method = "euclidean") %>% # Compute dissimilarity matrix
                         hclust(method = "ward.D2")     # Compute hierachical clustering
                     ##############
                     #captionclus = as.double(read_lines( "../docglom21/GenePPI/captionclus.txt"))
                     
                     clus4 = cutree(res.hc, input$captionclus)
                     testDF <- data.frame(clus4)
                     testVect <- structure(clus4)
                     clusters2 = data.frame(SYMBOL2=names(testVect), clustid=testVect, row.names=NULL)
                     
                     db <- gsub("\\..*","",clusters2$SYMBOL2)
                     db
                     tools::file_path_sans_ext(clusters2$SYMBOL2)
                     clusters <- clusters2 %>% 
                         mutate(SYMBOL = db)
                     
                     mylist<-merge(my_data2, clusters[,c("SYMBOL","clustid")], by="SYMBOL",all.x=TRUE)
                     lll <- split(mylist, as.factor(mylist$clustid))
                     mylist2 <- subset(mylist[,c("SYMBOL","clustid")])
                     mylist22 <- subset(unique(mylist[,c("SYMBOL","clustid")]))
                     # mylist22[order(mylist22$clustid, decreasing = FALSE), ]
                     
                     mydataorderclustid <-mylist22[order(mylist22$clustid,decreasing = TRUE), ]
                     #DT::datatable(as.data.frame(mydataorderclustid), options = list(scrollX = TRUE))
                     #library(readr)
                     #subset_priorized_genes <- read_csv("../docglom21/GenePPI/subset_priorized_genes.csv")
                     
                     gene_int_clus <- merge(subset_priorized_genes, mydataorderclustid[, c("SYMBOL", "clustid")], by="SYMBOL",all.x=TRUE)
                     #gene_int_clus[order(gene_int_clus$clustid,gene_int_clus$Priori,decreasing = TRUE), ]
                     gene_int_clus_ord <- gene_int_clus[order(gene_int_clus$clustid,-gene_int_clus$Priori), ]
                     DT::datatable(as.data.frame(gene_int_clus_ord), options = list(scrollX = TRUE))
                     
                     
                 }))
    
    ###################################################################################################################################
    observeEvent(input$actiontis,
                 output$slide22 <- DT::renderDT({
                     ##############################################################################
                     
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     
                     ###############################################################################
                     # Loading
                     library("tibble")
                     library(readr)
                     library(tidyverse)
                     #write.csv(gene_PrbId,file="../docglom21/GenePPI/gene_PrbId.csv")
                     
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     #gene_predicted <-subset(input,input$ENTREZID %in%  gene_prediction_Results$X)
                     gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     
                     GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                     #selection_seuil_0_0001 <- read_csv("../docglom21/GenePPI/selection_seuil_0_0001.csv")
                     selection_seuil_0_0001 <- as.data.frame(list(ENTREZID = gene_prediction_Results$ENTREZID,selection_based_simila =gene_prediction_Results$selection_based_simila,selection_based_selection = gene_prediction_Results$selection_based_selection))
                     tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
                     #cl4 <-subset(selection_seuil_0_0001,selection_seuil_0_0001$ENTREZID %in% gene_predicted$ENTREZID)
                     
                     cl4 <- gene_predicted$SYMBOL
                     #######################################################################################
                     #################################Tissus
                     tissues_selected = input$id_Tissues
                     #read_csv( "../docglom21/GenePPI/id_Tissues.csv",show_col_types = FALSE)
                     #tissus_all_clus1 <- subset(data2()[,c("SYMBOL",tissues_selected)],data2()[,"SYMBOL"] %in%  cl4)
                     
                     tissus_all_clus1 <- subset(tissus[,c("SYMBOL",tissues_selected)],tissus$SYMBOL %in%  cl4)
                     #write.csv(tissus_all_clus1,file="../docglom21/Priorization/tissus_all_clus1.csv")
                     #tissus_all_clus1[is.na(tissus_all_clus1)] = 0
                     
                     #means_tiss$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #tissus_all_clus1$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #colMeans(df, na.rm=TRUE)
                     means_tiss <- tissus_all_clus1 %>%
                         na.omit()
                     
                     tissus_all_clus1$mean <-rowMeans(tissus_all_clus1[,tissues_selected], na.rm=TRUE)
                     #tissus_all_clus1
                     #tissus_all_clus1[,2:3]
                     #df = subset(nbr_Tiss, select = -IDX1.1 )
                     ##########################################################################################################
                     #####################################BP         ###############means
                     df_BP_values_na <- data21() %>% na.omit() 
                     df_BP_values_na_sele <- subset(df_BP_values_na,df_BP_values_na$geneSymbol %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     df_BP_values_na_sele_na_norm <- as.data.frame(lapply(df_BP_values_na_sele[,6:7], min_max_norm))
                     df_BP_values_na_sele_norm_df <- df_BP_values_na_sele %>% 
                         mutate(zscore = df_BP_values_na_sele_na_norm)
                     res.by <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, mean)
                     #res.by
                     vvv <- as.data.frame(res.by)
                     names(vvv)[1] <-"SYMBOL"
                     names(vvv)[2] <-"mean_BP_DF"
                     BP_db <- vvv
                     
                     res.by_BP_nbr <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, length)
                     res.by_BP_nbr
                     vvv_res.by_BP_nbr <- as.data.frame(res.by_BP_nbr)
                     
                     names(vvv_res.by_BP_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_BP_nbr)[2] <-"nbr_BP"
                     nbr_BP <- vvv_res.by_BP_nbr 
                     
                     nbr_BP_DES_sel <- by(df_BP_values_na_sele_norm_df$Description, df_BP_values_na_sele$geneSymbol, paste)
                     nbr_BP_DES_df <- as.data.frame(nbr_BP_DES_sel)
                     names(nbr_BP_DES_df)[1] <-"SYMBOL"
                     names(nbr_BP_DES_df)[2] <-"DES_BP"
                     
                     nbr_BP_DES <- nbr_BP_DES_df 
                     ##########################################################################################################
                     ggg1_k <-  merge(tissus_all_clus1, BP_db[, c("SYMBOL", "mean_BP_DF")], by="SYMBOL",all.x=TRUE)
                     ggg12_k <-  merge(ggg1_k, nbr_BP[, c("SYMBOL", "nbr_BP")], by="SYMBOL",all.x=TRUE)
                     ggg13_k <-  merge(ggg12_k, nbr_BP_DES[, c("SYMBOL", "DES_BP")], by="SYMBOL",all.x=TRUE)
                     ############################################################################################################
                     ########################################################################
                     ##################################Expression                ####means
                     min_max_norm <- function(x) {
                         #(x - min(x)) / (max(x) - min(x))
                         (x - min(x)) / sd(x)}
                     EXP_norm0 <- as.data.frame(lapply(selection_seuil_0_0001[2:3], min_max_norm))
                     EXP_norm1 <- selection_seuil_0_0001 %>%   mutate(zscore = EXP_norm0$selection_based_selection)
                     #EXP_df <- EXP_norm1 %>%   mutate(SYMBOL = selection_seuil_0_0001_norm1$SYMBOL)
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     EXP_df <- merge(EXP_norm1, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     res.by_EXP <- by(EXP_df$zscore, EXP_df$SYMBOL, mean)
                     vvv_EXP <- as.data.frame(res.by_EXP)
                     names(vvv_EXP)[1] <-"SYMBOL"
                     names(vvv_EXP)[2] <-"mean_EXP_df"
                     
                     EXP_db <- vvv_EXP
                     
                     
                     ##########################################################################################################
                     ggg2_k <-  merge(ggg13_k, EXP_db[, c("SYMBOL", "mean_EXP_df")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ####################TFs
                     TFs <-  vroom::vroom("../GenePPI/input/TFs.csv",  locale = vroom::locale(encoding = "CP1252"))
                     
                     ghgwas = as.data.frame(list(SYMBOL= TFs$SYMBOL,
                                                 TF= TFs$TF,
                                                 Zscore= TFs$`Z-score`,
                                                 Fisher_score= TFs$`Fisher score `))
                     
                     TFs_na <- ghgwas %>% na.omit() 
                     TFs_na_sele <- subset(TFs_na,TFs_na$SYMBOL %in% cl4)
                     res.by_TFs <- by(TFs_na_sele$Fisher_score, TFs_na_sele$SYMBOL, mean)
                     res.by_TFs
                     vvv_TFs <- as.data.frame(res.by_TFs)
                     names(vvv_TFs)[1] <-"SYMBOL"
                     names(vvv_TFs)[2] <-"mean_TFs_df"
                     EXP_TFs <- vvv_TFs
                     res.by_TFs_nbr <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, length)
                     vvv_res.by_TFs_nbr <- as.data.frame(res.by_TFs_nbr)
                     
                     
                     names(vvv_res.by_TFs_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_TFs_nbr)[2] <-"nbr_TFs"
                     
                     nbr_TFs <- vvv_res.by_TFs_nbr
                     
                     nbr_TFs_DES_sel <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, paste)
                     nbr_TFs_DES_df <- as.data.frame(nbr_TFs_DES_sel)
                     names(nbr_TFs_DES_df)[1] <-"SYMBOL"
                     names(nbr_TFs_DES_df)[2] <-"DES_TFs"
                     
                     nbr_TFs_DES <- nbr_TFs_DES_df 
                     ##########################################################################################################
                     ggg3_k <-  merge(ggg2_k, EXP_TFs[, c("SYMBOL", "mean_TFs_df")], by="SYMBOL",all.x=TRUE)
                     ggg31_k <-  merge(ggg3_k, nbr_TFs[, c("SYMBOL", "nbr_TFs")], by="SYMBOL",all.x=TRUE)
                     ggg32_k <-  merge(ggg31_k, nbr_TFs_DES[, c("SYMBOL", "DES_TFs")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ########################################################################
                     #########################GWAS
                     selection_seuil_GWAs_na <- GWAS %>% na.omit() 
                     GWAs_na_sele <- subset(selection_seuil_GWAs_na,selection_seuil_GWAs_na$`GeneID` %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     selection_seuil_GWAs_na_norm <- as.data.frame(lapply(GWAs_na_sele[,10], min_max_norm))
                     selection_seuil_GWAs_na_norm_df <- GWAs_na_sele %>% 
                         mutate(zscore = selection_seuil_GWAs_na_norm)
                     
                     #mean_norm_GWAS <- mean(selection_seuil_GWAs_na_norm_df$zscore$P)
                     
                     res.by_GWAS <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$GeneID, mean)
                     res.by_GWAS
                     library(taRifx)
                     vvv_GWAS <- as.data.frame(res.by_GWAS)
                     names(vvv_GWAS)[1] <-"SYMBOL"
                     names(vvv_GWAS)[2] <-"mean_GWAS_df"
                     
                     EXP_GWAS <- vvv_GWAS 
                     
                     res.by_GWAS_nbr <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, length)
                     res.by_GWAS_nbr
                     vvv_res.by_GWAS_nbr <- as.data.frame(res.by_GWAS_nbr)
                     names(vvv_res.by_GWAS_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_GWAS_nbr)[2] <-"nbr_GWAS"
                     
                     nbr_GWAS <- vvv_res.by_GWAS_nbr 
                     nbr_GWAS_DES_sel <- by(selection_seuil_GWAs_na_norm_df$`Trait`, selection_seuil_GWAs_na_norm_df$`GeneID`, paste)
                     nbr_GWAS_DES_df <- as.data.frame(nbr_GWAS_DES_sel)
                     names(nbr_GWAS_DES_df)[1] <-"SYMBOL"
                     names(nbr_GWAS_DES_df)[2] <-"DES_GWAS"
                     
                     nbr_GWAS_DES <- nbr_GWAS_DES_df 
                     ##########################################################################################################
                     ggg4_k <-  merge(ggg32_k, EXP_GWAS[, c("SYMBOL", "mean_GWAS_df")], by="SYMBOL",all.x=TRUE)
                     ggg41_k <-  merge(ggg4_k, nbr_GWAS[, c("SYMBOL", "nbr_GWAS")], by="SYMBOL",all.x=TRUE)
                     ggg42_k <-  merge(ggg41_k, nbr_GWAS_DES[, c("SYMBOL", "DES_GWAS")], by="SYMBOL",all.x=TRUE)
                     #View(ggg42)
                     #########################################################################
                     ########################################################################
                     ##########################################function()
                     #prior = (result.mean_tissus + result.mean_expr)/2 + 
                     #ggg42[,c(6:8,10:13,15:16)][is.na(ggg42[,c(6:8,10:13,15:16)])] = 0
                     ggg42_k[is.na(ggg42_k)] = 0
                     ggg42fin_kidney_cl4 <- ggg42_k %>% rowwise() %>% mutate(Priori = ((mean+mean_EXP_df)/2) + (mean_BP_DF*nbr_BP) + (mean_TFs_df*nbr_TFs) + (mean_GWAS_df*nbr_GWAS))
                     #View(ggg42fin_kidney)
                     ###write.csv(as.matrix(ggg42fin_kidney_cl4), "../docglom21/GenePPI/ggg42fin_kidney_cl4.csv")#, row.names=FALSE
                     #write.xlsx(ggg42fin_kidney_cl4, "../docglom21/GenePPI/ggg42fin_kidney_cl4.xlsx")
                     #print(ggg42fin_kidney_cl4)
                     priori_scores <- ggg42fin_kidney_cl4[order(ggg42fin_kidney_cl4$Priori, decreasing = TRUE), ]
                     #DT::datatable(dplyr::printer, options = list(scrollX = TRUE))
                     
                     subset_priorized_genes <- subset(priori_scores[,c("SYMBOL","Priori")])
                     
                     #write.csv(subset_priorized_genes, "../docglom21/GenePPI/subset_priorized_genes.csv", row.names=FALSE)#, row.names=FALSE
                     DT::datatable(priori_scores, options = list(scrollX = TRUE))
                     #print(printer)
                     
                 }))
    #######################################################################################################################################
    #######################################################################################################################################
    observeEvent(input$actiontis,
                 output$plot24 <- renderPlot({ 
                     
                     
                     #################################################################################
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     
                     ####################################################################################
                     # Loading
                     library("tibble")
                     library(readr)
                     library(tidyverse)
                     #write.csv(gene_PrbId,file="../GenePPI/gene_PrbId.csv")
                     
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     #gene_predicted <-subset(input,input$ENTREZID %in%  gene_prediction_Results$ENTREZID)
                     gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                     
                     #selection_seuil_0_0001 <- read_csv("../GenePPI/selection_seuil_0_0001.csv")
                     selection_seuil_0_0001 <- as.data.frame(list(ENTREZID = gene_prediction_Results$ENTREZID,selection_based_simila =gene_prediction_Results$selection_based_simila,selection_based_selection = gene_prediction_Results$selection_based_selection))
                     
                     tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
                     #cl4 <-subset(selection_seuil_0_0001,selection_seuil_0_0001$ENTREZID %in% gene_predicted$ENTREZID)
                     
                     cl4 <- gene_predicted$SYMBOL
                     #######################################################################################
                     #################################Tissus
                     tissues_selected = input$id_Tissues
                     #read_csv( "../GenePPI/id_Tissues.csv",show_col_types = FALSE)
                     #tissus_all_clus1 <- subset(data2()[,c("SYMBOL",tissues_selected)],data2()[,"SYMBOL"] %in%  cl4)
                     
                     tissus_all_clus1 <- subset(tissus[,c("SYMBOL",tissues_selected)],tissus$SYMBOL %in%  cl4)
                     #write.csv(tissus_all_clus1,file="../docglom21/Priorization/tissus_all_clus1.csv")
                     #tissus_all_clus1[is.na(tissus_all_clus1)] = 0
                     
                     #means_tiss$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #tissus_all_clus1$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #colMeans(df, na.rm=TRUE)
                     means_tiss <- tissus_all_clus1 %>%
                         na.omit()
                     
                     tissus_all_clus1$mean <-rowMeans(tissus_all_clus1[,tissues_selected], na.rm=TRUE)
                     #tissus_all_clus1
                     #tissus_all_clus1[,2:3]
                     #df = subset(nbr_Tiss, select = -IDX1.1 )
                     ##########################################################################################################
                     #####################################BP         ###############means
                     df_BP_values_na <- data21() %>% na.omit() 
                     df_BP_values_na_sele <- subset(df_BP_values_na,df_BP_values_na$geneSymbol %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     df_BP_values_na_sele_na_norm <- as.data.frame(lapply(df_BP_values_na_sele[,6:7], min_max_norm))
                     df_BP_values_na_sele_norm_df <- df_BP_values_na_sele %>% 
                         mutate(zscore = df_BP_values_na_sele_na_norm)
                     res.by <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, mean)
                     #res.by
                     vvv <- as.data.frame(res.by)
                     names(vvv)[1] <-"SYMBOL"
                     names(vvv)[2] <-"mean_BP_DF"
                     BP_db <- vvv
                     res.by_BP_nbr <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, length)
                     res.by_BP_nbr
                     vvv_res.by_BP_nbr <- as.data.frame(res.by_BP_nbr)
                     names(vvv_res.by_BP_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_BP_nbr)[2] <-"nbr_BP"
                     
                     nbr_BP <- vvv_res.by_BP_nbr
                     
                     nbr_BP_DES_sel <- by(df_BP_values_na_sele_norm_df$Description, df_BP_values_na_sele$geneSymbol, paste)
                     nbr_BP_DES_df <- as.data.frame(nbr_BP_DES_sel)
                     names(nbr_BP_DES_df)[1] <-"SYMBOL"
                     names(nbr_BP_DES_df)[2] <-"DES_BP"
                     
                     nbr_BP_DES <- nbr_BP_DES_df 
                     ##########################################################################################################
                     ggg1_k <-  merge(tissus_all_clus1, BP_db[, c("SYMBOL", "mean_BP_DF")], by="SYMBOL",all.x=TRUE)
                     ggg12_k <-  merge(ggg1_k, nbr_BP[, c("SYMBOL", "nbr_BP")], by="SYMBOL",all.x=TRUE)
                     ggg13_k <-  merge(ggg12_k, nbr_BP_DES[, c("SYMBOL", "DES_BP")], by="SYMBOL",all.x=TRUE)
                     ############################################################################################################
                     ########################################################################
                     ##################################Expression                ####means
                     min_max_norm <- function(x) {
                         #(x - min(x)) / (max(x) - min(x))
                         (x - min(x)) / sd(x)}
                     EXP_norm0 <- as.data.frame(lapply(selection_seuil_0_0001[2:3], min_max_norm))
                     EXP_norm1 <- selection_seuil_0_0001 %>%   mutate(zscore = EXP_norm0$selection_based_selection)
                     #EXP_df <- EXP_norm1 %>%   mutate(SYMBOL = selection_seuil_0_0001_norm1$SYMBOL)
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     EXP_df <- merge(EXP_norm1, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     res.by_EXP <- by(EXP_df$zscore, EXP_df$SYMBOL, mean)
                     vvv_EXP <- as.data.frame(res.by_EXP)
                     names(vvv_EXP)[1] <-"SYMBOL"
                     names(vvv_EXP)[2] <-"mean_EXP_df"
                     
                     EXP_db <- vvv_EXP
                     
                     
                     ##########################################################################################################
                     ggg2_k <-  merge(ggg13_k, EXP_db[, c("SYMBOL", "mean_EXP_df")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ####################TFs
                     TFs <-  vroom::vroom("../GenePPI/input/TFs.csv",  locale = vroom::locale(encoding = "CP1252"))
                     
                     ghgwas = as.data.frame(list(SYMBOL= TFs$SYMBOL,
                                                 TF= TFs$TF,
                                                 Zscore= TFs$`Z-score`,
                                                 Fisher_score= TFs$`Fisher score `))
                     
                     TFs_na <- ghgwas %>% na.omit() 
                     TFs_na_sele <- subset(TFs_na,TFs_na$SYMBOL %in% cl4)
                     res.by_TFs <- by(TFs_na_sele$Fisher_score, TFs_na_sele$SYMBOL, mean)
                     res.by_TFs
                     
                     vvv_TFs <- as.data.frame(res.by_TFs)
                     names(vvv_TFs)[1] <-"SYMBOL"
                     names(vvv_TFs)[2] <-"mean_TFs_df"
                     EXP_TFs <- vvv_TFs
                     res.by_TFs_nbr <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, length)
                     vvv_res.by_TFs_nbr <- as.data.frame(res.by_TFs_nbr)
                     
                     
                     names(vvv_res.by_TFs_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_TFs_nbr)[2] <-"nbr_TFs"
                     
                     nbr_TFs <- vvv_res.by_TFs_nbr
                     
                     nbr_TFs_DES_sel <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, paste)
                     nbr_TFs_DES_df <- as.data.frame(nbr_TFs_DES_sel)
                     
                     names(nbr_TFs_DES_df)[1] <-"SYMBOL"
                     names(nbr_TFs_DES_df)[2] <-"DES_TFs"
                     
                     nbr_TFs_DES <- nbr_TFs_DES_df 
                     ##########################################################################################################
                     ggg3_k <-  merge(ggg2_k, EXP_TFs[, c("SYMBOL", "mean_TFs_df")], by="SYMBOL",all.x=TRUE)
                     ggg31_k <-  merge(ggg3_k, nbr_TFs[, c("SYMBOL", "nbr_TFs")], by="SYMBOL",all.x=TRUE)
                     ggg32_k <-  merge(ggg31_k, nbr_TFs_DES[, c("SYMBOL", "DES_TFs")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ########################################################################
                     #########################GWAS
                     GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                     
                     selection_seuil_GWAs_na <- GWAS %>% na.omit() 
                     GWAs_na_sele <- subset(selection_seuil_GWAs_na,selection_seuil_GWAs_na$`GeneID` %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     selection_seuil_GWAs_na_norm <- as.data.frame(lapply(GWAs_na_sele[,10], min_max_norm))
                     selection_seuil_GWAs_na_norm_df <- GWAs_na_sele %>% 
                         mutate(zscore = selection_seuil_GWAs_na_norm)
                     
                     mean_norm_GWAS <- mean(selection_seuil_GWAs_na_norm_df$zscore$P)
                     
                     res.by_GWAS <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, mean)
                     res.by_GWAS
                     library(taRifx)
                     vvv_GWAS <- as.data.frame(res.by_GWAS)
                     names(vvv_GWAS)[1] <-"SYMBOL"
                     names(vvv_GWAS)[2] <-"mean_GWAS_df"
                     
                     EXP_GWAS <- vvv_GWAS 
                     
                     res.by_GWAS_nbr <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, length)
                     res.by_GWAS_nbr
                     vvv_res.by_GWAS_nbr <- as.data.frame(res.by_GWAS_nbr)
                     names(vvv_res.by_GWAS_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_GWAS_nbr)[2] <-"nbr_GWAS"
                     
                     nbr_GWAS <- vvv_res.by_GWAS_nbr 
                     nbr_GWAS_DES_sel <- by(selection_seuil_GWAs_na_norm_df$`Trait`, selection_seuil_GWAs_na_norm_df$`GeneID`, paste)
                     nbr_GWAS_DES_df <- as.data.frame(nbr_GWAS_DES_sel)
                     names(nbr_GWAS_DES_df)[1] <-"SYMBOL"
                     names(nbr_GWAS_DES_df)[2] <-"DES_GWAS"
                     nbr_GWAS_DES <- nbr_GWAS_DES_df
                     ##########################################################################################################
                     ggg4_k <-  merge(ggg32_k, EXP_GWAS[, c("SYMBOL", "mean_GWAS_df")], by="SYMBOL",all.x=TRUE)
                     ggg41_k <-  merge(ggg4_k, nbr_GWAS[, c("SYMBOL", "nbr_GWAS")], by="SYMBOL",all.x=TRUE)
                     ggg42_k <-  merge(ggg41_k, nbr_GWAS_DES[, c("SYMBOL", "DES_GWAS")], by="SYMBOL",all.x=TRUE)
                     #View(ggg42)
                     #########################################################################
                     ########################################################################
                     ##########################################function()
                     #prior = (result.mean_tissus + result.mean_expr)/2 + 
                     #ggg42[,c(6:8,10:13,15:16)][is.na(ggg42[,c(6:8,10:13,15:16)])] = 0
                     ggg42_k[is.na(ggg42_k)] = 0
                     ggg42fin_kidney_cl4 <- ggg42_k %>% rowwise() %>% mutate(Priori = ((mean+mean_EXP_df)/2) + (mean_BP_DF*nbr_BP) + (mean_TFs_df*nbr_TFs) + (mean_GWAS_df*nbr_GWAS))
                     #View(ggg42fin_kidney)
                     ###write.csv(as.matrix(ggg42fin_kidney_cl4), "../GenePPI/ggg42fin_kidney_cl4.csv")#, row.names=FALSE
                     #write.xlsx(ggg42fin_kidney_cl4, "../GenePPI/ggg42fin_kidney_cl4.xlsx")
                     #print(ggg42fin_kidney_cl4)
                     priori_scores <- ggg42fin_kidney_cl4[order(ggg42fin_kidney_cl4$Priori, decreasing = TRUE), ]
                     #DT::datatable(dplyr::printer, options = list(scrollX = TRUE))
                     
                     subset_priorized_genes <- subset(priori_scores[,c("SYMBOL","Priori")])
                     #DT::datatable(priori_scores, options = list(scrollX = TRUE))
                     #print(printer)
                     library(readr)
                     library(tidyr)
                     library("cluster")
                     library("factoextra")
                     library("magrittr")
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     my_data <- gene_prediction_Results
                     #names(my_data)[1] <-"ENTREZID"
                     
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in%  my_data$ENTREZID)
                     my_data2 <- merge(my_data, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     my_data2 <- merge(aaaaaaaa[, c("SYMBOL", "ENTREZID")], my_data, by="ENTREZID",all.x=TRUE)
                     my_data3 <-  my_data2[,c("SYMBOL","ENTREZID","selection_based_simila","selection_based_selection")]
                     
                     ##########################################################################
                     
                     #####################################################################################################
                     my_dataclus  <- my_data2[,c("selection_based_simila","selection_based_selection")]
                     names <- make.unique(as.character(my_data3$SYMBOL))
                     #names <- make.names(as.character(my_data3$SYMBOL), unique=FALSE)
                     row.names(my_dataclus) <- names  
                     ##########################################################################
                     #DT::datatable(my_data2, options = list(scrollX = TRUE))
                     my_data <- my_dataclus %>%
                         na.omit() %>%          # Remove missing values (NA)
                         scale() 
                     # Compute
                     res.hc <- my_data %>%
                         scale() %>%                    # Scale the data
                         dist(method = "euclidean") %>% # Compute dissimilarity matrix
                         hclust(method = "ward.D2")     # Compute hierachical clustering
                     ##############
                     #captionclus = as.double(read_lines( "../GenePPI/captionclus.txt"))
                     
                     clus4 = cutree(res.hc, input$captionclus)
                     testDF <- data.frame(clus4)
                     testVect <- structure(clus4)
                     clusters2 = data.frame(SYMBOL2=names(testVect), clustid=testVect, row.names=NULL)
                     
                     db <- gsub("\\..*","",clusters2$SYMBOL2)
                     db
                     tools::file_path_sans_ext(clusters2$SYMBOL2)
                     clusters <- clusters2 %>% 
                         mutate(SYMBOL = db)
                     
                     mylist<-merge(my_data2, clusters[,c("SYMBOL","clustid")], by="SYMBOL",all.x=TRUE)
                     lll <- split(mylist, as.factor(mylist$clustid))
                     mylist2 <- subset(mylist[,c("SYMBOL","clustid")])
                     mylist22 <- subset(unique(mylist[,c("SYMBOL","clustid")]))
                     # mylist22[order(mylist22$clustid, decreasing = FALSE), ]
                     
                     mydataorderclustid <-mylist22[order(mylist22$clustid,decreasing = TRUE), ]
                     #DT::datatable(as.data.frame(mydataorderclustid), options = list(scrollX = TRUE))
                     #library(readr)
                     #subset_priorized_genes <- read_csv("../GenePPI/subset_priorized_genes.csv")
                     
                     gene_int_clus <- merge(subset_priorized_genes, mydataorderclustid[, c("SYMBOL", "clustid")], by="SYMBOL",all.x=TRUE)
                     #gene_int_clus[order(gene_int_clus$clustid,gene_int_clus$Priori,decreasing = TRUE), ]
                     gene_int_clus_ord <- gene_int_clus[order(gene_int_clus$clustid,-gene_int_clus$Priori), ]
                     ##################
                     library(ggplot2)
                     
                     #tissues_selected = read_csv( "../GenePPI/id_Tissues.csv",show_col_types = FALSE)
                     
                     
                     #ggplot(priori_scores, aes(x =Priori , y = mean, colour=factor(tissues_selected$x[1]))) +
                     #  geom_point() +
                     #  stat_smooth(method = "loess", se = F) + 
                     # stat_smooth(aes(group = 1), method = "lm", se = F)
                     subset_priorized_genes_mean <- subset(priori_scores[,c("SYMBOL","mean","Priori")])
                     gene_int_clus_mean <- merge(subset_priorized_genes_mean, mydataorderclustid[, c("SYMBOL", "clustid")], by="SYMBOL",all.x=TRUE)
                     
                     
                     
                     ggplot(gene_int_clus_mean, aes(x =Priori , y = mean, colour=factor(clustid))) +
                         geom_point() +
                         geom_smooth(se = F, span = 0.7)
                     
                     
                     
                 }))
    
    
    
    output$plot23<- renderPlot({ 
        
        #######################################################################################
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        library(dplyr)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
        
        gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
        
        
        
        
        #########################################################################################
        library(readr)
        library(ggplot2)
        #library(AnnotationDbi)
        #library(hgu133a.db)
        
        #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
        #c3g_genes_entrezid = c("8728" ,"718","719","731","4179","629" ,"1675","3075","3078" ,"3080" ,"10878", "81494", "3426" , "1378") 
        library(clusterProfiler)
        library(enrichplot)
        enrichGO <- enrichGO(gene_prediction_Results$ENTREZID, 
                             OrgDb = "org.Hs.eg.db",
                             ont="BP",##all
                             readable=TRUE)
        
        #write.csv(ego@result[["Description"]], "../GenePPI/ego_Description.csv", row.names=FALSE)
        #dotplot(ego)
        goplot(enrichGO)
    })
    
    output$slide23 <- DT::renderDT({
        #######################################################################
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        library(dplyr)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
        
        gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
        
        
        
        ######################################################################
        library(clusterProfiler)
        library(enrichplot)
        #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
        
        enrichGO <- enrichGO(gene_prediction_Results$ENTREZID, 
                             OrgDb = "org.Hs.eg.db",
                             ont="BP",##all
                             readable=TRUE)
        DT::datatable(as.data.frame(enrichGO@result[["Description"]]), options = list(scrollX = TRUE))
        
        
        #print(ego_Description_Results)
    })
    
    
    output$plot233<- renderPlot({ 
        ###############################################################################
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        library(dplyr)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
        
        gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
        
        
        ###############################################################################
        library(readr)
        library(ggplot2)
        #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
        library(ReactomePA)
        c3g_genes_entrezid = c("8728" ,"718","719","731","4179","629" ,"1675","3075","3078" ,"3080" ,"10878", "81494", "3426" , "1378") 
        #x_litt <- enrichPathway(gene=c(gene_prediction_Results$ENTREZID,c3g_genes_entrezid),pvalueCutoff=0.05, readable=T)
        x_litt <- enrichPathway(gene=gene_prediction_Results$ENTREZID,pvalueCutoff=0.05, readable=T)
        
        #cnetplot(x_litt, circular = TRUE, colorEdge = TRUE)
        #heatplot(x_litt)
        
    })
    
    
    
    output$plot2333<- renderPlot({ 
        library(readr)
        library(ggplot2)
        GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
        
        DF3 <- data.frame(chr=data22()[,'CHR'],
                          traits=data22()[,'Trait'],
                          genes= data22()[,'GeneID'])
        E <-  ggplot(data = DF3, aes(x = genes)) +
            geom_bar(aes(fill = traits))
        E+ coord_flip() +theme_minimal() 
        E <- E+ coord_flip() +theme_minimal() 
        E
        
        
    })
    
    output$plot23333<- renderPlot({ 
        ##############################################################################################
        
        library(tidyverse)
        
        fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                        pattern = "\\.csv$",
                                        full.names = TRUE)
        dataset_selection_0_000001 <- fullpath_0_000001 %>% 
            map_dfr(read.csv, header=TRUE, fill=TRUE)
        
        dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
        library(dplyr)
        names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
        #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
        
        gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
        
        ###################################################################################################
        library(readr)
        library(ggplot2)
        
        tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
        #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
        gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
        cl4 <- gene_predicted$SYMBOL
        tissus_all_clus1 <- subset(data2(),data2()[,"SYMBOL"] %in%  cl4)
        
        #tissus_all_clus1 <- subset(tissus[],tissus$SYMBOL %in%  cl4)
        tissus_all_clusallmeans <- colMeans(tissus_all_clus1[,2:14], na.rm=TRUE)
        
        tissus_all_clusallmeans_df <- as.data.frame(tissus_all_clusallmeans)
        ##
        write.csv( tissus_all_clusallmeans_df, "../GenePPI/tissus_all_clusallmeans_df.csv")
        
        tissus_all_clusallmeans_df <- read_csv("../GenePPI/tissus_all_clusallmeans_df.csv",show_col_types = FALSE)
        ###
        
        names(tissus_all_clusallmeans_df)[1]<-paste("tissus")
        names(tissus_all_clusallmeans_df)[2]<-paste("means")
        
        DF <- data.frame(means=tissus_all_clusallmeans_df$means,
                         tissus= tissus_all_clusallmeans_df$tissus)
        
        pb = ggplot(data = DF, aes(x = tissus))
        pb = pb + geom_bar(aes(fill = means))
        pb.polar = pb + coord_polar() +theme_minimal() +
            theme(axis.text.x = element_text(angle = 0, hjust = 1),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank()) +
            xlab("") + ylab("")
        
        pb.polar
        
        
    })
    ########################################################
    ###Gene Interaction############################
    #output$res <- renderPrint({
    #input$Tid
    # })
    output$res_Genes <- renderPrint({
        input$id_Genes
        #write.csv(input$id_Genes, "../GenePPI/id_Genes.csv")
        
    })
    
    
    observeEvent(input$actionvalidclust,
                 output$slide4411<- DT::renderDT({
                     ########################################################################
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     ################################################################################
                     # Loading
                     library("tibble")
                     library(readr)
                     library(tidyverse)
                     #write.csv(gene_PrbId,file="../GenePPI/gene_PrbId.csv")
                     id_Tissues = as.character(input$id_Tissues)
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     #gene_predicted <-subset(input,input$ENTREZID %in%  gene_prediction_Results$ENTREZID)
                     gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                     
                     #selection_seuil_0_0001 <- read_csv("../GenePPI/selection_seuil_0_0001.csv")
                     selection_seuil_0_0001 <- as.data.frame(list(ENTREZID = gene_prediction_Results$ENTREZID,selection_based_simila =gene_prediction_Results$selection_based_simila,selection_based_selection = gene_prediction_Results$selection_based_selection))
                     
                     tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
                     #cl4 <-subset(selection_seuil_0_0001,selection_seuil_0_0001$ENTREZID %in% gene_predicted$ENTREZID)
                     
                     cl4 <- gene_predicted$SYMBOL
                     #######################################################################################
                     #################################Tissus
                     tissues_selected = input$id_Tissues
                     #read_csv( "../GenePPI/id_Tissues.csv",show_col_types = FALSE)
                     #tissus_all_clus1 <- subset(data2()[,c("SYMBOL",tissues_selected)],data2()[,"SYMBOL"] %in%  cl4)
                     
                     tissus_all_clus1 <- subset(tissus[,c("SYMBOL",tissues_selected)],tissus$SYMBOL %in%  cl4)
                     #write.csv(tissus_all_clus1,file="../docglom21/Priorization/tissus_all_clus1.csv")
                     #tissus_all_clus1[is.na(tissus_all_clus1)] = 0
                     
                     #means_tiss$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #tissus_all_clus1$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #colMeans(df, na.rm=TRUE)
                     means_tiss <- tissus_all_clus1 %>%
                         na.omit()
                     
                     tissus_all_clus1$mean <-rowMeans(tissus_all_clus1[,tissues_selected], na.rm=TRUE)
                     #tissus_all_clus1
                     #tissus_all_clus1[,2:3]
                     #df = subset(nbr_Tiss, select = -IDX1.1 )
                     ##########################################################################################################
                     #####################################BP         ###############means
                     df_BP_values_na <- data21() %>% na.omit() 
                     df_BP_values_na_sele <- subset(df_BP_values_na,df_BP_values_na$geneSymbol %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     df_BP_values_na_sele_na_norm <- as.data.frame(lapply(df_BP_values_na_sele[,6:7], min_max_norm))
                     df_BP_values_na_sele_norm_df <- df_BP_values_na_sele %>% 
                         mutate(zscore = df_BP_values_na_sele_na_norm)
                     res.by <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, mean)
                     #res.by
                     vvv <- as.data.frame(res.by)
                     names(vvv)[1] <-"SYMBOL"
                     names(vvv)[2] <-"mean_BP_DF"
                     BP_db <- vvv
                     res.by_BP_nbr <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, length)
                     res.by_BP_nbr
                     vvv_res.by_BP_nbr <- as.data.frame(res.by_BP_nbr)
                     names(vvv_res.by_BP_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_BP_nbr)[2] <-"nbr_BP"
                     
                     nbr_BP <- vvv_res.by_BP_nbr 
                     
                     nbr_BP_DES_sel <- by(df_BP_values_na_sele_norm_df$Description, df_BP_values_na_sele$geneSymbol, paste)
                     nbr_BP_DES_df <- as.data.frame(nbr_BP_DES_sel)
                     names(nbr_BP_DES_df)[1] <-"SYMBOL"
                     names(nbr_BP_DES_df)[2] <-"DES_BP"
                     
                     nbr_BP_DES <- nbr_BP_DES_df 
                     ##########################################################################################################
                     ggg1_k <-  merge(tissus_all_clus1, BP_db[, c("SYMBOL", "mean_BP_DF")], by="SYMBOL",all.x=TRUE)
                     ggg12_k <-  merge(ggg1_k, nbr_BP[, c("SYMBOL", "nbr_BP")], by="SYMBOL",all.x=TRUE)
                     ggg13_k <-  merge(ggg12_k, nbr_BP_DES[, c("SYMBOL", "DES_BP")], by="SYMBOL",all.x=TRUE)
                     ############################################################################################################
                     ########################################################################
                     ##################################Expression                ####means
                     min_max_norm <- function(x) {
                         #(x - min(x)) / (max(x) - min(x))
                         (x - min(x)) / sd(x)}
                     EXP_norm0 <- as.data.frame(lapply(selection_seuil_0_0001[2:3], min_max_norm))
                     EXP_norm1 <- selection_seuil_0_0001 %>%   mutate(zscore = EXP_norm0$selection_based_selection)
                     #EXP_df <- EXP_norm1 %>%   mutate(SYMBOL = selection_seuil_0_0001_norm1$SYMBOL)
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     EXP_df <- merge(EXP_norm1, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     res.by_EXP <- by(EXP_df$zscore, EXP_df$SYMBOL, mean)
                     vvv_EXP <- as.data.frame(res.by_EXP)
                     names(vvv_EXP)[1] <-"SYMBOL"
                     names(vvv_EXP)[2] <-"mean_EXP_df"
                     
                     EXP_db <- vvv_EXP 
                     
                     
                     ##########################################################################################################
                     ggg2_k <-  merge(ggg13_k, EXP_db[, c("SYMBOL", "mean_EXP_df")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ####################TFs
                     TFs <-  vroom::vroom("../GenePPI/input/TFs.csv",  locale = vroom::locale(encoding = "CP1252"))
                     
                     ghgwas = as.data.frame(list(SYMBOL= TFs$SYMBOL,
                                                 TF= TFs$TF,
                                                 Zscore= TFs$`Z-score`,
                                                 Fisher_score= TFs$`Fisher score `))
                     
                     TFs_na <- ghgwas %>% na.omit() 
                     TFs_na_sele <- subset(TFs_na,TFs_na$SYMBOL %in% cl4)
                     res.by_TFs <- by(TFs_na_sele$Fisher_score, TFs_na_sele$SYMBOL, mean)
                     res.by_TFs
                     vvv_TFs <- as.data.frame(res.by_TFs)
                     names(vvv_TFs)[1] <-"SYMBOL"
                     names(vvv_TFs)[2] <-"mean_TFs_df"
                     EXP_TFs <- vvv_TFs
                     res.by_TFs_nbr <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, length)
                     vvv_res.by_TFs_nbr <- as.data.frame(res.by_TFs_nbr)
                     
                     
                     names(vvv_res.by_TFs_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_TFs_nbr)[2] <-"nbr_TFs"
                     
                     nbr_TFs <- vvv_res.by_TFs_nbr
                     
                     nbr_TFs_DES_sel <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, paste)
                     nbr_TFs_DES_df <- as.data.frame(nbr_TFs_DES_sel)
                     names(nbr_TFs_DES_df)[1] <-"SYMBOL"
                     names(nbr_TFs_DES_df)[2] <-"DES_TFs"
                     
                     nbr_TFs_DES <- nbr_TFs_DES_df 
                     ##########################################################################################################
                     ggg3_k <-  merge(ggg2_k, EXP_TFs[, c("SYMBOL", "mean_TFs_df")], by="SYMBOL",all.x=TRUE)
                     ggg31_k <-  merge(ggg3_k, nbr_TFs[, c("SYMBOL", "nbr_TFs")], by="SYMBOL",all.x=TRUE)
                     ggg32_k <-  merge(ggg31_k, nbr_TFs_DES[, c("SYMBOL", "DES_TFs")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ########################################################################
                     #########################GWAS
                     selection_seuil_GWAs_na <- GWAS %>% na.omit() 
                     GWAs_na_sele <- subset(selection_seuil_GWAs_na,selection_seuil_GWAs_na$`GeneID` %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     selection_seuil_GWAs_na_norm <- as.data.frame(lapply(GWAs_na_sele[,10], min_max_norm))
                     selection_seuil_GWAs_na_norm_df <- GWAs_na_sele %>% 
                         mutate(zscore = selection_seuil_GWAs_na_norm)
                     
                     mean_norm_GWAS <- mean(selection_seuil_GWAs_na_norm_df$zscore$P)
                     
                     res.by_GWAS <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, mean)
                     res.by_GWAS
                     library(taRifx)
                     vvv_GWAS <- as.data.frame(res.by_GWAS)
                     names(vvv_GWAS)[1] <-"SYMBOL"
                     names(vvv_GWAS)[2] <-"mean_GWAS_df"
                     
                     EXP_GWAS <- vvv_GWAS 
                     
                     res.by_GWAS_nbr <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, length)
                     res.by_GWAS_nbr
                     vvv_res.by_GWAS_nbr <- as.data.frame(res.by_GWAS_nbr)
                     names(vvv_res.by_GWAS_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_GWAS_nbr)[2] <-"nbr_GWAS"
                     
                     nbr_GWAS <- vvv_res.by_GWAS_nbr 
                     nbr_GWAS_DES_sel <- by(selection_seuil_GWAs_na_norm_df$`Trait`, selection_seuil_GWAs_na_norm_df$`GeneID`, paste)
                     nbr_GWAS_DES_df <- as.data.frame(nbr_GWAS_DES_sel)
                     names(nbr_GWAS_DES_df)[1] <-"SYMBOL"
                     names(nbr_GWAS_DES_df)[2] <-"DES_GWAS"
                     
                     nbr_GWAS_DES <- nbr_GWAS_DES_df
                     ##########################################################################################################
                     ggg4_k <-  merge(ggg32_k, EXP_GWAS[, c("SYMBOL", "mean_GWAS_df")], by="SYMBOL",all.x=TRUE)
                     ggg41_k <-  merge(ggg4_k, nbr_GWAS[, c("SYMBOL", "nbr_GWAS")], by="SYMBOL",all.x=TRUE)
                     ggg42_k <-  merge(ggg41_k, nbr_GWAS_DES[, c("SYMBOL", "DES_GWAS")], by="SYMBOL",all.x=TRUE)
                     #View(ggg42)
                     #########################################################################
                     ########################################################################
                     ##########################################function()
                     #prior = (result.mean_tissus + result.mean_expr)/2 + 
                     #ggg42[,c(6:8,10:13,15:16)][is.na(ggg42[,c(6:8,10:13,15:16)])] = 0
                     ggg42_k[is.na(ggg42_k)] = 0
                     ggg42fin_kidney_cl4 <- ggg42_k %>% rowwise() %>% mutate(Priori = ((mean+mean_EXP_df)/2) + (mean_BP_DF*nbr_BP) + (mean_TFs_df*nbr_TFs) + (mean_GWAS_df*nbr_GWAS))
                     #View(ggg42fin_kidney)
                     ###write.csv(as.matrix(ggg42fin_kidney_cl4), "../GenePPI/ggg42fin_kidney_cl4.csv")#, row.names=FALSE
                     #write.xlsx(ggg42fin_kidney_cl4, "../GenePPI/ggg42fin_kidney_cl4.xlsx")
                     #print(ggg42fin_kidney_cl4)
                     priori_scores <- ggg42fin_kidney_cl4[order(ggg42fin_kidney_cl4$Priori, decreasing = TRUE), ]
                     #DT::datatable(dplyr::printer, options = list(scrollX = TRUE))
                     
                     subset_priorized_genes <- subset(priori_scores[,c("SYMBOL","Priori")])
                     #DT::datatable(priori_scores, options = list(scrollX = TRUE))
                     #print(printer)
                     library(readr)
                     library(tidyr)
                     library("cluster")
                     library("factoextra")
                     library("magrittr")
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     my_data <- gene_prediction_Results
                     #names(my_data)[1] <-"ENTREZID"
                     
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in%  my_data$ENTREZID)
                     my_data2 <- merge(my_data, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     my_data2 <- merge(aaaaaaaa[, c("SYMBOL", "ENTREZID")], my_data, by="ENTREZID",all.x=TRUE)
                     my_data3 <-  my_data2[,c("SYMBOL","ENTREZID","selection_based_simila","selection_based_selection")]
                     
                     ##########################################################################
                     
                     #####################################################################################################
                     my_dataclus  <- my_data2[,c("selection_based_simila","selection_based_selection")]
                     names <- make.unique(as.character(my_data3$SYMBOL))
                     #names <- make.names(as.character(my_data3$SYMBOL), unique=FALSE)
                     row.names(my_dataclus) <- names  
                     ##########################################################################
                     #DT::datatable(my_data2, options = list(scrollX = TRUE))
                     my_data <- my_dataclus %>%
                         na.omit() %>%          # Remove missing values (NA)
                         scale() 
                     # Compute
                     res.hc <- my_data %>%
                         scale() %>%                    # Scale the data
                         dist(method = "euclidean") %>% # Compute dissimilarity matrix
                         hclust(method = "ward.D2")     # Compute hierachical clustering
                     ##############
                     #captionclus = as.double(read_lines( "../GenePPI/captionclus.txt"))
                     
                     clus4 = cutree(res.hc, input$captionclus)
                     testDF <- data.frame(clus4)
                     testVect <- structure(clus4)
                     clusters2 = data.frame(SYMBOL2=names(testVect), clustid=testVect, row.names=NULL)
                     
                     db <- gsub("\\..*","",clusters2$SYMBOL2)
                     db
                     tools::file_path_sans_ext(clusters2$SYMBOL2)
                     clusters <- clusters2 %>% 
                         mutate(SYMBOL = db)
                     
                     mylist<-merge(my_data2, clusters[,c("SYMBOL","clustid")], by="SYMBOL",all.x=TRUE)
                     lll <- split(mylist, as.factor(mylist$clustid))
                     mylist2 <- subset(mylist[,c("SYMBOL","clustid")])
                     mylist22 <- subset(unique(mylist[,c("SYMBOL","clustid")]))
                     # mylist22[order(mylist22$clustid, decreasing = FALSE), ]
                     
                     mydataorderclustid <-mylist22[order(mylist22$clustid,decreasing = TRUE), ]
                     #DT::datatable(as.data.frame(mydataorderclustid), options = list(scrollX = TRUE))
                     #library(readr)
                     #subset_priorized_genes <- read_csv("../GenePPI/subset_priorized_genes.csv")
                     
                     gene_int_clus <- merge(subset_priorized_genes, mydataorderclustid[, c("SYMBOL", "clustid")], by="SYMBOL",all.x=TRUE)
                     #gene_int_clus[order(gene_int_clus$clustid,gene_int_clus$Priori,decreasing = TRUE), ]
                     gene_int_clus_ord <- gene_int_clus[order(gene_int_clus$clustid,-gene_int_clus$Priori), ]
                     DT::datatable(as.data.frame(gene_int_clus_ord), options = list(scrollX = TRUE))
                     
                     
                 }))
    
    
    
    observeEvent(input$action3,
                 output$slide441 <- DT::renderDT({
                     
                     ################################################################################
                     library(tidyverse)
                     
                     fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                     pattern = "\\.csv$",
                                                     full.names = TRUE)
                     dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                         map_dfr(read.csv, header=TRUE, fill=TRUE)
                     
                     dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                     library(dplyr)
                     names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                     #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                     
                     gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                     
                     
                     ###############################################################################
                     #id_Genes = as.character(input$id_Genes)
                     
                     # Loading
                     library("tibble")
                     library(readr)
                     library(tidyverse)
                     #write.csv(gene_PrbId,file="../GenePPI/gene_PrbId.csv")
                     GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                     
                     #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                     #gene_predicted <-subset(input,input$ENTREZID %in%  gene_prediction_Results$ENTREZID)
                     gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     
                     #selection_seuil_0_0001 <- read_csv("../GenePPI/selection_seuil_0_0001.csv")
                     selection_seuil_0_0001 <- as.data.frame(list(ENTREZID = gene_prediction_Results$ENTREZID,selection_based_simila =gene_prediction_Results$selection_based_simila,selection_based_selection = gene_prediction_Results$selection_based_selection))
                     tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
                     #cl4 <-subset(selection_seuil_0_0001,selection_seuil_0_0001$ENTREZID %in% gene_predicted$ENTREZID)
                     
                     cl4 <- gene_predicted$SYMBOL
                     #######################################################################################
                     #################################Tissus
                     tissues_selected = input$id_Tissues
                     #read_csv( "../GenePPI/id_Tissues.csv",show_col_types = FALSE)
                     #tissus_all_clus1 <- subset(data2()[,c("SYMBOL",tissues_selected)],data2()[,"SYMBOL"] %in%  cl4)
                     
                     tissus_all_clus1 <- subset(tissus[,c("SYMBOL",tissues_selected)],tissus$SYMBOL %in%  cl4)
                     #write.csv(tissus_all_clus1,file="../docglom21/Priorization/tissus_all_clus1.csv")
                     #tissus_all_clus1[is.na(tissus_all_clus1)] = 0
                     
                     #means_tiss$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #tissus_all_clus1$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                     #colMeans(df, na.rm=TRUE)
                     means_tiss <- tissus_all_clus1 %>%
                         na.omit()
                     
                     tissus_all_clus1$mean <-rowMeans(tissus_all_clus1[,tissues_selected], na.rm=TRUE)
                     #tissus_all_clus1
                     #tissus_all_clus1[,2:3]
                     #df = subset(nbr_Tiss, select = -IDX1.1 )
                     ##########################################################################################################
                     #####################################BP         ###############means
                     df_BP_values_na <- data21() %>% na.omit() 
                     df_BP_values_na_sele <- subset(df_BP_values_na,df_BP_values_na$geneSymbol %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     df_BP_values_na_sele_na_norm <- as.data.frame(lapply(df_BP_values_na_sele[,6:7], min_max_norm))
                     df_BP_values_na_sele_norm_df <- df_BP_values_na_sele %>% 
                         mutate(zscore = df_BP_values_na_sele_na_norm)
                     res.by <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, mean)
                     #res.by
                     vvv <- as.data.frame(res.by)
                     names(vvv)[1] <-"SYMBOL"
                     names(vvv)[2] <-"mean_BP_DF"
                     BP_db <- vvv
                     res.by_BP_nbr <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, length)
                     res.by_BP_nbr
                     vvv_res.by_BP_nbr <- as.data.frame(res.by_BP_nbr)
                     names(vvv_res.by_BP_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_BP_nbr)[2] <-"nbr_BP"
                     
                     nbr_BP <- vvv_res.by_BP_nbr 
                     
                     nbr_BP_DES_sel <- by(df_BP_values_na_sele_norm_df$Description, df_BP_values_na_sele$geneSymbol, paste)
                     nbr_BP_DES_df <- as.data.frame(nbr_BP_DES_sel)
                     names(nbr_BP_DES_df)[1] <-"SYMBOL"
                     names(nbr_BP_DES_df)[2] <-"DES_BP"
                     
                     nbr_BP_DES <- nbr_BP_DES_df 
                     ##########################################################################################################
                     ggg1_k <-  merge(tissus_all_clus1, BP_db[, c("SYMBOL", "mean_BP_DF")], by="SYMBOL",all.x=TRUE)
                     ggg12_k <-  merge(ggg1_k, nbr_BP[, c("SYMBOL", "nbr_BP")], by="SYMBOL",all.x=TRUE)
                     ggg13_k <-  merge(ggg12_k, nbr_BP_DES[, c("SYMBOL", "DES_BP")], by="SYMBOL",all.x=TRUE)
                     ############################################################################################################
                     ########################################################################
                     ##################################Expression                ####means
                     min_max_norm <- function(x) {
                         #(x - min(x)) / (max(x) - min(x))
                         (x - min(x)) / sd(x)}
                     EXP_norm0 <- as.data.frame(lapply(selection_seuil_0_0001[2:3], min_max_norm))
                     EXP_norm1 <- selection_seuil_0_0001 %>%   mutate(zscore = EXP_norm0$selection_based_selection)
                     #EXP_df <- EXP_norm1 %>%   mutate(SYMBOL = selection_seuil_0_0001_norm1$SYMBOL)
                     aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                     EXP_df <- merge(EXP_norm1, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                     res.by_EXP <- by(EXP_df$zscore, EXP_df$SYMBOL, mean)
                     vvv_EXP <- as.data.frame(res.by_EXP)
                     names(vvv_EXP)[1] <-"SYMBOL"
                     names(vvv_EXP)[2] <-"mean_EXP_df"
                     
                     EXP_db <- vvv_EXP
                     
                     
                     ##########################################################################################################
                     ggg2_k <-  merge(ggg13_k, EXP_db[, c("SYMBOL", "mean_EXP_df")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ####################TFs
                     TFs <-  vroom::vroom("../GenePPI/input/TFs.csv",  locale = vroom::locale(encoding = "CP1252"))
                     
                     ghgwas = as.data.frame(list(SYMBOL= TFs$SYMBOL,
                                                 TF= TFs$TF,
                                                 Zscore= TFs$`Z-score`,
                                                 Fisher_score= TFs$`Fisher score `))
                     
                     TFs_na <- ghgwas %>% na.omit() 
                     TFs_na_sele <- subset(TFs_na,TFs_na$SYMBOL %in% cl4)
                     res.by_TFs <- by(TFs_na_sele$Fisher_score, TFs_na_sele$SYMBOL, mean)
                     res.by_TFs
                     vvv_TFs <- as.data.frame(res.by_TFs)
                     names(vvv_TFs)[1] <-"SYMBOL"
                     names(vvv_TFs)[2] <-"mean_TFs_df"
                     EXP_TFs <- vvv_TFs
                     res.by_TFs_nbr <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, length)
                     vvv_res.by_TFs_nbr <- as.data.frame(res.by_TFs_nbr)
                     
                     
                     names(vvv_res.by_TFs_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_TFs_nbr)[2] <-"nbr_TFs"
                     
                     nbr_TFs <- vvv_res.by_TFs_nbr
                     nbr_TFs_DES_sel <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, paste)
                     nbr_TFs_DES_df <- as.data.frame(nbr_TFs_DES_sel)
                     names(nbr_TFs_DES_df)[1] <-"SYMBOL"
                     names(nbr_TFs_DES_df)[2] <-"DES_TFs"
                     
                     nbr_TFs_DES <- nbr_TFs_DES_df 
                     ##########################################################################################################
                     ggg3_k <-  merge(ggg2_k, EXP_TFs[, c("SYMBOL", "mean_TFs_df")], by="SYMBOL",all.x=TRUE)
                     ggg31_k <-  merge(ggg3_k, nbr_TFs[, c("SYMBOL", "nbr_TFs")], by="SYMBOL",all.x=TRUE)
                     ggg32_k <-  merge(ggg31_k, nbr_TFs_DES[, c("SYMBOL", "DES_TFs")], by="SYMBOL",all.x=TRUE)
                     ########################################################################
                     ########################################################################
                     #########################GWAS
                     selection_seuil_GWAs_na <- GWAS %>% na.omit() 
                     GWAs_na_sele <- subset(selection_seuil_GWAs_na,selection_seuil_GWAs_na$`GeneID` %in% cl4)
                     min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                     selection_seuil_GWAs_na_norm <- as.data.frame(lapply(GWAs_na_sele[,10], min_max_norm))
                     selection_seuil_GWAs_na_norm_df <- GWAs_na_sele %>% 
                         mutate(zscore = selection_seuil_GWAs_na_norm)
                     
                     mean_norm_GWAS <- mean(selection_seuil_GWAs_na_norm_df$zscore$P)
                     
                     res.by_GWAS <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, mean)
                     res.by_GWAS
                     library(taRifx)
                     vvv_GWAS <- as.data.frame(res.by_GWAS)
                     names(vvv_GWAS)[1] <-"SYMBOL"
                     names(vvv_GWAS)[2] <-"mean_GWAS_df"
                     
                     EXP_GWAS <- vvv_GWAS 
                     
                     res.by_GWAS_nbr <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, length)
                     res.by_GWAS_nbr
                     vvv_res.by_GWAS_nbr <- as.data.frame(res.by_GWAS_nbr)
                     names(vvv_res.by_GWAS_nbr)[1] <-"SYMBOL"
                     names(vvv_res.by_GWAS_nbr)[2] <-"nbr_GWAS"
                     
                     nbr_GWAS <- vvv_res.by_GWAS_nbr 
                     nbr_GWAS_DES_sel <- by(selection_seuil_GWAs_na_norm_df$`Trait`, selection_seuil_GWAs_na_norm_df$`GeneID`, paste)
                     nbr_GWAS_DES_df <- as.data.frame(nbr_GWAS_DES_sel)
                     names(nbr_GWAS_DES_df)[1] <-"SYMBOL"
                     names(nbr_GWAS_DES_df)[2] <-"DES_GWAS"
                     
                     nbr_GWAS_DES <- nbr_GWAS_DES_df
                     ##########################################################################################################
                     ggg4_k <-  merge(ggg32_k, EXP_GWAS[, c("SYMBOL", "mean_GWAS_df")], by="SYMBOL",all.x=TRUE)
                     ggg41_k <-  merge(ggg4_k, nbr_GWAS[, c("SYMBOL", "nbr_GWAS")], by="SYMBOL",all.x=TRUE)
                     ggg42_k <-  merge(ggg41_k, nbr_GWAS_DES[, c("SYMBOL", "DES_GWAS")], by="SYMBOL",all.x=TRUE)
                     #View(ggg42)
                     #########################################################################
                     ########################################################################
                     ##########################################function()
                     #prior = (result.mean_tissus + result.mean_expr)/2 + 
                     #ggg42[,c(6:8,10:13,15:16)][is.na(ggg42[,c(6:8,10:13,15:16)])] = 0
                     ggg42_k[is.na(ggg42_k)] = 0
                     ggg42fin_kidney_cl4 <- ggg42_k %>% rowwise() %>% mutate(Priori = ((mean+mean_EXP_df)/2) + (mean_BP_DF*nbr_BP) + (mean_TFs_df*nbr_TFs) + (mean_GWAS_df*nbr_GWAS))
                     #View(ggg42fin_kidney)
                     ###write.csv(as.matrix(ggg42fin_kidney_cl4), "../GenePPI/ggg42fin_kidney_cl4.csv")#, row.names=FALSE
                     #write.xlsx(ggg42fin_kidney_cl4, "../GenePPI/ggg42fin_kidney_cl4.xlsx")
                     #print(ggg42fin_kidney_cl4)
                     priori_scores <- ggg42fin_kidney_cl4[order(ggg42fin_kidney_cl4$Priori, decreasing = TRUE), ]
                     #DT::datatable(dplyr::printer, options = list(scrollX = TRUE))
                     
                     subset_priorized_genes <- subset(priori_scores[,c("SYMBOL","Priori")])
                     
                     
                     
                     
                     
                     
                     
                     
                     #print(myselectionread)
                     #req(data2())
                     
                     #subset_myselection_genes <- subset(subset_priorized_genes[,c("SYMBOL","Priori")],subset_priorized_genes$SYMBOL %in%  myselection_genes$x)
                     subset_myselection_genes <- subset(subset_priorized_genes[,c("SYMBOL","Priori")],subset_priorized_genes$SYMBOL %in% input$id_Genes)
                     
                     #file.remove("../GenePPI/id_Genes.csv")
                     #DT::datatable(subset_myselection_genes, options = list(scrollX = TRUE))
                     
                     DT::datatable(subset_myselection_genes, options = list(scrollX = TRUE))
                     
                 }))
    
    observeEvent(input$action3,
                 observeEvent(input$action3,
                              output$slide442 <- renderText({
                                  
                                  #################################################################################################
                                  library(tidyverse)
                                  
                                  fullpath_0_000001 <- list.files(path = "../GenePPI/test/",
                                                                  pattern = "\\.csv$",
                                                                  full.names = TRUE)
                                  dataset_selection_0_000001 <- fullpath_0_000001 %>% 
                                      map_dfr(read.csv, header=TRUE, fill=TRUE)
                                  
                                  dataset_selected_0_000001 <- filter(dataset_selection_0_000001, dataset_selection_0_000001$Selected_score != 1.000)
                                  library(dplyr)
                                  names(dataset_selected_0_000001)[1]<-paste("ENTREZID")
                                  #write.csv(subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust),file="../GenePPI/gene_prediction_Results.csv", row.names=FALSE)
                                  
                                  gene_prediction_Results<- subset(dataset_selected_0_000001, dataset_selected_0_000001$selection_based_selection >= input$bw_adjust)
                                  
                                  
                                  #id_Genes = as.character(input$id_Genes)
                                  
                                  # Loading
                                  library("tibble")
                                  library(readr)
                                  library(tidyverse)
                                  #write.csv(gene_PrbId,file="../GenePPI/gene_PrbId.csv")
                                  GWAS <- read_csv("../GenePPI/input/GWAS.csv",show_col_types = FALSE)
                                  
                                  #gene_prediction_Results <- read_csv("../GenePPI/gene_prediction_Results.csv",show_col_types = FALSE)
                                  #gene_predicted <-subset(input,input$ENTREZID %in%  gene_prediction_Results$ENTREZID)
                                  gene_predicted <-subset(data111(),data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                                  
                                  #selection_seuil_0_0001 <- read_csv("../GenePPI/selection_seuil_0_0001.csv")
                                  selection_seuil_0_0001 <- as.data.frame(list(ENTREZID = gene_prediction_Results$ENTREZID,selection_based_simila =gene_prediction_Results$selection_based_simila,selection_based_selection = gene_prediction_Results$selection_based_selection))
                                  tissus <- read_csv("../GenePPI/input/Tissues.csv",show_col_types = FALSE)
                                  #cl4 <-subset(selection_seuil_0_0001,selection_seuil_0_0001$ENTREZID %in% gene_predicted$ENTREZID)
                                  
                                  cl4 <- gene_predicted$SYMBOL
                                  #######################################################################################
                                  #################################Tissus
                                  tissues_selected = input$id_Tissues
                                  #read_csv( "../GenePPI/id_Tissues.csv",show_col_types = FALSE)
                                  #tissus_all_clus1 <- subset(data2()[,c("SYMBOL",tissues_selected)],data2()[,"SYMBOL"] %in%  cl4)
                                  
                                  tissus_all_clus1 <- subset(tissus[,c("SYMBOL",tissues_selected)],tissus$SYMBOL %in%  cl4)
                                  #write.csv(tissus_all_clus1,file="../docglom21/Priorization/tissus_all_clus1.csv")
                                  #tissus_all_clus1[is.na(tissus_all_clus1)] = 0
                                  
                                  #means_tiss$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                                  #tissus_all_clus1$mean <- means_tiss_coup(means_tiss$Kidney,means_tiss$`Kidney cancer cell` )
                                  #colMeans(df, na.rm=TRUE)
                                  means_tiss <- tissus_all_clus1 %>%
                                      na.omit()
                                  
                                  tissus_all_clus1$mean <-rowMeans(tissus_all_clus1[,tissues_selected], na.rm=TRUE)
                                  #tissus_all_clus1
                                  #tissus_all_clus1[,2:3]
                                  #df = subset(nbr_Tiss, select = -IDX1.1 )
                                  ##########################################################################################################
                                  #####################################BP         ###############means
                                  df_BP_values_na <- data21() %>% na.omit() 
                                  df_BP_values_na_sele <- subset(df_BP_values_na,df_BP_values_na$geneSymbol %in% cl4)
                                  min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                                  df_BP_values_na_sele_na_norm <- as.data.frame(lapply(df_BP_values_na_sele[,6:7], min_max_norm))
                                  df_BP_values_na_sele_norm_df <- df_BP_values_na_sele %>% 
                                      mutate(zscore = df_BP_values_na_sele_na_norm)
                                  res.by <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, mean)
                                  #res.by
                                  vvv <- as.data.frame(res.by)
                                  names(vvv)[1] <-"SYMBOL"
                                  names(vvv)[2] <-"mean_BP_DF"
                                  BP_db <- vvv
                                  res.by_BP_nbr <- by(df_BP_values_na_sele_norm_df$zscore$pvalue, df_BP_values_na_sele_norm_df$geneSymbol, length)
                                  res.by_BP_nbr
                                  vvv_res.by_BP_nbr <- as.data.frame(res.by_BP_nbr)
                                  names(vvv_res.by_BP_nbr)[1] <-"SYMBOL"
                                  names(vvv_res.by_BP_nbr)[2] <-"nbr_BP"
                                  
                                  nbr_BP <- vvv_res.by_BP_nbr 
                                  
                                  nbr_BP_DES_sel <- by(df_BP_values_na_sele_norm_df$Description, df_BP_values_na_sele$geneSymbol, paste)
                                  nbr_BP_DES_df <- as.data.frame(nbr_BP_DES_sel)
                                  names(nbr_BP_DES_df)[1] <-"SYMBOL"
                                  names(nbr_BP_DES_df)[2] <-"DES_BP"
                                  
                                  nbr_BP_DES <- nbr_BP_DES_df 
                                  ##########################################################################################################
                                  ggg1_k <-  merge(tissus_all_clus1, BP_db[, c("SYMBOL", "mean_BP_DF")], by="SYMBOL",all.x=TRUE)
                                  ggg12_k <-  merge(ggg1_k, nbr_BP[, c("SYMBOL", "nbr_BP")], by="SYMBOL",all.x=TRUE)
                                  ggg13_k <-  merge(ggg12_k, nbr_BP_DES[, c("SYMBOL", "DES_BP")], by="SYMBOL",all.x=TRUE)
                                  ############################################################################################################
                                  ########################################################################
                                  ##################################Expression                ####means
                                  min_max_norm <- function(x) {
                                      #(x - min(x)) / (max(x) - min(x))
                                      (x - min(x)) / sd(x)}
                                  EXP_norm0 <- as.data.frame(lapply(selection_seuil_0_0001[2:3], min_max_norm))
                                  EXP_norm1 <- selection_seuil_0_0001 %>%   mutate(zscore = EXP_norm0$selection_based_selection)
                                  #EXP_df <- EXP_norm1 %>%   mutate(SYMBOL = selection_seuil_0_0001_norm1$SYMBOL)
                                  aaaaaaaa <- subset(data111()[,c("ENTREZID","SYMBOL")],data111()[,"ENTREZID"] %in% gene_prediction_Results$ENTREZID)
                                  EXP_df <- merge(EXP_norm1, aaaaaaaa[, c("SYMBOL", "ENTREZID")], by="ENTREZID",all.x=TRUE)
                                  res.by_EXP <- by(EXP_df$zscore, EXP_df$SYMBOL, mean)
                                  vvv_EXP <- as.data.frame(res.by_EXP)
                                  names(vvv_EXP)[1] <-"SYMBOL"
                                  names(vvv_EXP)[2] <-"mean_EXP_df"
                                  
                                  EXP_db <- vvv_EXP 
                                  
                                  
                                  ##########################################################################################################
                                  ggg2_k <-  merge(ggg13_k, EXP_db[, c("SYMBOL", "mean_EXP_df")], by="SYMBOL",all.x=TRUE)
                                  ########################################################################
                                  ####################TFs
                                  TFs <-  vroom::vroom("../GenePPI/input/TFs.csv",  locale = vroom::locale(encoding = "CP1252"))
                                  
                                  ghgwas = as.data.frame(list(SYMBOL= TFs$SYMBOL,
                                                              TF= TFs$TF,
                                                              Zscore= TFs$`Z-score`,
                                                              Fisher_score= TFs$`Fisher score `))
                                  
                                  TFs_na <- ghgwas %>% na.omit() 
                                  TFs_na_sele <- subset(TFs_na,TFs_na$SYMBOL %in% cl4)
                                  res.by_TFs <- by(TFs_na_sele$Fisher_score, TFs_na_sele$SYMBOL, mean)
                                  res.by_TFs
                                  vvv_TFs <- as.data.frame(res.by_TFs)
                                  names(vvv_TFs)[1] <-"SYMBOL"
                                  names(vvv_TFs)[2] <-"mean_TFs_df"
                                  EXP_TFs <- vvv_TFs
                                  res.by_TFs_nbr <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, length)
                                  vvv_res.by_TFs_nbr <- as.data.frame(res.by_TFs_nbr)
                                  
                                  
                                  names(vvv_res.by_TFs_nbr)[1] <-"SYMBOL"
                                  names(vvv_res.by_TFs_nbr)[2] <-"nbr_TFs"
                                  
                                  nbr_TFs <- vvv_res.by_TFs_nbr
                                  nbr_TFs_DES_sel <- by(TFs_na_sele$TF, TFs_na_sele$SYMBOL, paste)
                                  nbr_TFs_DES_df <- as.data.frame(nbr_TFs_DES_sel)
                                  names(nbr_TFs_DES_df)[1] <-"SYMBOL"
                                  names(nbr_TFs_DES_df)[2] <-"DES_TFs"
                                  
                                  nbr_TFs_DES <- nbr_TFs_DES_df 
                                  ##########################################################################################################
                                  ggg3_k <-  merge(ggg2_k, EXP_TFs[, c("SYMBOL", "mean_TFs_df")], by="SYMBOL",all.x=TRUE)
                                  ggg31_k <-  merge(ggg3_k, nbr_TFs[, c("SYMBOL", "nbr_TFs")], by="SYMBOL",all.x=TRUE)
                                  ggg32_k <-  merge(ggg31_k, nbr_TFs_DES[, c("SYMBOL", "DES_TFs")], by="SYMBOL",all.x=TRUE)
                                  ########################################################################
                                  ########################################################################
                                  #########################GWAS
                                  selection_seuil_GWAs_na <- GWAS %>% na.omit() 
                                  GWAs_na_sele <- subset(selection_seuil_GWAs_na,selection_seuil_GWAs_na$`GeneID` %in% cl4)
                                  min_max_norm <- function(x) {(x - min(x)) / sd(x)}
                                  selection_seuil_GWAs_na_norm <- as.data.frame(lapply(GWAs_na_sele[,10], min_max_norm))
                                  selection_seuil_GWAs_na_norm_df <- GWAs_na_sele %>% 
                                      mutate(zscore = selection_seuil_GWAs_na_norm)
                                  
                                  mean_norm_GWAS <- mean(selection_seuil_GWAs_na_norm_df$zscore$P)
                                  
                                  res.by_GWAS <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, mean)
                                  res.by_GWAS
                                  vvv_GWAS <- as.data.frame(res.by_GWAS)
                                  names(vvv_GWAS)[1] <-"SYMBOL"
                                  names(vvv_GWAS)[2] <-"mean_GWAS_df"
                                  
                                  EXP_GWAS <- vvv_GWAS 
                                  
                                  res.by_GWAS_nbr <- by(selection_seuil_GWAs_na_norm_df$zscore$P, selection_seuil_GWAs_na_norm_df$`GeneID`, length)
                                  res.by_GWAS_nbr
                                  vvv_res.by_GWAS_nbr <- as.data.frame(res.by_GWAS_nbr)
                                  names(vvv_res.by_GWAS_nbr)[1] <-"SYMBOL"
                                  names(vvv_res.by_GWAS_nbr)[2] <-"nbr_GWAS"
                                  
                                  nbr_GWAS <- vvv_res.by_GWAS_nbr 
                                  nbr_GWAS_DES_sel <- by(selection_seuil_GWAs_na_norm_df$`Trait`, selection_seuil_GWAs_na_norm_df$`GeneID`, paste)
                                  nbr_GWAS_DES_df <- as.data.frame(nbr_GWAS_DES_sel)
                                  names(nbr_GWAS_DES_df)[1] <-"SYMBOL"
                                  names(nbr_GWAS_DES_df)[2] <-"DES_GWAS"
                                  
                                  nbr_GWAS_DES <- nbr_GWAS_DES_df
                                  ##########################################################################################################
                                  ggg4_k <-  merge(ggg32_k, EXP_GWAS[, c("SYMBOL", "mean_GWAS_df")], by="SYMBOL",all.x=TRUE)
                                  ggg41_k <-  merge(ggg4_k, nbr_GWAS[, c("SYMBOL", "nbr_GWAS")], by="SYMBOL",all.x=TRUE)
                                  ggg42_k <-  merge(ggg41_k, nbr_GWAS_DES[, c("SYMBOL", "DES_GWAS")], by="SYMBOL",all.x=TRUE)
                                  #View(ggg42)
                                  #########################################################################
                                  ########################################################################
                                  ##########################################function()
                                  #prior = (result.mean_tissus + result.mean_expr)/2 + 
                                  #ggg42[,c(6:8,10:13,15:16)][is.na(ggg42[,c(6:8,10:13,15:16)])] = 0
                                  ggg42_k[is.na(ggg42_k)] = 0
                                  ggg42fin_kidney_cl4 <- ggg42_k %>% rowwise() %>% mutate(Priori = ((mean+mean_EXP_df)/2) + (mean_BP_DF*nbr_BP) + (mean_TFs_df*nbr_TFs) + (mean_GWAS_df*nbr_GWAS))
                                  #View(ggg42fin_kidney)
                                  ###write.csv(as.matrix(ggg42fin_kidney_cl4), "../GenePPI/ggg42fin_kidney_cl4.csv")#, row.names=FALSE
                                  #write.xlsx(ggg42fin_kidney_cl4, "../GenePPI/ggg42fin_kidney_cl4.xlsx")
                                  #print(ggg42fin_kidney_cl4)
                                  priori_scores <- ggg42fin_kidney_cl4[order(ggg42fin_kidney_cl4$Priori, decreasing = TRUE), ]
                                  #DT::datatable(dplyr::printer, options = list(scrollX = TRUE))
                                  
                                  subset_priorized_genes <- subset(priori_scores[,c("SYMBOL","Priori")])
                                  
                                  #subset_myselection_genes <- subset(subset_priorized_genes[,c("SYMBOL","Priori")],subset_priorized_genes$SYMBOL %in%  myselection_genes$x)
                                  subset_myselection_genes <- subset(subset_priorized_genes[,c("SYMBOL","Priori")],subset_priorized_genes$SYMBOL %in%  input$id_Genes)
                                  
                                  #file.remove("../GenePPI/id_Genes.csv")
                                  #DT::datatable(subset_myselection_genes, options = list(scrollX = TRUE))
                                  res_intera <-  log10((subset_myselection_genes$Priori[1])+log10(subset_myselection_genes$Priori[2]))/(2+log10(max(subset_myselection_genes$Priori)))
                                  
                                  interaction_score <- cbind(subset_myselection_genes, res_intera)
                                  #print(paste0("You've selected the genes named ", input$id_Genes[], ". It's mean value is: ", res_intera))
                                  
                                  #paste0("You've selected the genes named ", res_intera)
                                  # DT::datatable(res_intera, options = list(scrollX = TRUE))
                                  print(paste0("Your interaction score based MGTI model is ", res_intera))
                                  
                              }))
    )
    
    
    
    
}

shiny::shinyApp(ui, server)