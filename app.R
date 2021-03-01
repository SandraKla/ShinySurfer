####################################### Libraries ########################################

source("libraries.R")

####################################### Scripts ##########################################

file.source=list.files("ggseg3d//R",pattern="*.R",full.names = TRUE)
lapply(file.source, source,.GlobalEnv)

file.source=list.files("R",pattern="*.R",full.names = TRUE)
lapply(file.source, source,.GlobalEnv)
load("desterieux_3d.rda")

source("geom_flat_violin.R")

####################################### User Interface ###################################

ui <- fluidPage(
  dashboardPage(
    
    ### Header ###
    dashboardHeader(title="ShinySurfer", titleWidth = 350),
    ### Sidebar ###
    dashboardSidebar( width = 350, 
      sidebarMenu(id="sideM", 
        
        ### Start ###
        menuItem("Start",tabName = "updata", icon = icon("home"), selected = T,
                 prettyRadioButtons("data_type",label = "Data File Type", inline = T,
                                    choices = c("FreeSurfer","CERES")), #"Others")),
                 checkboxInput("check_old_new_data", label = "Old data format", value = FALSE),
                 fileInput("data_tablelh","Freesurfer - LH (only new format)",accept = c("txt")),
                 fileInput("data_tablerh","Freesurfer - RH (only new format)",accept = c("txt")),
                 fileInput("data_table","Upload data:", accept = c("csv","xlsx","xls")),
                 
                 conditionalPanel(
                   condition="output.dataFileLoad==true",
                   fileInput("name_file","Names correction", accept = c("xlsx","xls")))),
        
        ### Quality Control ###
        menuItem("Quality Control",tabName = "qc",icon=icon("chart-bar"), expandedName = "qc",
                 conditionalPanel(
                   condition="output.dataFileLoad==true",
                   uiOutput("fil_qc"), 
                   uiOutput("qc_kon"),
                   actionButton("dp","Make Quality Raincloud")
                 )),
        
        ### Descriptive Statistics ###
        menuItem("Descriptive Statistics",expandedName = "ds",icon=icon("brain"),
                 conditionalPanel(
                   condition="output.dataFileLoad==true && input.data_type=='FreeSurfer'",
                   uiOutput("fil_ui"),
                   uiOutput("ds_kon"),
                   
                   conditionalPanel(
                     condition = "input.com==1",
                     uiOutput("com_cd")
                   ),
    
                   prettyRadioButtons(inputId = "select_hemisphere",
                                      label = "Choose Hemisphere",
                                      choices = c("left","right","both"),
                                      selected = "both",inline = TRUE),
                   
                   checkboxInput("col","Color and Value", value = FALSE),
                   
                   checkboxInput("com","Composity",value = TRUE),
                   #checkboxInput("check_download","Download",FALSE),
                   
                   actionButton("ab","Brain Map")
                   #conditionalPanel(
                   #  condition = "input.check_download==1",
                   #  textInput(inputId = "name",
                   #            "file name"
                   # ),
                   #  
                   #  selectInput(inputId = "format",
                   #              "file format",
                   #              choices = c("svg","pdf","png"),
                   #              "pdf"
                   # ),
                   #  div(style="height:100px;",
                   #      checkboxInput("down_filter","With Filer Label",value = FALSE)
                   #      
                   # ),
                   #  actionButton("download","Download image"),
                   #  hr()
                   #)
                 )
        ),
        
        ### Linear Regression ###
        menuItem("Linear Regression",expandedName = "ss",icon=icon("chart-line"),
                 conditionalPanel(
                   condition="output.dataFileLoad==true",
                   uiOutput("fil_ss"),
                   uiOutput("ss_kon"),
                   actionButton("rp","Regression Plots")
                 )
        ),
        
        ### Lasso Regression ###
        menuItem("Lasso Regression",expandedName = "ls",icon=icon("chart-line"),
                 conditionalPanel(
                   condition="output.dataFileLoad==true",
                   uiOutput("fil_ls"),
                   uiOutput("ls_kon"),
                   actionButton("lp","TableLasso generation")
                 )
        ),
        
        ### Restart Button ###
        tags$button("Restart", id="restart", 
                    type="button", class="btn btn-danger action-button", 
                    onclick="history.go(0)")
      )
    ),  
    
    ### MainBar ###
    dashboardBody(
      
      ### Start ###
      conditionalPanel(
        condition = "!(input.sidebarItemExpanded=='qc')
                    && !(input.sidebarItemExpanded=='ss')
                    && !(input.sidebarItemExpanded=='ds')
                    && !(input.sidebarItemExpanded=='ls')",
        includeMarkdown("www//Home.md")
      ),

      ### Quality Control ###
      
      conditionalPanel(
        condition="output.dataFileLoad==true && input.sidebarItemExpanded=='qc'&&
        output.dp == 1",
        uiOutput("qc_tabs_filter")),
      
      conditionalPanel(
        condition="output.dataFileLoad==true && input.sidebarItemExpanded=='qc'",
        uiOutput("qc_tabs")),
      
      ### Descriptive Statistics ###
      conditionalPanel(condition="input.col==1 && input.sidebarItemExpanded=='ds'",
                       colourInput("color_obergrenze", "Please select the color of the upper bound", "red"),
                       numericInput(inputId = 'wert_obergrenze',
                                    label = 'Please choose a value for the upper bound',
                                    4.2),
                       
                       colourInput("color_untergrenze", "Please select the color of the lower bound", "blue"),
                       numericInput(inputId = 'wert_untergrenze',
                                    label = 'Please choose a value for the lower bound',
                                    1.5),
                       actionButton("add_mitte", "Add new values and colors"),
                       actionButton("remove_mitte","Remove")
      ),
      
      ### Descriptive Statistics ###
      conditionalPanel(
        condition="output.dataFileLoad==true && input.sidebarItemExpanded=='ds' && input.data_type=='FreeSurfer'",
        tabsetPanel(
          type="tabs",id="ds_tab",
          tabPanel("Table",DT::dataTableOutput("ds_table")),
          tabPanel("Statistics",DT::dataTableOutput("ds_composity")),
          tabPanel("3D",plotlyOutput("ggseg3d",height = "700px"))
        )
      ),
      
      ### Linear Regression ###
      conditionalPanel(
        condition="output.dataFileLoad==true && input.sidebarItemExpanded=='ss'",
        uiOutput("ss_tabs")
        # tabsetPanel(
        #   type="tabs",id="ss_tab",
        #   tabPanel("Table",DT::dataTableOutput("ss_table")),
        #   tabPanel("Regression Plots",plotOutput("regression",height ="40000px",width = "1000px"))
        # ),
      ),
      
      ### Lasso Regression ###
      conditionalPanel(
        condition="output.dataFileLoad==true && input.sidebarItemExpanded=='ls'",
        tabsetPanel(
          type="tabs",id="ls_tab",
          tabPanel("Table",DT::dataTableOutput("ls_table")),
          tabPanel("Lasso tabel",DT::dataTableOutput("lasso_table"))
        )
      )
    )
  )
)

####################################### Server ###########################################

server<-function(input, output,session) {
  
  OASIS <<- NULL

  ### Get Dataset ###  
  get_data_file <- reactive({
    saving <<- 
    if(!is.null(input$data_table)){
      if(input$check_old_new_data == FALSE){
        
        lh <- read.delim(input$data_tablelh[["datapath"]])
        lh$BrainSegVolNotVent <- NULL
        lh$eTIV <- NULL
        rh <- read.delim(input$data_tablerh[["datapath"]])
        rh$BrainSegVolNotVent <- NULL
        rh$eTIV <- NULL
        
        demographics <- read.csv(input$data_table[["datapath"]])
        OASIS <- cbind(demographics, lh, rh)
        OASIS$lh.aparc.a2009s.thickness <- NULL
        OASIS$lh_MeanThickness_thickness <- NULL
        OASIS$rh.aparc.a2009s.thickness <- NULL
        OASIS$rh_MeanThickness_thickness <- NULL
        colnames(OASIS)[1] <- "ID"} 
      else{
        OASIS <<- read_excel(input$data_table[["datapath"]]) 
      }
      
      OASIS <<- OASIS
      return(TRUE)
    }else{
      # OASIS <<- read_excel("OASIS_behavioral.xlsx")
      # return(TRUE)
      return(FALSE)
    }
  })
  
  ### output.dataFileLoad - Constant to check if data are loaded ###
  output$dataFileLoad <- reactive({
    return(get_data_file())
  })
  
  outputOptions(output,'dataFileLoad',suspendWhenHidden=FALSE)
  
  
  ##################################### Data preprocess ##################################

  ### Transform the names of OASIS and get the data ###
  get_oasis <- reactive({
    is.null(input$data_table)
    if(!is.null(input$name_file)){
      area <- read_excel(input$name_file[["datapath"]],col_names = FALSE)
      return(oasis.tidy(session,area,OASIS))
    }else{
      oasis_data <- OASIS
      return(oasis_data)
    }
  })
  
  ### Fill User Interface ###
  get_fil <- reactive({
    input$fil
  })
  
  get_fil_com <- reactive({
    input$fil_com
  })
  
  
  
  get_ss_fil <- reactive({
    input$ss_fil
  })
  
  get_ls_fil <- reactive({
    input$ls_fil
  })

  ### load desterieux_3d ###
  get_altes <- reactive({
    desterieux_neu <- desterieux_3d
    oa <- get_oasis()
    cols <- ncol(oa)
    t_name <- names(oa[(cols-147):cols])
    for (j in 1:6) {
      if (desterieux_neu[[3]][[j]] == "left") {
        for (i in 1:82) {
          desterieux_neu[[4]][[j]][[1]][[i]]<-t_name[trunc((i+1)/2)]
        }
        for (i in 84:149) {
          desterieux_neu[[4]][[j]][[1]][[i]]<-t_name[trunc(i/2)]
        }
      }else{
        for (i in 1:82) {
          desterieux_neu[[4]][[j]][[1]][[i]]<-t_name[trunc((i+149)/2)]
        }
        for (i in 84:149) {
          desterieux_neu[[4]][[j]][[1]][[i]]<-t_name[trunc((i+148)/2)]
        }
      }
    }
    return(desterieux_neu)
  })
  
  
  ### ###
  output$fil_ui <- renderUI({
  
    if(input$com==0){
      tagList(
        selectInput("fil",label = "Filter",
                    choices = names(get_oasis()),multiple = TRUE)
      ) 
    }else{
      input$name_file
      tagList(
        selectInput("fil_com",label = "Filter",
                    choices = names(get_oasis())[-1],multiple = TRUE)
      )
    }
  })
  
  
  
  ### ###
  output$fil_ss <- renderUI({
    input$name_file
    tagList(
      selectInput("ss_fil",label = "Filter",choices = names(get_oasis())[-1],multiple = TRUE)
    )
  })
  
  ### ###
  output$fil_ls <- renderUI({
    input$name_file
    tagList(
      selectInput("ls_fil",label = "Filter",choices = names(get_oasis())[-1],multiple = TRUE)
    )
  })
  
  ### get the explan names
  get_explan_names <- reactive({
    input$data_table
    input$data_type
    cols <- ncol(OASIS)
    if(input$data_type=="FreeSurfer"){
      explans <- dplyr::select(OASIS,-(cols-147):-cols)
      names_explan <- get.regression.col(explans)
    }else if(input$data_type=="CERES"){
      explans <- dplyr::select(OASIS,1:(cols-274))
      names_explan <- get.regression.col(explans)
    }#else{
    #  names_explan <- names(OASIS)
    #}
    
    return(names_explan)
  })  
  
  ##### Filtering #####
  ### filter the data from according to selected condition ###
  get_choice <- reactive({    # get the select col and return the selected date
    input$data_table
    col_input <- get_fil()
    col_com_input <- get_fil_com()
    u_oasis <- get_oasis()
    if(input$com==0){
      for (col in col_input) {
        v <- input[[col]]
        if(!(is.null(v)|| ""==v)){
          if(as.character(col)=='sex'){
            if(input$sex!="All"){
              u_oasis <- u_oasis[u_oasis$sex==input$sex,]
            }}
          else{
            u_oasis <- u_oasis[u_oasis[[col]]==v,]
          }}
      }
    }else{
      
      for (col in col_com_input) {
        u_oasis <- get.choice(col,data.table =u_oasis,seletedValue = input[[paste0(col,"_range")]],col.type = "_range")
      }
    }
    return(u_oasis)
  })
  
  
  
  get_ss_choice <- reactive({    # get the select col and return the selected date
    col_input <- get_ss_fil()
    u_oasis <- get_oasis()
    
    for (col in col_input) {
      u_oasis <- get.choice(col,data.table =u_oasis,seletedValue = input[[paste0(col,"_ss")]],col.type = "_ss")
    }
    return(u_oasis)
  })
  
  get_ls_choice <- reactive({    # get the select col and return the selected date
    col_input <- get_ls_fil()
    u_oasis <- get_oasis()
    for (col in col_input) {
      u_oasis <- get.choice(col,data.table =u_oasis,seletedValue = input[[paste0(col,"_ls")]],col.type = "_ls")
    }
    return(u_oasis)
  })
  
  ### ###
  output$ds_kon <- renderUI({   
    x <- vector("list",length=length(get_fil()))
    if(input$com==0||is.null(input$com)){
      for (ff in get_fil()) {
        x <- append(x,list(
          if(as.character(ff)=="sex"){
            selectInput("sex",
                        label = "sex",
                        choices = unique(append("All",get_choice()[["sex"]])),
                        selected = {
                          if (is.null(input[[ff]])||"All"==input[[ff]]){
                            "All"
                          } else{
                            input[[ff]]
                          }
                        })
          }
          else{
            selectInput(
              inputId = paste0(ff),
              label = as.character(ff),
              choices = c(" "="",sort(unique(get_choice()[[ff]]))),
              selected = {
                if (is.null(input[[ff]])||""==input[[ff]]){
                  ""
                } else{
                  input[[ff]]
                }
              }
            )}
        ))}
    }else{
      for (ff in get_fil_com()) {
        x <- append(x,list(
          if(as.character(ff)=="ID"){}
          else if(as.character(ff)=="sex"){
            selectInput("sex_range",
                        label = "sex",
                        choices = unique(append("All",c("F","M"))),
                        selected = {
                          if (is.null(input[["sex_range"]])||"All"==input[["sex_range"]]){
                            "All"
                          } else{
                            input[["sex_range"]]
                          }
                        })
          }else{
            sliderInput(paste0(ff,"_range"),paste(ff),
                        min = min(get_oasis()[[ff]]),max=max(get_oasis()[[ff]]),
                        value = {if(!is.null(input[[paste0(ff,"_range")]])){
                          input[[paste0(ff,"_range")]]
                        }else{
                          c(min(get_oasis()[[ff]]),max(get_oasis()[[ff]]))
                        }
                        }
            )
          }
        ))
      }
    }
    return(x)
  })
  
  ### ###
  observeEvent(input$qc_col,{
    cols <- input$qc_col
    if("All" %in% cols){
      updateSelectInput(session,"qc_col",selected = names(dplyr::select(get_qc_choice(),-c("ID","sex"))))
    }
  })
  
  ### Stastics
  output$ss_kon <- renderUI({
    x <- NULL
    for (ff in get_ss_fil()) {
      x <- append(x,list(
        render.ui.output(ff,get_oasis()[[ff]],input[[paste0(ff,"_ss")]],ui_type="_ss")
      ))
    }
    
    x <- append(x,list(selectInput("explan",label="Explanatory variable",choices = get_explan_names())))
    
    # if(input$data_type=="Others"){
    #   x <- append(x,list(selectInput("explan_2",label="Data variable",choices = get_explan_names())))
    # }
    return(x)
  })
  
  ### ###
  output$ls_kon <- renderUI({
    x <- NULL
    for (ff in get_ls_fil()) {
      x <- append(x,list(
        render.ui.output(ff,get_oasis()[[ff]],input[[paste0(ff,"_ls")]],ui_type="_ls")
      ))
      # x <- append(x,list(
      #   if(as.character(ff)=="sex"){
      #     selectInput("sex_qc",
      #                 label = "sex",
      #                 choices = unique(append("All",c("F","M"))),
      #                 selected = {
      #                   if (is.null(input[["sex_ls"]])||"All"==input[["sex_ls"]]){
      #                     "All"
      #                   } else{
      #                     input[["sex_qc"]]
      #                   }
      #                 })
      #   }else{
      #     sliderInput(paste0(ff,"_ls"),paste(ff),
      #                 min = min(get_oasis()[[ff]]),max=max(get_oasis()[[ff]]),
      #                 value = {if(!is.null(input[[paste0(ff,"_ls")]])){
      #                   input[[paste0(ff,"_ls")]]
      #                 }else{
      #                   c(min(get_oasis()[[ff]]),max(get_oasis()[[ff]]))
      #                 }
      #                 }
      #     )
      #   }
      # ))
    }
    
    x <- append(x,list(selectInput("lasso_variable",label="Explanatory variable",choices = get_explan_names())))
    # if(input$data_type=="Others"){
    #   x <- append(x,list(selectInput("lasso_variable_2",label="Data variable",choices = get_explan_names(),multiple = TRUE)))
    # }
    return(x)
  })
  
  ### get the composite way ###
  output$com_cd <- renderUI({
    tagList(
      prettyRadioButtons("com_way_c",label = "Central tendency",choices = c("mean","median"),inline = TRUE),
      prettyRadioButtons("com_way_d",label = "Dispersion",choices = c("SD","SEM"),selected = character(0),inline = TRUE)
    )
  })
  
  ### ###
  observeEvent(input$com_way_c,{
    output$com_cd <- renderUI({
      tagList(
        prettyRadioButtons("com_way_c",label = "Central tendency",choices = c("mean","median"),selected = input$com_way_c,inline = TRUE),
        prettyRadioButtons("com_way_d",label = "Dispersion",choices = c("SD","SEM"),selected = character(0),inline = TRUE)
      )
    })
    com_cd <<- input$com_way_c
  })
  
  ### ###
  observeEvent(input$com_way_d,{
    output$com_cd <- renderUI({
      tagList(
        prettyRadioButtons("com_way_c",label = "Central tendency",choices = c("mean","median"),selected = character(0),inline = TRUE),
        prettyRadioButtons("com_way_d",label = "Dispersion",choices = c("SD","SEM"),selected = input$com_way_d,inline = TRUE)
      )
    })
    com_cd <<- input$com_way_d
  })
  
  com_cd <<-"mean"
  aus_daten <- reactive({
    input$com_way_c
    input$com_way_d
    switch (as.vector(com_cd),
            "median" = "median",
            "mean" = "mean",
            "SD" = "sd",
            "SEM" =  "sem")
  })
  

  
  ### ###
  output$ds_table<- DT::renderDataTable({
    ausgewaehlte_daten <- get_choice()
    DT::datatable(ausgewaehlte_daten,class = "display nowrap",options = list(scrollX=TRUE))
  })
  
  ### ###
  output$ds_composity <- DT::renderDataTable({
    data <- get_choice()
    cols <- ncol(data)
    data <- data[(cols-147):cols]
    data_mean <-apply(data, 2, mean)
    
    data_median <- apply(data, 2, median)
    data_sd <- apply(data, 2, sd)
    data_sem <- apply(data, 2, sem)
    
    frame_statistics <- tibble("thickness"=names(data),"mean"=data_mean,
                               "median"=data_median,
                               "SD"=data_sd,"SEM"=data_sem)
    frame_statistics[2:4] <- frame_statistics[2:4]%>%mutate_if(is.numeric,round,2)
    frame_statistics[5] <- frame_statistics[5]%>%mutate_if(is.numeric,round,4)
    
    DT::datatable(frame_statistics,options = list(scrolly=TRUE))
  })
  
  ### ###
  output$ss_table<- DT::renderDataTable({
    ausgewaehlte_daten <- get_ss_choice()
    DT::datatable(ausgewaehlte_daten,class = "display nowrap",options = list(scrollX=TRUE))
  })
  
  ### ###
  output$ls_table<- DT::renderDataTable({
    ausgewaehlte_daten <- get_ls_choice()
    DT::datatable(ausgewaehlte_daten,class = "display nowrap",options = list(scrollX=TRUE))
  })
  
  ### color update auto ###
  observeEvent({
    input$com_way_c
    input$com_way_d
    is.null(input$com_way)
    input$sidebarItemExpanded
    lapply(get_fil(), function(x){input[[x]]})
    lapply(get_fil_com(), function(x){input[[paste0(x,"_range")]]})
    input$select_hemisphere
    
  },{
    if(is.null(OASIS)){return()}  
    
    ausgewaehlte_daten <- get_choice()
    cols <- ncol(ausgewaehlte_daten)
    wert<-ausgewaehlte_daten[(cols-147):cols]
    wert <- tibble(apply(wert, 2, aus_daten()))
    
    if(aus_daten()!="sem"){
      wert <- wert%>%mutate_if(is.numeric,round,2)
    }else{
      wert <- wert%>%mutate_if(is.numeric,round,4)
    }
    
    max_wert<-max(wert)
    min_wert<-min(wert)
    updateNumericInput(session,"wert_obergrenze",value = max_wert)
    updateNumericInput(session, inputId = "wert_untergrenze", value = min_wert)
    }
  )
  
  ### control panel tab ###
  observeEvent(input$single_region,{
    if(input$single_region==0){
      hideTab(inputId ="tab",target = "DistributionPlot")
      
    }
    if(input$single_region==1){
      showTab(inputId ="tab",target = "DistributionPlot")
    }
  })
  
  ### ###
  observeEvent(input$com,{
    if(input$com==0){
      hideTab(inputId ="tab",target="Quality Control")
    }
    
    if(input$com==1){
      showTab(inputId ="tab",target="Quality Control")
    }
  })
 
  ### Add new values and colors(UI) ###
  index_selection <- reactiveVal(1)
  observeEvent(input$add_mitte, {
    insertUI(
      selector = "#add_mitte",
      where = "beforeBegin",
      ui = tagList(column(
        12,
        colourInput(inputId = paste("color_mitte", index_selection(), sep = "_"),
                    label = paste("new colors", index_selection()),
                    "black"),
        numericInput(inputId = paste("wert_mitte", index_selection(), sep = "_"),
                     label = paste("new values ", index_selection()),
                     " ")
      )
      ))
    new_index <- index_selection() + 1
    index_selection(new_index)
  })
  
  observeEvent(input$remove_mitte, {
    removeUI(selector = paste0(".col-sm-12:has(#wert_mitte_", index_selection()-1, ")"))
    removeUI(selector = paste0(".col-sm-12:has(#color_mitte_", index_selection()-1, ")"))
    index_selection(index_selection()-1)
    
  })
  
  ### ###
  get_auswahl_data <- reactive({
    auswahl_area <- get_choice()
    cols <- ncol(auswahl_area)
    if(nrow(auswahl_area)==0){
      showModal(modalDialog(title = "INPUT ERROR",
                            "The inputed date is empty",
                            easyClose = TRUE))
      return(NULL)
    }
    
    
    auswahl_area <- auswahl_area[(cols-147):cols]
    if (nrow(get_choice())==1) {
      
    }else if(input$com==0){
      showModal(modalDialog(title = "INPUT ERROR",
                            "The inputed date should be one line or composite display selected",
                            easyClose = TRUE))
      return(NULL)
    }
    else{
      auswahl_area <- apply(auswahl_area, 2, aus_daten())
    }
    
    auswahl_data <- tibble(
      area = as.character(names(auswahl_area)),
      wert = as.numeric(auswahl_area),
      stringsAsFactors = FALSE
    )
    if(aus_daten()!="sem"){
      auswahl_data <- auswahl_data%>%mutate_if(is.numeric,round,2)
      
    }else{
      auswahl_data <- auswahl_data%>%mutate_if(is.numeric,round,4)
    }
    
    auswahl_data[[" "]] <- paste(auswahl_data$area,", Wert ist ",auswahl_data$wert)
    return(auswahl_data)
  })
  
  ### Generate 3D brain map ###
  output$ggseg3d<- renderPlotly({
    input$ab  
    if(input$ab==0)
      return()
    isolate({
      auswahl_data <- get_auswahl_data()
      if (is.null(auswahl_data)) {
        return()
      }
      
      ### new values and colors ###
      
      auswahl_wert <- c(input$wert_obergrenze,input$wert_untergrenze)
      auswahl_color <- c(input$color_obergrenze,input$color_untergrenze)
      if (1<index_selection()) {
        for (i in 1:(index_selection()-1)) {
          auswahl_wert[i+2] <- input[[paste("wert_mitte", i, sep = "_")]]
          auswahl_color[i+2] <- input[[paste("color_mitte", i, sep = "_")]]
        }
      }
      names(auswahl_wert) <- auswahl_color
      
      ### select_hemisphere ###
      auswahl_hemisphere<-input$select_hemisphere
      if(auswahl_hemisphere=="both"){auswahl_hemisphere=c("left","right")}
      
    
      ### plot ggseg3d ###
      
      gg <<- ggseg3d(.data = auswahl_data,
                     atlas = get_altes(),
                     colour = "wert", text = " ",
                     surface = "LCBC",
                     palette = sort(auswahl_wert),
                     hemisphere = auswahl_hemisphere,
                     na.alpha= .5,
                     show.legend = TRUE,
                     options.legend = (colorbar=list(title=list(text="mm")))) %>%
        pan_camera("left lateral") %>% 
        event_register("plotly_relayout") %>% 
        layout(scene=list(camera=list(center=list(z=-0.4))))%>%
        remove_axes()
      
      
      filter_data <<- NULL
      if(input$com==0){
        for (i in input$fil) {
          filter_data <<- paste(filter_data,i,":",input[[i]],"\n",sep=" ")
        }
      }else{
        for (i in input$fil_com) {
          i_range=paste(i,"_range",sep="")
          if(i_range=="sex_range"){
            filter_data <<- paste(filter_data,"sex: ",input[[i_range]],"\n",sep = "")
          }else{
            
            filter_data <<- paste(filter_data,i,": ","[",min(input[[i_range]]),",",max(input[[i_range]]),"]","\n",sep="")
          }
        }
      }
      if(is.null(filter_data)){
        gg
      }else{
        gg%>%layout(annotations=list(visible=TRUE,
                                     text=filter_data,
                                     showarrow=FALSE,
                                     x=0,y=1,
                                     align="left",
                                     font=list(family="Arial",size=13)))
      }
    })
  })
  
  ### ###
  observeEvent(input$ab,{
    updateTabsetPanel(session,"ds_tab",selected = "3D")
  })

  # get_eye <- reactive({
  #   d <- event_data("plotly_relayout")
  #   if(input$ab==0||is.null(d)){
  #     return(NULL)
  #   }else {
  #     return(d$scene.camera[["eye"]])
  #   }
  # })
  
  
  # #p<-output$ggseg3d
  # observeEvent(input$download,{
  #   if(is.null(get_eye())){
  #     if(isFALSE(input$down_filter)){
  #       orca(gg, paste(input$name,input$format, sep = ".", collapse = NULL))
  #     }else{
  #       orca(gg%>%layout(annotations=list(visible=TRUE,text=filter_data,showarrow=FALSE,
  # x=0,y=1,align="left",font=list(family="Arial",size=13))),
  #            paste(input$name,input$format, sep = ".", collapse = NULL))
  #     }
  #   }else{
  #     eye <- get_eye()
  #     scene=list(camera=list(eye=list(x=eye$x,y=eye$y,z=eye$z)))
  #     # gg_eye <- gg%>%layout(scene=scene)
  #     if(isFALSE(input$down_filter)){
  #       orca(gg%>%layout(scene=scene),paste(input$name,input$format, sep = ".", collapse = NULL))
  #     }else{
  #       gg_eye <- gg%>%layout(scene=scene,annotations=list(visible=TRUE,text=filter_data,showarrow=FALSE,
  # x=0,y=1,align="left",font=list(family="Arial",size=13)))
  #       orca(gg_eye,paste(input$name,input$format, sep = ".", collapse = NULL))
  #     }
  #   }
  # })
  
  ### distributionPlot when single person ###
  output$distributionPlot<-renderPlot({
    if(input$single_region==0)
      return()
    # if(input$ab==0)
    #   return()
    if(is.null(input[["ID"]])){}
    isolate({
      region<-input$region
      auswahl_area <- get_choice()
      if(nrow(get_choice())!=1){
        return()
      }
      
      auswahl_region <- input$region
      oasis_data <- get_oasis()[-1:-3]
      selectedData <- oasis_data[auswahl_region]
      colnames(selectedData) <- c("distributionPlot")
      
      selectedData$distributionPlot<-as.numeric(selectedData$distributionPlot)
      
      save<-auswahl_area[[auswahl_region]]
      save<-as.numeric(save)
      
      ggplot(selectedData,
             aes(x = distributionPlot)
      ) + geom_density() + geom_point(aes(save,0),col="red", size=8)
    })
    
  })
  
  ########################################## Quality Control #############################
  ### Sidebar for Quality Control ###
  output$fil_qc <- renderUI({
    
    input$name_file
    tagList(selectInput("qc_fil",label = "Filter", choices = names(get_oasis())[-1], multiple = TRUE))
  })
  
  ### Filter for the Rainbow Plot ###
  output$qc_tabs_filter <- renderUI({
    
    input$name_file
    tagList(selectInput("qc_fil",label = "Filter", choices = names(get_oasis())[-1], multiple = TRUE))
  })
  
  ### Get the selected values
  get_qc_fil <- reactive({
    input$qc_fil
  })
  
  ### Sidebar for Quality Control ###
  output$qc_kon <- renderUI({
    x <- NULL
    for (selected_values in get_qc_fil()) {
      x <- append(x,list(
        render.ui.output(selected_values,get_oasis()[[selected_values]],
                         input[[paste0(selected_values,"_qc")]],ui_type="_qc")
      ))
    }
    # if(input$data_type=="Others"){
    #   x <- append(x,list(
    #     selectInput("qc_col","Selected Cols to RainCloud",selected = NULL,multiple = TRUE,
    #                 choices = c("All",names(get_qc_choice())))
    #   ))
    # }
    return(x)
  })
  
  ### Get the selected values and the selected data
  get_qc_choice <- reactive({   
    col_input <- get_qc_fil()
    u_oasis <- get_oasis()
    
    for (col in col_input) {
      u_oasis <- get.choice(col,data.table = u_oasis,seletedValue = input[[paste0(col,"_qc")]],
                            col.type = "_qc")
    }
    return(u_oasis)
  })
  
  ### Update Tabs after Button Click ###
  observeEvent(input$dp,{
    updateTabsetPanel(session,"qc_tab","Quality Raincloud")
  })
  
  ### Quality Control - Plot after Clicking the Button ###
  output$quality <- renderPlot({
    if(is.null(input$dp) || input$dp==0){return(NULL)}
    
    isolate({
      data <- get_qc_choice()
      cols <- ncol(data)
      if(input$data_type=="FreeSurfer"){
        data <- data[(cols-147):cols]
        name_level <- names(data)
        data <- melt(data)
        names(data) <- c("area","thickness")
        data$lr <- substr(data$area,1,1)
        data[which(data$lr=='l'|data$lr=="L"),]$lr <- "left"
        data[which(data$lr=='r'|data$lr=="R"),]$lr <- "right"
        data$lr <- as.factor(data$lr)
        
        ##format the thickness area names the rearrange the factor levels
        if(!is.null(input$name_file)){
          name_level <- rev(unique(substring(name_level,2)))
          data$area <- factor(x=substring(data$area,2),levels=name_level,ordered = TRUE)
        }else{
          name_level <- rev(unique(substring(name_level,4)))
          data$area <- factor(x=substring(data$area,4),levels=name_level,ordered = TRUE)
        }
        
        p <- ggplot(data,aes(x=area,y=thickness,fill=area))+
          geom_flat_violin(position=position_nudge(x=0.2,y=0),adjust=1,trim = TRUE)+
          geom_point(position = position_jitter(width=.1),size=.2,aes(color=area),show.legend = FALSE)+
          geom_boxplot(aes(x=as.numeric(area)+0.2,y=thickness),outlier.shape = NA,alpha=0.3,width=0.1,color="BLACK")+
          coord_flip()+
          facet_wrap(~lr)+
          theme_cowplot()+
          guides(fill=FALSE)
        
      }else if(input$data_type=="CERES"){
        data <- get_qc_choice()
        qc.cols <- ncol(data)
        qc.data <- data[(qc.cols-273):qc.cols]
        p <- get.ceres.qc.plot(qc.data)
      }#else{
      #  data <- get_qc_choice()
      #  cols <- input$qc_col
      #  p <- get.other.qc.plot(data,cols)
      #}
      return(p)
    })
  })
  
  ### Quality Control - Table ###
  output$qc_table<- DT::renderDataTable({
    ausgewaehlte_daten <- get_qc_choice()
    DT::datatable(ausgewaehlte_daten,class = "display nowrap",options = list(scrollX=TRUE))
  })
  
  ### Mainbody Quality Control ###
  output$qc_tabs <- renderUI({
    tagList(
      tabsetPanel(type="tabs",id="qc_tab",
                  tabPanel("Table",DT::dataTableOutput("qc_table")),
                  if(input$data_type=="FreeSurfer"){
                    tabPanel("Quality Raincloud",plotOutput("quality",height = "7500px",width = "1300px"))
                  }else if(input$data_type=="CERES"){
                    tabPanel("Quality Raincloud",plotOutput("quality",height = "8500px",width = "1500px"))
                  }#else{
                  #  tabPanel("Quality Raincloud",plotOutput("quality",height = paste0(((length(input$qc_col)+1)%/%2)*250,"px"),
                   #                                         width = "1300px"))
                  #}
      )
    )
  })
  
  ########################################################################################
  
  ### ###
  observeEvent(input$rp,{
    updateTabsetPanel(session,"ss_tab","Regression Plots")
  })
  
  ### ###
  observeEvent(input$lp,{
    updateTabsetPanel(session,"ls_tab","Lasso tabel")
  })
  
  ### ###
  output$ss_tabs <- renderUI({
    tagList(
      tabsetPanel(type="tabs",id="ss_tab",
                  tabPanel("Table",DT::dataTableOutput("ss_table")),
                  if(input$data_type=="FreeSurfer"){
                    tabPanel("Regression Plots",plotOutput("regression",height ="40000px",width = "1000px"))
                  }else if(input$data_type=="CERES"){
                    tabPanel("Regression Plots",plotOutput("regression",height = "59000px",width = "1500px"))
                  }else{
                    # tabPanel("Quality Raincloud",plotOutput("quality",height = 
                    #paste0(((length(input$qc_col)+1)%/%2)*250,"px"),width = "1300px"))
                    tabPanel("Regression Plots",plotOutput("regression",height = "500px",width = "500px"))
                  }
      )
    )
  })
  
  ### ###
  output$regression <- renderPlot({
    input$rp
    if(input$rp==0){
      return(0)
    }
    
    isolate({
      data <- get_ss_choice()
      cols <- ncol(data)
      var_explan <- as.character(input$explan)
      if(input$data_type=="FreeSurfer"){
        p <- add_lm_trace_freesurfer(data,var_explan)
      }else if(input$data_type=="CERES"){
        p <- get.ceres.lm.plots(data,var_explan)
      }else{
        p <- ggplot(OASIS,aes_string(x=input$explan,y=input$explan_2))+
          geom_point()+
          stat_smooth(method = lm,level = 0.95)
      }
      return(p)
    })
    
    
  })
  
  ### lasso regression output ###
  observeEvent(input$lp, { 
    dat<-get_ls_choice()
    col.length <- length(dat)
    count_lasso<-which(names(dat)==input$lasso_variable)
    
    if(input$data_type=="FreeSurfer"){
      dat <- select(dat,input$lasso_variable,(col.length-147):col.length)
    }else if(input$data_type=="CERES"){
      dat <- select(dat,input$lasso_variable,(col.length-273):col.length)
    }else{
      dat <- select(dat,input$lasso_variable,input$lasso_variable_2)
    }
    
    lasso.b.all <- lasso_bootstrap(dat,names(dat)[1])
    prop.nonzero.all <- get.proportion.of.nonzero.coeffcients(lasso.b.all[,-1])
    consistent.sign.all <- get.sign.consistency(lasso.b.all[,-1])
    bs.data <<- data.frame(prop.nonzero.all,consistent.sign.all)
    rownames(bs.data) <- colnames(lasso.b.all)[-1]
    label_ExplantoryVariable<-input$lasso_variable
    output$lasso_table<- DT::renderDataTable({
      DT::datatable(bs.data,class = "display nowrap",options = list(scrollX=TRUE),
                    caption = paste("Explantory variable:", label_ExplantoryVariable))
    })
  })
}

shinyApp(ui = ui, server = server)