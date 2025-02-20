library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(reactable)
library(htmltools)
library(openxlsx)

source("functions.R")

# read types for station codes
dftype <- read.table("stn_index_type.txt", sep=";", header=T)

# status class boundaries
dfbnds <- read.table("class_boundaries.txt", sep=";", header=T)

# read species lists for matching with observations
dfgrps  <- read.table("species_lists.txt", sep=";", header=T)

dfgrps <- dfgrps  %>%
  mutate(taxaID=row_number()) %>%
  relocate(taxaID, .before=1) %>%
  mutate(RSLA2=RSLA.1.2) %>%
  rename(RSLA1=RSLA.1.2, RSLA3 = RSLA.3, RSL4=RSL.4) %>% 
  relocate(RSLA2, .after=RSLA1)


function(input, output, session) {
  
  options <- get_options()
  
  xl_sheets <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext %in% c("xlsx","xlsm","xls"), "Please select an Excel file to read"))
    
    progress <- Progress$new(session, min=1, max=10)
    on.exit(progress$close())
    
    progress$set(message = 'Reading Excel data',
                 detail = "shouldn't take long...")
    
    list_df <- read_excel_all(file$datapath, progress)
        
    return(list_df)
  })
  
  sheet_initial <- reactive({
    req(xl_sheets())
    default <- default_sheet() 
    sheets <- names(xl_sheets())
    sheets <- sheets[tolower(sheets)==tolower(default)]
    
    if(length(sheets)>0){
      return(sheets)
    }else{
      return(NULL)
    }   

  })
  
  xl_names <- reactive({
    req(xl_sheets())
    sheet_names <- names(xl_sheets())
    return(sheet_names)
  })
  
  xl_data <- reactive({
    req(xl_sheets())
    req(input$selectSheet)
    df_list <- xl_sheets()
    return(df_list[[input$selectSheet]])
  })
  
  sheet_options <- reactive({
    req(xl_names())
    req(input$selectSheet)
    df_list <- xl_sheets()
    if("Instillinger" %in% xl_names()){
      return(df_list[["Instillinger"]])
    }else{
      return(defaults())
    }
  })
  
  station_data <- reactive({
    req(xl_data())
    return(NULL)
  })
  
  
  stations_ok <- reactive({
    req(xl_data())
    req(sheet_options())

    df <- xl_data()
    
    check_rows <- c("Prosjekt",
                    "Stasjonskode",
                    "Gammel kode",
                    "Stasjonsnavn",
                    "Dato (tid)")
    
    # row_names <- df[,1]
    row_names <- df %>% purrr::pluck(1)
    row_names <- row_names[tolower(row_names) %in% tolower(check_rows)]
    
    if(length(row_names)<length(check_rows)){
      ok = FALSE
      msg <- paste0(input$selectSheet, 
                    ": does not appear to be a macroalgae observation sheet. Check your input selection")
    }else{
      ok = TRUE
      msg <- "Station data OK"
    }
    return(list(ok=ok, msg=msg))
  })
  
  observe({
    xl_data()
    accordion_panel_open("setup","Stations")
  })
  
  output$selectSheet <- renderUI(
    tagList(selectInput(
      "selectSheet", 
      "Select sheet:", 
      choices = xl_names(),
      selected = sheet_initial(),
      selectize = T
      ))
  )
  
  output$station_warning <- renderUI({
    ok <- stations_ok()[["ok"]]
    msg <- stations_ok()[["msg"]]
    if(ok){
      return(
        tagList(
          div()
        )
      )
    }else{
      return(
        tagList(
          div(style="color:red", msg)
        )
      )
    }
    })
  
  
  stations <- reactive({
    req(stations_ok())
    req(xl_data())
    if(stations_ok()$ok){
      return(observation_info(xl_data(), options))
    }else{
      return(NULL) #dfobs) #data.frame())
    }
  })
  
  shore_types <- reactive({
    req(stations_ok())
    req(xl_data())
    req(stations)
    if(stations_ok()$ok){
      return(observation_info(xl_data(), options))
    }else{
      return(NULL)
    }
  })
  
  

  output$stations <- renderReactable({
    req(stations_ok())
    req(stations())
    
    #  this is to call index calculations before loading the tab
    observe({df_indices()})
    
    df <- stations()
    if(!is.null(df)){
          if(ncol(df)>0){
      if("date" %in% names(df)){
        df <- df %>%
          mutate(date=format.POSIXct(date)) 
      }
    }else{
      df <- data.frame(project=c(), stn_code=c(), old_code=c(), stn_name=c(), date=c(), observer=c())
    }
    
      reactable(df,
                sortable = F,
                selection = "single", 
                onClick = "select",
                style = list(fontSize = "0.8rem"),
                columns = list(
                  col = colDef(show=F),
                  project = colDef(show=T, name = options$station$project$row_name, width = 100),
                  stn_code = colDef(show=T, name = options$station$stn_code$row_name, width = 100),
                  old_code = colDef(show=F, name = options$station$old_code$row_name),
                  stn_name = colDef(show=T, name = options$station$stn_name$row_name),
                  date = colDef(show=T, name = options$station$date$row_name, width = 160),
                  observer = colDef(show=T, name = options$station$observer$row_name, width = 120),
                  points = colDef(show = T, name ="Poeng", width = 70),
                  f = colDef(show = T, name ="F", width = 50),
                  .selection = colDef(show=T)
                  
                ), # columns
                defaultColDef = colDef(minWidth = 150, show=F, vAlign = "bottom"),
                compact = TRUE,
                wrap = FALSE,
                fullWidth = FALSE,
                resizable = TRUE,
                bordered = TRUE,
                defaultPageSize = 15,
                highlight = TRUE,
                theme = reactableTheme(
                  headerStyle = list(background = "#f7f7f8"),
                  rowSelectedStyle=list(backgroundColor = "#c0d6e4", color = "#000"),
                  cellPadding = "3px 1px"
                )
      ) # reactable
    }else{
      return(NULL)
    }    
  })
  
  
  stn_points <- reactive({
    req(stations_ok())
    req(stations())
    selected <- getReactableState("stations", "selected")

    df <- station_points( stations(), selected, options)
  
  })
  
  
  output$points_table <- renderReactable({
    req(stations_ok())
    req(stations())
    req(stn_points())
    
    df <- stn_points()
    
    if(is.null(df)){
      return(NULL)
    }else{
      reactable(df,
                sortable = F,
                style = list(fontSize = "0.8rem"),
                columns = list(
                  Parameter = colDef(show=F),
                  group = colDef(show=T, name="Gruppe"),
                  name = colDef(show=T, name="Karakteristik",),
                  Points = colDef(name="Poeng", show=T, aggregate = "sum", width=50)
                ), # columns
                defaultColDef = colDef(minWidth = 220, show=F, vAlign = "bottom"),
                compact = TRUE,
                wrap = FALSE,
                fullWidth = FALSE,
                resizable = TRUE,
                bordered = TRUE,
                defaultPageSize = 100,
                highlight = TRUE,
                groupBy = "group",
                theme = reactableTheme(
                  headerStyle = list(background = "#f7f7f8"),
                  cellPadding = "3px 1px"
                ))
    }
  })
  
  output$row_selected <- renderText({
    selected <- getReactableState("stations", "selected")
    paste0("selected: ", input$stations_rows_selected)
  })
  
  
  obs_data <- reactive({
    
    req(stations_ok())
    req(stations())
    
    df <- species_data(xl_data(), stations(), options)
  
    return(df)
  })
  
  output$observations <-renderReactable({
    
    req(stations_ok())
    req(stations())
    req(obs_data())
    
    df <- obs_data()
    
    if(is.null(df)){
      return(NULL)
    }else{
      reactable(df,
                sortable = F,
                style = list(fontSize = "0.8rem"),
                columns = list(
                  Kode = colDef(width = 60),
                  CF = colDef(width = 30),
                  SP = colDef(width = 30),
                  NB = colDef(width = 30),
                  Navn = colDef(width = 300)
                ), # columns
                defaultColDef = colDef(minWidth = 55, show=T, vAlign = "bottom"),
                compact = TRUE,
                wrap = FALSE,
                fullWidth = T,
                resizable = TRUE,
                bordered = TRUE,
                defaultPageSize = 999,
                highlight = TRUE,
                theme = reactableTheme(
                  headerStyle = list(background = "#f7f7f8"),
                  cellPadding = "1px 1px"
                ))
    }
  })
  
  output$btnDownloadInds <- downloadHandler(
    
    filename = function() {
      paste0("download ",format.Date(Sys.time(), 
                                     "%Y%m%d_%H%M%S"), ".xlsx")
      
      
      
    },
    content = function(file) {
      
      progress <- Progress$new(session, min=1, max=10)
      on.exit(progress$close())
      
      progress$set(message = 'Preparing download',
                   detail = "please wait...")

      wb<- excel_results(df_indices(),
                          matched_obs(),
                         results_eqr_tab())
      
      saveWorkbook(wb, file = file, overwrite = TRUE)
      
    }
  )
  
  
  matched_obs <- reactive({
    req(stations_ok())
  
    
    progress <- Progress$new(session, min=1, max=10)
    on.exit(progress$close())
    
    progress$set(message = 'Matching species',
                 detail = "please wait...")
    # match observations with species lists
    df <- match_obs_species_lists(stations(), obs_data(), dfgrps, dftype, progress)
    return(df)
  })
  
  matched_obs_stn_select <- reactive({
    req(stations_ok())
    req(matched_obs())
    req(df_indices())
    
    df <- matched_obs()
    
    selected <- getReactableState("tbl_indices", "selected")
    
    if(is.null(selected)){
      return(NULL)
    }else{
      stn_select <- df_indices()$stn_code
      stn_select <- stn_select[selected]
      df <- df %>%
        filter(stn_code == stn_select) %>%
        filter(!is.na(value))
      return(df)
    }
  })
  
  
  obs_counts <- reactive({
    
    df <- obs_counts(matched_obs())
    
  })
  
  output$matched_obs <-renderReactable({
    
    req(stations_ok())
   
    
    df <- matched_obs_stn_select()
    
    if(is.null(df)){
      return(NULL)
    }else{
      reactable(df,
                filterable = TRUE, minRows = 10,
                sortable = T,
                style = list(fontSize = "0.8rem"),
                #width = 1100,
                columns = list(
                  stn_code = colDef(name=options$station$stn_name$row_name, width=60),
                  index = colDef(name="Index", width=60),
                  Group = colDef(name="Gruppe", width=60),
                  taxaID = colDef(name="TaxaID", show = F),
                  Opport. = colDef(width=60),
                  ESG = colDef(width=60),
                  TAXA = colDef(name="Taxa liste", width=250),
                  Kode = colDef(width=70),
                  CF = colDef(width=30),
                  SP = colDef(width=30),
                  NB = colDef(width=30),
                  Navn = colDef(width=200),
                  value = colDef(name="1-6", width=40)
                ), # columns
                defaultColDef = colDef(minWidth = 15, show=T, vAlign = "bottom"),
                compact = TRUE,
                wrap = FALSE,
                fullWidth = FALSE,
                resizable = TRUE,
                bordered = TRUE,
                defaultPageSize = 20,
                highlight = TRUE,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(10, 20, 50),
                theme = reactableTheme(
                  headerStyle = list(background = "#f7f7f8"),
                  cellPadding = "1px 1px"
                ))
    }
  })
  
  
  df_indices <- reactive({
    req(stations_ok())
  
    df <- obs_indices(matched_obs(), stations())
    
    return(df)
  })
  
  output$tbl_indices <-renderReactable({
    
    req(stations_ok())
    #req(stations())
    #req(obs_data())
    #req(matched_obs())
    
    df <- df_indices()
    
    if(is.null(df)){
      return(NULL)
    }else{
      df <- df %>%
        relocate(nG, .before="nB")
      
      reactable(df,
                selection = "single", 
                onClick = "select",
                sortable = F,
                style = list(fontSize = "0.8rem"),
                #width = 1100,
                columns = list(
                  stn_code = colDef(name=options$station$stn_name$row_name, width=60),
                  index = colDef(name="Index", width=55),
                  points = colDef(name="Poeng", width=55),
                  f = colDef(name="f", width=65, format = colFormat(digits = 2)),
                  n_total = colDef(name="N", width=50),
                  nG = colDef(name="Grøn", width=50),
                  nB = colDef(name="Brun", width=50),
                  nR = colDef(name="Rød", width=50),
                  nESG1 = colDef(name="ESG1", width=50),
                  nESG2 = colDef(name="ESG2", width=50),
                  n_opp = colDef(name="Opp.", width=50),
                  pctG= colDef(name="%Gr", width=55, format = colFormat(digits = 1)),
                  pctR= colDef(name="%Rd", width=55, format = colFormat(digits = 1)),
                  pctB= colDef(name="%Br", width=55, format = colFormat(digits = 1)),
                  n_norm = colDef(name="f.N", width=55, format = colFormat(digits = 1)),
                  ESG12 = colDef(name="ESG 1:2", width=65, format = colFormat(digits = 3)),
                  pctOpp= colDef(name="%opp", width=55, format = colFormat(digits = 1)),
                  sumB = colDef(name="sum B", width=55, format = colFormat(digits = 1)),
                  sumG = colDef(name="sum G", width=55, format = colFormat(digits = 1))
                  
                ), # columns
                defaultColDef = colDef(minWidth = 55, show=T, vAlign = "bottom"),
                compact = TRUE,
                wrap = FALSE,
                fullWidth = FALSE,
                resizable = TRUE,
                bordered = TRUE,
                defaultPageSize = 20,
                highlight = TRUE,
                theme = reactableTheme(
                  headerStyle = list(background = "#f7f7f8"),
                  cellPadding = "1px 1px"
                ))
    }
  })
  
  output$comment_indices <- renderUI({
    req(stations_ok())
    df <- df_indices()
    if(is.null(df)){
      return(NULL) 
    }else{
      msg <- "All indices are calculated for all stations, even though some are not included in EQR calculations."
      tagList(
        div(style="font-size: 0.8rem", msg)
      )
    }
      })
  
  results_eqr <- reactive({
    req(stations_ok())
    df <- df_indices()
    if(is.null(df)){
      return(NULL)
    }else{
      dfeqr <- eqr_results(df, dfbnds)
      return(dfeqr)
    }
  })
  
  results_eqr_stn <- reactive({
    req(stations_ok())
    df <- results_eqr()
    
    if(is.null(df)){
      return(NULL)
    }else{
      dfeqr <- eqr_results_mean(df)
      return(dfeqr)
    }
  })
  
  results_eqr_tab <- reactive({
    req(stations_ok())
    
    df <- results_eqr()
    dfstn <- results_eqr_stn()
    
    if(is.null(dfstn)){
      return(NULL)
    }else{
        dfeqr <- eqr_for_table(df, dfstn)
        return(dfeqr)
    }
  })
  
  
  output$tbl_eqr <-renderReactable({
    
    req(stations_ok())
    
    df <- results_eqr_tab()
    
    if(is.null(df)){
      return(NULL)
    }else{
      df <- df %>%
        mutate(classID=ifelse(is.na(classID),0,classID))
      reactable(df,
                sortable = F,
                style = list(fontSize = "0.8rem"),
                #width = 1100,
                filterable = TRUE,
                ,
                rowStyle = JS("function(rowInfo) {
                                          const value = rowInfo.values['calc']
                                          if (value == 'EQR') {
                                            return { fontWeight: 'bold' }
                                          } 
                                        }"),
                
                columns = list(
                  index = colDef(name="Index", width=55, show=T, 
                                 filterInput = function(values, name) {
                                   tags$select(
                                     # Set to undefined to clear the filter
                                     onchange = sprintf("Reactable.setFilter('tbl_eqr', '%s', event.target.value || undefined)", name),
                                     # "All" has an empty value to clear the filter, and is the default option
                                     tags$option(value = "", "All"),
                                     lapply(unique(values), tags$option),
                                     "aria-label" = sprintf("Filter %s", name),
                                     style = "width: 100%; height: 24px;"
                                   )}),
                  stn_code = colDef(name=options$station$stn_name$row_name, width=110, show=T, 
                                    filterInput = function(values, name) {
                                      tags$select(
                      # Set to undefined to clear the filter
                      onchange = sprintf("Reactable.setFilter('tbl_eqr', '%s', event.target.value || undefined)", name),
                      # "All" has an empty value to clear the filter, and is the default option
                      tags$option(value = "", "All"),
                      lapply(unique(values), tags$option),
                      "aria-label" = sprintf("Filter %s", name),
                      style = "width: 100%; height: 24px;"
                    )
                  }),
                  calc = colDef(show=F),
                  description = colDef(name="Parameter", width=250, filterable = F),
                  value = colDef(name="Verdi", format = colFormat(digits=1), width=60, filterable = F),
                  eqr00 = colDef(name="0.0", format = colFormat(digits=1), width=50, filterable = F),
                  eqr02 = colDef(name="0.2", format = colFormat(digits=1), width=50, filterable = F),
                  eqr04 = colDef(name="0.4", format = colFormat(digits=1), width=50, filterable = F),
                  eqr06 = colDef(name="0.6", format = colFormat(digits=1), width=50, filterable = F),
                  eqr08 = colDef(name="0.8", format = colFormat(digits=1), width=50, filterable = F),
                  eqr10 = colDef(name="1.0", format = colFormat(digits=1), width=50, filterable = F),
                  EQR = colDef(format = colFormat(digits=3), width=80, filterable = F),
                  classID = colDef(show=F),
                  Class = colDef(name="Status", width=80, filterable = F,
                                 style = JS("function(rowInfo) {
                                          const value = rowInfo.values['classID']
                                          let color
                                          if (value < 1) {
                                            color = '#ffffff'
                                          } else if (value < 2) {
                                            color = '#ff0000'
                                          } else if (value < 3) {
                                            color = '#ffc000'
                                          } else if (value < 4) {
                                            color = '#ffff00'
                                          } else if (value < 5) {
                                            color = '#92d050'
                                          } else {
                                            color = '#00b0f0'
                                          }
                                          return { backgroundColor: color }
                                          }")
                  ),
                  note = colDef(name="Kommentar", width=180, filterable = F)
                ), # columns
                columnGroups = list(
                  colGroup(name = "EQR grenser", columns = c("eqr00", "eqr02", "eqr04", "eqr06", "eqr08", "eqr10")),
                  colGroup(name = "Beregning", columns = c("description","value")),
                  colGroup(name = "Resultat", columns = c("EQR","Class"))
                ),
                defaultColDef = colDef(minWidth = 55, show=T, vAlign = "bottom"),
                compact = TRUE,
                wrap = FALSE,
                fullWidth = FALSE,
                resizable = TRUE,
                bordered = TRUE,
                defaultPageSize = 100,
                highlight = TRUE,
                theme = reactableTheme(
                  headerStyle = list(background = "#f7f7f8"),
                  cellPadding = "1px 1px"
                ))
    }
  })

}
