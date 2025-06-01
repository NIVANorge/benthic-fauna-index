options(shiny.reactlog = TRUE)
shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "benthicfauna-cache")))

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(reactable)
library(htmltools)
library(openxlsx)
library(ambiR)
library(stringr)
library(shinyjs)
library(ggplot2)

source("functions.R")


table_font <- "0.7rem"

# status class boundaries
dfbnds <- read.table("class_boundaries.txt", sep=";", header=T)

function(input, output, session) {

  # possible input structures
  label_long <- "long (DB)"
  label_wide_species <-  "wide (species in columns)"
  label_wide_station <-  "wide (stations in columns)"

  vals <- reactiveValues()

  # --------------  xl_sheets() ------------------------

  xl_sheets <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext %in% c("xlsx","xlsm","xls"), "Please select an Excel file to read"))

    progress <- Progress$new(session, min=1, max=10)
    on.exit(progress$close())

    progress$set(message = 'Reading Excel data',
                 detail = "shouldn't take long...")

    list_df <- read_excel_all(file$datapath, F, progress)

    return(list_df)
  })


  # --------------  xl_sheets_ambi() ------------------------

  xl_sheets_ambi <- reactive({
    file_ambi <- input$file_ambi
    ext <- tools::file_ext(file_ambi$datapath)

    req(file_ambi)
    validate(need(ext %in% c("xlsx","xlsm","xls"), "Please select an Excel file to read"))

    progress <- Progress$new(session, min=1, max=10)
    on.exit(progress$close())

    progress$set(message = 'Reading Excel data',
                 detail = "shouldn't take long...")

    list_df <- read_excel_all(file_ambi$datapath, header=T, progress)

    return(list_df)
  })


  # --------------  xl_names() ------------------------

  xl_names <- reactive({
    req(xl_sheets())
    sheet_names <- names(xl_sheets())
    return(sheet_names)
  })


  # --------------  xl_names_ambi() ------------------------

  xl_names_ambi <- reactive({
    req(xl_sheets_ambi())
    sheet_names <- names(xl_sheets_ambi())
    return(sheet_names)
  })


  # --------------  xl_data() ------------------------

  xl_data <- reactive({
    req(xl_sheets())
    req(input$selectedSheet)
    df_list <- xl_sheets()
    return(df_list[[input$selectedSheet]])
  })


  # --------------  xl_data_ambi() ------------------------

  xl_data_ambi <- reactive({
    req(xl_sheets_ambi())
    req(input$selectedSheet_ambi)
    df_list <- xl_sheets_ambi()
    return(df_list[[input$selectedSheet_ambi]])
  })


  # --------------  sheet_options() ------------------------

  sheet_options <- reactive({
    req(xl_names())
    req(input$selectedSheet)
    df_list <- xl_sheets()
    if("Instillinger" %in% xl_names()){
      return(df_list[["Instillinger"]])
    }else{
      return(defaults())
    }
  })


  # --------------  station_column() ------------------------

  station_data <- reactive({
    req(xl_data())
    req(stations_ok())

    ok <- stations_ok()[["ok"]]

    df <- xl_data()
    if(ok){
      df <- df %>%
        distinct(across(all_of(station_column())))
      return(df)
    }else{
      return(NULL)
    }
  })


  # --------------  station_column() ------------------------

  station_column <- reactive({
    req(xl_data())
    df <- xl_data()

    stncol <- ""
    colnames <- names(df)
    for(x in c("stn", "station")){
      for(y in colnames){
        if(stringr::str_detect(tolower(y),x)){
          stncol <- y
        }
      }
    }
    return(stncol)
  })

  # --------------  data_structures() ------------------------

  data_structures <- reactive({
    req(xl_names())
    c(label_long,
      label_wide_station,
      label_wide_species)
  })

  # --------------  stations_ok() ------------------------

  stations_ok <- reactive({
    req(station_column())

    if(station_column()==""){
      ok = FALSE
      msg <- paste0(input$selectedSheet,
                    ": does not appear to contain stations IDs. Check your input selection")
    }else{
      ok = TRUE
      msg <- "Stations OK"
    }
    return(list(ok=ok, msg=msg))
  })


  # --------------  observe(input$selectForm) ------------------------

  observe({
    req(input$selectForm)
    xl_data()
    #accordion_panel_open("setup","Stations")
    accordion_panel_open("setup","Observations")
    accordion_panel_open("setup","Columns and rows")

    accordion_panel_remove(
      id="setup2",
      "Observations transposed"
    )

    if(input$selectForm != label_long){
      #
    #}else{
      accordion_panel_insert(
        id="setup2",
        panel= accordion_panel(
          id="panel_obs_transp",
          title = "Observations transposed",
          icon = bsicons::bs_icon("file-earmark-spreadsheet"),
          uiOutput("obs_warning") ,
          reactableOutput("observations")
          ),
        target = "panel_obs",
        position = "after"
      )

    }

  })

  # --------------  output$selectSheet (Observations input) ------------------------

  output$selectSheet <- renderUI({
    req(xl_names())
    tagList(selectInput(
      "selectedSheet",
      "Select sheet:",
      choices = xl_names(),
      #selected = sheet_initial(),
      selectize = T
      ))
  })

  # --------------  output$selectSheetAMBI (AMBI input) ------------------------

  output$selectSheetAMBI <- renderUI({
    req(xl_names_ambi())
    tagList(selectInput(
      "selectedSheet_ambi",
      "Select sheet:",
      choices = xl_names_ambi(),
      #selected = sheet_initial(),
      selectize = T
    ))
  })

  # --------------  output$selectStructure (Observations input) ------------------------

  output$selectStructure <- renderUI({
    req(xl_data())
    guess_form <- guess_structure(xl_data())

    req(input$selectedSheet)
    tagList(selectInput(
      "selectForm",
      "Select data layout:",
      choices = data_structures(),
      selected = guess_form,
      selectize = T
    ))
 })

  # --------------  output$checkHeader (Observations input) ------------------------

  output$checkHeader <- renderUI({
    req(input$selectForm)
    #req(sheet_rows())
    #req(sheet_columns())
    # if(is.null(vals$header_check)){
    #   chk_val <- TRUE
    # }else{
    #   chk_val <- vals$header_check
    # }

    if(input$selectForm==label_long){
    tagList(checkboxInput(
      "hasHeader",
      "has column names",
      value = TRUE
    ))}else{
      # tagList(checkboxInput(
      #   "hasHeader",
      #   "has column names",
      #   FALSE
      # ))
      NULL
    }
  })

  # --------------  output$selectColumnRowGroup (Observations input) ------------------------

  output$selectColumnRowGroup <- renderUI({
    req(sheet_rows())
    req(sheet_columns())

    if(input$selectForm==label_wide_station){
      res <- NULL
    }else{
      val_choices <- sheet_columns()
      if(input$selectForm==label_wide_species){
        #val_sel <- val_choices[1]
        val_sel <- "none"
        val_choices <- c("none",val_choices)
      }else{
        #val_sel <- match_list(val_choices, "ty")
        val_sel <- "none"
        val_choices <- c("none",val_choices)
      }

      res <- tagList(selectInput(
        "colrowGroup",
        "M-AMBI type column:",
        choices = val_choices,
        selectize = T,
        selected = val_sel
      ))
    }

  })

  # --------------  output$selectColumnRowStn (Observations input) ------------------------

  output$selectColumnRowStn <- renderUI({
    req(sheet_rows())
    req(sheet_columns())

    if(input$selectForm==label_wide_station){
      val_choices <- sheet_rows()
      val_sel <- val_choices[1]
      val_choices <- c("none",val_choices)
      res <- tagList(selectInput(
        "colrowStn",
        "Station row:",
        choices = val_choices,
        selected = val_sel,
        selectize = F
      ))
    }else{
      val_choices <- sheet_columns()
      if(input$selectForm==label_wide_species){
        val_sel <- val_choices[1]
        val_choices <- c("none",val_choices)
      }else{
        val_sel <- match_list(val_choices, "st")
        val_choices <- c("none",val_choices)
      }

      res <- tagList(selectInput(
        "colrowStn",
        "Station column:",
        choices = val_choices,
        selectize = T,
        selected = val_sel
      ))
    }

})

  # --------------  output$selectColumnRowRep (Observations input) ------------------------

  output$selectColumnRowRep <- renderUI({
    req(sheet_rows())
    req(sheet_columns())

    if(input$selectForm==label_wide_station){
      val_choices <- sheet_rows()
      res <- tagList(selectInput(
        "colrowRep",
        "Replicates row:",
        choices = c("none", val_choices),
        selectize = F
      ))
    }else{
      val_choices <- sheet_columns()
      if(input$selectForm==label_wide_station){
        val_sel <- val_choices[1]
      }else{
        val_sel <- match_list(val_choices, "rep")
        val_choices <- c("none", val_choices)
      }

      res <- tagList(selectInput(
        "colrowRep",
        "Replicates column:",
        choices = val_choices,
        selectize = T,
        selected = val_sel
      ))
    }
    return(res)
})

  # --------------  output$selectColumnRowSpecies (Observations input) ------------------------

  output$selectColumnRowSpecies <- renderUI({
    req(sheet_rows())
    req(sheet_columns())

    if(input$selectForm==label_wide_species){
      val_choices <- sheet_rows()
      val_sel <- val_choices[1]
      res <-  tagList(selectInput(
        "colrowSpec",
        "Species row:",
        choices = val_choices,
        selectize = F,
        selected = val_sel
      ))
    }else{
      val_choices <- sheet_columns()
      if(input$selectForm==label_wide_station){
        val_sel <- val_choices[1]
      }else{
        val_sel <- match_list(val_choices, "sp")
      }
      res <- tagList(selectInput(
        "colrowSpec",
        "Species column:",
        choices = val_choices,
        selectize = T,
        selected = val_sel
      ))
    }
    return(res)
  })

  # --------------  output$selectColumnRowCount (Observations input) ------------------------

  output$selectColumnRowCount <- renderUI({
    req(sheet_rows())
    req(sheet_columns())

    if(is.null(input$colrowSpec)){
      return(NULL)
    }

    if(input$selectForm==label_long){
      val_choices <- sheet_columns()
      val_choices <- val_choices[val_choices != input$colrowSpec]
      val_sel <- match_list(val_choices, c("count","ab","pop"))

      res <- tagList(selectInput(
        "colrowCount",
        "Abundance/count column:",
        choices = sheet_columns(),
        selectize = T,
        selected = val_sel
      ))
    }else{
      res <- NULL
    }
    return(res)
  })

  # --------------  output$selectColumnStnAMBI (AMBI input) ------------------------

  output$selectColumnStnAMBI <- renderUI({



      val_choices <- sheet_columns_ambi()
      val_sel <- match_list(val_choices, "st")
      val_choices <- c("none",val_choices)

      res <- tagList(selectInput(
        "colStnAMBI",
        "Station column:",
        choices = val_choices,
        selectize = T,
        selected = val_sel
      ))

  })

  # --------------  output$selectColumnGroupAMBI (AMBI input) ------------------------

  output$selectColumnGroupAMBI <- renderUI({

    val_choices <- sheet_columns_ambi()
   #val_sel <- match_list(val_choices, "st")
    val_sel <- "none"
    val_choices <- c("none",val_choices)

    res <- tagList(selectInput(
      "colGroupAMBI",
      "M-AMBI type column:",
      choices = val_choices,
      selectize = T,
      selected = val_sel
    ))

  })

  # --------------  output$selectAMBIsource (AMBI input) ------------------------

  output$selectAMBIsource <- renderUI({

    source_names <- c("current calculations",
      "load AMBI results from file")
    source_ids <- c(1,2)


    res <- tagList(radioButtons(
      inputId="radioAMBI",
      label=NULL,
      choiceNames = source_names,
      choiceValues = source_ids,
      selected = source_ids[1]
    ))
  })

  # --------------  observe(input$selectAMBIsource) ------------------------

  observe({
    req(input$radioAMBI)
    accordion_panel_remove(
      id="acc_mambi_input",
      "panel_mambi_import"
    )

    if(input$radioAMBI == "2"){
      #
      #}else{
      accordion_panel_insert(
        id="acc_mambi_input",
        panel= accordion_panel(
          value = "panel_mambi_import",
          title = "Import AMBI results",
          icon = bsicons::bs_icon("file-earmark-spreadsheet"),
          verticalLayout(
            fileInput("file_ambi",
                      "Select AMBI results file",
                      accept=c(".xlsx",".xls",".xlsm"),
                      width='100%'),
            uiOutput("selectSheetAMBI"),
            uiOutput("selectColumnStnAMBI"),
            uiOutput("selectColumnAMBI"),
            uiOutput("selectColumnH"),
            uiOutput("selectColumnS"),
            uiOutput("selectColumnGroupAMBI"))
        ),
        target = "panel_mambi_source",
        position = "after"
      )

      accordion_panel_open("acc_mambi_input","panel_mambi_import")

    }

  })


  # --------------  output$selectColumnAMBI (AMBI input) ------------------------

  output$selectColumnAMBI <- renderUI({


    req(sheet_columns_ambi())

    val_choices <- sheet_columns_ambi()
    val_sel <- match_list(val_choices, "ambi")
    res <- tagList(selectInput(
      "colAMBI",
      "AMBI column:",
      choices = val_choices,
      selectize = T,
      selected = val_sel
    ))
  })

  # --------------  output$selectColumnH (AMBI input) ------------------------

  output$selectColumnH <- renderUI({

    req(sheet_columns_ambi())
    val_choices <- sheet_columns_ambi()
    val_sel <- match_list(val_choices, "h")
    res <- tagList(selectInput(
      "colH",
      "H column:",
      choices = val_choices,
      selectize = T,
      selected = val_sel
    ))
  })

  # --------------  output$selectColumnS ------------------------

  output$selectColumnS <- renderUI({


    req(sheet_columns_ambi())
    val_choices <- sheet_columns_ambi()
    val_stn <- match_list(val_choices, "st")
    val_choices2 <- val_choices[!val_choices == val_stn]
    val_sel <- match_list(val_choices2, "s")
    res <- tagList(selectInput(
      "colS",
      "S column:",
      choices = val_choices,
      selectize = T,
      selected = val_sel
    ))
  })


  # --------------  sheet_columns_ambi() ------------------------

  sheet_columns_ambi <- reactive({
    req(xl_data_ambi())
    names <- names(xl_data_ambi())
    return(names)
  })

  # --------------  output$obs_warning ------------------------

  output$obs_warning <- renderUI({

    req(obs_data())
    n <- obs_data()[["dropped"]]
    msg <- obs_data()[["msg"]]
    if(is.null(n)){
      n <- 0
    }
    msg <- paste0(n," values dropped (", msg, "). Check your input selections!")

    if(n==0){
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





  # --------------  sheet_columns() ------------------------

  sheet_columns <- reactive({

    df <- req(obs_data_raw())
    use_head <- ifelse(is.null(input$hasHeader),
                       TRUE,
                       input$hasHeader)

    if(use_head){
      cols <- names(df)
      if(input$selectForm!=label_long){
        names(cols) <- as.character(1:length(cols))
      }

    }else{
      cols <- 1:ncol(df)
    }
    #cols <- c("none", cols)
    return(cols)
  })

  # --------------  sheet_rows() ------------------------

  sheet_rows <- reactive({

    df <- req(obs_data_raw())
    use_head <- ifelse(is.null(input$hasHeader),
                       TRUE,
                       input$hasHeader)

    if(use_head){
      rows <- 1:(nrow(df)-1)
    }else{
      rows <- 1:nrow(df)
    }
    #rows <- c("none", rows)
    return(rows)
  })

  # --------------  obs_data_raw() ------------------------

  obs_data_raw <- reactive({

    req(input$selectForm)
    req(xl_data())
    df <- xl_data()

    if(input$selectForm==label_long){
      use_col_names <- ifelse(is.null(input$hasHeader),
                              FALSE,
                              input$hasHeader)
    }else{
      use_col_names <- FALSE
    }

    if(use_col_names){
      columnnames <- df[1,] %>% unlist()
      columnnames <- fix_column_names(columnnames)
      names(df) <- columnnames
      df <- df[2:nrow(df),]
    }

    return(df)

  })


  # --------------  output$observationsraw ------------------------

  output$observationsraw <-renderReactable({

    #req(stations_ok())
    #req(stations())
    req(obs_data_raw())

    df <- obs_data_raw()

    show_head <- isolate(input$selectForm)

    if(is.null(show_head)){
      col_default <- colDef(minWidth = 55, show=T, vAlign = "bottom")
    }else{
      if(isolate(input$selectForm)==label_long){
        col_default <- colDef(minWidth = 55, show=T, vAlign = "bottom")
      }else{
        col_default <- colDef(name="", minWidth = 55, show=T, vAlign = "bottom")
      }
    }

    if(is.null(df)){
      return(NULL)
    }else{
      reactable(df,
                sortable = F,
                style = list(fontSize = table_font),
                columns = list(
                ), # columns
                defaultColDef = col_default,
                compact = TRUE,
                wrap = FALSE,
                fullWidth = T,
                resizable = TRUE,
                bordered = TRUE,
                defaultPageSize = 10,
                highlight = TRUE,
                theme = reactableTheme(
                  headerStyle = list(background = "#f7f7f8"),
                  cellPadding = "1px 1px"
                ))
    }
  })



  # --------------  obs_data() ------------------------

  obs_data <- reactive({

    req(input$selectedSheet)
    req(obs_data_raw())
    req(input$selectForm)
    req(input$colrowSpec)
    #req(check_header())

    df <- obs_data_raw()

    form <- input$selectForm
    idStn <- input$colrowStn
    idRep <- input$colrowRep
    idSpec <- input$colrowSpec
    idCount <- input$colrowCount
    idGrp <- input$colrowGroup

    idGrp <- ifelse(is.null(idGrp),"none",idGrp)
    has_header <- input$hasHeader
    if(input$selectForm!=label_long){
      has_header <- F
    }

    progress <- Progress$new(session, min=1, max=100)
    on.exit(progress$close())

    progress$set(message = 'Reorganizing data',
                 detail = "shouldn't take long...")

    df <- reform_data(df, form,
                      idStn, idRep, idSpec, idCount, idGrp,
                      label_long,
                      label_wide_species,
                      label_wide_station,
                      has_header,
                      progress)


    return(df)

  })


  # --------------  observe(obs_data()) ------------------------

  observe({
    obs_data()
    vals$clicked <- NULL
  })


  # -------------- output$observations ------------------------

  output$observations <-renderReactable({

    #req(stations_ok())
    #req(stations())
    req(obs_data())

    df <- obs_data()[["df"]]

    if(is.null(df)){
      return(reactable())
    }else{
      reactable(df,
                sortable = T,
                filterable = T,
                style = list(fontSize = table_font),
                columns = list(
                  group0 = colDef(show=FALSE)
                ),
                defaultColDef = colDef(minWidth = 55, show=T, vAlign = "bottom"),
                compact = TRUE,
                wrap = FALSE,
                fullWidth = T,
                resizable = TRUE,
                bordered = TRUE,
                defaultPageSize = 10,
                highlight = TRUE,
                theme = reactableTheme(
                  headerStyle = list(background = "#f7f7f8"),
                  cellPadding = "1px 1px"
                ))
    }
  })

  species_summary <- reactive({
    req(matched_spec())
    df <-matched_spec()

    dfn <- df %>%
      mutate(species=ifelse(is.na(group0),"unrecognized",
                         ifelse(RA==1,"reallocatable","other"))) %>%
      group_by(species) %>%
      summarise(n=n(), .groups="drop")

    df <-data.frame(species="Total", n=nrow(df)) %>%
        bind_rows(dfn)
    return(df)

  })

  # -------------- output$tblSpecCount ------------------------

  output$tblSpecCount <- renderReactable({
    req(species_summary())
    df <- species_summary()
    ntotal <- df %>%
      filter(species=="Total") %>%
      pull(n) %>%
      as.character()
    df <- df %>%
      filter(!species %in% c("Total","other"))

    reactable(df,
              sortable = F,
              style = list(fontSize = table_font),
              columns = list(
                species = colDef(name="Species"),
                n = colDef(name = ntotal, minWidth = 50)
              ), # columns
              defaultColDef = colDef(minWidth = 150, vAlign = "bottom"), # show=T,
              compact = TRUE,
              wrap = FALSE,
              fullWidth = F,
              resizable = F,
              bordered = TRUE,
              highlight = TRUE,
              theme = reactableTheme(
                headerStyle = list(background = "#f7f7f8"),
                cellPadding = "1px 1px"
              ))
  })

  # -------------- output$tblSpec ------------------------

  output$tblSpec <- renderReactable({

    req(matched_spec())
    req(input$showSpecies)

    df <- matched_spec()

    show_option <- input$showSpecies

    if(show_option != "all"){
      df <- df %>%
        filter(edit==1)
    }


    df <- df %>%
      mutate(reset=ifelse(is.na(group), 0,
                          ifelse(is.na(group0),1,
                                 ifelse(group0!=group, edit, 0))))

    if(show_option == "ignored"){
      df <- df %>%
        filter(is.na(group))
    }


    npg <- nrow(df)
    if(npg>0){
      #npg <- ifelse(npg < 20, 20, 10)

    if(is.null(df)){
      return(NULL)
    }else{
      # df <- df %>%
      #   mutate(edit=ifelse(is.na(group),1,ifelse(RA==1,1,"")))

      reactable(df,
                #selection = "single",
                #onClick = "select",
                height = 500,
                #width = 400,
                filterable = F,
                sortable = T,
                style = list(fontSize = table_font),
                #rowStyle =
                columns = list(
                  Species = colDef(minWidth = 150,
                                   filterable = T),
                  group = colDef(name="Group",
                                 minWidth = 60,
                                 cell = function(value){
                                   if(is.na(value)){
                                     val <- ""
                                   }else{
                                     value = value + 1
                                     val <- c("none","I","II","III","IV","V")[value]
                                   }
                                   return(val)
                                 },
                                 style=JS("function(rowInfo) {
                      if(rowInfo.values['group'] != rowInfo.values['group0']){
                        return { backgroundColor:'rgba(0, 255, 0, 0.05)'}
                      }else if(rowInfo.values['group0'] === null){
                          return { backgroundColor:'rgba(255, 0, 0, 0.05)'  }
                      }else{
                          return
                        }
                      }")),
                  group0= colDef(show = F),
                  RA = colDef(show=F),
                  edit = colDef(
                    width = 50,
                    headerStyle = "border-right:none;border-top:none;border-bottom:none;background:none;",
                    name = "",
                    sortable = FALSE,
                    style = "border-right:none;border-top:none;",
                    cell = function(value){
                      if(value==1){
                        htmltools::tags$button("Edit")
                      }else{
                        ""
                      }
                    }

                  ),
                 reset = colDef(
                   width = 50,
                   headerStyle = "border:none;background:none;",
                   name = "",
                   sortable = FALSE,
                   style = "border-left:none;border-right:none;border-top:none;",
                   cell = function(value){
                     if(value==1){
                       htmltools::tags$button("Reset")
                     }else{
                       ""
                     }
                   }

                 )
                ), # columns
                defaultColDef = colDef(minWidth = 55, vAlign = "bottom"), # show=T,
                compact = TRUE,
                wrap = FALSE,
                fullWidth = T,
                resizable = T,
                bordered = TRUE,
                defaultPageSize = npg,
                highlight = TRUE,
                theme = reactableTheme(
                  headerStyle = list(background = "#f7f7f8"),
                  cellPadding = "1px 1px"
                ),
                onClick = JS("function(rowInfo, column) {
    // Only handle click events on the 'details' column
    if (column.id != 'edit' && column.id != 'reset') {
       return
    }

      // Send the click event to Shiny, which will be available in input$show_details
      // Note that the row index starts at 0 in JavaScript, so we add 1



      if (window.Shiny) {
        if(column.id == 'edit'){
            Shiny.setInputValue('edit_species', { index: rowInfo.index + 1 }, { priority: 'event' })
        }else{
            Shiny.setInputValue('edit_species', { index: -1 * (rowInfo.index + 1)}, { priority: 'event' })
            }
      }

  }"))
    }

  }else{
    NULL
  }})

  # -------------- matched_spec() ------------------------

  matched_spec <- reactive({

    # need to remove the dependence on ambi results
    # so that species matching is done first
    # then ambi is calculated

    req(ambi_first())

    df <- ambi_first()[["matched"]]

    # df <- df %>%
    #   mutate(edit=ifelse(is.na(group),1,ifelse(RA==1,1,"")))

    if("group_note" %in% names(df)){
      df <- df %>%
        mutate(group0 = as.numeric(stringr::str_extract(group_note, "(?<=\\:).+"))) %>%
        mutate(group0 = ifelse(is.na(group0),ifelse(is.na(RA),NA,group),group0))
    }else{
      df <- df %>%
        mutate(group0=ifelse(is.na(RA),NA,group))
    }
    df <- df %>%
      mutate(edit=ifelse(is.na(group0),1,ifelse(RA==1,1,0)))

    df <- df %>%
      distinct(Species, group, group0, RA, edit) %>%
      arrange(Species)
    return(df)
  })

  # -------------- species_displayed() ------------------------

  species_displayed <- reactive({


    req(matched_spec())
    df <- matched_spec()
    #
    show_option <- input$showSpecies

    if(show_option!="all"){
      df <- df %>%
        filter(edit==1)
    }

    if(show_option == "ignored"){
      df <- df %>%
        filter(is.na(group))
    }

    return(df)
  })


  # -------------- observeEvent(input$edit_species) ------------------------

  observeEvent(input$edit_species, {

    req(species_displayed())

    df <- species_displayed()

    ix <- input$edit_species %>% unlist()

    if(ix>0){

      grp <- df[ix,"group"]
      species <- df[ix,"Species"]

      if(is.na(grp)){
        val_sel <- NULL
      }else{
        val_sel <- as.numeric(grp)
      }

      showModal(change_group(selected=val_sel, species = species))

    }else{

      df_changes <- vals$df_changes

      if(!is.null(df_changes)){
        ix <- -1*ix
        speciesi <- df[ix,"Species"] %>% unlist()

        df_changes <- df_changes %>%
          filter(Species!=speciesi)
        vals$df_changes <- df_changes

        vals$clicked <- NULL
      }

    }

  })

  # -------------- change_group() ------------------------

  change_group <- function( selected=NULL, species=NA) {
    modalDialog(
      size = "s",
      # paste0(ifelse(is.na(species),"select",
      #               tags$em(species)), " group:"),
      tags$p(tags$em(species)),
      tags$div(
        tags$table(
          tags$tr(
            tags$td(valign="top", "group: ", style="padding:8px"),
            tags$td(valign="top",
              selectInput(
                "new_group",
                label=  NULL,
                choices = c(
                  "Not allocated" = 0,
                  "I" = 1, "II" = 2, "III" = 3, "IV" = 4, "V" = 5
                ),
                selectize = F,
                width="150px",
                selected = ifelse(is.na(selected),NULL, selected)
              )
            )
          )
        )
      ),
      tags$div(
        tags$table(
          tags$tr(
            tags$td(width="50%",
              reactableOutput("tblSimilar")
              ),tags$td("")))),
      footer = tagList(
        actionButton("change", "Apply"),
        modalButton("Cancel")
      )
    )
  }

  # -------------- match_row() ------------------------

  match_row <- reactive({


    df_list <- ambiR::AMBI_species()
    df <- species_displayed()
    ix <- input$edit_species %>% unlist()
    if(ix>0){

      speciesi <- df[ix,"Species"] %>% unlist()

      speciesi <- strsplit(speciesi, " ") %>% unlist()
      speciesi <- speciesi[1]

      df_match <- data.frame(species = speciesi, X="X")

      df <- df_list %>%
        bind_rows(df_match) %>%
        dplyr::arrange(species) %>%
        mutate(ix=row_number())

      n <- df %>%
        filter(X=="X") %>%
        pull(ix)

      n <- # floor(n / page_size)

      return(n)

    }
  })

  # -------------- observeEvent(input$new_group) ------------------------

  observeEvent(input$new_group, {

    action_manual <- isolate(vals$manual)

    if(is.null(action_manual)){
      action_manual <- TRUE
    }

    if(action_manual){
      updateReactable(
        outputId = "tblSimilar",
        selected = NA
      )
      vals$clicked <- NULL

    }else{
      action_manual <- TRUE
    }

    vals$manual <- action_manual

  })

  # -------------- observeEvent(input$choose_species) ------------------------

  observeEvent(input$choose_species, {

    row <- input$choose_species %>% unlist()

    rowid <- row[1]
    grp <- row[2]

    vals$manual <- FALSE

    updateSelectInput(
      inputId = "new_group",
      selected = grp
    )

    updateReactable(
      outputId = "tblSimilar",
      selected = rowid
    )


  })


  # -------------- output$tblSimilar ------------------------

  output$tblSimilar <- renderReactable({


    df <- species_displayed()
    ix <- input$edit_species %>% unlist()
    if(ix>0){
      RA <- df[ix,"RA"]
    }else{
        RA <- NA
      }
      if(is.na(RA)){

        df_list <- ambiR::AMBI_species()

        n <- match_row()
        n1 <- n-5
        n1 <- ifelse(n1<1,1,n1)
        n2 <- n+100
        n2 <- ifelse(n2>nrow(df_list),nrow(df_list),n2)

        df_list <- df_list[n1:n2,]

        res <- reactable(df_list,
                         selection = "single",
                  sortable = F,
                  style = list(fontSize = table_font),
                  columns = list(
                    species = colDef(name="Species",
                                     minWidth = 200,
                                     #cell = function(value) {em(value)},
                                     show=T),
                    group = colDef(name="Group",
                                   show=T,
                                   minWidth = 50,
                                   cell = function(value){
                                     if(is.na(value)){
                                       val <- ""
                                     }else{
                                       value = value + 1
                                       val <- c("none","I","II","III","IV","V")[value]
                                     }
                                     return(val)
                                   },
                    )
                  ), # columns
                  defaultColDef = colDef(minWidth = 70, show=F, vAlign = "bottom"),
                  paginationType = "simple",
                  showPageInfo =F,
                  compact = TRUE,
                  wrap = FALSE,
                  fullWidth = F,
                  resizable = F,
                  bordered = TRUE,
                  defaultPageSize = 10,
                  highlight = T,
                  theme = reactableTheme(
                    headerStyle = list(background = "#f7f7f8"),
                    rowSelectedStyle=list(backgroundColor = "#c0d6e4", color = "#000"),
                    cellPadding = "3px 1px"
                  ),
                  onClick = JS("function(rowInfo) {
Shiny.setInputValue('choose_species', { index: rowInfo.index + 1 , group: rowInfo.values['group']}, { priority: 'event' })
                               }")  # group: rowInfo.values['group'] #
        )

      }else{
        res <-empty_table()
      }
    res
})

  # -------------- observeEvent(input$$change) ------------------------

  observeEvent(input$change, {

    removeModal()

    grp_new <- as.numeric(input$new_group)

    df <- species_displayed()

    ix <- input$edit_species %>% unlist()
    grpi <- df[ix,"group"] %>% unlist()
    speciesi <- df[ix,"Species"] %>% unlist()

    if(is.na(grpi)){
      grpi <- -1
    }
    if(grp_new != grpi){
         #cat(paste0(speciesi," ", grpi," -> ", grp_new, "\n"))

      df_new <- data.frame(Species=speciesi, group=grp_new)
      df_changes <- vals$df_changes

      if(is.null(df_changes)){
        df_changes <- df_new
      }else{
        df_changes <- df_changes %>%
          filter(Species!=speciesi)
        df_changes <- df_changes %>%
          bind_rows(df_new)
      }
      vals$df_changes <- df_changes

      # updateReactable("table", data = filtered)

    }

  })


  # ------- observe(ambi_res()) ------

  observe({
    res <- ambi_res()

    if(is.null(res$AMBI)){
      # disable the downdload button on page load
      shinyjs::hide("btnDownloadInds")
    }else{
      shinyjs::show("btnDownloadInds")
    }
  })


  # ------- observe(mambi_res()) ------

  observe({

    res <- mambi_res()


    if(is.null(res)){
      #reset M-AMBI reference conditions
      vals$changes_refcond <- NULL

      # disable the downdload button on page load
      shinyjs::hide("btnDownloadInds2")
    }else{
      if(nrow(res)==0){
        shinyjs::hide("btnDownloadInds2")
      }else{
        shinyjs::show("btnDownloadInds2")
      }
    }
  })


#  limits_AMBI = c(bad = 6, high = 0),
#  limits_H = c(bad = 0, high = NA),
#  limits_S = c(bad = 0, high = NA),
#  bounds = c(PB = 0.2, MP = 0.39, GM = 0.53, HG = 0.77)


  # -------------- output$chkMAMBI ------------------------

  output$chkMAMBI <- renderUI({

    req(ambi_res_mambi())

    tagList(checkboxInput(
      "doMAMBI",
      label="M-AMBI",
      F #,
#      width = "100%"
    ))
  })


  # -------------- observeEvent(input$doMAMBI) ------------------------

  observeEvent(input$doMAMBI, {

    accordion_panel_remove(
      id="acc_ambi","panel_mambi")

    accordion_panel_remove(
      id="acc_ambi" ,"M-AMBI"
    )

    show_mambi <- input$doMAMBI

    show_mambi <- ifelse(is.null(show_mambi),F,show_mambi)

    if(show_mambi==T){
      accordion_panel_insert(
        id="acc_ambi",
        panel= accordion_panel(
          title = "M-AMBI",
          value = "panel_mambi",
          icon = bsicons::bs_icon("file-earmark-spreadsheet"),
          reactableOutput("tblMAMBI")
        ),
        target = "panel_ambi_rep",
        position = "after"
      )
    }else{

      accordion_panel_remove(
        id="acc_ambi" ,"M-AMBI"
        )
    }

  }, ignoreInit = TRUE, ignoreNULL=FALSE)

  # -------------- bounds_mambi() ------------------------

  bounds_mambi <- reactive({
    data.frame(PB = 0.2, MP = 0.39, GM = 0.53, HG = 0.77)
  })

  # -------------- refconds_mambi() ------------------------

  refconds_mambi <- reactive({
    req(ambi_res_mambi())

    df_bnds <- bounds_mambi()

    df <- ambi_res_mambi()$AMBI
    if(is.null(df)){
      return(NULL)
    }
    df <- df %>%
      filter(!is.na(AMBI))
    if(nrow(df)==0){
      return(NULL)
    }

    check_cols <- c("AMBI","H","S")
    check_cols <- check_cols[check_cols %in% names(df)]
    if(length(check_cols)<3){
      return(NULL)
    }

# browser()
    if("MAMBIgrp" %in% names(df)){
      df <- df %>%
        group_by(MAMBIgrp) %>%
        summarise(H_max=max(H,na.rm=T),
                  S_max=max(S,na.rm=T),
                  .groups = "drop")
    }else{
      df <- df %>%
        summarise(H_max=max(H,na.rm=T),
                  S_max=max(S,na.rm=T))
    }

    df <- df %>%
      mutate(refcond_H = H_max,
             refcond_S = S_max,
             modified_H = 0,
             modified_S = 0)

    df <- df %>%
      merge(df_bnds, all=T) %>%
      mutate(modified_PB = 0,
             modified_MP = 0,
             modified_GM = 0,
             modified_HG = 0)

    # browser()
    if(!is.null(vals$changes_refcond)){
      df_changes <- vals$changes_refcond
      if("MAMBIgrp" %in% names(df)){
        if("MAMBIgrp" %in% names(df_changes)){
          df <- df %>%
            left_join(df_changes, by="MAMBIgrp")
        }else{
          df <- df %>%
            mutate(H_new=NA, S_new=NA,
                   PB_new=NA, MP_new=NA,
                   GM_new=NA, HG_new=NA)
        }
      }else{
        if("MAMBIgrp" %in% names(df_changes)){
          df_changes <- df_changes %>%
            filter(is.na(MAMBIgrp))
          if(nrow(df_changes)==0){
            df_changes <- data.frame(H_new=NA, S_new=NA,
                                     PB_new=NA, MP_new=NA,
                                     GM_new=NA, HG_new=NA)
          }
          }
          df <- df %>%
            bind_cols(df_changes)
      }
      df <- df %>%
        mutate(refcond_H = ifelse(is.na(H_new), refcond_H, H_new),
               refcond_S = ifelse(is.na(S_new), refcond_S, S_new),
               PB = ifelse(is.na(PB_new), PB, PB_new),
               MP = ifelse(is.na(MP_new), MP, MP_new),
               GM = ifelse(is.na(GM_new), GM, GM_new),
               HG = ifelse(is.na(HG_new), HG, HG_new),
               modified_H = ifelse(is.na(H_new), 0, 1),
               modified_S = ifelse(is.na(S_new), 0 ,1),
               modified_PB = ifelse(is.na(PB_new), 0, 1),
               modified_MP = ifelse(is.na(MP_new), 0 ,1),
               modified_GM = ifelse(is.na(GM_new), 0 ,1),
               modified_HG = ifelse(is.na(HG_new), 0 ,1))

    }

    return(df)
  })

  # -------------- mambi_res() ------------------------

  mambi_res <- reactive({

    req(ambi_res_mambi())

    df <- ambi_res_mambi()[["AMBI"]]

    bounds_mambi <- bounds_mambi()
    refconds_mambi <- refconds_mambi()

    if(is.null(df)){
      return(NULL)
    }else{
      if("Station" %in% names(df)){
        by_var <- "Station"
      }else{
        by_var <-  NULL
      }

      check_vars <- c("AMBI","H","S")
      for(vari in check_vars){
        if(!vari %in% names(df)){
          # not all required columns are present
          return(NULL)
        }
      }

      mambi <- batch_mambi(df, by=by_var,
                           bounds = bounds_mambi,
                           refconds = refconds_mambi)
      return(mambi)

      }
  })


  output$txtRefcon <- renderUI({

    req(refconds_mambi())

    if(is.null(refconds_mambi())){
      tagList(p(""))
    }else{
          tagList(p("Default values for reference conditions for Shannon Wiener diversity index (H')
                     and Species richness (S) are obtained from the maximum values calculated from
                     observation data."),
            p("Click on a value to modify it and specify a new reference condition."),
            p("Default values for the thresholds between status classes can also be modified here."))

    }
  })

  # -------------- output$tblRefCond ------------------------
  output$tblRefCond<- renderReactable({

     df <- refconds_mambi()

     if(is.null(df)){
       return(NULL)
     }

     df <- df %>%
       mutate(reset=ifelse(refcond_H==H_max, 0, 1))  %>%
       mutate(reset=ifelse(refcond_S==S_max, reset, 1)) %>%
       mutate(reset=ifelse(modified_PB==1, 1, reset)) %>%
       mutate(reset=ifelse(modified_MP==1, 1, reset)) %>%
       mutate(reset=ifelse(modified_GM==1, 1, reset)) %>%
       mutate(reset=ifelse(modified_HG==1, 1, reset))

     show_mambi_grp <- FALSE

     if("MAMBIgrp" %in% names(df)){
       grp <- df$MAMBIgrp
       grp <- grp[!is.na(grp)]
       if(length(grp)>0){
         show_mambi_grp <- TRUE
       }
     }else{
       df <- df %>%
         mutate(MAMBIgrp="x")
     }
     df <- df %>%
       relocate(MAMBIgrp)

     reactable(df,
               sortable = F,
               #onClick = "select",
               style = list(fontSize = "1em"),
               columns = list(
                 MAMBIgrp = colDef(html = TRUE,
                                   header= JS('function() {
        return `<div>Type</div><div>M-AMBI</div>`
      }'),
                                   show=show_mambi_grp,
                                   minWidth = 70),
                 refcond_H = colDef(name="H'",
                                    show=T,
                                    minWidth = 50,
                            format=colFormat(digits = 3),
                            style=JS("function(rowInfo) {
                      if(rowInfo.values['refcond_H'] != rowInfo.values['H_max']){
                        return { backgroundColor:'rgba(0, 255, 0, 0.05)'}
                      }else{
                          return
                        }
                      }")),
                 refcond_S = colDef(name="S",
                                    show=T,
                                    minWidth = 50,
                                    format=colFormat(digits = 0),
                                    style=JS("function(rowInfo) {
                      if(rowInfo.values['refcond_S'] != rowInfo.values['S_max']){
                        return { backgroundColor:'rgba(0, 255, 0, 0.05)'}
                      }else{
                          return
                        }
                      }")),
                 PB = colDef(name="PB",
                             html = TRUE,
                             header=JS('function() {
        return `<div>Poor/</div><div>Bad</div>`
      }'),
                                    show=T,
                                    minWidth = 50,
                                    format=colFormat(digits = 3),
                                    style=JS("function(rowInfo) {
                      if(rowInfo.values['modified_PB'] == 1){
                        return { backgroundColor:'rgba(0, 255, 0, 0.05)'}
                      }else{
                          return
                        }
                      }")),
                 MP = colDef(name="MP",
                             html = TRUE,
                             header=JS('function() {
        return `<div>Mod/</div><div>Poor</div>`
      }'),
                             show=T,
                             minWidth = 50,
                             format=colFormat(digits = 3),
                             style=JS("function(rowInfo) {
                      if(rowInfo.values['modified_MP'] == 1){
                        return { backgroundColor:'rgba(0, 255, 0, 0.05)'}
                      }else{
                          return
                        }
                      }")),
                 GM = colDef(name="GM",
                             html = TRUE,
                             header=JS('function() {
        return `<div>Good/</div><div>Mod</div>`
      }'),
                             show=T,
                             minWidth = 50,
                             format=colFormat(digits = 3),
                             style=JS("function(rowInfo) {
                      if(rowInfo.values['modified_GM'] == 1){
                        return { backgroundColor:'rgba(0, 255, 0, 0.05)'}
                      }else{
                          return
                        }
                      }")),
                 HG = colDef(name="HG",
                             html = TRUE,
                             header=JS('function() {
        return `<div>High/</div><div>Good</div>`
      }'),
                             show=T,
                             minWidth = 50,
                             format=colFormat(digits = 3),
                             style=JS("function(rowInfo) {
                      if(rowInfo.values['modified_HG'] == 1){
                        return { backgroundColor:'rgba(0, 255, 0, 0.05)'}
                      }else{
                          return
                        }
                      }")),

                 reset = colDef(
                   show=T,
                   minWidth = 60,
                   headerStyle = "border:none;background:none;",
                   name = "",
                   sortable = FALSE,
                   style = "border-right:none;border-top:none;",
                   #style = "border-left:none;border-right:none;border-top:none;",
                   cell = function(value){
                     if(value==1){
                       htmltools::tags$button("Reset")
                     }else{
                       ""
                     }
                   }

                 )

               ), # columns
               defaultColDef = colDef(minWidth = 70,
                                      show=F, vAlign = "bottom"),
               compact = TRUE,
               wrap = FALSE,
               fullWidth = F,
               resizable = F,
               bordered = TRUE,
               defaultPageSize = 15,
               highlight = TRUE,
               theme = reactableTheme(
                 headerStyle = list(background = "#f7f7f8"),
                 rowSelectedStyle=list(backgroundColor = "#c0d6e4",
                                       color = "#000"),
                 cellPadding = "3px 1px"
               ),
               onClick = JS("function(rowInfo, column) {
    // Only handle click events on the 'details' column
    // if (column.id != 'edit' && column.id != 'reset') {
    //   return
    // }
    if (column.id == 'MAMBIgrp') {
       return
     }


      // Send the click event to Shiny, which will be available in input$show_details
      // Note that the row index starts at 0 in JavaScript, so we add 1



      if (window.Shiny) {
        if(column.id == 'refcond_H'){
            Shiny.setInputValue('edit_refcond',{ index: rowInfo.index + 1,metric: 'refcond_H', val: rowInfo.values['refcond_H'], minval: 0, maxval:9999 }, { priority: 'event'})
        }else if(column.id == 'refcond_S'){
            Shiny.setInputValue('edit_refcond',{ index: rowInfo.index + 1,metric: 'refcond_S', val: rowInfo.values['refcond_S'], minval: 0, maxval:9999  }, { priority: 'event'})
        }else if(column.id == 'PB'){
            Shiny.setInputValue('edit_refcond',{ index: rowInfo.index + 1,metric: 'PB', val: rowInfo.values['PB'], minval: 0, maxval: rowInfo.values['MP'] }, { priority: 'event' })
        }else if(column.id == 'MP'){
            Shiny.setInputValue('edit_refcond',{ index: rowInfo.index + 1,metric: 'MP', val: rowInfo.values['MP'], minval: rowInfo.values['PB'], maxval: rowInfo.values['GM']}, { priority: 'event' })
        }else if(column.id == 'GM'){
            Shiny.setInputValue('edit_refcond',{ index: rowInfo.index + 1,metric: 'GM', val: rowInfo.values['GM'], minval: rowInfo.values['MP'], maxval: rowInfo.values['HG']}, { priority: 'event' })
        }else if(column.id == 'HG'){
            Shiny.setInputValue('edit_refcond',{ index: rowInfo.index + 1,metric: 'HG', val: rowInfo.values['HG'], minval: rowInfo.values['GM'], maxval: 1}, { priority: 'event' })
        }else{
             Shiny.setInputValue('edit_refcond',{ index: rowInfo.index + 1,metric: 'reset', val: 0 }, { priority: 'event' })
            }
      }

  }")
     )


  })



  # -------------- observeEvent(input$edit_refcond) ------------------------

  observeEvent(input$edit_refcond, {
    #browser()
    req(refconds_mambi())

    df <- refconds_mambi()

    ix <- input$edit_refcond %>% unlist()
    metric <- ix[2]
    val <- ix[3] %>% as.numeric()

    val <- ifelse(metric=="refcond_S", val, format(val, digits=4))

    ix <- ix[1] %>% as.numeric()


    if("MAMBIgrp" %in% names(df)){
      name_grp <- df$MAMBIgrp[ix]
      name_grp <- paste0(" (", name_grp,")")
    }else{
      name_grp <-""
    }


    if(metric=="reset"){

      df_changes <- vals$changes_refcond
      df_refcon <- isolate(refconds_mambi())

      ix <- input$edit_refcond %>% unlist()
      ix <- ix[1] %>% as.numeric()

      if(!is.null(df_changes)){

        if("MAMBIgrp" %in% names(df_refcon)){
         match_grp <-  df_refcon$MAMBIgrp[ix]
         match_grp <- ifelse(is.na(match_grp),"",match_grp)
         list_grp <- df_changes$MAMBIgrp
         list_grp <- ifelse(is.na(list_grp),"",list_grp)

         ix <- (1:nrow(df_changes))
         ix <- ix[list_grp==match_grp]

        }

        df_changes[ix,"S_new"] <- NA
        df_changes[ix,"H_new"] <- NA
        df_changes[ix,"PB_new"] <- NA
        df_changes[ix,"MP_new"] <- NA
        df_changes[ix,"GM_new"] <- NA
        df_changes[ix,"HG_new"] <- NA
      }

      vals$changes_refcond <- df_changes

    }else{
      title <- refcon_title(metric)
      metric <- paste0("new", metric)
      showModal(change_refcond(metric=metric, value="",
                               title=title,
                               placeholder = val))

    }

  })


  refcon_title <- function(metric, name_grp=NA_character_){

    title <- ifelse(metric=="refcond_H","H'- Shannon Wiener diversity index","")
    title <- ifelse(metric=="refcond_S","S - Species richness",title)
    title <- ifelse(metric=="PB","M-AMBI Poor/Bad Threshold",title)
    title <- ifelse(metric=="MP","M-AMBI Moderate/Poor Threshold",title)
    title <- ifelse(metric=="GM","M-AMBI Good/Moderate Threshold",title)
    title <- ifelse(metric=="HG","M-AMBI High/Good Threshold",title)
    #title <- paste0(title, name_grp)
    return(title)
  }

  # -------------- change_refcond() ------------------------

  change_refcond <- function(metric, value="", title,
                             placeholder = NULL,
                             warning=NULL) {

    modalDialog(
      size = "s",

      tags$p(tags$em(title)),
      tags$div(
        tagList(textInput(
          inputId = metric,
          label=  NULL,
          value = value,
          width= "100px",
          placeholder = placeholder
        ))
      ),
      tags$p(style="color:red", warning),
      footer = tagList(
        actionButton("changeRefCond", "Apply"),
        modalButton("Cancel")
      )
    )
  }



  # -------------- observeEvent(input$$changeRefCond) ------------------------

  observeEvent(input$changeRefCond, {
    # browser()


    new_s <- input$newrefcond_S
    new_h <- input$newrefcond_H
    new_pb <- input$newPB
    new_mp <- input$newMP
    new_gm <- input$newGM
    new_hg <- input$newHG

    ix <- input$edit_refcond %>% unlist()
    metric_in <- ix[2]
    preval <-  ix[3] %>% as.numeric()
    minval <-  ix[4] %>% as.numeric()
    maxval <-  ix[5] %>% as.numeric()
    ix <- ix[1] %>% as.numeric()
    df_changes <- vals$changes_refcond
    df_refcon <- isolate(refconds_mambi())

    bChanged <- F
    bOK <- T
    warning <- NULL

    pattern <- "([[:digit:]]*\\.*[[:digit:]]*)"

    if(metric_in=="refcond_H"){
      if(!is.null(new_h)){
      if(new_h!=""){
        bChanged <- T
        value <- regmatches(new_h,regexpr(pattern,new_h))
        value <- ifelse(value=="",NA, as.numeric(value))
        metric <- "H_new"
        if(value<=minval){
          bOK <- F
          warning <- paste0("Reference condition H' must be > ",
                            minval)
        }
        }
      }
    }
    if(metric_in=="refcond_S"){
      if(!is.null(new_s)){
        if(new_s!=""){
          bChanged <- T
          value <- regmatches(new_s,regexpr(pattern,new_s))
          value <- ifelse(value=="",NA, as.numeric(value))
          metric <- "S_new"
          if(value<=minval){
            bOK <- F
            warning <- paste0("Reference condition S must be > ",
                              minval)
          }
        }
      }
    }
    if(metric_in=="PB"){
      if(!is.null(new_pb)){
        if(new_pb!=""){
          bChanged <- T
          value <- regmatches(new_pb,regexpr(pattern,new_pb))
          value <- ifelse(value=="",NA, as.numeric(value))
          metric <- "PB_new"
          if(value<=minval){
            bOK <- F
            warning <- paste0("Poor/Bad threshold must be > ",
                              minval)
          }
          if(value>=maxval){
            bOK <- F
            warning <- paste0("Poor/Bad threshold must be < Moderate/Poor threshold (", maxval, ")")
          }

        }
      }
    }
    if(metric_in=="MP"){
      if(!is.null(new_mp)){
        if(new_mp!=""){
          bChanged <- T
          value <- regmatches(new_mp,regexpr(pattern,new_mp))
          value <- ifelse(value=="",NA, as.numeric(value))
          metric <- "MP_new"
          if(value<=minval){
            bOK <- F
            warning <- paste0("Moderate/Poor threshold must be > Poor/Bad threshold (",minval,")")
          }
          if(value>=maxval){
            bOK <- F
            warning <- paste0("Moderate/Poor threshold must be < Good/Moderate threshold (", maxval, ")")
          }

        }
      }
    }
    if(metric_in=="GM"){
      if(!is.null(new_gm)){
        if(new_gm!=""){
          bChanged <- T
          value <- regmatches(new_gm,regexpr(pattern,new_gm))
          value <- ifelse(value=="",NA, as.numeric(value))
          metric <- "GM_new"
          if(value<=minval){
            bOK <- F
            warning <- paste0("Good/Moderate threshold must be > Moderate/Poor threshold (",minval,")")
          }
          if(value>=maxval){
            bOK <- F
            warning <- paste0("Good/Moderate threshold must be < High/Good threshold (", maxval, ")")
          }
        }
      }
    }

    if(metric_in=="HG"){
      if(!is.null(new_hg)){
        if(new_hg!=""){
          bChanged <- T
          value <- regmatches(new_hg,regexpr(pattern,new_hg))
          value <- ifelse(value=="",NA, as.numeric(value))
          metric <- "HG_new"
          if(value<=minval){
            bOK <- F
            warning <- paste0("High/Good threshold must be > Good/Moderate threshold (",minval,")")
          }
          if(value>=maxval){
            bOK <- F
            warning <- paste0("High/Good threshold must be < ", maxval)
          }

        }
      }
    }

    if(bOK==F){

      removeModal()
      title <- refcon_title(metric_in)
      showModal(change_refcond(metric=paste0("new", metric_in),
                               value=value,
                               title=title,
                               placeholder = preval,
                               warning=warning))

    }else{
      #  check for consistent threshold values
      removeModal()

    if(bChanged==T){




      if(is.null(df_changes)){
        df_changes <- df_refcon
        if("MAMBIgrp" %in% names(df_changes)){
          df_changes <- df_changes %>%
            select(MAMBIgrp) %>%
            mutate(H_new=NA, S_new=NA,
                   PB_new=NA, MP_new=NA,
                   GM_new=NA, HG_new=NA)
        }else{
          df_changes <- data.frame(H_new=NA, S_new=NA,
                                   PB_new=NA, MP_new=NA,
                                   GM_new=NA, HG_new=NA)
        }
      }else{
        if("MAMBIgrp" %in% names(df_changes)){
          if(!"MAMBIgrp" %in% names(df_refcon)){
            ix <- (1:nrow(df_changes))
            ix <- ix[is.na(df_changes$MAMBIgrp)]

            if(length(ix)==0){
              dfx <- data.frame(MAMBIgrp=NA,
                               H_new=NA,
                               S_new=NA,
                               PB_new=NA, MP_new=NA,
                               GM_new=NA, HG_new=NA)
              df_changes <- df_changes %>%
                bind_rows(dfx)

              ix <- nrow(df_changes)
            }
          }else{
            match_grp <-  df_refcon$MAMBIgrp[ix]
            match_grp <- ifelse(is.na(match_grp),"",match_grp)
            list_grp <- df_changes$MAMBIgrp
            list_grp <- ifelse(is.na(list_grp),"",list_grp)

            ix2 <- (1:nrow(df_changes))
            ix2 <- ix2[list_grp==match_grp]

            if(length(ix2)==0){
              dfx <- df_refcon %>%
                distinct(MAMBIgrp) %>%
                mutate(H_new=NA, S_new=NA,
                       PB_new=NA, MP_new=NA,
                       GM_new=NA, HG_new=NA)
              df_changes <- dfx %>%
                bind_rows(df_changes)
            }else{
              ix <- ix2
            }
          }
        }else{
          if("MAMBIgrp" %in% names(df_refcon)){
            dfx <- df_refcon %>%
              distinct(MAMBIgrp) %>%
              mutate(H_new=NA, S_new=NA,
                     PB_new=NA, MP_new=NA,
                     GM_new=NA, HG_new=NA)
            df_changes <- dfx %>%
              bind_rows(df_changes)
          }else{
            xxxx <- 1
            # needed?"

          }

        }
      }


      df_changes[ix, metric] <- value

      vals$changes_refcond <- df_changes
    }
    }
  })



  # -------------- output$tblMAMBI ------------------------

  output$tblMAMBI <- renderReactable({

    req(ambi_res_mambi())
    req(mambi_res())

    #df <- mambi_res()$MAMBI
    df <- mambi_res()
    df_main <- ambi_res_mambi()[["AMBI"]]

    if(is.null(df)){
      return(NULL)
    }
    if(nrow(df)==0){
      df <- reactable(data.frame(msg="...check your input"),
                sortable = F,
                columns = list(
                  msg = colDef(show=T,
                               name="Error calculating M-AMBI")),
                style = list(fontSize = "2em"),
                defaultColDef = colDef(
                  minWidth = 60,
                  show=T,
                  vAlign = "bottom"),
                compact = TRUE,
                wrap = FALSE,
                fullWidth = T,
                resizable = F,
                bordered = F,
                borderless = TRUE,
                defaultPageSize = 15,
                highlight = F,
                theme = reactableTheme(
                  headerStyle = list(
                    borderColor = "#FFFFFF"
                    ))
      )
      return(df)

    }


    # sel <- ambi_selected()


    stn_filter <- TRUE

    if("Station" %in% names(df_main)){
      stns <- df_main$Station

      # sel <- ifelse(is.null(sel),"",stns[sel])
      # if(sel!=""){
      #   df <- df %>%
      #     filter(Station == sel)
      #   stn_filter <- FALSE
      # }

      show_stn <- TRUE
    }else{
      show_stn <- FALSE
      df <- df %>%
        mutate(Station="x")
      sel <- ""
    }


    if("MAMBIgrp" %in% names(df)){
      show_mambi_grp <- TRUE
    }else{
      show_mambi_grp <- FALSE
      df <- df %>%
        mutate(MAMBIgrp="x")
    }

    df <- df %>%
      rowwise() %>%
      mutate(class_id=.classID(EQR)) %>%
      ungroup() %>%
      mutate(class_id=ifelse(is.na(Bounds),class_id,0)) %>%
      mutate(Bounds=ifelse(is.na(Bounds),"",Bounds))


    class_colours <- .classcolors()

    if(F){
      # returns a vector of the five colours used to represent status classes
      c('#FF0000','#FFC000','#FFFF00','#92D050','#00B0F0')
    }

    pct_format <- colFormat(percent = TRUE, digits = 1)
    pct_minwidth <- 50

    # Station
    # Bounds AMBI H S x y z MAMBI EQR
    reactable(df,
              sortable = F,
              style = list(fontSize = table_font),
              rowStyle = JS("function(rowInfo) {
    if(rowInfo.values['Bounds']!=''){
      return { color: 'rgba(0, 0, 0, 0.5)', backgroundColor:'rgba(0, 0, 0, 0.03)'}
    }
  }"),        showSortable=T,
              columns = list(
                HG = colDef(show=F),
                GM = colDef(show=F),
                MP = colDef(show=F),
                PB = colDef(show=F),
                MAMBIgrp = colDef(html = TRUE,
                                  header= JS('function() {
        return `<div>Type</div><div>M-AMBI</div>`
      }'),
                                  show=show_mambi_grp, minWidth = 70,
                                  sortable = TRUE, filterable = TRUE),
                Station = colDef(show=show_stn, minWidth = 70,
                                 filterable = TRUE,
                                 sortable = TRUE),
                AMBI = colDef(format=colFormat(digits = 3), minWidth = 50),
                H = colDef(name="H'",
                           format=colFormat(digits = 3), minWidth = 50),
                x = colDef(format=colFormat(digits = 3), minWidth = 50),
                y = colDef(format=colFormat(digits = 3), minWidth = 50),
                z = colDef(format=colFormat(digits = 3), minWidth = 50),
                MAMBI = colDef(name = "M-AMBI",
                  format=colFormat(digits = 3), minWidth = 70),
                EQR = colDef(format=colFormat(digits = 3), minWidth = 50),
                Status = colDef(style=JS("function(rowInfo) {
                      if(rowInfo.values['class_id']==1){
                        return { backgroundColor:'rgba(255, 0, 0, 0.5)'}
                      }else if(rowInfo.values['class_id']==2){
                          return { backgroundColor:'rgba(255, 192, 0, 0.5)'  }
                      }else if(rowInfo.values['class_id']==3){
                          return { backgroundColor:'rgba(255, 255, 0, 0.5)'  }
                      }else if(rowInfo.values['class_id']==4){
                          return { backgroundColor:'rgba(146, 208, 80, 0.5)'  }
                      }else if(rowInfo.values['class_id']==5){
                          return { backgroundColor:'rgba(0, 176, 240, 0.5)'  }
                      }else{
                          return
                        }
                      }")),
                class_id=colDef(show=F)
              ),
              defaultColDef = colDef(minWidth = 60, show=T, vAlign = "bottom"),
              compact = TRUE,
              wrap = FALSE,
              fullWidth = F,
              resizable = F,
              bordered = TRUE,
              defaultPageSize = 15,
              highlight = TRUE,
              theme = reactableTheme(
                headerStyle = list(background = "#f7f7f8"),
                rowSelectedStyle=list(backgroundColor = "#c0d6e4", color = "#000"),
                cellPadding = "3px 1px"
              )
    )

  })




  # -------------- output$selectShowSpecies ------------------------


  output$selectShowSpecies <- renderUI({

    req(obs_data())

    val_choices <- c(
      "All species" = "all",
      "species can be edited" = "editable",
      "species will be ignored" = "ignored"
    )
    val_sel <- val_choices[2]

    tagList(selectInput(
      "showSpecies",
      label=  NULL,
      choices = val_choices,
      selectize = T,
      selected = val_sel
    ))
  })

  # -------------- ambi_first() ------------------------

  ambi_first <- reactive({

    req(obs_data())
    df_changes <- vals$df_changes
    df <- obs_data()$df

    ambi_calculation(df, df_changes)
  })



  # -------------- ambi_res() ------------------------

  ambi_res <- reactive({

    req(vals$first_click)
    req(obs_data())
    req(ambi_first())

    #input$start_calculation
    is_clicked <- vals$clicked
    is_clicked <- ifelse(is.null(is_clicked), F, is_clicked)
    if(is_clicked==T){

      df_changes <- vals$df_changes
      df <- obs_data()$df


      progress <- Progress$new(session, min=1, max=10)
      on.exit(progress$close())

      progress$set(message = 'Calculating AMBI index',
                   detail = "shouldn't take long...")


      res <- ambi_calculation(df, df_changes, progress)
      return(res)
    }else{
      return(list(AMBI=NULL))
    }

  })

  # -------------- ambi_res_file() ------------------------

  ambi_res_file <- reactive({

    req(xl_data_ambi())
    req(input$colH)
    req(input$colS)
    req(input$colAMBI)
    req(input$colStnAMBI)

    cols_select <- c(input$colGroupAMBI,
                     input$colStnAMBI,
                     input$colAMBI,
                     input$colH,
                     input$colS)

    df <- xl_data_ambi()

    for(i in (1:length(cols_select))){
      if(!cols_select[i] %in% names(df)){
        df <- df %>%
          mutate(!!cols_select[i]:= NA )
      }
      if(length(cols_select[cols_select==cols_select[i]])>1){
        if(cols_select[i]!="none"){
          new_name <- paste0(cols_select[i], i)
          df <- df %>%
            mutate(!!new_name:= !!as.name(cols_select[i]))
          cols_select[i]<- new_name
        }
      }
    }

    colnames <- c("MAMBIgrp",
                  "Station",
                  "AMBI",
                  "H",
                  "S")

    colnames <- colnames[cols_select!="none"]
    cols_select <- cols_select[cols_select!="none"]


    df <- df %>%
      select(any_of(cols_select))

    names(df) <- colnames

    df <- df %>%
      mutate(across(any_of(c("AMBI","H","S")),
                           as.numeric))
    return(list(AMBI=df))
  })

  ambi_res_mambi <- reactive({
    req(input$radioAMBI)

    if(input$radioAMBI=="2"){
      return(ambi_res_file())
    }else{
      return(ambi_res())
    }
  })



  # -------------- ambi_selected() ------------------------


  ambi_selected <- reactive(getReactableState("tblAMBI", "selected"))


  # -------------- output$tblAMBIrep() ------------------------

  output$tblAMBIrep <- renderReactable({

    req(ambi_res())

    df_main <- ambi_res()[["AMBI"]]
    df <- ambi_res()[["AMBI_rep"]]


    if(is.null(df)){
      return(empty_table())
    }else{

    sel <- ambi_selected()

    stn_filter <- TRUE

    if("Station" %in% names(df_main)){
      stns <- df_main$Station
      sel <- ifelse(is.null(sel),"",stns[sel])
      if(sel!=""){
        df <- df %>%
          filter(Station == sel)
        stn_filter <- FALSE
      }
      show_stn <- TRUE
    }else{
      show_stn <- FALSE
      df <- df %>%
        mutate(Station="x")
      sel <- ""
    }

    if("MAMBIgrp" %in% names(df)){
      show_mambi_grp <- TRUE
    }else{
      show_mambi_grp <- FALSE
      df <- df %>%
        mutate(MAMBIgrp="x")
    }
    df <- df %>%
      relocate(MAMBIgrp)


    if("Replicate" %in% names(df)){
      show_rep <- TRUE
    }else{
      show_rep <- FALSE
      df <- df %>%
        mutate(Replicate="x")
    }


    pct_format <- colFormat(percent = TRUE, digits = 1)
    pct_minwidth <- 50

    reactable(df,
              sortable = F,
              #selection = "none",
              onClick = "select",
              style = list(fontSize = table_font),
              columns = list(
                Station = colDef(show=show_stn, minWidth = 70,
                                 filterable = stn_filter,
                                 sortable = TRUE),
                MAMBIgrp = colDef(html = TRUE,
                                  header= JS('function() {
        return `<div>Type</div><div>M-AMBI</div>`
      }'),
                                  show=show_mambi_grp, minWidth = 70,
                                  sortable = TRUE, filterable = TRUE),
                Replicate = colDef(show=show_rep, minWidth = 70,
                                 filterable = TRUE,
                                 sortable = TRUE),
                AMBI = colDef(format=colFormat(digits = 3), minWidth = 50),
                N = colDef(minWidth = 50),
                S = colDef(minWidth = 40),
                fNA = colDef(name="%NA", format=pct_format, minWidth = pct_minwidth),
                I = colDef(format=pct_format, minWidth = pct_minwidth),
                II = colDef(format=pct_format, minWidth = pct_minwidth),
                III = colDef(format=pct_format, minWidth = pct_minwidth),
                IV = colDef(format=pct_format, minWidth = pct_minwidth),
                V = colDef(format=pct_format, minWidth = pct_minwidth)
              ), # columns
              rowStyle = function(index) {
                if (df[index, "Station"] == sel) {
                  #list(fontWeight="bold", backgroundColor ="#c0d6e4" ) # "#f0f5f9" "hsl(233, 9%, 25%)")
                }
              },
              defaultColDef = colDef(minWidth = 70, show=T, vAlign = "bottom"),
              compact = TRUE,
              wrap = FALSE,
              fullWidth = F,
              resizable = F,
              bordered = TRUE,
              defaultPageSize = 15,
              highlight = TRUE,
              theme = reactableTheme(
                headerStyle = list(background = "#f7f7f8"),
                rowSelectedStyle=list(backgroundColor = "#c0d6e4", color = "#000"),
                cellPadding = "3px 1px"
              )
    )
}
  })

  empty_table<-function(){
    reactable(data=data.frame(x=""),
              columns=list(
                x=colDef(name="",
                         headerStyle = "border:none;background:none;",
                         style="border:none;background:none;")),
              sortable = F,
              onClick = NULL,
              compact = TRUE,
              wrap = FALSE,
              fullWidth = F,
              resizable = F,
              outlined = F,
              borderless = TRUE,
              defaultPageSize = 10,
              highlight = F
    )
  }

  # -------------- output$warnAMBI ------------------------

  output$warnAMBI <- renderUI({

    req(ambi_res())
    req(species_summary())

    df_sum <- species_summary()
    df <- ambi_res()[["AMBI"]]

    is_clicked <- vals$clicked
    is_clicked <- ifelse(is.null(is_clicked), F, is_clicked)

    if(is.null(df) & is_clicked==T){
      msg <- paste0("No results returned by AMBI() function!")
      return(
        tagList(
          div(style="color:red", msg)
      ))
    }else{
      return(NULL)
    }

  })

  # -------------- output$plotAMBI ------------------------

   output$plotAMBI <- renderPlot({

     req(ambi_res())


     # "Undisturbed", "Slightly disturbed", "Moderately disturbed", "Heavily disturbed")
     ambi_bounds <- c(1.2, 3.3, 5.0)


     res <- ambi_res()

     df <- res$AMBI
     df_rep <- res$AMBI_rep

     if(!is.null(df)){
       if(nrow(df)>0){
         df_text <- data.frame(y = c(0.6, 2.25, 4.4, 5.5),
                      status=c("Undisturbed", "Slightly disturbed",
                              "Moderately disturbed", "Heavily disturbed"))

     if("Station" %in% names(df)){
       lab_stn <- element_text()
     }else{
       lab_stn <- element_blank()
       df <- df %>%
         mutate(Station=" ")
       if("AMBI_SD" %in% names(df)){
       df_rep <- df_rep %>%
         mutate(Station=" ")
       }
     }


     if("MAMBIgrp" %in% names(df)){
       show_mambi_grp <- TRUE
     }else{
       show_mambi_grp <- FALSE
       df <- df %>%
         mutate(MAMBIgrp="x")
       if("AMBI_SD" %in% names(df)){
         df_rep <- df_rep %>%
           mutate(MAMBIgrp="x")
         df_rep <- df_rep %>%
           relocate(MAMBIgrp)
       }
     }
     df <- df %>%
       relocate(MAMBIgrp)

     df_text <- df %>%
       distinct(MAMBIgrp,Station) %>%
       group_by(MAMBIgrp) %>%
       slice(1) %>%
       ungroup() %>%
       merge(df_text, all=T)

     #if(!is.null(df_rep)){
     if("AMBI_SD" %in% names(df)){
       show_err <- T
       df <- df %>%
         mutate(ymin = AMBI-AMBI_SD, ymax=AMBI+AMBI_SD)

     }else{
       show_err <- F
     }

     p <- ggplot(df,aes(x=Station,y=AMBI)) +
       geom_hline(yintercept=ambi_bounds,
                  linetype=1, linewidth=0.5, alpha=0.1,
                  colour = "black") +
       geom_text(data=df_text, aes(x=Station, y=y, label=status),
                 hjust=0, colour="lightgrey", nudge_x = -0.55)

     if(show_err==T){
         p <- p +
           geom_linerange(aes(x=Station, ymin=ymin, ymax=ymax),
                          alpha=0.5)
         p <- p +
           geom_jitter(data=df_rep,
                       aes(x=Station, y=AMBI), size=1,
                         width = 0.1, height=0, alpha=0.5)

       }
     p <- p+
       geom_point(colour='red') +
       theme_linedraw(base_size = 12) +
       theme(panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y =  element_blank(),
             panel.grid.minor.y = element_blank(),
             strip.background = element_blank(),
             strip.text = element_text(colour="black"),
             axis.title.x = lab_stn) +
       scale_y_continuous(limits = c(0,6), breaks=seq(0,6,1), expand = c(0,0))
     if(show_mambi_grp==T){
        p <- p +
          facet_wrap(.~MAMBIgrp, ncol=4)
     }


     p

     }
   }},
     height = 200, width = 600)


  # -------------- output$tblAMBI ------------------------

  output$tblAMBI <- renderReactable({
    req(ambi_res())

    df <- ambi_res()[["AMBI"]]

    if(is.null(df)){
      return(empty_table())
    }else{

    pct_format <- colFormat(percent = TRUE, digits = 1)
    pct_minwidth <- 50

    if("Station" %in% names(df)){
      show_stn <- TRUE
    }else{
      show_stn <- FALSE
      df <- df %>%
        mutate(Station="x")
    }
    if("MAMBIgrp" %in% names(df)){
      show_mambi_grp <- TRUE
    }else{
      show_mambi_grp <- FALSE
      df <- df %>%
        mutate(MAMBIgrp="x")
    }
    df <- df %>%
      relocate(MAMBIgrp)

    if(nrow(df)==1){
      selection_type <- NULL
    }else{
      selection_type <- "single"
    }

    df <- df %>%
      mutate(Disturbance=short_version(Disturbance))

    reactable(df,
              sortable=F,
              showSortable=T,
              selection = selection_type,
              onClick = "select",
              style = list(fontSize = "0.70rem"),
              columns = list(
                MAMBIgrp = colDef(html = TRUE,
                header= JS('function() {
        return `<div>Type</div><div>M-AMBI</div>`
      }'),
                show=show_mambi_grp, minWidth = 70,
                               sortable = TRUE, filterable = TRUE),
                Station = colDef(show=show_stn, minWidth = 70,
                                 sortable = TRUE, filterable = TRUE),
                AMBI = colDef(format=colFormat(digits = 3), minWidth = 50),
                AMBI_SD = colDef(html = TRUE,
                                 format=colFormat(digits = 3),
                                 header= JS('function() {
        return `<div>AMBI</div><div>Std. Dev.</div>`
      }'), minWidth = 50),
                N = colDef(minWidth = 50),
                S = colDef(minWidth = 40),
                H = colDef(name="H'",
                  format=colFormat(digits = 3), minWidth = 50),
                fNA = colDef(name="%NA", format=pct_format, minWidth = pct_minwidth),
                 I = colDef(format=pct_format, minWidth = pct_minwidth),
                 II = colDef(format=pct_format, minWidth = pct_minwidth),
                 III = colDef(format=pct_format, minWidth = pct_minwidth),
                 IV = colDef(format=pct_format, minWidth = pct_minwidth),
                 V = colDef(format=pct_format, minWidth = pct_minwidth),
                Disturbance=colDef(
                  name = "Disturbance",
                  minWidth = 90)

              ), # columns
              defaultColDef = colDef(minWidth = 70, show=T,
                                     vAlign = "bottom"),
              compact = TRUE,
              wrap = TRUE, # FALSE,
              fullWidth = F,
              resizable = F,
              bordered = TRUE,
              defaultPageSize = 15,
              highlight = TRUE,
              theme = reactableTheme(
                headerStyle = list(background = "#f7f7f8"),
                rowSelectedStyle=list(backgroundColor = "#c0d6e4", color = "#000"),
                cellPadding = "3px 1px"
              )
    )
}
  })

  # -------------- observeEvent(input$start_calculation) ------------------------

  observeEvent(input$start_calculation,{

    if(is.null(vals$first_click)){

      accordion_panel_insert(
        id="acc_ambi",
        accordion_panel(
          value = "panel_ambi_fig",
          title = "AMBI Figures",
          icon = bsicons::bs_icon("file-earmark-spreadsheet"),
          plotOutput("plotAMBI")
        ),
        target = "panel_ambi",
        position = "after"
      )

    }


    vals$clicked <- TRUE
    vals$first_click <- TRUE
    accordion_panel_remove(
      id="acc_ambi",
      "AMBI Replicates"
    )
    accordion_panel_remove(
      id="acc_ambi",
      "panel_ambi_rep"
    )
    # accordion_panel_remove(
    #   id="acc_ambi",
    #   "AMBI Figures"
    # )
    # accordion_panel_remove(
    #   id="acc_ambi",
    #   "panel_ambi_fig"
    # )

    df <- isolate(ambi_res()[["AMBI_rep"]])

    if(!is.null(df)){

      accordion_panel_insert(
        id="acc_ambi",
        accordion_panel(
          value = "panel_ambi_rep",
          title = "AMBI Replicates",
          icon = bsicons::bs_icon("file-earmark-spreadsheet"),
          reactableOutput("tblAMBIrep")
        ),
        target = "panel_ambi",
        position = "after"
      )

    }

  })


  # -------------- output$btnCalculate ------------------------


  output$btnCalculate <- renderUI({
    req(ambi_first())
    #res_ambi <- ambi_res()[["AMBI"]]
    if(is.null(vals$first_click)){
     icon_name <- "play"
     lab <- "Calculate"
    }else{
     icon_name <- "redo"
     lab <- "Update"
    }
    is_clicked <- vals$clicked
    is_clicked <- ifelse(is.null(is_clicked), F, is_clicked)
    if(is_clicked==T){
      res <- tagList(disabled(
        actionButton(
          "start_calculation",
          label=lab,
          icon=icon(icon_name)
        )))
    }else{
      res <- tagList(
        actionButton(
          "start_calculation",
          label=lab,
          icon=icon(icon_name)
        ))
    }
  })

  # -------------- output$btnCalcMAMBI ------------------------

  output$btnCalcMAMBI <- renderUI({
    req(ambi_first())
    #res_ambi <- ambi_res()[["AMBI"]]
    if(is.null(vals$first_click)){
      icon_name <- "play"
      lab <- "Calculate"
    }else{
      icon_name <- "redo"
      lab <- "Update"
    }
    is_clicked <- vals$clicked
    is_clicked <- ifelse(is.null(is_clicked), F, is_clicked)
    if(is_clicked==T){
      res <- tagList(disabled(
        actionButton(
          "start_mambi",
          label=lab,
          icon=icon(icon_name)
        )))
    }else{
      res <- tagList(
        actionButton(
          "start_mambi",
          label=lab,
          icon=icon(icon_name)
        ))
    }
  })

  # -------------- output$btnDownloadInds2 ------------------------

  output$btnDownloadInds2 <- downloadHandler(

    filename = function() {
      paste0("download ",
             format.Date(Sys.time(),
                         "%Y%m%d_%H%M%S"),
             ".xlsx")
    },
    content = function(file) {

      res <- isolate(ambi_res())
      res2 <- tryCatch({
        isolate(mambi_res())
      },warning = function(w){
        message('Warning isolating mambi_res()!')
        print(w)
      },error = function(e){
        message('Error isolating mambi_res()!')
        print(e)
        return(NULL)
      })


      progress <- Progress$new(session, min=1, max=10)
      on.exit(progress$close())

      progress$set(message = 'Preparing download',
                   detail = "please wait...")

      wb <- excel_results(res, res2)

      saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )

  # -------------- output$btnDownloadInds ------------------------


  output$btnDownloadInds <- downloadHandler(

    filename = function() {
      paste0("download ",
             format.Date(Sys.time(),
                         "%Y%m%d_%H%M%S"),
             ".xlsx")
    },
    content = function(file) {

      res <- isolate(ambi_res())
      res2 <- tryCatch({
        isolate(mambi_res())
      },warning = function(w){
        message('Warning isolating mambi_res()!')
        print(w)
      },error = function(e){
        message('Error isolating mambi_res()!')
        print(e)
        return(NULL)
      })


        progress <- Progress$new(session, min=1, max=10)
        on.exit(progress$close())

        progress$set(message = 'Preparing download',
                     detail = "please wait...")

        wb <- excel_results(res, res2)

        saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )

}
