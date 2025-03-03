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

source("functions.R")

cat(paste0(sass_cache_get(),"\n"))
# status class boundaries
dfbnds <- read.table("class_boundaries.txt", sep=";", header=T)

function(input, output, session) {

  # possible input structures
  label_long <- "long (DB)"
  label_wide_species <-  "wide (species in columns)"
  label_wide_station <-  "wide (stations in columns)"

  vals <- reactiveValues()


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


  data_structures <- reactive({
    req(xl_names())
    c(label_long,
      label_wide_station,
      label_wide_species)
  })

  stations_ok <- reactive({
    req(station_column())

    if(station_column()==""){
      ok = FALSE
      msg <- paste0(input$selectSheet,
                    ": does not appear to contain stations IDs. Check your input selection")
    }else{
      ok = TRUE
      msg <- "Stations OK"
    }
    return(list(ok=ok, msg=msg))
  })

  observe({
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


  output$selectSheet <- renderUI(

    tagList(selectInput(
      "selectSheet",
      "Select sheet:",
      choices = xl_names(),
      #selected = sheet_initial(),
      selectize = T
      ))
  )

  output$selectStructure <- renderUI(
    tagList(selectInput(
      "selectForm",
      "Select data layout:",
      choices = data_structures(),
      selectize = T
    ))
  )


  output$checkHeader <- renderUI({
    req(sheet_rows())
    req(sheet_columns())

    if(input$selectForm==label_long){
    tagList(checkboxInput(
      "hasHeader",
      "has column names",
      TRUE
    ))}else{
      # tagList(checkboxInput(
      #   "hasHeader",
      #   "has column names",
      #   FALSE
      # ))
      NULL
    }
  })

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

  obs_data_raw <- reactive({

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
                style = list(fontSize = "0.8rem"),
                columns = list(
                  Kode = colDef(width = 60),
                  CF = colDef(width = 30),
                  SP = colDef(width = 30),
                  NB = colDef(width = 30),
                  Navn = colDef(width = 300)
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



  obs_data <- reactive({
    req(obs_data_raw())
    req(input$selectForm)

    df <- obs_data_raw()

    form <- input$selectForm
    idStn <- input$colrowStn
    idRep <- input$colrowRep
    idSpec <- input$colrowSpec
    idCount <- input$colrowCount
    has_header <- input$hasHeader

    progress <- Progress$new(session, min=1, max=10)
    on.exit(progress$close())

    progress$set(message = 'Transposing data',
                 detail = "shouldn't take long...")

    df <- reform_data(df, form,
                      idStn, idRep, idSpec, idCount,
                      label_long,
                      label_wide_species,
                      label_wide_station,
                      has_header,
                      progress)
    return(df)

  }, )



  output$observations <-renderReactable({

    #req(stations_ok())
    #req(stations())
    req(obs_data())

    df <- obs_data()[["df"]]

    if(is.null(df)){
      return(reactable())
    }else{
      reactable(df,
                sortable = F,
                style = list(fontSize = "0.8rem"),
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
              style = list(fontSize = "0.8rem"),
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
      npg <- ifelse(npg < 20, 20, 10)

    if(is.null(df)){
      return(NULL)
    }else{
      # df <- df %>%
      #   mutate(edit=ifelse(is.na(group),1,ifelse(RA==1,1,"")))

      reactable(df,
                #selection = "single",
                #onClick = "select",
                filterable = F,
                sortable = F,
                style = list(fontSize = "0.8rem"),
                #rowStyle =
                columns = list(
                  Species = colDef(minWidth = 150),
                  group = colDef(name="Group",
                                 minWidth = 70,
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
                fullWidth = F,
                resizable = F,
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


  matched_spec <- reactive({
    req(ambi_res())

    df <- ambi_res()[["matched"]]

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

  species_displayed <- reactive({


    req(matched_spec())
    df <- matched_spec()
    # browser()
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
      }

    }

  })

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
    }else{
      action_manual <- TRUE
    }

    vals$manual <- action_manual

  })


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
                  style = list(fontSize = "0.7rem"),
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

  observe({
    res <- ambi_res()

    if(is.null(res$AMBI)){
      # disable the downdload button on page load
      shinyjs::hide("btnDownloadInds")
    }else{
      shinyjs::show("btnDownloadInds")
    }
  })


#  limits_AMBI = c(bad = 6, high = 0),
#  limits_H = c(bad = 0, high = NA),
#  limits_S = c(bad = 0, high = NA),
#  bounds = c(PB = 0.2, MP = 0.39, GM = 0.53, HG = 0.77)

  output$chkMAMBI <- renderUI({

    req(ambi_res())

    tagList(checkboxInput(
      "doMAMBI",
      label="M-AMBI",
      F #,
#      width = "100%"
    ))
  })

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


  mambi_res <- reactive({

    req(ambi_res())
    # req(input$doMAMBI)
    # doMAMBI <- input$doMAMBI
    # doMAMBI <- ifelse(is.null(doMAMBI), F, doMAMBI)
    # if(doMAMBI==F){
    #   return(NULL)
    # }else{

    df <- ambi_res()[["AMBI"]]

    bounds_mambi <- c(PB = 0.2, MP = 0.39, GM = 0.53, HG = 0.77)

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


      mambi <- tryCatch({
        ambiR::MAMBI(df, by=by_var, bounds = bounds_mambi)
      },warning = function(w){
        message('Warning from MAMBI()!')
        print(w)
      },error = function(e){
        message('Error in MAMBI()!')
        print(e)
        return(NULL)
      })

      if(is.null(mambi)){
        return(res=list(err="Error calculating M-AMBI"))
      }else{
        mambi <- sort_results(mambi)

        mambi <- mambi %>%
          rowwise() %>%
          mutate(Status=.class_names()[.classID(EQR)]) %>%
          ungroup()


        bounds<- data.frame(
          Bounds=c("H/G","G/M","M/P","P/B"),
          MAMBI = c(bounds_mambi["HG"], bounds_mambi["GM"],
                    bounds_mambi["MP"], bounds_mambi["PB"]))

        return(res=list("MAMBI"=mambi, "bounds"=bounds))
      }

      }
     #}
  })


  output$tblMAMBI <- renderReactable({
    req(mambi_res())

    df <- mambi_res()$MAMBI

    if(is.null(df)){
      return(NULL)
    }

    df <- df %>%
      rowwise() %>%
      mutate(class_id=.classID(EQR)) %>%
      ungroup() %>%
      mutate(Bounds=ifelse(is.na(Bounds),"",Bounds))

    class_colours <- .classcolors()

    if(F){
      # returns a vector of the five colours used to represent status classes
      c('#FF0000','#FFC000','#FFFF00','#92D050','#00B0F0')
    }

    pct_format <- colFormat(percent = TRUE, digits = 1)
    pct_minwidth <- 60

    # Station
    # Bounds AMBI H S x y z MAMBI EQR
    reactable(df,
              sortable = F,
              style = list(fontSize = "0.8rem"),
              rowStyle = JS("function(rowInfo) {
    if(rowInfo.values['Bounds']!=''){
      return { color: 'rgba(0, 0, 0, 0.5)', backgroundColor:'rgba(0, 0, 0, 0.03)'}
    }
  }"),
              columns = list(
                AMBI = colDef(format=colFormat(digits = 3), minWidth = 50),
                H = colDef(format=colFormat(digits = 3), minWidth = 50),
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



  ambi_res <- reactive({

    req(obs_data())
    df_changes <- vals$df_changes


    df <- obs_data()$df

    if(!"Species" %in% names(df)){
      return(NULL)
    }

    if("Replicate" %in% names(df)){
      var_rep <- "Replicate"
    }else{
      var_rep <- NA_character_
    }
    if("Station" %in% names(df)){
      var_by <- c("Station")
    }else{
      var_by <- NULL
    }


    # saveRDS(df, file="../notes/breaks_ambi.Rds")

     res <- tryCatch({ambiR::AMBI(df,
                      var_rep = var_rep,
                      var_species="Species",
                      var_count = "Count",
                      df_species = df_changes,
                      by=var_by, quiet = T)
    },warning = function(w){
      message('Warning from AMBI()!')
      print(w)
    },error = function(e){
      message('Error in AMBI()!')
      print(e)
      return(NULL)
    })

    df_res <- res$AMBI
    if(is.null(df_res)){
      # no results from AMBI

    }else{
      df_res <- sort_results(df_res)
      res$AMBI <- df_res
    }

    df_res <- res$AMBI_rep
    if(!is.null(df_res)){
      df_res <- sort_results(df_res)
      res$AMBI_rep <- df_res
    }

    return(res)
  })



  ambi_selected <- reactive(getReactableState("tblAMBI", "selected"))

  output$tblAMBIrep <- renderReactable({

    req(ambi_res())

    df_main <- ambi_res()[["AMBI"]]
    df <- ambi_res()[["AMBI_rep"]]


    if(is.null(df)){
      return(empty_table())
    }else{

    sel <- ambi_selected()

    if("Station" %in% names(df_main)){
      stns <- df_main$Station
      sel <- ifelse(is.null(sel),"",stns[sel])
      if(sel!=""){
        df <- df %>%
          filter(Station == sel)
      }
      show_stn <- TRUE
    }else{
      show_stn <- FALSE
      df <- df %>%
        mutate(Station="x")
      sel <- ""
    }


    pct_format <- colFormat(percent = TRUE, digits = 1)
    pct_minwidth <- 60

    reactable(df,
              sortable = F,
              #selection = "none",
              onClick = "select",
              style = list(fontSize = "0.8rem"),
              columns = list(
                Station = colDef(show=show_stn),
                AMBI = colDef(format=colFormat(digits = 3), minWidth = 50),
                N = colDef(minWidth = 50),
                S = colDef(minWidth = 50),
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


  output$warnAMBI <- renderUI({

    req(ambi_res())
    req(species_summary())

    df_sum <- species_summary()
    df <- ambi_res()[["AMBI"]]
    if(is.null(df)){
      msg <- paste0("No results returned by AMBI() function!")
      return(
        tagList(
          div(style="color:red", msg)
      ))
    }else{
      return(NULL)
    }

  })

  output$tblAMBI <- renderReactable({
    req(ambi_res())

    df <- ambi_res()[["AMBI"]]

    if(is.null(df)){
      return(empty_table())
    }else{

    pct_format <- colFormat(percent = TRUE, digits = 1)
    pct_minwidth <- 60

    reactable(df,
              sortable = F,
              selection = "single",
              onClick = "select",
              style = list(fontSize = "0.8rem"),
              columns = list(
                AMBI = colDef(format=colFormat(digits = 3), minWidth = 50),
                N = colDef(minWidth = 50),
                S = colDef(minWidth = 50),
                H = colDef(format=colFormat(digits = 3), minWidth = 50),
                fNA = colDef(name="%NA", format=pct_format, minWidth = pct_minwidth),
                 I = colDef(format=pct_format, minWidth = pct_minwidth),
                 II = colDef(format=pct_format, minWidth = pct_minwidth),
                 III = colDef(format=pct_format, minWidth = pct_minwidth),
                 IV = colDef(format=pct_format, minWidth = pct_minwidth),
                 V = colDef(format=pct_format, minWidth = pct_minwidth)
              ), # columns
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



  output$btnDownloadInds <- downloadHandler(

    filename = function() {
      paste0("download ",
             format.Date(Sys.time(),
                         "%Y%m%d_%H%M%S"),
             ".xlsx")
    },
    content = function(file) {

      res <- isolate(ambi_res())
      res2 <- isolate(mambi_res())

        progress <- Progress$new(session, min=1, max=10)
        on.exit(progress$close())

        progress$set(message = 'Preparing download',
                     detail = "please wait...")

        wb <- excel_results(res, res2)

        saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )

}
