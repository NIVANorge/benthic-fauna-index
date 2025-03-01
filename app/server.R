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
      # browser()
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


  output$checkHeader <- renderUI(
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
  )

  output$selectColumnRowStn <- renderUI({
    if(input$selectForm==label_wide_station){
      res <- tagList(selectInput(
        "colrowStn",
        "Station row:",
        choices = sheet_rows(),
        selectize = F
      ))
    }else{

      val_sel <- match_list(sheet_columns(), "st")

      res <- tagList(selectInput(
        "colrowStn",
        "Station column:",
        choices = sheet_columns(),
        selectize = T,
        selected = val_sel
      ))
    }

})

  output$selectColumnRowRep <- renderUI({
    if(input$selectForm==label_wide_station){
      res <- tagList(selectInput(
        "colrowRep",
        "Replicates row:",
        choices = sheet_rows(),
        selectize = F
      ))
    }else{

      val_sel <- match_list(sheet_columns(), "rep")

      res <- tagList(selectInput(
        "colrowRep",
        "Replicates column:",
        choices = sheet_columns(),
        selectize = T,
        selected = val_sel
      ))
    }
    return(res)
})

  output$selectColumnRowSpecies <- renderUI({
    if(input$selectForm==label_wide_species){
      res <-  tagList(selectInput(
        "colrowSpec",
        "Species row:",
        choices = sheet_rows(),
        selectize = F
      ))
    }else{
      val_sel <- match_list(sheet_columns(), "sp")
      res <- tagList(selectInput(
        "colrowSpec",
        "Species column:",
        choices = sheet_columns(),
        selectize = T,
        selected = val_sel
      ))
    }
    return(res)
  })

  output$selectColumnRowCount <- renderUI({
    if(input$selectForm==label_long){

      val_sel <- match_list(sheet_columns(), c("count","ab","pop"))

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
    }else{
      cols <- 1:ncol(df)
    }
    cols <- c("none", cols)
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
    rows <- c("none", rows)
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
    df <- obs_data_raw()

    form <- input$selectForm
    idStn <- input$colrowStn
    idRep <- input$colrowRep
    idSpec <- input$colrowSpec
    idCount <- input$colrowCount
    has_header <- input$hasHeader

    df <- reform_data(df, form,
                      idStn, idRep, idSpec, idCount,
                      label_long,
                      label_wide_species,
                      label_wide_station,
                      has_header)

    return(df)

  })



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
    df <- matched_spec()

    if(is.null(input$unmatched)){
      show_unmatched_only <- F
    }else{
      show_unmatched_only <- input$unmatched
    }

    if(show_unmatched_only){
      df <- df %>%
        filter(edit==1)
    }

    df <- df %>%
      mutate(reset=ifelse(is.na(group), 0,
                          ifelse(is.na(group0),1,
                                 ifelse(group0!=group, edit, 0))))

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

    if(is.null(input$unmatched)){
      show_unmatched_only <- F
    }else{
      show_unmatched_only <- input$unmatched
    }

    if(show_unmatched_only){
      df <- df %>%
        filter(edit==1)
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
        speciesi <- df[ix,"Species"]

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
        res <- reactable(data=data.frame(x=""),
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



  output$chkUnmatched <- renderUI({

    req(ambi_res())

    tagList(checkboxInput(
        "unmatched",
        "Show only unrecognized or reallocatable species",
        TRUE,
        width = "100%"
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

    res <- ambiR::AMBI(df,
                      var_rep = var_rep,
                      var_species="Species",
                      var_count = "Count",
                      df_species = df_changes,
                      by=var_by, quiet = T)

    df <- res$AMBI
    df <- sort_results(df)
    res$AMBI <- df

    df <- res$AMBI_rep
    if(!is.null(df)){
      df <- sort_results(df)
      res$AMBI_rep <- df
    }

    return(res)
  })



  ambi_selected <- reactive(getReactableState("tblAMBI", "selected"))

  output$tblAMBIrep <- renderReactable({

    req(ambi_res())

    df_main <- ambi_res()[["AMBI"]]
    df <- ambi_res()[["AMBI_rep"]]


    if(is.null(df)){
      return(NULL)
    }else{

    sel <- ambi_selected()

    if("Station" %in% names(df_main)){
      stns <- df_main$Station
      sel <- ifelse(is.null(sel),"",stns[sel])
      if(sel!=""){
        df <- df %>%
          filter(Station == sel)
      }
    }else{
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


  output$tblAMBI <- renderReactable({
    req(ambi_res())

    df <- ambi_res()[["AMBI"]]

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

        progress <- Progress$new(session, min=1, max=10)
        on.exit(progress$close())

        progress$set(message = 'Preparing download',
                     detail = "please wait...")

        wb <- excel_results(res)

        saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )

}
