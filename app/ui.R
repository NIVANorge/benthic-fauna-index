library(shiny)
library(bslib)
library(bsicons)
library(reactable)
library(shinyjs)

tagList(
  useShinyjs(),
page_navbar(
  title = "Benthic fauna index calculations",
  id = "page",
  #theme = bs_theme(font_scale = 0.8), #"0.7rem"
  tags$style(type='text/css', "
  .form-control {font-size: 0.8rem;}
  button { font-size: 0.8rem; }
  .btn { font-size: 0.8rem; }
   label { font-size: 0.8rem; }
   label.control-label {
   font-size: 0.8rem;
   padding: 0px 6px 0px 6px;}
   .selectize-input { font-size: 0.8rem;}
   .selectize-dropdown {font-size: 0.8rem;}
   " ),

  nav_panel(title="Observations",

            icon = bsicons::bs_icon("file-earmark-arrow-up"),

            layout_columns(col_widths = c(3,7),

            accordion(id="setup",
                      open = "Choose Excel file",
                      accordion_panel(
                        title = "Import Excel data",
                        icon = bsicons::bs_icon("file-earmark-excel"), # menu
                        verticalLayout(
                          fileInput("file1",
                                    "Choose a file containing benthic fauna observations",
                                    accept=c(".xlsx",".xls",".xlsm"),
                                    width='100%'),
                          uiOutput("selectSheet"),
                          uiOutput("selectStructure"))
                      ),
                      accordion_panel(
                        title = "Columns and rows",
                        icon = bsicons::bs_icon("file-earmark-spreadsheet"),  # bar-chart

                        verticalLayout(uiOutput("checkHeader"),
                                       uiOutput("selectColumnRowStn"),
                                       uiOutput("selectColumnRowRep"),
                                       uiOutput("selectColumnRowSpecies"),
                                       uiOutput("selectColumnRowCount"),
                                       uiOutput("selectColumnRowGroup"))
                      )
            ), # accordion

            accordion(id="setup2",
                      open = "Observations",
                      accordion_panel(
                        value = "panel_obs",
                        title = "Observations",
                        icon = bsicons::bs_icon("file-earmark-spreadsheet"),  # bar-chart
                        reactableOutput("observationsraw")
                      )
            ) # accordion
            ) # layout_columns
  ),


  nav_panel(title="AMBI",
            icon = bsicons::bs_icon("star"),

            layout_columns(
              col_widths = c(4,8),

              accordion(id="species",
                        open = TRUE,
                        accordion_panel(
                          title = "Species summary",
                          icon = bsicons::bs_icon("file-earmark-spreadsheet"),
                          verticalLayout(
                            reactableOutput("tblSpecCount")
                          )
                        ),  #   accordion_panel
                        accordion_panel(
                          title = "Species",
       icon = bsicons::bs_icon("file-earmark-spreadsheet"),
                          verticalLayout(
                            uiOutput("selectShowSpecies"),
                            reactableOutput("tblSpec")
                          )
                        )

              ), # accordion,
              accordion(id="acc_ambi",
                        layout_columns(col_widths = c(3,5,3),
                                       uiOutput("btnCalculate"),
                                       p(""),
                                       shinyjs::hidden(
                    downloadButton("btnDownloadInds", "")
                                         )),

                        #verticalLayout(

                          accordion_panel(
                            value="panel_ambi",
                            title = "AMBI",
          icon = bsicons::bs_icon("file-earmark-spreadsheet"),
                            uiOutput("warnAMBI"),
                            reactableOutput("tblAMBI")
                          )

                        #)  # verticalLayout
              ) # accordion
            )
            ),

  nav_panel(title="M-AMBI",
            icon = bsicons::bs_icon("bar-chart-line"),
            layout_columns(
              col_widths = c(3,8),
              accordion(id="acc_mambi_input",

                        accordion_panel(
                          value = "panel_mambi_source",
                          title = "M-AMBI Source data",
                          icon = bsicons::bs_icon("star"),
                          uiOutput("selectAMBIsource")
                        )
                         # accordion_panel for ambi excel

                        ), # accordian
              accordion(id="acc_mambi",
                        layout_columns(col_widths = c(8,3),
                                       p(""),
                                       shinyjs::hidden(
                          downloadButton("btnDownloadInds2", "")
                                       )),
                        accordion_panel(
                          value = "panel_mambi_options",
                          title = "M-AMBI Reference Conditions",
            icon = bsicons::bs_icon("file-earmark-spreadsheet"),

            layout_columns(col_widths = c(4,4),
                             uiOutput("txtRefcon"),
                             reactableOutput("tblRefCond", inline = T)
                        )
                      ), # accordion_panel
                        accordion_panel(
                          value = "panel_mambi",
                          title = "M-AMBI",
                          icon = bsicons::bs_icon("file-earmark-spreadsheet"),
                          reactableOutput("tblMAMBI")
                        ) # accordion_panel

              ) # accordian
              ) # layout_columns

  ),

  nav_panel(title="Information",
            icon = bsicons::bs_icon("info-square"),
            "Information"
            )
)
)

