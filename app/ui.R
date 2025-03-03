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
                                       uiOutput("selectColumnRowCount"))
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
  nav_panel(title="Index calculations",
            icon = bsicons::bs_icon("calculator"),

            layout_columns(
              col_widths = c(4,7,1),

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
                            uiOutput("chkUnmatched"),
                            reactableOutput("tblSpec")
                          )
                        ) #,  #   accordion_panel
                        # accordion_panel(
                        #   title = "Options",
                        #   icon = bsicons::bs_icon("gear"),
                        #   verticalLayout(
                        #     uiOutput("chkMAMBI")
                        #   )
                        # )  #   accordion_panel

              ), # accordion,
              accordion(id="acc_ambi",
                        verticalLayout(
                          accordion_panel(
                            value="panel_ambi",
                            title = "AMBI",
                            icon = bsicons::bs_icon("file-earmark-spreadsheet"),
                            uiOutput("warnAMBI"),
                            reactableOutput("tblAMBI")
                          ),
                          accordion_panel(
                            value = "panel_ambi_rep",
                            title = "AMBI Replicates",
                            icon = bsicons::bs_icon("file-earmark-spreadsheet"),
                            reactableOutput("tblAMBIrep")
                          ), # accordion_panel
                          accordion_panel(
                           value = "panel_mambi",
                           title = "M-AMBI",
                           icon = bsicons::bs_icon("file-earmark-spreadsheet"),
                           reactableOutput("tblMAMBI")
                          ) # accordion_panel
                        )  # verticalLayout
              ), # accordion

              verticalLayout(
                shinyjs::hidden(
                  downloadButton("btnDownloadInds", "")
            ),
            p("")
            )
            )),



  nav_panel(title="Information",
            icon = bsicons::bs_icon("info-square"),
            "Information"
            )
)
)

