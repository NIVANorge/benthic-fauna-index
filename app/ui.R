library(shiny)
library(bslib)
library(bsicons)
library(reactable)

page_navbar(
  title = "Benthic fauna index calculations",
  id = "page",
  nav_panel(title="Observations",
            icon = bsicons::bs_icon("file-earmark-arrow-up"),

            layout_columns(col_widths = c(3,5),

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
                        title = "Observations",
                        icon = bsicons::bs_icon("file-earmark-spreadsheet"),  # bar-chart
                        reactableOutput("observationsraw")
                      ),
                      accordion_panel(
                        title = "Observations transposed",
                        icon = bsicons::bs_icon("file-earmark-spreadsheet"),
                        uiOutput("obs_warning") ,
                        reactableOutput("observations")

                      ),
            ) # accordion
            ) # layout_columns
            ),
  nav_panel(title="Index calculations",
            icon = bsicons::bs_icon("calculator"),

            layout_columns(

            accordion(id="species",
                      open = TRUE,
                      accordion_panel(
                        title = "Species",
                        icon = bsicons::bs_icon("file-earmark-spreadsheet"),
                        verticalLayout(
                          uiOutput("chkUnmatched"),
                          reactableOutput("tblSpec")
                        )
                                            )  #   accordion_panel
                      ), # accordion,
            accordion(id="ambi",
                      open = TRUE,
                      verticalLayout(
                        accordion_panel(
                          title = "AMBI",
                          icon = bsicons::bs_icon("file-earmark-spreadsheet"),

                          reactableOutput("tblAMBI")

                        ),
                      accordion_panel(
                        title = "AMBI Replicates",
                        icon = bsicons::bs_icon("file-earmark-spreadsheet"),
                            reactableOutput("tblAMBIrep")
                          ) # accordion_panel
                        )  # verticalLayout
            ), # accordion
            col_widths = c(3,5))),



  nav_panel(title="Information",
            icon = bsicons::bs_icon("info-square"),
            "Information"
            )
)


