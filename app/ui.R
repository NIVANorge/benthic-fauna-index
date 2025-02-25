library(shiny)
library(bslib)
library(bsicons)
library(reactable)

page_navbar(
  title = "Benthic fauna index calculations",
  id = "page",
  nav_panel(title="Observations",
            icon = bsicons::bs_icon("file-earmark-arrow-up"),

            layout_columns(col_widths = c(4,8),


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

                      )#,
                      # accordion_panel(
                      #   title = "Stations",
                      #   icon = bsicons::bs_icon("geo-alt"), # sliders
                      #   layout_columns(
                      #     "", # reactableOutput("stations"),
                      #     "", #reactableOutput("points_table"),
                      #     col_widths = c(7,5)),
                      #   uiOutput("station_warning")
                      # )
                      #,
                      #accordion_panel(
                      #  title = "Options",
                      #  icon = bsicons::bs_icon("check2-square"), # calendar-date
                      #  "Options"
                      #),

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
            ) # layout_columns
            )),
  nav_panel(title="Indices",
            icon = bsicons::bs_icon("calculator"),
            accordion(id="calculations",
                      open = TRUE,
                      accordion_panel(
                        title = "Index calculations",
                        icon = bsicons::bs_icon("file-earmark-spreadsheet"),  # bar-chart
                        layout_columns(
                          "","", #  reactableOutput("tbl_indices"),p(""),
                          col_widths = c(11,1)),
                        "", #uiOutput("comment_indices")
                        ),
                      accordion_panel(
                        title = "Matched observations",
                        icon = bsicons::bs_icon("file-earmark-spreadsheet"),  # bar-chart
                        layout_columns(
                          "","", #reactableOutput("matched_obs"),p(""),
                          col_widths = c(11,1))))
            ),

  # nav_panel(title="EQR Results",
  #           icon = bsicons::bs_icon("bar-chart"),
  #           accordion(id="eqr_results",
  #                     open = TRUE,
  #                     accordion_panel(
  #                       title = "EQR Results",
  #                       icon = bsicons::bs_icon("bar-chart"),  # bar-chart
  #
  #                       layout_columns(
  #                         "", "",
  #                                     # reactableOutput("tbl_eqr"),
  #                                     #  downloadButton("btnDownloadInds" ,
  #                                     #                 label=HTML("&nbsp;Download"),
  #                                     #                 title=HTML("Download results"),
  #                                     #                 icon=icon("download"),
  #                                     #                 style="padding: 6px 15px 7px 15px;margin: 10px 0px 8px 0px;"),
  #                                      col_widths = c(8,1))))
  #
  # ),

  nav_panel(title="Information",
            icon = bsicons::bs_icon("info-square"),
            "Information"
            )
)


