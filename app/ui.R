ui <- function(request) {

  tagList( # Added functionality for not losing your settings
    # Java to prompt the students to click a button
    # Java script https://community.rstudio.com/t/keeping-track-of-idle-time-during-app-usage/1735
    tags$script("
              (function() {
  var timeoutWarningMsecs = 12 * 60 * 1000;
  var idleTimer;

  function onTimeout() {
    alert('Warning: Session is about to time out! Please click a button to prevent losing progress.');
  }

  function startIdleTimer() {
    if (idleTimer) clearTimeout(idleTimer);
    idleTimer = setTimeout(onTimeout, timeoutWarningMsecs);
  }

  $(document).on('shiny:message shiny:inputchanged', startIdleTimer);

})();"),
    tags$style(type = "text/css", "text-align: justify"),
    tags$html(lang = "en"), # Add language attribute
    tags$head(tags$link(rel = "shortcut icon", href = "macroeddi_ico_green.ico")), # Add icon for web bookmarks
    tags$head(includeHTML(("google-analytics.html"))),
    tags$header(
      introBox(
        img(src = "eddie_banner_2020_test.png", height = 100,
            width = 1544, top = 5, alt = "Banner for Macrosystems EDDIE"),
        data.step = 1,
        data.intro = help_text["welcome", 1]
      )
    ),
    fluidPage(
      column(11,
             br(),
             p(tags$b("Teaching materials associated with this module can be found at ",
                      tags$a(href="http://module11.macrosystemseddie.org", 
                             "http://module11.macrosystemseddie.org.", target="_blank"))),
             h2(tags$b("Module 11: Time Series Modeling and Prediction of Environmental Data")),
             bookmarkButton(id = "bookmarkBtn", label = "Bookmark my progress"),
             p(tags$em("At any time, use this button to obtain a link that saves your progress."))
             ),
      column(1, align = "right",
             br(),
             introBox(
               actionButton("help", label = "Help", icon = icon("question-circle")), data.step = 7, data.intro = help_text["help", 1]
             )
      )
    ),
    navbarPage(position = "static-top", id = "maintab",
               tags$header(
                 fluidRow(
                   )
                 ),
               # 1. Module Overview ----
               tabPanel(introBox(tags$b("Module Overview"),
                                 data.step = 2,
                                 data.intro = help_text["tab_nav1", 1]
               ),
               value = "mtab1",
               introjsUI(), # must include in UI

               tags$style(".btn-file {
             background-color:#98CAB2;
             border-color: #579277;
             }

             .progress-bar {
             background-color: #579277;
             }"),
               # Change progress bar color
               tags$style(paste0("
                                   .irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
  background: ", slider_col, ";
  border-color: ", slider_col, ";
}")),
               includeCSS("www/slider_cols.css"),
               # javascript for formatting of images, justifying text etc.
               tags$style(HTML("
               .irs-bar {
                        border-color: transparent;
                        background-color: transparent;
                        }
                        #first {
                        border: 4px double red;
                        }
                        #13a_graz {
                        margin-bottom: 10px;
                        }
                        #bla_border {
                        border: 2px solid black;
                        }
                        #bla_border2 {
                        border: 1px solid black;
                        box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                        }
                        #txt_j {
                        text-align: justify;
                        }
                        #txt_c {
                        text-align: center;
                        }
                        #txt_l {
                        text-align: left;
                        }
                        #ackn {
                        color: gray;
                        font-size: 12px
                        }
                        #pheno img {
                        transition:transform 0.25s ease;
                        max-width: 100%; width: 100%; height: auto
                        }
                        #nextBtn1:hover {
                        background-color: yellow;
                        }
                        #dl_btn {
                        width:290px
                        }
                        #pheno:hover img{
    -webkit-transform:scale(1.5);
    transform:scale(1.5);
}
                        #wh_link a {
                        color: #FFFFFF
                        }
                        #q6_tab {
                        'border':'1px solid #ddd'
                        }
                        .box.box-solid.box-primary>.box-header {

                }

                .box.box-solid.box-primary{

                background:#cee3f1
                }
                .box.box-solid.box-success{

                background: #DDE4E1;
                }
                .box.box-solid.box-warning>.box-header {

                }

                .box.box-solid.box-warning{

                background:#FFBE85
                }
                        ")),
               introBox(
                 fluidRow(
                   column(6,
                          #* Module text ----
                          h2("Time Series Modeling and Prediction of Environmental Data"),
                          h3("Summary"),
                          p(style="text-align: justify;", module_text["intro_eco_forecast", ]),
                          p(style="text-align: justify;", module_text["this_module", ])
                   ),
                   column(5, offset = 1,
                          br(), br(), br(),
                          img(src = "mod11_conceptual_figure.png", height = "80%",
                              width = "80%", align = "left", alt = "Diagram of environmental data being ingested into time series models.")
                   )
                 ), data.step = 8, data.intro = help_text["start", 1]
               ),
               hr(),
               fluidRow(
                 column(4,
                        h3("Module Activities"),
                        tags$ul(
                          tags$li(style="text-align: justify;", module_text["act_A", ]),
                          tags$li(style="text-align: justify;", module_text["act_B", ]),
                          tags$li(style="text-align: justify;", module_text["act_C", ])
                        )

                 ),
                 column(6, offset = 2,
                        h3("Learning Outcomes"),
                        tags$line(),
                        tags$ul(
                          tags$li(style="text-align: justify;", module_text["LO1", ]),
                          tags$li(style="text-align: justify;", module_text["LO2", ]),
                          tags$li(style="text-align: justify;", module_text["LO3", ]),
                          tags$li(style="text-align: justify;", module_text["LO4", ]))
                 )
               ),
               hr(),
               fluidRow(
                 column(3,
                        h3("Macrosystems EDDIE"),
                        p(style="text-align: justify;", module_text["Macro", ]),
                        p(HTML(paste0("For more information see the website ", a(href = "https://serc.carleton.edu/eddie/macrosystems/index.html", "here", target = "_blank"), ".")))
                 ),
                 column(3,
                        h3("Privacy Policy"),
                        p(style="text-align: justify;", module_text["privacy_policy", ], HTML(paste0("For information regarding assessment data, please visit our website ", a(href = "https://serc.carleton.edu/eddie/macrosystems/assessment", "here", target = "_blank"), "."))),
                        p()
                 ),
                 column(5, offset = 1,
                        br(), br(),
                        img(src = "MacroEDDIE Logo.png", height = "70%",
                            width = "70%", align = "center", alt = "Macrosystems EDDIE logo.")
                        )
                 )
               ),
               # 2. Presentation recap ----
               tabPanel(title = tags$b("Presentation"), value = "mtab2",
                        fluidRow(
                          column(4,
                                 h3("Presentation"),
                                 p("The presentation accompanying this module provides an introduction to time series modeling and the ecological data and models used in this module."),
                                 p(tags$b("What is a time series model?")),
                                 tags$ul(
                                   tags$li(module_text["time_series_model", ])
                                 ),
                                 p(tags$b("Why are time series models increasingly applied to analyze environmental data?")),
                                 tags$ul(
                                   tags$li(module_text["models_applied", ])
                                 ),
                                 p(tags$b("What is out-of-sample prediction?")),tags$ul(
                                   tags$li(module_text["oos_prediction", ])
                                 ),
                                 p(tags$b("What is a standardized data format and why is it useful?")),tags$ul(
                                   tags$li(module_text["standardized_data", ])
                                 ),
                                 p(tags$b("What can we learn by comparing predictive performance across multiple models?")),tags$ul(
                                   tags$li(module_text["compare_models", ])
                                 ),
                                 p("Click through the slides to recap some of the main points from the lecture.")
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Key Slides",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("slides", width = "600px", height = "450px")
                                 )
                          )
                        )
               ),
               # 3. Introduction ----
               tabPanel(title = "Introduction", value = "mtab3",
                        fluidRow(
                          column(10,
                                 h3("Workflow for this module"),
                                 tags$ol(
                                   tags$li(style="text-align: justify;", module_text["workflow1", ]),
                                   tags$li(style="text-align: justify;", module_text["workflow2", ]),
                                   tags$li(style="text-align: justify;", module_text["workflow3", ]),
                                   tags$li(style="text-align: justify;", module_text["workflow4", ])
                                   )
                                 )
                          ),
                        hr(),
                        fluidRow(
                          column(10,
                                 h3("Video guide for this module"),
                                 HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/T1-k7VYwsHg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                 )
                        ),
                        hr(),
                        fluidRow(
                          column(6, 
                                 h3("Student Handout"),
                                 p("Within the Introduction and Activities A, B and C tabs there are questions for students to complete as part of this module. These can be completed by writing your answers into the final report template, which can be downloaded as a Word document (.docx) below."),
                                 tags$style(type="text/css", "#stud_dl {background-color:#0d3658;color: white}"),
                                 conditionalPanel("output.handoutbuilt",
                                                  downloadButton(outputId = "stud_dl", label = "Download Final Report Template")
                                                  )
                                 ),
                          column(6,
                                 h3("Saving your progress"),
                                 p(style="text-align: justify;", "As you go, fill out answers to questions in the final report Word document. Some of the plots you generate in the Shiny app will be needed for the final report. When prompted, be sure to download these plots so you can copy-paste them into the final report."),
                                 p(style="text-align: justify;", "If you run out of time to finish all the activities you can save your progress and return to it at a later date. Click the 'Bookmark my progress' button at the top of the page and you will obtain a link, which you should save by copy-pasting it at the top of your final report. When you are ready to resume work, paste the link into your web browser, and it will load a Shiny app session that contains your progress."),
                                 br()
                                 )
                          ),
                        hr(),
                        fluidRow(
                          column(10, align = "left",
                                 box(id = "box1", width = 10, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(8, offset = 1,
                                              h3("Before you start..."),
                                              p("Download your final report (Word document) and input your name and Student ID. Then, answer the following questions in the final report."),
                                              introBox(
                                                h3(tags$b("Think about it!")),
                                                p(tags$b(quest["q1", 1])),
                                                data.step = 5, data.intro = help_text["questions", 1]
                                              ),
                                              p(tags$b(quest["q2", 1], width = "90%"))
                                              )
                                       )
                                     )
                                 )
                          ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Data sources"),
                                 p(HTML(paste0('This module will introduce key concepts within ecological forecasting through exploration of data from the following sources:')))
                          ),
                          column(6, align = "center",
                                 h3("add logos here")
                          )
                        )
               ),
               
               # 5. Activity A ----
               tabPanel(title = "Activity A", value = "mtab4",
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                  h2("Activity A: Select an environmental case study, visualize data, and fit a model"),
                                  p("Complete objectives 1-3 to gather the information you will need for your model. Then, complete objectives 4-5 to fit and assess a time series model.")
                                 )
                          )
                        ),
                        tabsetPanel(id = "tabseries1",
                                    tabPanel(title = "Objective 1 - Select case study",

                                             value = "obj1", id = "wh_link",

                                             tags$style("outline: 5px dotted green;"),
                                             #* Objective 1 ----
                                             introBox(
                                               fluidRow(
                                                 column(12,
                                                        wellPanel(style = paste0("background: ", obj_bg),
                                                                  h3("Objective 1 - Select case study"),
                                                                  p(module_text["obj_01", ])
                                                        )
                                                 )
                                               ),
                                               hr(),
                                               fluidRow(
                                                 #** Choose site ----
                                                 column(4,
                                                        h4("Site Names"),
                                                        p("Select a site in the table to highlight on the map"),
                                                        conditionalPanel("input.row_num > 25",
                                                                         selectizeInput("row_num", "Select row",
                                                                                        choices = 1:nrow(sites_df),
                                                                                        options = list(
                                                                                          placeholder = 'Please select a row',
                                                                                          onInitialize = I('function() { this.setValue(""); }'))
                                                                         )
                                                        ),
                                                        DTOutput("table01", fill = TRUE),
                                                        fluidRow(
                                                          column(12,
                                                                 wellPanel(
                                                                   h4(tags$b("About Site")),
                                                                   textOutput("site_info")
                                                                 )
                                                          )
                                                        )
                                                 ),
                                                 #** Site map ----
                                                 column(4,
                                                        h4("Map of environmental case study sites"),
                                                        wellPanel(
                                                          leafletOutput("sitemap")
                                                        )
                                                 ),
                                                 #** Site photo ----
                                                 column(4,
                                                        h4("Site photo"),
                                                        wellPanel(
                                                          imageOutput("site_photo"),
                                                          textOutput("site_photo_info")
                                                        )
                                                 )
                                               ),
                                               hr(),
                                               data.step = 4, data.intro = help_text["objectives", 1], data.position = "top"),
                                             fluidRow(
                                               column(12, align = "left",
                                                      box(id = "box3", width = 10, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q3", 1])),
                                                                   tags$ul(
                                                                     tags$li(id = "txt_j", quest["q3a", ]),
                                                                     tags$li(id = "txt_j", quest["q3b", ]),
                                                                     tags$li(id = "txt_j", quest["q3c", ]),
                                                                     tags$li(id = "txt_j", quest["q3d", ]),
                                                                     tags$li(id = "txt_j", quest["q3e", ])
                                                                   ),
                                                                   p(tags$b(quest["q4", 1]))
                                                                   )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(10, offset = 1,
                                                      h3("Next step"),
                                                      p("We will explore the environmental data associated with this case study.")
                                                      )
                                             )
                                             ),
                                    #* Objective 2 - Explore the Data ----
                                    tabPanel(title = "Objective 2 - Explore data",  value = "obj2",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 2 - Explore data"),
                                                                p(style="text-align: justify;", module_text["obj_02", ])
                                                                )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(8, offset = 2,
                                                      h3("Variable descriptions"),
                                                      DT::DTOutput("var_desc")
                                               )
                                             ),
                                             hr(),
                                             #** Data Table ----
                                             fluidRow(
                                               column(4,
                                                      h3("Data Table"),
                                                      p("This is a Shiny data table. It is interactive and allows you to navigate through the data table by searching or clicking through the different pages."),
                                                      DT::DTOutput("site_datatable")
                                               ),
                                               #** Plot of data ----
                                               column(8,
                                                      h3("Data Plot"),
                                                      p("All plots in this Shiny app are generated using Plotly. This allows you to hover your mouse over the plot to get information from each of the plots. You can inspect the data closely by clicking and zooming into particular areas. There is also a tool box at the top of the plot."),
                                                      selectizeInput("view_var", "Select variable",
                                                                     choices = unique(site_vars$variable_name)
                                                                     ),
                                                      plotlyOutput("var_plot"),
                                                      useShinyjs(),  # Set up shinyjs
                                                      wellPanel(
                                                        h4("Variable Description"),
                                                        textOutput("txt_out")
                                                      )
                                               )
                                             ),
                                             # hr(),
                                             # fluidRow(
                                             #   column(4,
                                             #          h3("Calculate statistics"),
                                             #          selectInput("stat_calc", label = "Select calculation:", choices = stats),
                                             #          wellPanel(
                                             #            textOutput("out_stats")
                                             #          )
                                             #   ),
                                             #   column(8,
                                             #          box(id = "box4", width = 12, status = "primary",
                                             #              solidHeader = TRUE,
                                             #              fluidRow(
                                             #                column(10, offset = 1,
                                             #                       h3("Questions"),
                                             #                       h4(quest["q4", 1]),
                                             #                       DTOutput("q6_tab"),
                                             #                       br()
                                             #                )
                                             #              )
                                             #          )
                                             #   )
                                             # ),
                                             hr()
                                             ),
                                    tabPanel(title = "Objective 3 - Learn about ARIMA model", value = "obj3",
                                             #* Objective 3 - Explore variable relationships ----
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 3 - Learn about ARIMA model"),
                                                                p(style="text-align: justify;", module_text["obj_03", ])
                                                      )
                                               )
                                             )
                                             ),
                                    tabPanel(title = "Objective 4 - Fit model", value = "obj4",
                                             #* Objective 4 - Understand the ecological model ----
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 4 - Fit model"),
                                                                p(module_text["obj_04", ])
                                                      )
                                               )
                                             )
                                             ),
                                    #* Objective 5 - Run ecological model ----
                                    tabPanel(title = "Objective 5 - Assess model fit", value = "obj5",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 5 - Assess model fit"),
                                                                p(module_text["obj_05", ])
                                                                )
                                                      )
                                               )
                                             )
                                    )
                        ),
               # 5. Activity B ----
               tabPanel(title = "Activity B", value = "mtab5",
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                 h2("Activity B: Generate and assess your first forecast"),
                                 h4("Forecast!"),
                                 p("Complete objectives 6-11 to complete the steps involved with the forecast.")
                                 )
                          )
                          ),
                        tabsetPanel(id = "tabseries2",
                                    #* Objective 6 - Examine uncertainty ----
                                    tabPanel(title = "Objective 6 - Examine uncertainty", value = "obj6",
                                             #** Forecasting text ----
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 6 - Understand uncertainty and explore a weather forecast"),
                                                                p(style="text-align: justify;", module_text["obj_06", ])
                                                      ))
                                             )
                                             ),
                                    #* Objective 7 - Prepare inputs ----
                                    tabPanel(title = "Objective 7 - Prepare inputs", value = "obj7",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 7 - Prepare inputs"),
                                                                p(style="text-align: justify;", module_text["obj_07", ])
                                                                )
                                                      )
                                             )
                                             ),
                                    #* Objective 8 - Run Forecast ----
                                    tabPanel(title = "Objective 8 - Forecast", value = "obj8",
                                             #** Input Uncertainty ----
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 8 - Generate an Ecological Forecast"),
                                                                p(style="text-align: justify;", module_text["obj_08", ])
                                                                )
                                                      )
                                               )
                                             ),
                                    #* Objective 9 - Communicate Forecast ----
                                    tabPanel(title = "Objective 9 - Communicate forecast",  value = "obj9",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 9 - Communicate an Ecological Forecast"),
                                                                p(style="text-align: justify;", module_text["obj_09", ])
                                                                )
                                                      )
                                               )
                                             ),
                                    #* Objective 10 - Assess Forecast ----
                                    tabPanel(title = "Objective 10 -  Assess forecast",  value = "obj10",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 10 - Assess an Ecological Forecast"),
                                                                p(style="text-align: justify;", module_text["obj_10", ])
                                                                )
                                                      )
                                               )
                                             )

                        )
               ),
               # 6. Activity C ----
               tabPanel(title = "Activity C", value = "mtab6",
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                 h2("Activity C: Complete the forecast cycle"),
                                 h4("Update model and make new forecast"),
                                 p("For Activity C, you will complete the forecast cycle (and begin it again!) by updating your model and generating a second forecast.")
                          )
                          )
                        ),
                        tabsetPanel(id = "tabseries3",
                                    
                                    #* Objective 11 - Update Model ----
                                    tabPanel(title = "Objective 11 - Update model",  value = "obj11",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 11 - Update Model"),
                                                                p(style="text-align: justify;", module_text["obj_11", ])
                                                      )
                                               )
                                             )
                                    ),
                                    #* Objective 12 - New Forecast ----
                                    tabPanel(title = "Objective 12 - Next forecast",  value = "obj12",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 12 - Next Forecast"),
                                                                p(style="text-align: justify;", module_text["obj_12", ])
                                                      )
                                               )
                                             )
                                    ) #end Obj 12 
                                    ) # end tabset Panel
               ), #end Act C
    ), #end navbarPage
    # Tab navigation buttons ----
    br(), hr(),
    introBox(
      # h4("Use the buttons below to navigate through the tabs", align = "center"),
      box(width = 12, status = "success",
          solidHeader = TRUE,
          fluidRow(

            column(5, align = "center",
                   # wellPanel(
                   # style = paste0("background: ", nav_bg),
                   br(),
                   # h5("Navigate to previous tab"),
                   hover_action_button(
                     inputId = "prevBtn1",
                     label = "< Module Overview",
                     button_animation = "glow",
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #0d3658; padding:15px; font-size:22px;")
                   ),
                   # actionButton("prevBtn1", "< Module Overview",
                   #              style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #0d3658; padding:15px; font-size:22px;")),
                   bsTooltip("prevBtn1", title = "Navigate to previous tab", placement = "left", trigger = "hover"),
                   br(), br()
                   # )

            ),
            column(2, align = "center",
                   # style = paste0("background: ", nav_bg),
                   br(),
                   br(), br()
            ),
            column(5, align = "center",
                   # wellPanel(
                   # style = paste0("background: ", nav_bg),
                   br(),
                   # h5("Navigate to next tab"),
                   use_hover(popback = TRUE),
                   hover_action_button(
                     inputId = "nextBtn1",
                     label = "Introduction >",
                     button_animation = "glow",
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #0d3658; padding:15px; font-size:22px;")
                   ),
                   # actionButton("nextBtn1", "Introduction >",
                   #              style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #0d3658; padding:15px; font-size:22px;")),
                   bsTooltip("nextBtn1", title = "Navigate to next tab", placement = "right", trigger = "hover"),
                   br(), br()
                   # )
            )
          )
      ), data.step = 3, data.intro = help_text["tab_nav2", 1], data.position = "right"
    ),
    hr(),
    fluidRow(
      column(8, offset = 1,
             br(),
             p(module_text["acknowledgement", ]),
             p(app_update_txt)
      )
    )
  )
}
