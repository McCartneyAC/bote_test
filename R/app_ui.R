#' main app ui
#'
#' @return shiny ui
#' @export
#'
app_ui <-  dashboardPage(skin = "black",
                    dashboardHeader(title = "Back of the Envelope"
                                    #dropdownMenuOutput("formula_message")
                    ),
                    dashboardSidebar(
                      sidebarMenu(id = "sidebar",
                                  tags$br(),
                                  tags$h3("Information:"),
                                  menuItem("About", tabName = "grand_about", icon = icon("book")),
                                  menuItem("FAQ", tabName = "FAQ_tab", icon = icon("info")),
                                  tags$h3("Your Data:"),
                                  # split upload from model
                                  menuItem("Upload", tabName = "reg_upload", icon = icon("upload")),
                                  #upload with data
                                  #  this one probably isn't ever going to be a thing:
                                  menuItem("Dossier", tabName = "reg_dossier", icon = icon("id-card"),
                                           badgeLabel = "broken", badgeColor = "red"),
                                  menuItem("Describe", tabName = "reg_desc", icon = icon("list-ol")),
                                  menuItem("Correlation Table", tabName = "reg_cor", icon = icon("th")),
                                  tags$h3("Regression:"),
                                  menuItem("Model", tabName = "reg_model", icon = icon("cogs")),
                                  #model adjustments and model view.
                                  menuItem("Summary", tabName = "reg_sum", icon = icon("list-alt")),
                                  menuItem("Plots", tabName = "reg_plot", icon = icon("line-chart")),
                                  menuItem("Diagnostics", tabName = "reg_ddx", icon = icon("x-ray")),
                                  menuItem("Outliers", tabName = "reg_outlier", icon = icon("wrench"),
                                           badgeLabel = "partial", badgeColor = "orange")




                      ), # sidebarmenu
                      tags$hr(),
                      socialButton(url = "mailto:mccartneyac@gmail.com", type = "at"),
                      socialButton(url = "https://www.reddit.com/r/learnrstats/", type = "reddit"),
                      socialButton(url = "https://www.r-project.org/", type = "r-project"),
                      socialButton(url = "https://paypal.me/mccartneyac", type = "paypal"),
                      socialButton(url = "https://github.com/McCartneyAC/average_of_polls/", type = "github")
                    ), #sidebar
                    dashboardBody(
                      tabItems(


                        #masthead
                        tabItem(tabName = "grand_about",
                                box(title = "About",width = 7,solidHeader = TRUE,
                                    tags$p("This is intended to be a toy point-and-click-style regression tool to practice R Shiny application development and to enumerate the complexities available in regression analysis. Like R itself, this tool comes with absolutely no warranty. Use the features to quickly explore options for regression and their effect on your analysis, but resist the urge to p-hack.")
                                ) , #box
                                box(title = "Use", width = 7,solidHeader = TRUE,
                                    tags$p("Use the tool by uploading your own data set in one of the listed formats. Browse your data and examine the variables' descriptive statistics, as well as the table of correlations, then create your model to run. Check the model diagnostics, distribution of error terms, and your outliers to determine if there are better options for dealing with your data. More sophisticated modeling techniques are being added on an ongoing basis."), tags$p("What this app doesn't do: This app does not allow for any kind of data preparation (yet). Techniques such as interaction terms, exponential terms, or complex extensions such as regression discontinuity need to be done in whatever data-preparation program you choose to use (e.g. excel) before data can be uploaded and used here. For example, to include polynomials, create a new variable in your dataset that is x",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      tags$sup("2"), "and re-upload the dataset to run a new regression.  Fixed effects are supported, but if you wish to choose your reference category, you will need to create dummy variables in your dataset and re-upload.")), # use
                                box(title= "Credit", width =7,solidHeader = TRUE, #for all the good it does.
                                    tags$p("Back of the Envelope was built with myriad R packages, among them: Shiny & shinydashboard for the UI, DT for the tables, psych for its ubiquitous describe function, SjPlot for model summaries, correlations, and margins plots, estimatr and MASS for robust estimations, lindia for the diagnostic models, my own package mccrr, and the tidyverse."),
                                    tags$p(tags$b("last updated: 6/22/2020")),
                                    socialButton(url = "https://github.com/McCartneyAC/Back_of_the_Envelope", type = "github")
                                ) #box
                        ) , #tabItem

                        # DATA SUBSECTION
                        #
                        tabItem(tabName = "reg_upload",
                                box( title = "Upload and Model",width = 7,
                                     fileInput("FileInput", "Input Your Data Set"),
                                     helpText("Dataset must be one of: .csv, .sav, .dta, .xlsx, or .rda")
                                ), #upload box
                                box(title = "Your Data", width = 11,
                                    DT::dataTableOutput("reg_data_table")
                                ) # box (Dataset output)

                        ), #tabItem (reg upload)
                        # # Variables
                        tabItem(tabName = "reg_dossier", title = "Dossier",
                                box(
                                  tags$p("The Dossier function allows you to select an individual observation among your data and observe all of its variables. This can be useful for detecting data problems, examining particular outliers, or seeing your variables at a glance."),
                                  selectInput("reg_dossier_ID","Choose an ID Variable:",
                                              choices = NULL,
                                              selected = NULL),
                                  selectInput("reg_dossier_choice", "Choose an Observation:",
                                              choices = NULL,
                                              selected = NULL)
                                ), #box
                                box(
                                  DT::dataTableOutput("dossier")
                                ) #box
                        ),# tabItem Dossier
                        # # Describe
                        tabItem(tabName = "reg_desc", title = "Describe",
                                box(title = "Data Description", width  = 10,
                                    DT::dataTableOutput("description"))
                        ),

                        # # Correlation
                        tabItem(tabName = "reg_cor", title = "Correlations",
                                box(title = "Correlation Matrix", width = 12, height = 700,
                                    plotOutput("cors")
                                )# box
                        ), #tabItem




                        # MODEL SUBSECTION


                        tabItem(tabName = "reg_model", title = "Build your Model",
                                box(title = "Your Model:",width = 7,
                                    textOutput("regformula")
                                ), # model output box



                                box(title = "Build Your Model:",width = 7,
                                    radioButtons(
                                      inputId = "rgrssn", label = "Regression:",
                                      choices = c("linear" = "linear",
                                                  "logistic" = "logistic")
                                    ),
                                    #shinywidget
                                    materialSwitch(inputId = "rbst", label = "Robust Standard Errors"),
                                    #wired_toggle(inputId = "rbst", label = "Robust Standard Errors"),
                                    tags$p(tags$b("Select your variables for analysis:")),
                                    selectInput(inputId = "responsevar",
                                                label = "Your DV / Response Variable:",
                                                choices = NULL
                                    ),

                                    selectizeInput("indevars", "Your IV / Predictor Variable(s):",
                                                   choices = NULL,
                                                   selected = NULL,
                                                   multiple = TRUE,
                                                   options = NULL),
                                    selectInput("instrument","Instrumental Variable (forthcoming)",
                                                choices = NULL,
                                                selected = NULL),
                                    selectInput(
                                      inputId = "clstr",
                                      label = "Clustering Type:",
                                      choices = c("No Clustering",
                                                  "Fixed Effects",
                                                  "Cluster Standard Errors",
                                                  "Multilevel / LME / HLM (coming soon)"),
                                      selected = NA
                                    ),
                                    selectInput(inputId = "clust",
                                                label = "Cluster Variable:",
                                                choices = NULL
                                    ) #, #select input
                                    # tags$p("Subset by Variable"),
                                    # selectInput(inputId = "subset_var",
                                    #             label = "Subset Variable:",
                                    #             choices = NULL
                                    # ),
                                    # selectInput(inputId = "subset_val",
                                    #             label = "Subset Value:",
                                    #             choices = NULL
                                    # )
                                ) #model building box

                        ), #tabitem model building
                        # # Plot
                        tabItem(tabName = "reg_plot", title = "Plot",
                                tabsetPanel(type = "tabs",
                                            tabPanel("Marginal Effects",
                                                     box(title = "Marginal Effects", width = 9,
                                                         plotOutput("marginal")) #box
                                            ),
                                            tabPanel("One IV",
                                                     box(title = "Univariate Plot", width = 9,
                                                         tags$p("This plot accepts only the first Independent Variable chosen."), tags$br(),
                                                         tags$p("If your model has more than 1 predictor, it will be ignored.
                                                              To change which variable is shown, simply return to the model page and re-order the variables. "),
                                                         plotOutput("bivariate"),
                                                         tags$p("residuals:"),
                                                         plotOutput("bivar_resid"))
                                            ),
                                            tabPanel("Two IVs",
                                                     box(title = "Multivariate Plot", width = 9,
                                                         tags$p("This plot accepts only the first two Independent Variables chosen."), tags$br(),
                                                         tags$p("If your model has more than 2 predictors, those are ignored.
                                                              To change which variable is represented by color, simply return to the model page and re-order the variables. "),
                                                         plotOutput("trivariate"))#box
                                            ),
                                            tabPanel("Added Variable Plots",
                                                     box(title = "Added Variable Plots", width = 9,
                                                         selectInput(inputId = "restricted",
                                                                     label = "Select your Predictor",
                                                                     choices = NULL
                                                         ),
                                                         helpText("AV Plots require at least two continuous predictor variables. Otherwise you will receive an error."),
                                                         plotOutput("avplot")

                                                     ) #box
                                            ) #tab panel (added variable)
                                ) #tabset panel (plotting tabs)
                        ), #tabitem (plots page)
                        # # Summary
                        tabItem(tabName = "reg_sum", title = "Output Summary",
                                box(
                                  #  TODO: Be sure to include a null_model if LME is selected and if a cluster is chosen
                                  # tags$br(),
                                  htmlOutput("tabmodel")
                                ) #box
                        ),
                        # # Diagnostic Plots
                        tabItem(tabName = "reg_ddx", title = "Diagnostic Plots",
                                tabsetPanel(type = "tabs",
                                            tabPanel("Residuals",
                                                     box(plotOutput("hist_resid"))
                                            ),
                                            tabPanel("QQ Plot",
                                                     box(plotOutput("normal_qq"))
                                            ),
                                            tabPanel("Residual vs Fitted",
                                                     box(plotOutput("resid_v_fitted"))
                                            )# tab panel
                                ) #tabset panel
                        ), #reg_ddx tab
                        # # Outliers
                        tabItem(tabName = "reg_outlier", title = "Outlier Analysis",
                                tabsetPanel(type = "tabs",
                                            tabPanel("Cook's Distance",
                                                     box(plotOutput("cooks_d"))
                                            ), # Cook's Distance
                                            tabPanel("Leverage",
                                                     box(
                                                       tags$p("Coming soon, I promise"),
                                                       tags$p("Here: interactive graph to allow for point-and-click deletion of outlying points."),
                                                       plotOutput("brush_plot", height = 350,
                                                                  click = "plot1_click",
                                                                  brush = brushOpts(
                                                                    id = "plot1_brush"
                                                                  )),
                                                       actionButton("exclude_toggle", "Toggle points"),
                                                       actionButton("exclude_reset", "Reset")
                                                     )),
                                            tabPanel("Influence Index",
                                                     box(
                                                       tags$p("Leverage: extremity on X"),
                                                       tags$p("Discrepancy: extremity on Y"),
                                                       tags$p("Coming soon, I promise"),
                                                       tags$p("First - Studentized Residuals"),
                                                       tags$p("Second - hat values"),
                                                       # car::influenceIndexPlot(model())
                                                     )#box
                                            )#tabPanel
                                ) #tabset panel
                        ),# tab item.
                        tabItem(tabName = "FAQ_tab",
                                box(title = "Frequently Asked",
                                    tags$p(tags$b("How did it get started? "),
                                           "Back of the Envelope is the culmination of two (and maybe more?) project ideas that I have worked on in the year 2019. When I first learned to use Shiny R, I couldn't get the idea out of my head that someone should build a point-and-click style regression tool that utilized all and only those presets that I found helpful and that gave its output in ways that I tended to use when doing homework or preparing presentations and publications. Several extant R packages were outputting results in APA format our otherwise had defaults that were best-in-the-industry for a grad student. After leaving grad school, I put this idea into practice. The original version used wired.js and R's xkcd package to make all regression plots and fonts look hand-drawn. In this iteration, I have restored defaults so you can use it directly in publications. You're welcome...but I do miss the sketchiness.

The idea is a dangerous amount of statistical sophistication: enough to give it a strong semblance of accuracy, but leaving out the super technical details that might be important for research publication. Back of the Envelope regression is good enough for stat homework and basic publications."
                                    ),
                                    tags$p(tags$b("How can I support the project?"),
                                           "By clicking on the paypal link on the left, you can contribute to the project and keep it up and running."),
                                    tags$p(tags$b("I got an error I don't understand!"), "In general, getting an error means you tried to do something that is either unsupported by the app at this time or impossible under the constraints or definitions of contemporary statistics. For example, supplying a continuous variable for logistic regression will get an error. (Supplying a binary variable to linear regression will get you a linear probability model.). Otherwise, leaving things blank will produce errors--did you forget to upload data or define a model? That's the usual culprit."),
                                    tags$p(tags$b("I can't run the model I want!"),
                                           "As noted above, some things just aren't possible. For example, residuals are not normally distributed around a logistic regression model, so trying to check the residuals graph for logistic models will throw an error. If you're trying to do something that you think should be possible, please contact me (the @ symbol on the left). Binary models with mixed effects are currently unsupported, but it's in the works."),
                                    tags$p(tags$b("Any mathematical notes?"),
                                           "Yes. Things like sums of squares vary from package to package, and significantly, between R and Stata. The sums of squares and standard errors found herein may or may not match those of other software. In general, it's best to cite your estimation package. Most of the available regression models herein are from the packages: MASS, CAR, estimatr, or lme4.")
                                ), #box
                                box(tags$p("Include a diagram of available options (errors, etc)")) #box
                        ) # tab item. (LAST ONE)

                      ) #tabitems
                    ) #Dashboard Body
) #Dashboard Page
