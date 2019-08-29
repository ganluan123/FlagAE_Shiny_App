
shinyUI (fluidPage(

  # enter the title
  titlePanel(title="Flag Adverse Event"),

  # create the layout
  sidebarLayout(
    sidebarPanel(width=2,
      # ADSL input
      fileInput("ADSLInput", "Please upload ADSL file", 
                accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
      # reminder about ADSL dataset
      uiOutput("ADSLreminder"),
      br(),
      # ADAE input
      fileInput("ADAEInput", "Please upload ADAE file",
                accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
      # reminder about ADAE dataset
      uiOutput("ADAEreminder"),
      br(),
      htmlOutput("fileloadreminder")

    ),

    mainPanel(
      # divide the output into several tabset
      tabsetPanel(type = "tabs",

                  ################################################################################
                  ##############                                             #####################
                  ############## get the summary of the adverase event data  #####################
                  ##############                                             #####################
                  ################################################################################

                    tabPanel ("Summary",
                              # "AE summary" is to get the summary information
                              # of the dataset, including how many patients in
                              # control group, in treatment group, number of SoC
                              # and number of PT
                              DT::dataTableOutput("AEsummary"),
                              br(), br(),

                              # number of AE and parameter for PREplot
                              fluidRow(
                                textOutput("PREplottext"),
                                tags$head(tags$style("#PREplottext{
                                                      font-size: 20px;
                                                     font-style: bold;
                                                     }")),
                                column(width=4, offset=0,
                                       uiOutput("PTnumPREplot")
                                       ),
                                column(width=4, offset = 0,
                                       uiOutput("paramPREplot")
                                       )
                              ),
                              plotOutput("PREplot"),
                              uiOutput("PREplotdownjpeg"),
                              br(), br(),



                              # "AEdata" is the output from preprocess4 which
                              # summarizes the information in term of adverse events
                              # including number of occurances in treatment and control group
                              # also the index for SoC, and PT
                              DT::dataTableOutput("AEdata"),
                              uiOutput("AEdatatabledown")
                              ),

                  ################################################################################
                  ##############                                             #####################
                  ##############              Binomial CI plot               #####################
                  ##############                                             #####################
                  ################################################################################

                    tabPanel("Binomial CI",

                             # Let user select the number of AE to show and the confidence level
                             # use uiOutput to make sure the option will not show before
                             # user selecting to show the plot
                             # uiOutput("PTnumInput"),
                             numericInput("PTnumInput", "number of adverse events to show", min = 1, value = 10),
                             # uiOutput("confInput"),
                             numericInput("confInput", "confidence level for Binomial CI",
                                          value = 0.95, min=0, max=1, step = 0.025),
                             # Let user to select whether to calcuate the result from Fisher Exact Test
                             #checkboxInput("BCIInput", "Check the box to show the Fisher Exact Test result", FALSE),
                             actionButton("BCIInput", "Run", width='25%'),
                             br(), br(),

                             # plot the AEs with biggest difference in incidence risk between
                             # treatment and control group
                             plotOutput("BCIplot"),
                             # download option
                             # use uiOutput to make sure that the download button wont appear
                             # before the plot shown


                             uiOutput("BCIplotdownjpeg"),


                             br(),br(),

                             # show the information for AEs in the plot
                             DT::dataTableOutput("TopAE"),
                             # download option
                             # use uiOutput to make sure that the download button wont appear
                             # before the table shown
                             uiOutput("BCItabledown")
                             ),

                  ###############################################################################
                  ##############                                             #####################
                  ##############        3 stage Hierarchical model           #####################
                  ##############                                             #####################
                  ################################################################################


                  tabPanel("Hierarchical Model",



                           fluidRow(
                             column(9,
                                    offset=0,

                                        tags$h4("Gibbs sampling paramters and Hyperparameters"),
                                        br(),
                                        uiOutput("Hierpaperlink"),
                                        br(),
                                    fluidRow(
                                      column(3,
                                             numericInput("HieradaptInput", "Adaptation", value = 1000),
                                             numericInput("HierthinInput", "Thin", value = 20),
                                             numericInput("alpha.theta.input",HTML("&alpha;<sub>&theta;</sub>") , value = 3),
                                             numericInput("mu.theta.0.0.input",HTML("&mu;<sub>&theta;00</sub>") , value = 0),
                                             numericInput("beta.gamma.input",HTML("&beta;<sub>&gamma;</sub>") , value = 1),
                                             numericInput("beta.gamma.0.0.input",HTML("&beta;<sub>&gamma;00</sub>") , value = 1),
                                             numericInput("lambda.beta.input",HTML("&lambda;<sub>&beta;</sub>") , value = 0.1)

                                             ),
                                      column(3,
                                             numericInput("HierburnInput", "Burn In", value = 5000),
                                             numericInput("HierchainInput", "Chains", value = 2),
                                             numericInput("mu.gamma.0.0.input",HTML("&mu;<sub>&gamma;00</sub>") , value = 0),
                                             numericInput("alpha.theta.0.0.input",HTML("&alpha;<sub>&theta;00</sub>") , value = 3),
                                             numericInput("beta.theta.input",HTML("&beta;<sub>&theta;</sub>") , value = 1),
                                             numericInput("tau.theta.0.0.input",HTML("&tau;<sub>&theta;00</sub>") , value = 0.1)


                                             ),
                                      column(3,
                                             numericInput("HieriterInput", "Iterations", value = 10000),
                                             numericInput("alpha.gamma.input",HTML("&alpha;<sub>&gamma;</sub>") ,  value = 3),
                                             numericInput("alpha.gamma.0.0.input",HTML("&alpha;<sub>&gamma;00</sub>") , value = 3),
                                             numericInput("lambda.alpha.input",HTML("&lambda;<sub>&alpha;</sub>"), value = 0.1),
                                             numericInput("tau.gamma.0.0.input",HTML("&tau;<sub>&gamma;00</sub>") , value = 0.1),
                                             numericInput("beta.theta.0.0.input",HTML("&beta;<sub>&theta;00</sub>") , value = 1)
                                             )
                                    ),
                                    actionButton("HierInput", "Run", width = '75%')
                                    ),


                             column(3,
                                    offset=0,
                                    div(style='margin-left: -10em;',
                                    # tags$h4("Plot top Adeverse Events"),
                                    tags$h4("Select the plot"),
                                    radioButtons("Hierplotselect", label=NULL, choices=c("Compare raw data and model", "Plot Top Adverse Events"),
                                                 selected='Compare raw data and model'),
                                    numericInput("Hierplotptnum", "Number of AEs to plot", value=10),
                                    selectInput("Hierplotparam", "summary statistics based on to select AE",
                                                c("risk difference", "odds ratio"), selected = "risk difference"),

                                    # if user select to plot based on "odds ratio",
                                    # provide the user option to specify the y-axis limit
                                    uiOutput("HierORxlimLB"),
                                    uiOutput("HierORxlimUB"),

                                    actionButton("HierplotInput", "Plot", width='65%'),

                                    # show the text reminder to make sure that user run the model
                                    # before they try to plot out the AEs
                                    uiOutput("Hiermodelfirst"),

                                    # plot top AEs from the Hierrachical model
                                    plotOutput("Hierplot"),

                                    uiOutput("Hierplotdown"),

                                    # download option for table of top AEs
                                    uiOutput("Hiertabledown")
                                    )
                             )
                           ),

                  
                           # since the simulation usually takes a long time,
                           # have a run button to make sure that the simulation
                           # only run after selecting all the parameter and hit the run button

                           br(),br(),

                           # Hierarchical model table output
                           DT::dataTableOutput("Hierfulltable"),

                           # download option
                           uiOutput("Hierfulltabledown")

                  ),

                  ################################################################################
                  ##############                                             #####################
                  ##############              Ising Prior model              #####################
                  ##############                                             #####################
                  ################################################################################
                  tabPanel("Ising Prior model",

                           fluidRow(
                           column(6,
                                  offset=0,
                                  tags$h4("Gibbs sampling paramters and Hyperparameters"),
                                  br(),
                                  uiOutput("Isingpaperlink"),
                                  br(),
                                  fluidRow(
                                    column(3,
                                           offset=0,
                                           numericInput("IsingburnInput", "Burn In", value = 5000),
                                           numericInput("IsingthinInput", "Thin", value = 20),
                                           numericInput("beta.input", HTML("&beta;"), value = 0.75),
                                           numericInput("beta.t.input", HTML("&beta;<sup>t</sup>"), value = 0.75),
                                           numericInput("beta.c.input", HTML("&beta;<sup>c</sup>"), value = 0.75),
                                           numericInput("theta.input", HTML("&theta;"), value = 0.02)

                                           ),
                                    column(3,
                                           offset=0,
                                           numericInput("IsingiterInput", "Iterations", value = 10000),
                                           numericInput("alpha.input",HTML("&alpha;") , value = 0.25),
                                           numericInput("alpha.t.input",HTML("&alpha;<sup>t</sup>") , value = 0.25),
                                           numericInput("alpha.c.input",HTML("&alpha;<sup>c</sup>") , value = 0.25),
                                           numericInput("rho.input", HTML("&rho;"), value = 1)
                                           )
                                  ),
                                  actionButton("IsingInput", "Run", width = '50%')
                           ),




                           column(6,
                                  offset=0,
                                  div(style='margin :0%;',
                                      # tags$h4("Plot top Adeverse Events"),
                                      tags$h4("Select the plot"),
                                      radioButtons("Isingplotselect", label=NULL, choices=c("Compare raw data and model", "Plot Top Adverse Events"),
                                                   selected='Compare raw data and model'),
                                      # Let user input the Gibbs sampling paramters
                                      numericInput("Isingplotptnum", "Number of AEs to plot", value=10),
                                      selectInput("Isingplotparam", "summary statistics based on to select AE",
                                                  c("risk difference", "odds ratio"), selected = "risk difference"),

                                      # if user select to plot based on "odds ratio",
                                      # provide the user option to specify the y-axis limit
                                      uiOutput("IsingORxlimLB"),
                                      uiOutput("IsingORxlimUB"),

                                      actionButton("IsingplotInput", "Plot", width='50%'),

                                      # show the text reminder to make sure that user run the model
                                      # before they try to plot out the AEs
                                      uiOutput("Isingmodelfirst"),

                                      # plot top AEs from the Hierrachical model
                                      plotOutput("Isingplot"),

                                      uiOutput("Isingplotdown"),

                                      # download option for table of top AEs
                                      uiOutput("Isingtabledown")
                                  )
                           )

                           ),
                           # since the simulation usually takes a long time,
                           # have a run button to make sure that the simulation
                           # only run after selecting all the parameter and hit the run button

                           br(),br(),

                           # Hierarchical model table output
                           DT::dataTableOutput("Isingfulltable"),

                           # download option
                           uiOutput("Isingfulltabledown")

                           ),

                  ################################################################################
                  ##############                                             #####################
                  ##############          Comparison of two models          #####################
                  ##############                                             #####################
                  ################################################################################

                  tabPanel("Model Comparison",
                           fluidRow(
                             column(6,
                                    offset=0,
                                    div(style='margin:0%;',
                                        tags$h4("compare by plotting"),
                                        numericInput("HIplotptnum", "Number of AEs from each model to plot", value=10),
                                        selectInput("HIplotparam", "summary statistics based on to select AE",
                                                    c("risk difference", "odds ratio"), selected = "risk difference"),

                                        # if user select to plot based on "odds ratio",
                                        # provide the user option to specify the y-axis limit
                                        uiOutput("HIORxlimLB"),
                                        uiOutput("HIORxlimUB"),

                                        actionButton("HIplotInput", "Plot", width='50%'),

                                        # show the text reminder to make sure that user run the model
                                        # before they try to plot out the AEs
                                        uiOutput("HImodelfirst"),

                                        # plot top AEs from the Hierrachical model
                                        plotOutput("HIplot"),

                                        uiOutput("HIplotdown"),

                                        # download option for table of top AEs
                                        uiOutput("HItabledown")

                                        )
                                    ),

                             column(6,
                                    offset=0,
                                    div(style='margin:0%;',
                                        tags$h4("compare by cross validation"),
                                        numericInput("CVkfdInput", "k value for k-fold cross validation", min=2, value=5),
                                        actionButton("CVInput", "Run", width='50%'),
                                        br(),br(),
                                        DT::dataTableOutput("CVlosstable"),
                                        # download option for table of Loss
                                        uiOutput("CVtabledown")


                                        ))
                           ))

              )


    )

  )

)
)
