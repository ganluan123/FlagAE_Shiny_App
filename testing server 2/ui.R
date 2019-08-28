
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
      br()
      # htmlOutput("fileloadreminder")

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
                              )

                  ################################################################################
                  ##############                                             #####################
                  ##############              Binomial CI plot               #####################
                  ##############                                             #####################
                  ################################################################################

                    # tabPanel("Binomial CI",
                    # 
                    #          # Let user select the number of AE to show and the confidence level
                    #          # use uiOutput to make sure the option will not show before
                    #          # user selecting to show the plot
                    #          uiOutput("PTnumInput"),
                    #          uiOutput("confInput"),
                    #          # Let user to select whether to calcuate the result from Fisher Exact Test
                    #          #checkboxInput("BCIInput", "Check the box to show the Fisher Exact Test result", FALSE),
                    #          actionButton("BCIInput", "Run", width='25%'),
                    #          br(), br(),
                    # 
                    #          # plot the AEs with biggest difference in incidence risk between
                    #          # treatment and control group
                    #          plotOutput("BCIplot"),
                    #          # download option
                    #          # use uiOutput to make sure that the download button wont appear
                    #          # before the plot shown
                    # 
                    # 
                    #          uiOutput("BCIplotdownjpeg"),
                    # 
                    # 
                    #          br(),br(),
                    # 
                    #          # show the information for AEs in the plot
                    #          DT::dataTableOutput("TopAE"),
                    #          # download option
                    #          # use uiOutput to make sure that the download button wont appear
                    #          # before the table shown
                    #          uiOutput("BCItabledown")
                    #          )

                  

              )


    )

  )

)
)
