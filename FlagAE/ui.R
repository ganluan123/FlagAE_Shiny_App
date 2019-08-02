library(shiny)
ui <- fluidPage(

  # enter the title
  titlePanel(title="Flag Adverse Event with Bayesian Methods"),

  # create the layout
  sidebarLayout(
    sidebarPanel(
      # ADSL input
      fileInput("ADSLInput", "Please upload ADSL file"),
      # ADAE input
      fileInput("ADAEInput", "Please upload ADAE file")

    ),

    mainPanel(
      # divide the output into several tabset
      tabsetPanel(type = "tabs",

                  ################################################################################
                  ##############                                             #####################
                  ############## get the summary of the adverase event data  #####################
                  ##############                                             #####################
                  ################################################################################

                    tabPanel ("Adverse Event Summary",
                              # "AE summary" is to get the summary information
                              # of the dataset, including how many patients in
                              # control group, in treatment group, number of SoC
                              # and number of PT
                              DT::dataTableOutput("AEsummary"),
                              br(), br(),

                              # "AEdata" is the output from preprocess4 which
                              # summarizes the information in term of adverse events
                              # including number of occurances in treatment and control group
                              # also the index for SoC, and PT
                              DT::dataTableOutput("AEdata")
                              ),

                  ################################################################################
                  ##############                                             #####################
                  ##############        Fisher exact test plot               #####################
                  ##############                                             #####################
                  ################################################################################

                    tabPanel("Fisher Exact Test",

                             # Let user select the number of AE to show and the confidence level
                             # use uiOutput to make sure the option will not show before
                             # user selecting to show the plot
                             uiOutput("PTnumInput"),
                             uiOutput("confInput"),
                             # Let user to select whether to calcuate the result from Fisher Exact Test
                             #checkboxInput("FETInput", "Check the box to show the Fisher Exact Test result", FALSE),
                             actionButton("FETInput", "Run", width='100%'),
                             br(), br(),

                             # plot the AEs with biggest difference in incidence risk between
                             # treatment and control group
                             plotOutput("FETplot"),
                             # download option
                             # use uiOutput to make sure that the download button wont appear
                             # before the plot shown
                             uiOutput("FETplotdown"),

                             br(),br(),

                             # show the information for AEs in the plot
                             DT::dataTableOutput("TopAE"),
                             # download option
                             # use uiOutput to make sure that the download button wont appear
                             # before the table shown
                             uiOutput("FETtabledown")
                             ),

                  ################################################################################
                  ##############                                             #####################
                  ##############        3 stage Hierarchical model           #####################
                  ##############                                             #####################
                  ################################################################################


                  tabPanel("Three Stage Hierarchical Model",

                           fluidRow(
                             column(5,
                                    h4("Gibbs sampling paramters"),
                                    # Let user to select the Gibbs sampling paramters
                                    uiOutput("HierSampleInput")

                                    ),

                             column(5,
                                    h4("Initials"),
                                    uiOutput("HierDiffInput"),
                                    #checkboxInput("HierDiffInitInput", "Check to use DIFFERENT initials for different chains"),
                                    # take the initials
                                    uiOutput("HierInitInput")

                                    )


                           ),


                           # since the simulation usually takes a long time,
                           # have a run button to make sure that the simulation
                           # only run after selecting all the parameter and hit the run button
                           actionButton("HierInput", "Run", width = '100%'),
                           br(),br(),
                           # Hierarchical model table output
                           DT::dataTableOutput("Hiertable")



                  )

                   )


    )

  )

)
