# increase the maximum allowed size for uploading file
options(shiny.maxRequestSize=100*1024^2)
# call the libarary FlagAE
library(FlagAE)
library(DT)
library(ggplot2)


#####################################################################################################
#######################################################################################################
ADSL<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\demo_ADSL_TRTCTR.csv")
ADAE<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\demo_ADAE_TRTCTR.csv")

#######################################################################################################
#######################################################################################################

server <- function(input, output) {

  ################################################################################
  ##############                                             #####################
  ############## get the summary of the adverase event data  #####################
  ##############                                             #####################
  ################################################################################

  AEdata<-function(){
   preprocess(adsl=ADSL, adae=ADAE)
  }


#######################################################################################################
#######################################################################################################
#   # get the ADSL.csv
#   # ADSL is a reactive variable
#   ADSL<-reactive({
#     ADSLread<-input$ADSLInput
#     if (is.null(ADSLread))
#       return(NULL)
#     read.csv(ADSLread$datapath, header=TRUE)
#   })
#
#   # get the ADAE.csv
#   # ADAE is a reactive variable
#   ADAE<-reactive({
#     ADAEread<-input$ADAEInput
#     if (is.null(ADAEread))
#       return(NULL)
#     read.csv(ADAEread$datapath, header=TRUE)
#   })
#
#   # get the AEdata from preprocess4 function in libarary FlagAE
#   AEdata<-reactive({
#
#     ADSLread<-input$ADSLInput
#     ADAEread<-input$ADAEInput
#     if(is.null(ADSLread) | is.null(ADAEread)) return (NULL)
#
#     preprocess(adsl=ADSL(), adae=ADAE())
#   })
#
#
# ###############################################################################################################
# ###############################################################################################################
  # output subject and adverse event summary
  output$AEsummary<-DT::renderDataTable({

    if (is.null (AEdata())) return ()

    Item<-c("total subjects", "treatment group", "control group", "total SOC", "total PT")
    Number<-c(AEdata()$Nt[1]+AEdata()$Nc[1], AEdata()$Nt[1], AEdata()$Nc[1], max(AEdata()$b), max(AEdata()$i))
    df<-data.frame(Item=Item, Number=Number)
    },
    caption = "Subject and adverse event summary"

    )

  # output the adverse event summary
  output$AEdata<-DT::renderDataTable({
    if (is.null(AEdata())) return ()

    AEdata() },

    caption = "Adverse event summary"

    )


  ################################################################################
  ##############                                             #####################
  ##############        Fisher exact test plot               #####################
  ##############                                             #####################
  ################################################################################


  # output the Plot for fisher exact test

  # take PTnumInput and confInput if user select to plot out Fisher Exact Test Plot
  output$PTnumInput<-renderUI({
    #if(input$BCIInput==FALSE ) return (NULL)
    numericInput("PTnumInput", "Insert the number of adverse events to show", value = 10)
  })

  output$confInput<-renderUI({
    #if(input$BCIInput==FALSE)  return (NULL)
    numericInput("confInput", "Insert the confidence level for fisher exact test",
                               value = 0.95, min=0, max=1, step = 0.025)
  })

  # create the plot with activation button

  # the BCIv is to make sure that the plot and table will
  # disappear and will not run before user click 'Run' button
  BCIv <- reactiveValues(doPlot = FALSE)

  observeEvent(input$BCIInput, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    BCIv$doPlot <- input$BCIInput
  })

  # my understanding is that this is to make sure that
  # whenever user change PTnumInput or confInput, the plot
  # will disappear and wont run before user click run button
  observeEvent((input$PTnumInput & input$confInput), {
    BCIv$doPlot <- FALSE
  })

  BCIplotInput<-function(){

    if (BCIv$doPlot==0 | is.null(input$PTnumInput) | is.null(input$confInput)) return ()
    #if (BCIv$doPlot==0 ) return ()
    #if (!(BCIv$doPlot==0 | is.null(input$PTnumInput) | is.null(input$confInput))){
    isolate({
    BCIplot(aedata=AEdata(), ptnum = input$PTnumInput, conf.level = input$confInput)
    })

  }
  # plot out the plot
  output$BCIplot<-renderPlot({
    if(BCIv$doPlot==FALSE) return ()
    BCIplotInput()

  })
  # provide the download option for user

  output$BCIplotdownjpeg<-renderUI({
    if(!(BCIv$doPlot==FALSE) & !(is.null(input$PTnumInput)) & !(is.null(input$confInput))){
      downloadButton('BCIplotdownloadjpeg', "Download the plot")
    }
  })

  output$BCIplotdownloadjpeg <- downloadHandler(
    filename <- "BCIplot.jpeg",
    content <- function(file) {
      jpeg(file, width = 1100, height=720, quality = 450, pointsize = 9, res = 180)
      # jpeg(file)
      BCIplotInput()
      dev.off()
  })


  # create the table for details of AE selected in above plot
  BCItableInput<-reactive({
    if (BCIv$doPlot==0 | is.null(input$PTnumInput) | is.null(input$confInput)) return ()

    topPTlist<-BCItable(aedata=AEdata(), ptnum=input$PTnumInput, conf.level = input$confInput)
  })

  output$TopAE<-DT::renderDataTable({
    BCItableInput()
  },
  caption = "Detail information of adverse events shown in plot above"
  )

  # create the download option for user to download the table
  output$BCItabledown<-renderUI({
    if(!(BCIv$doPlot==0) & !(is.null(input$PTnumInput)) & !(is.null(input$confInput))){
      downloadButton('BCItabledownload', "Download the table")
    }
  })

  output$BCItabledownload<-downloadHandler(
    filename = function(){paste("BCItable", ".csv")},
    content =function(file){
      write.csv(BCItableInput(), file, row.names = FALSE)
    })




  ################################################################################
  ##############                                             #####################
  ##############        3 stage Hierarchical model           #####################
  ##############                                             #####################
  ################################################################################


  # show the table from the hierarchical model
  # summary of the hierarchical model
  # we DO NOT want the model to run each time user change the a single paramter
  # we want the model to run after user finish changing all parameters
  # and hit the run button

  # the Hierv is to make sure that the plot and table will
  # disappear and will not run before user click 'Run' button
  Hierv <- reactiveValues(doPlot = FALSE)

  observeEvent(input$HierInput, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    Hierv$doPlot <- input$HierInput
  })

  observeEvent((input$HieradaptInput & input$HierburnInput &
                  input$HieriterInput & input$HierthinInput &
                  input$HierchainInput &
                  input$alpha.gamma.input & input$alpha.theta.input &
                  input$mu.gamma.0.0.input & input$alpha.gamma.0.0.input &
                  input$mu.theta.0.0.input & input$alpha.theta.0.0.input &
                  input$lambda.alpha.input &
                  input$beta.gamma.input & input$beta.theta.input &
                  input$tau.gamma.0.0.input & input$beta.gamma.0.0.input &
                  input$tau.theta.0.0.input & input$beta.theta.0.0.input &
                  input$lambda.beta.input), {
    Hierv$doPlot <- FALSE
  })



  #run the model
  Hiermodel<- function(){

    if (Hierv$doPlot==FALSE ) return ()

    isolate({

        HierData<-Hier(aedata=AEdata(), n_burn=input$HierburnInput,
                       n_iter=input$HieriterInput, thin=input$HierthinInput, n_adapt = input$HieradaptInput,
                       n_chain = input$HierchainInput,
                       alpha.gamma =input$alpha.gamma.input, beta.gamma = input$beta.gamma.input,
                       alpha.theta = input$alpha.theta.input, beta.theta = input$beta.theta.input,
                       mu.gamma.0.0 = input$mu.gamma.0.0.input, tau.gamma.0.0 = input$tau.gamma.0.0.input,
                       alpha.gamma.0.0 = input$alpha.gamma.0.0.input, beta.gamma.0.0 = input$beta.gamma.0.0.input,
                       lambda.alpha = input$lambda.alpha.input, lambda.beta = input$lambda.beta.input,
                       mu.theta.0.0 = input$mu.theta.0.0.input, tau.theta.0.0 = input$tau.theta.0.0.input,
                       alpha.theta.0.0 = input$alpha.theta.0.0.input, beta.theta.0.0 = input$beta.theta.0.0.input)

     })
  }

  # show the table
  HIERDATA<-reactive({
    if (Hierv$doPlot==FALSE) return ()
    Hiermodel()
  })
  output$Hierfulltable<-DT::renderDataTable({
    HIERDATA()
  })

  # table download option
  output$Hierfulltabledown<-renderUI({
    if (Hierv$doPlot==FALSE) return ()
    downloadButton('Hierfulltabledownload', "Download the whole table")
  })

  output$Hierfulltabledownload<-downloadHandler(
    filename <- function(){paste("Hiermodelfulltable", ".csv")},
    content <-function(file){
      write.csv(Hiermodel(), file, row.names = FALSE)
    }
  )

  # show the Hier plot of top AEs
  # if user choose to plot based on "odds ratio", provide the option to select y-axis limit
  output$HierORylimLB<-renderUI({
    if (input$Hierplotparam=="risk difference") return ()
    numericInput("HierORylimLBInput", "y-axis lower limit", value=0)
  })

  output$HierORylimUB<-renderUI({
    if (input$Hierplotparam=="risk difference") return ()
    numericInput("HierORylimUBInput", "y-axis upper limit", value=5)
  })

  HierplotInput<-function(){
    if (Hierv$doPlot==FALSE) return ()
    if (input$HierplotInput==FALSE) return()
    isolate({
      if (input$Hierplotparam=="risk difference"){
        Hierplot(HIERDATA(), ptnum=input$Hierplotptnum, param=input$Hierplotparam)
      }
      else {
        Hierplot(HIERDATA(), ptnum=input$Hierplotptnum, param=input$Hierplotparam,
                 OR_ylim = c(input$HierORylimLBInput, input$HierORylimUBInput))
      }
    })

  }

  output$Hierplot<-renderPlot({
    if(is.null(HierplotInput())) return ()
    if(input$HierplotInput==FALSE) return ()
    HierplotInput()
  })

  # show the warning if user try to plot before running model
  output$Hiermodelfirst<-renderUI({
    if(is.null(HierplotInput()) & !(input$HierplotInput==FALSE)) {
      textOutput("Modelfirstoutput")
      # span(textOutput("Modelfirstoutput"), style="color:red")
    }
  })

  output$Modelfirstoutput<-renderText({
    # if (!(is.null(HierplotInput()) & !(input$HierplotInput==FALSE))) return ()
    "Please run the model before plot"
  })

  # download option for Hier plot of top AEs
  output$Hierplotdown<-renderUI({
    if(!is.null(HierplotInput()) & !(input$HierplotInput==FALSE)){
      downloadButton('Hierplotdownload', "Download the plot")
    }
  })

  output$Hierplotdownload <- downloadHandler(
    filename <- function(){paste0("Hireplot", ".jpeg")},
    content <- function(file) {
      #jpeg(file, width = 1100, height=720, quality = 450, pointsize = 9, res = 180)
      #jpeg(file)
      ggsave(file, plot=HierplotInput())
    })


  # download option for table containing information of AE in the plot
  HiertableInput<-function(){
    if (Hierv$doPlot==FALSE) return ()
    Hiertable(HIERDATA(), ptnum=input$Hierplotptnum, param=input$Hierplotparam)
  }

  output$Hiertabledown<-renderUI({
    if(is.null(HierplotInput())) return ()
    if(input$HierplotInput==FALSE) return ()
    downloadButton('Hiertabledownload', "Download table for details of AEs shown in plot")
  })

  output$Hiertabledownload<-downloadHandler(
    filename <- function(){paste("Hiertable", ".csv")},
    content <-function(file){
      write.csv(HiertableInput(), file, row.names = FALSE)
    }
  )







}
