# increase the maximum allowed size for uploading file
options(shiny.maxRequestSize=100*1024^2)
# call the libarary FlagAE
library(FlagAE)
library(DT)
library(ggplot2)


#####################################################################################################
#######################################################################################################
# ADSL<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\demo_ADSL_TRTCTR.csv")
# ADAE<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\demo_ADAE_TRTCTR.csv")

#######################################################################################################
#######################################################################################################

server <- function(input, output) {

  ################################################################################
  ##############                                             #####################
  ############## get the summary of the adverase event data  #####################
  ##############                                             #####################
  ################################################################################

  # AEdata<-function(){
  #  preprocess(adsl=ADSL, adae=ADAE)
  # }


#######################################################################################################
#######################################################################################################
  # get the ADSL.csv
  # ADSL is a reactive variable
  ADSL<-reactive({
    ADSLread<-input$ADSLInput
    if (is.null(ADSLread))
      return(NULL)
    read.csv(ADSLread$datapath, header=TRUE)
  })

  # reminder about ADSL dataset
  output$ADSLreminder<-renderUI({
    if(is.null(ADSL())) return (NULL)
    if (!('USUBJID' %in% names(ADSL()))) return (textOutput("ADSLreminderUSUBJID"))
    if (!('TRTCTR' %in% names(ADSL()))) return (textOutput("ADSLreminderTRTCTR"))
  })

  # reminder for column 'USUBJID'
  output$ADSLreminderUSUBJID<-renderText({
    "ADSL should include the column 'USUBJID' "
  })

  # reminder for column 'TRTCTR'
  output$ADSLreminderTRTCTR<-renderText({
    "ADSL should include the column 'TRTCTR' "
  })

  # reminder about ADAE dataset
  output$ADAEreminder<-renderUI({
    if(is.null(ADAE())) return (NULL)
    if (!('USUBJID' %in% names(ADAE()))) return (textOutput("ADAEreminderUSUBJID"))
    if (!('AEBODSYS' %in% names(ADAE()))) return (textOutput("ADAEreminderAEBODSYS"))
    if (!('AEDECOD' %in% names(ADAE()))) return (textOutput("ADAEreminderAEDECOD"))
  })

  # reminder for column 'USUBJID'
  output$ADAEreminderUSUBJID<-renderText({
    "ADAE should include the column 'USUBJID' "
  })

  # reminder for column 'AEBODSYS'
  output$ADAEreminderAEBODSYS<-renderText({
    "ADAE should include the column 'AEBODSYS' "
  })

  # reminder for column 'AEDECOD'
  output$ADAEreminderAEDECOD<-renderText({
    "ADAE should include the column 'AEDECOD' "
  })


  # get the ADAE.csv
  # ADAE is a reactive variable
  ADAE<-reactive({
    ADAEread<-input$ADAEInput
    if (is.null(ADAEread))
      return(NULL)
    read.csv(ADAEread$datapath, header=TRUE)
  })

  # # reminder about ADSL dataset
  # output$ADAEreminder<-renderText({
  #   "ADAE should containing the following columns: "
  # })

  # get the AEdata from preprocess4 function in libarary FlagAE
  AEdata<-reactive({

    ADSLread<-input$ADSLInput
    ADAEread<-input$ADAEInput
    if(is.null(ADSLread) | is.null(ADAEread)) return (NULL)
    if(!( (('USUBJID' %in% names(ADSL()))) & ('TRTCTR' %in% names(ADSL()))
          & ('USUBJID' %in% names(ADAE())) & ('AEBODSYS' %in% names(ADAE()))
          & ('AEDECOD' %in% names(ADAE())) )) return (NULL)

    preprocess(adsl=ADSL(), adae=ADAE())
  })


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

  output$PREplottext<-renderText({
    if (!(is.null(AEdata()))) "Plot of risk difference/odds ratio from raw data"
  })


  # take PTnumPREplot and paramPREplot
  output$PTnumPREplot<-renderUI({
    if (is.null (AEdata())) return ()
    numericInput("PTnumPREplot", "number of adverse events to show", value = 50)
  })

  output$paramPREplot<-renderUI({
    if (is.null (AEdata())) return ()
    selectInput("paramPREplot", "summary statistics based on to plot",
                c("risk difference", "odds ratio"), selected = "risk difference")
  })

  # creat the plot from fucntion PREplot
  PREplotInput<-function(){
    if (!(is.null(AEdata()))){
      PREplot(aedata=AEdata(), ptnum=input$PTnumPREplot, param=input$paramPREplot)
    }
  }

  # plot out the plot
  output$PREplot<-renderPlot({
    if(is.null(AEdata()) | is.null(input$paramPREplot) | is.null(input$PTnumPREplot)) return ()
    PREplotInput()
  })

  # download option
  output$PREplotdownjpeg<-renderUI({
    if(!(is.null(AEdata()))){
      downloadButton('PREplotdownloadjpeg', "Download the plot")
    }
  })

  output$PREplotdownloadjpeg <- downloadHandler(
    filename <- paste0("PREplot_with_",input$paramPREplot,".jpeg"),
    content <- function(file) {
      ggsave(file, plot=PREplotInput())
    })

  # output the adverse event summary
  output$AEdata<-DT::renderDataTable({
    if (is.null(AEdata())) return ()

    drops<-c("b", "i", "j")
    AEdata() [, !names(AEdata()) %in% drops]
    },
    caption = "Adverse event summary"
  )

  # create the download option for user to download the table
  output$AEdatatabledown<-renderUI({
    if(!(is.null(AEdata()))){
      downloadButton('AEdatatabledownload', "Download the table")
    }
  })

  output$AEdatatabledownload<-downloadHandler(
    filename = function(){paste("AEdata", ".csv")},
    content =function(file){
      write.csv(AEdata(), file, row.names = FALSE)
    })


  ################################################################################
  ##############                                             #####################
  ##############            Binomial CI plot                 #####################
  ##############                                             #####################
  ################################################################################


  # output the Plot for fisher exact test

  # take PTnumInput and confInput if user select to plot out Fisher Exact Test Plot
  output$PTnumInput<-renderUI({
    #if(input$BCIInput==FALSE ) return (NULL)
    numericInput("PTnumInput", "number of adverse events to show", value = 10)
  })

  output$confInput<-renderUI({
    #if(input$BCIInput==FALSE)  return (NULL)
    numericInput("confInput", "confidence level for Binomial CI",
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
      # jpeg(file, width = 1100, height=720, quality = 450, pointsize = 9, res = 180)
      # BCIplotInput()
      # dev.off()
      ggsave(file, plot=BCIplotInput())
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

  # link for original paper
  Hierpaperurl<-a(tags$h4("Click here for detailed model information (Check model 1b)")
                  , href="https://onlinelibrary.wiley.com/doi/full/10.1111/j.0006-341X.2004.00186.x?sid=nlm%3Apubmed"
                  , target="_blank")

  output$Hierpaperlink<-renderUI({
    tagList(Hierpaperurl)
  })

  # the Hierv is to make sure that the plot and table will
  # disappear and will not run before user click 'Run' button
  Hierv <- reactiveValues(doPlot = FALSE)

  # also initiate this for cross validation
  CVv <- reactiveValues(doPlot = FALSE)

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
    # also update for Cross validation calculation
    CVv$doPlot <- FALSE
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

    drops<-c("b", "i", "j")
    HIERDATA() [, !names(HIERDATA()) %in% drops]

  })

  # table download option
  output$Hierfulltabledown<-renderUI({
    if (Hierv$doPlot==FALSE) return ()
    downloadButton('Hierfulltabledownload', "Download the whole table")
  })

  output$Hierfulltabledownload<-downloadHandler(
    filename <- function(){paste("Hiermodelfulltable", ".csv")},
    content <-function(file){
      #write.csv(Hiermodel(), file, row.names = FALSE)
      drops<-c("b", "i", "j")
      write.csv(Hiermodel()[, !names(Hiermodel()) %in% drops], file, row.names = FALSE)
    }
  )

  # show the Hier plot of top AEs
  # if user choose to plot based on "odds ratio", provide the option to select y-axis limit
  output$HierORxlimLB<-renderUI({
    if (input$Hierplotparam=="risk difference" | input$Hierplotselect=='Compare raw data and model') return ()
    numericInput("HierORxlimLBInput", "x-axis lower limit", value=0)
  })

  output$HierORxlimUB<-renderUI({
    if (input$Hierplotparam=="risk difference" | input$Hierplotselect=='Compare raw data and model') return ()
    numericInput("HierORxlimUBInput", "x-axis upper limit", value=5)
  })

  HierplotInput<-function(){
    if (Hierv$doPlot==FALSE) return ()
    if (input$HierplotInput==FALSE) return()
    isolate({
      if (input$Hierplotselect=='Plot Top Adverse Events'){
        if (input$Hierplotparam=="risk difference"){
          return(Hierplot(HIERDATA(), ptnum=input$Hierplotptnum, param=input$Hierplotparam))
        }
        else {
          return(Hierplot(HIERDATA(), ptnum=input$Hierplotptnum, param=input$Hierplotparam,
                   OR_xlim = c(input$HierORxlimLBInput, input$HierORxlimUBInput)))
        }
      }

      if (input$Hierplotselect=='Compare raw data and model'){
        return(Compareplot(modeldata=HIERDATA(), ptnum=input$Hierplotptnum, param=input$Hierplotparam))
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
    if(is.null(HIERDATA()) & !(input$HierplotInput==FALSE)) {
      textOutput("HierModelfirstoutput")
      # span(textOutput("Modelfirstoutput"), style="color:red")
    }
  })

  output$HierModelfirstoutput<-renderText({
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
    filename <- function(){paste0("Hierplot_",input$Hierplotparam, ".jpeg")},
    content <- function(file) {
      #jpeg(file, width = 1100, height=720, quality = 450, pointsize = 9, res = 180)
      #jpeg(file)
      ggsave(file, plot=HierplotInput())
    })


  # download option for table containing information of AE in the plot
  HiertableInput<-function(){
    if (Hierv$doPlot==FALSE | input$Hierplotselect=='Compare raw data and model' ) return ()
    Hiertable(HIERDATA(), ptnum=input$Hierplotptnum, param=input$Hierplotparam)
  }

  output$Hiertabledown<-renderUI({
    if(is.null(HierplotInput())) return ()
    if(input$HierplotInput==FALSE | input$Hierplotselect=='Compare raw data and model') return ()
    downloadButton('Hiertabledownload', "Download table for details of AEs shown in plot")
  })

  output$Hiertabledownload<-downloadHandler(
    filename <- function(){paste("Hiertable", ".csv")},
    content <-function(file){
      drops<-c("b", "i", "j")
      write.csv(HiertableInput()[,!names(HiertableInput()) %in% drops ], file, row.names = FALSE)
    }
  )


  ################################################################################
  ##############                                             #####################
  ##############           Ising Prior model                 #####################
  ##############                                             #####################
  ################################################################################

  # link for original paper
  Isingpaperurl<-a(tags$h4("Click here for detailed model information")
                  , href="https://onlinelibrary.wiley.com/doi/full/10.1111/biom.12051"
                  , target="_blank")

  output$Isingpaperlink<-renderUI({
    tagList(Isingpaperurl)
  })


  # show the table from the hierarchical model
  # summary of the hierarchical model
  # we DO NOT want the model to run each time user change the a single paramter
  # we want the model to run after user finish changing all parameters
  # and hit the run button

  # the Isingv is to make sure that the plot and table will
  # disappear and will not run before user click 'Run' button
  Isingv <- reactiveValues(doPlot = FALSE)

  observeEvent(input$IsingInput, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    Isingv$doPlot <- input$IsingInput
  })

  observeEvent((input$IsingburnInput &
                  input$IsingiterInput & input$IsingthinInput &
                  input$alpha.input & input$beta.input &
                  input$alpha.t.input & input$beta.t.input &
                  input$alpha.c.input & input$beta.c.input &
                  input$rho.input & input$theta.input
  ), {
    Isingv$doPlot <- FALSE
    # also update for Cross validation calculation
    CVv$doPlot <- FALSE
  })



  #run the model
  Isingmodel<- function(){

    if (Isingv$doPlot==FALSE ) return ()

    isolate({

      IsingData<-Ising(aedata=AEdata(), n_burn=input$IsingburnInput,
                       n_iter=input$IsingiterInput, thin=input$IsingthinInput,
                       alpha_ =input$alpha.input, beta_ = input$beta.input,
                       alpha.t =input$alpha.t.input, beta.t = input$beta.t.input,
                       alpha.c =input$alpha.c.input, beta.c = input$beta.c.input,
                       rho=input$rho.input, theta=input$theta.input)

    })
  }

  # show the table
  ISINGDATA<-reactive({
    if (Isingv$doPlot==FALSE) return ()
    Isingmodel()
  })
  output$Isingfulltable<-DT::renderDataTable({
    drops<-c("b", "i", "j")
    ISINGDATA()[,!names(ISINGDATA()) %in% drops ]
  })

  # table download option
  output$Isingfulltabledown<-renderUI({
    if (Isingv$doPlot==FALSE) return ()
    downloadButton('Isingfulltabledownload', "Download the whole table")
  })

  output$Isingfulltabledownload<-downloadHandler(
    filename <- function(){paste("Isingmodelfulltable", ".csv")},
    content <-function(file){
      write.csv(Isingmodel(), file, row.names = FALSE)
    }
  )

  # show the Ising plot of top AEs
  # if user choose to plot based on "odds ratio", provide the option to select y-axis limit
  output$IsingORxlimLB<-renderUI({
    if (input$Isingplotparam=="risk difference" | input$Isingplotselect=='Compare raw data and model') return ()
    numericInput("IsingORxlimLBInput", "x-axis lower limit", value=0)
  })

  output$IsingORxlimUB<-renderUI({
    if (input$Isingplotparam=="risk difference" | input$Isingplotselect=='Compare raw data and model') return ()
    numericInput("IsingORxlimUBInput", "x-axis upper limit", value=5)
  })

  IsingplotInput<-function(){
    if (Isingv$doPlot==FALSE) return ()
    if (input$IsingplotInput==FALSE) return()
    isolate({
      if (input$Isingplotselect=='Plot Top Adverse Events'){
        if (input$Isingplotparam=="risk difference"){
          return(Isingplot(ISINGDATA(), ptnum=input$Isingplotptnum, param=input$Isingplotparam))
        }
        else {
          return(Isingplot(ISINGDATA(), ptnum=input$Isingplotptnum, param=input$Isingplotparam,
                          OR_xlim = c(input$IsingORxlimLBInput, input$IsingORxlimUBInput)))
        }
      }

      if (input$Isingplotselect=='Compare raw data and model'){
        return(Compareplot(modeldata=ISINGDATA(), ptnum=input$Isingplotptnum, param=input$Isingplotparam))
      }

    })

  }

  output$Isingplot<-renderPlot({
    if(is.null(IsingplotInput())) return ()
    if(input$IsingplotInput==FALSE) return ()
    IsingplotInput()
  })

  # show the warning if user try to plot before running model
  output$Isingmodelfirst<-renderUI({
    if(is.null(ISINGDATA()) & !(input$IsingplotInput==FALSE)) {
      textOutput("IsingModelfirstoutput")
      # span(textOutput("Modelfirstoutput"), style="color:red")
    }
  })

  output$IsingModelfirstoutput<-renderText({
    # if (!(is.null(IsingplotInput()) & !(input$IsingplotInput==FALSE))) return ()
    "Please run the model before plot"
  })

  # download option for Ising plot of top AEs
  output$Isingplotdown<-renderUI({
    if(!is.null(IsingplotInput()) & !(input$IsingplotInput==FALSE)){
      downloadButton('Isingplotdownload', "Download the plot")
    }
  })

  output$Isingplotdownload <- downloadHandler(
    filename <- function(){paste0("Isingplot_",input$Isingplotparam ,".jpeg")},
    content <- function(file) {
      ggsave(file, plot=IsingplotInput())
    })


  # download option for table containing information of AE in the plot
  IsingtableInput<-function(){
    if (Isingv$doPlot==FALSE | input$Isingplotselect=='Compare raw data and model') return ()
    Isingtable(ISINGDATA(), ptnum=input$Isingplotptnum, param=input$Isingplotparam)
  }

  output$Isingtabledown<-renderUI({
    if(is.null(IsingplotInput())) return ()
    if(input$IsingplotInput==FALSE | input$Isingplotselect=='Compare raw data and model') return ()
    downloadButton('Isingtabledownload', "Download table for details of AEs shown in plot")
  })

  output$Isingtabledownload<-downloadHandler(
    filename <- function(){paste("Isingtable", ".csv")},
    content <-function(file){
      drops<-c("b", "i", "j")
      write.csv(IsingtableInput()[,!names(IsingtableInput()) %in% drops ], file, row.names = FALSE)
    }
  )

  ################################################################################
  ##############                                             #####################
  ##############      Comparison of two models               #####################
  ##############                                             #####################
  ################################################################################

  ###############################################################################
  ###############################################################################
  # compare by plotting
  # if user choose to plot based on "odds ratio", provide the option to select y-axis limit
  output$HIORxlimLB<-renderUI({
    if (input$HIplotparam=="risk difference") return ()
    numericInput("HIORxlimLBInput", "y-axis lower limit", value=0)
  })

  output$HIORxlimUB<-renderUI({
    if (input$HIplotparam=="risk difference") return ()
    numericInput("HIORxlimUBInput", "y-axis upper limit", value=5)
  })

  # create the plot
  HIplotInput<-function(){
    if (Hierv$doPlot==FALSE) return ()
    if (Isingv$doPlot==FALSE) return ()
    if (input$HIplotInput==FALSE) return()
    isolate({
      if (input$HIplotparam=="risk difference"){
        HIplot(hierdata=HIERDATA(), isingdata=ISINGDATA(), aedata=AEdata(), ptnum=input$HIplotptnum, param=input$HIplotparam)
      }
      else {
        HIplot(hierdata=HIERDATA(), isingdata=ISINGDATA(), aedata=AEdata(), ptnum=input$HIplotptnum, param=input$HIplotparam,
                 OR_xlim = c(input$HIORxlimLBInput, input$HIORxlimUBInput))
      }
    })

  }

  output$HIplot<-renderPlot({
    if(is.null(HIplotInput())) return ()
    if(input$HIplotInput==FALSE) return ()
    HIplotInput()
  })


  # show the warning if user try to plot before running model
  output$HImodelfirst<-renderUI({
    if((Hierv$doPlot==FALSE | Isingv$doPlot==FALSE) & !(input$HIplotInput==FALSE)) {
      textOutput("HIModelfirstoutput")
      # span(textOutput("Modelfirstoutput"), style="color:red")
    }
  })

  output$HIModelfirstoutput<-renderText({
    # if (!(is.null(IsingplotInput()) & !(input$IsingplotInput==FALSE))) return ()
    "Please run both models before plot"
  })

  # download option for plot of top AEs
  output$HIplotdown<-renderUI({
    if(!is.null(HIplotInput()) & !(input$HIplotInput==FALSE)){
      downloadButton('HIplotdownload', "Download the plot")
    }
  })

  output$HIplotdownload <- downloadHandler(
    filename <- function(){paste0("Hierarchical_Ising_plot_",input$HIplotparam ,".jpeg")},
    content <- function(file) {
      ggsave(file, plot=HIplotInput())
  })

  # download option for table containing information of AE in the plot
  HItableInput<-function(){
    if (Hierv$doPlot==FALSE) return ()
    if (Isingv$doPlot==FALSE) return ()
    if (input$HIplotInput==FALSE) return()
    HItable(hierdata=HIERDATA(),isingdata=ISINGDATA(), ptnum=input$HIplotptnum, param=input$HIplotparam)
  }

  output$HItabledown<-renderUI({
    if(is.null(HIplotInput())) return ()
    if(input$HIplotInput==FALSE) return ()
    downloadButton('HItabledownload', "Download table for details of AEs shown in plot")
  })

  output$HItabledownload<-downloadHandler(
    filename <- function(){paste("Hier_Ising_table", ".csv")},
    content <-function(file){
      write.csv(HItableInput(), file, row.names = FALSE)
    }
  )

  ###############################################################################
  ###############################################################################


  observeEvent(input$CVInput, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    CVv$doPlot <- input$CVInput
  })

  observeEvent((input$CVkfdInput), {
    CVv$doPlot <- FALSE
  })


  CVtableInput<-reactive({
    if (CVv$doPlot==0 ) return ()
    CVAELIST<-kfdpar(ADSL(), ADAE(), k=input$CVkfdInput)
    # CVAELIST<-kfdpar(ADSL, ADAE, k=input$CVkfdInput)

    CVLOSSHIER<-CVhier(AElist=CVAELIST, n_burn=input$HierburnInput,
                     n_iter=input$HieriterInput, thin=input$HierthinInput, n_adapt = input$HieradaptInput,
                     n_chain = input$HierchainInput,
                     alpha.gamma =input$alpha.gamma.input, beta.gamma = input$beta.gamma.input,
                     alpha.theta = input$alpha.theta.input, beta.theta = input$beta.theta.input,
                     mu.gamma.0.0 = input$mu.gamma.0.0.input, tau.gamma.0.0 = input$tau.gamma.0.0.input,
                     alpha.gamma.0.0 = input$alpha.gamma.0.0.input, beta.gamma.0.0 = input$beta.gamma.0.0.input,
                     lambda.alpha = input$lambda.alpha.input, lambda.beta = input$lambda.beta.input,
                     mu.theta.0.0 = input$mu.theta.0.0.input, tau.theta.0.0 = input$tau.theta.0.0.input,
                     alpha.theta.0.0 = input$alpha.theta.0.0.input, beta.theta.0.0 = input$beta.theta.0.0.input)

    CVLOSSISING<-CVising(AElist=CVAELIST, n_burn=input$IsingburnInput,
                       n_iter=input$IsingiterInput, thin=input$IsingthinInput,
                       alpha_ =input$alpha.input, beta_ = input$beta.input,
                       alpha.t =input$alpha.t.input, beta.t = input$beta.t.input,
                       alpha.c =input$alpha.c.input, beta.c = input$beta.c.input,
                       rho=input$rho.input, theta=input$theta.input)

    LOSSTABLE<-data.frame(row.names = c("Train_Loss", "Test_Loss"), Hierachical_Model=c(CVLOSSHIER$trainloss, CVLOSSHIER$testloss),
               Ising_Model=c(CVLOSSISING$trainloss, CVLOSSISING$testloss))
    round(LOSSTABLE,4)
  })

  output$CVlosstable<-DT::renderDataTable({
    CVtableInput()
  },
  caption = "Cross Validation Loss of Two Models"
  )

  output$CVtabledown<-renderUI({
    if(is.null(CVtableInput())) return ()

    downloadButton('CVtabledownload', "Download table of cross validation loss")
  })

  output$CVtabledownload<-downloadHandler(
    filename <- function(){paste("CVLoss", ".csv")},
    content <-function(file){
      write.csv(CVtableInput(), file, row.names = FALSE)
    }
  )











}
