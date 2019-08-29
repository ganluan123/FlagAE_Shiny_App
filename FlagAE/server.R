library(shiny)
# increase the maximum allowed size for uploading file
# options(shiny.maxRequestSize=100*1024^2)

#######################################################################################################
#######################################################################################################

shinyServer(function(input, output) {
  
  library(FlagAE)
  library(DT)
  library(ggplot2)

  ################################################################################
  ##############                                             #####################
  ############## get the summary of the adverase event data  #####################
  ##############                                             #####################
  ################################################################################

  output$fileloadreminder<-renderUI(
    HTML(
      paste(
        c('ADSL should include columns: USUBJID and TRTCTR.',
          " ADAE should include columns: USUBJID, AEBODSYS, and AEDECOD"),
        collapse = "<br>"
      )
    )
  )
  
  # get the ADSL.csv
  # ADSL is a reactive variable

  ADSL<-reactive({
    req(input$ADSLInput)
    return(read.csv(input$ADSLInput$datapath, header=TRUE))
  })
  
  # create a variable to make sure that whenever user upload a new ADSL file, all the previous calculation
  # will disappear and will not recalculate before use hit the RUN or PLOT button
  ADSLupload<-reactive({
    req(input$ADSLInput)
    runif(1)
  })
  
  
  # reminder about ADSL dataset
  output$ADSLreminder<-renderUI({
    # if(is.null(ADSL())) return (NULL)
    if (!('USUBJID' %in% names(ADSL())) & !is.null(ADSL())) return (span(textOutput("ADSLreminderUSUBJID"), style="color:red"))
    if (!('TRTCTR' %in% names(ADSL())) & !is.null(ADSL())) return (span(textOutput("ADSLreminderTRTCTR"), style="color:red"))
  })

  # reminder for column 'USUBJID'
  output$ADSLreminderUSUBJID<-renderText({
    "Error: ADSL should include the column 'USUBJID' "
  })

  # reminder for column 'TRTCTR'
  output$ADSLreminderTRTCTR<-renderText({
    "Error: ADSL should include the column 'TRTCTR' "
  })

  # reminder about ADAE dataset
  output$ADAEreminder<-renderUI({
    # if(is.null(ADAE())) return (NULL)
    if (!('USUBJID' %in% names(ADAE())) & !is.null(ADAE())) return (span(textOutput("ADAEreminderUSUBJID"), style="color:red"))
    if (!('AEBODSYS' %in% names(ADAE())) & !is.null(ADAE())) return (span(textOutput("ADAEreminderAEBODSYS"), style="color:red"))
    if (!('AEDECOD' %in% names(ADAE())) & !is.null(ADAE())) return (span(textOutput("ADAEreminderAEDECOD"), style="color:red"))
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
    req(input$ADAEInput)
    return(read.csv(input$ADAEInput$datapath, header=TRUE))
  })
  
  ADAEupload<-reactive({
    req(input$ADAEInput)
    runif(1)
  })
  
  # get the AEdata from preprocess4 function in libarary FlagAE
  AEdata<-reactive({

    req(input$ADSLInput)
    req(input$ADAEInput)
    if(( (('USUBJID' %in% names(ADSL()))) & ('TRTCTR' %in% names(ADSL()))
          & ('USUBJID' %in% names(ADAE())) & ('AEBODSYS' %in% names(ADAE()))
          & ('AEDECOD' %in% names(ADAE())) )) return (preprocess(adsl=ADSL(), adae=ADAE()))
  })

  # output subject and adverse event summary
  output$AEsummary<-DT::renderDataTable({

    req(AEdata())
    
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
    # if (is.null (AEdata())) return ()
    req(AEdata())
    numericInput("PTnumPREplot", "number of adverse events to show", value = 50)
  })

  output$paramPREplot<-renderUI({
    # if (is.null (AEdata())) return ()
    req(AEdata())
    selectInput("paramPREplot", "summary statistics based on to plot",
                c("risk difference", "odds ratio"), selected = "risk difference")
  })

  # creat the plot from fucntion PREplot
  PREplotInput<-function(){
    req(input$PTnumPREplot)
    req(input$paramPREplot)
    if (!(is.null(AEdata()))){
      PREplot(aedata=AEdata(), ptnum=input$PTnumPREplot, param=input$paramPREplot)
    }
  }

  # plot out the plot
  output$PREplot<-renderPlot({
    if (!(is.null(AEdata()) | is.null(input$paramPREplot) | is.null(input$PTnumPREplot))){
      PREplotInput()
    } 
    
  })

  # download option
  output$PREplotdownjpeg<-renderUI({
    req(input$PTnumPREplot)
    req(input$paramPREplot)
    if(!(is.null(AEdata()))){
      downloadButton('PREplotdownloadjpeg', "Download the plot")
    }
  })

  output$PREplotdownloadjpeg <- downloadHandler(
    filename <- paste0("PREplot_with_",input$paramPREplot,".jpeg"),
    content <- function(file) {
      req(input$PTnumPREplot)
      req(input$paramPREplot)
      ggsave(file, plot=PREplotInput())
    })

  # output the adverse event summary
  output$AEdata<-DT::renderDataTable({
    # if (is.null(AEdata())) return ()
    req(AEdata())

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


  # output the Plot for Binomial confidence interval

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
 
  observeEvent((input$PTnumInput & input$confInput
                & ADSLupload() & ADAEupload()), {
    BCIv$doPlot <- FALSE
  })

  BCIplotInput<-function(){
    req(AEdata())
    req(input$PTnumInput)
    req(input$confInput)
    if (!(BCIv$doPlot==0 | is.null(input$PTnumInput) | is.null(input$confInput))) {
      isolate({
        BCIplot(aedata=AEdata(), ptnum = input$PTnumInput, conf.level = input$confInput)
      })
    }
  }
  # plot out the plot
  output$BCIplot<-renderPlot({
    req(BCIplotInput())
    if(!(BCIv$doPlot==FALSE)) {
      BCIplotInput()
    }
  })
  # provide the download option for user

  output$BCIplotdownjpeg<-renderUI({
    req(BCIplotInput())
    req(input$PTnumInput)
    req(input$confInput)
    if(!(BCIv$doPlot==FALSE) & !(is.null(input$PTnumInput)) & !(is.null(input$confInput))){
      downloadButton('BCIplotdownloadjpeg', "Download the plot")
    }
  })

  output$BCIplotdownloadjpeg <- downloadHandler(
    filename <- "BCIplot.jpeg",
    content <- function(file) {
      req(BCIplotInput())
      req(input$PTnumInput)
      req(input$confInput)
      ggsave(file, plot=BCIplotInput())
  })
  
  # create the table for details of AE selected in above plot
  BCItableInput<-reactive({
    req(AEdata())
    req(input$PTnumInput)
    req(input$confInput)
    if (!(BCIv$doPlot==0 | is.null(input$PTnumInput) | is.null(input$confInput))) {
      topPTlist<-BCItable(aedata=AEdata(), ptnum=input$PTnumInput, conf.level = input$confInput)
    }
  })

  output$TopAE<-DT::renderDataTable({
    req(BCItableInput())
    BCItableInput()
  },
  caption = "Detail information of adverse events shown in plot above"
  )

  # create the download option for user to download the table
  output$BCItabledown<-renderUI({
    req(BCItableInput())
    req(input$PTnumInput)
    req(input$confInput)
    if(!(BCIv$doPlot==0) & !(is.null(input$PTnumInput)) & !(is.null(input$confInput))){
      downloadButton('BCItabledownload', "Download the table")
    }
  })

  output$BCItabledownload<-downloadHandler(
    filename = function(){paste("BCItable", ".csv")},
    content =function(file){
      req(BCItableInput())
      req(input$PTnumInput)
      req(input$confInput)
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
                  input$lambda.beta.input
                & ADSLupload() & ADAEupload()
                ), {
    Hierv$doPlot <- FALSE
    # also update for Cross validation calculation
    CVv$doPlot <- FALSE
  })



  #run the model
  Hiermodel<- function(){

    if (!(Hierv$doPlot==FALSE )) {
      
      req(AEdata())
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

    
  }

  # show the table
  HIERDATA<-reactive({
    # req(Hiermodel())
    if (!(Hierv$doPlot==FALSE)){
      Hiermodel()
    } 
  })
  
  output$Hierfulltable<-DT::renderDataTable({
    req(HIERDATA())
    drops<-c("b", "i", "j")
    HIERDATA() [, !names(HIERDATA()) %in% drops]

  })

  # table download option
  output$Hierfulltabledown<-renderUI({
    req(Hiermodel())
    if (!(Hierv$doPlot==FALSE)){
      downloadButton('Hierfulltabledownload', "Download the whole table")
    } 
  })

  output$Hierfulltabledownload<-downloadHandler(
    filename <- function(){paste("Hiermodelfulltable", ".csv")},
    content <-function(file){
      req(Hiermodel())
      drops<-c("b", "i", "j")
      write.csv(Hiermodel()[, !names(Hiermodel()) %in% drops], file, row.names = FALSE)
    }
  )

  # show the Hier plot of top AEs
  # if user choose to plot based on "odds ratio", provide the option to select y-axis limit
  output$HierORxlimLB<-renderUI({
    if (!(input$Hierplotparam=="risk difference" | input$Hierplotselect=='Compare raw data and model')){
      numericInput("HierORxlimLBInput", "x-axis lower limit", value=0)
    } 
  })

  output$HierORxlimUB<-renderUI({
    if (!(input$Hierplotparam=="risk difference" | input$Hierplotselect=='Compare raw data and model')){
      numericInput("HierORxlimUBInput", "x-axis upper limit", value=5)
    } 
  })

  HierplotInput<-function(){
    if ((!(Hierv$doPlot==FALSE)) & (!(input$HierplotInput==FALSE))){
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
  }

  output$Hierplot<-renderPlot({
    req(HierplotInput())
    req(input$HierplotInput)
    HierplotInput()
  })

  # show the warning if user try to plot before running model
  output$Hiermodelfirst<-renderUI({
    if(is.null(HIERDATA()) & !(input$HierplotInput==FALSE)) {
      # textOutput("HierModelfirstoutput")
      span(textOutput("HierModelfirstoutput"), style="color:red")
      # span(textOutput("Modelfirstoutput"), style="color:red")
    }
  })

  output$HierModelfirstoutput<-renderText({
    "Error: Please run the model before plot"
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
      req(HierplotInput())
      ggsave(file, plot=HierplotInput())
    })


  # download option for table containing information of AE in the plot
  HiertableInput<-function(){
    req(HIERDATA())
    if (!(Hierv$doPlot==FALSE | input$Hierplotselect=='Compare raw data and model')){
      Hiertable(HIERDATA(), ptnum=input$Hierplotptnum, param=input$Hierplotparam)
    } 
  }

  output$Hiertabledown<-renderUI({
    req(HierplotInput())
    req(HiertableInput())
    if (!(input$HierplotInput==FALSE | input$Hierplotselect=='Compare raw data and model')){
      downloadButton('Hiertabledownload', "Download table for details of AEs shown in plot")
    } 
  })

  output$Hiertabledownload<-downloadHandler(
    filename <- function(){paste("Hiertable", ".csv")},
    content <-function(file){
      req(HiertableInput())
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
                & ADSLupload() & ADAEupload()
  ), {
    Isingv$doPlot <- FALSE
    # also update for Cross validation calculation
    CVv$doPlot <- FALSE
  })



  #run the model
  Isingmodel<- function(){

    if (!(Isingv$doPlot==FALSE )){
      isolate({
        
        IsingData<-Ising(aedata=AEdata(), n_burn=input$IsingburnInput,
                         n_iter=input$IsingiterInput, thin=input$IsingthinInput,
                         alpha_ =input$alpha.input, beta_ = input$beta.input,
                         alpha.t =input$alpha.t.input, beta.t = input$beta.t.input,
                         alpha.c =input$alpha.c.input, beta.c = input$beta.c.input,
                         rho=input$rho.input, theta=input$theta.input)
        
      })
    } 
  }

  # show the table
  ISINGDATA<-reactive({
    if (!(Isingv$doPlot==FALSE)){
      Isingmodel()
    } 
  })
  output$Isingfulltable<-DT::renderDataTable({
    req(ISINGDATA())
    drops<-c("b", "i", "j")
    ISINGDATA()[,!names(ISINGDATA()) %in% drops ]
  })

  # table download option
  output$Isingfulltabledown<-renderUI({
    req(Isingmodel())
    if (!(Isingv$doPlot==FALSE)){
      downloadButton('Isingfulltabledownload', "Download the whole table")
    } 
  })

  output$Isingfulltabledownload<-downloadHandler(
    filename <- function(){paste("Isingmodelfulltable", ".csv")},
    content <-function(file){
      req(Isingmodel())
      write.csv(Isingmodel(), file, row.names = FALSE)
    }
  )

  # show the Ising plot of top AEs
  # if user choose to plot based on "odds ratio", provide the option to select y-axis limit
  output$IsingORxlimLB<-renderUI({
    if (!(input$Isingplotparam=="risk difference" | input$Isingplotselect=='Compare raw data and model')){
      numericInput("IsingORxlimLBInput", "x-axis lower limit", value=0)
    } 
  })

  output$IsingORxlimUB<-renderUI({
    if (!(input$Isingplotparam=="risk difference" | input$Isingplotselect=='Compare raw data and model')){
      numericInput("IsingORxlimUBInput", "x-axis upper limit", value=5)
    }
  })

  IsingplotInput<-function(){
    
    if ((!(Isingv$doPlot==FALSE)) & (!(input$IsingplotInput==FALSE))){
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
  }

  output$Isingplot<-renderPlot({
    req(IsingplotInput())
    if(!(input$IsingplotInput==FALSE)){
      IsingplotInput()
    } 
  })

  # show the warning if user try to plot before running model
  output$Isingmodelfirst<-renderUI({
    if(is.null(ISINGDATA()) & !(input$IsingplotInput==FALSE)) {
      span(textOutput("IsingModelfirstoutput"), style="color:red")
    }
  })

  output$IsingModelfirstoutput<-renderText({
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
      req(IsingplotInput())
      ggsave(file, plot=IsingplotInput())
    })


  # download option for table containing information of AE in the plot
  IsingtableInput<-function(){
    if (!(Isingv$doPlot==FALSE | input$Isingplotselect=='Compare raw data and model')){
      Isingtable(ISINGDATA(), ptnum=input$Isingplotptnum, param=input$Isingplotparam)
    }
  }

  output$Isingtabledown<-renderUI({
    req(IsingplotInput())
    if(!(input$IsingplotInput==FALSE | input$Isingplotselect=='Compare raw data and model')){
      downloadButton('Isingtabledownload', "Download table for details of AEs shown in plot")
    }
  })

  output$Isingtabledownload<-downloadHandler(
    filename <- function(){paste("Isingtable", ".csv")},
    content <-function(file){
      req(IsingtableInput())
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
    if (!(input$HIplotparam=="risk difference")){
      numericInput("HIORxlimLBInput", "y-axis lower limit", value=0)
    } 
  })

  output$HIORxlimUB<-renderUI({
    if (!(input$HIplotparam=="risk difference")){
      numericInput("HIORxlimUBInput", "y-axis upper limit", value=5)
    }
  })

  # create the plot
  HIplotInput<-function(){
    if ( (!(Hierv$doPlot==FALSE)) & (!(Isingv$doPlot==FALSE)) & (!(input$HIplotInput==FALSE))){
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
  }

  output$HIplot<-renderPlot({
    req(HIplotInput())
    if (!(input$HIplotInput==FALSE)){
      HIplotInput()
    } 
  })


  # show the warning if user try to plot before running model
  output$HImodelfirst<-renderUI({
    if((Hierv$doPlot==FALSE | Isingv$doPlot==FALSE) & !(input$HIplotInput==FALSE)) {
      span(textOutput("HIModelfirstoutput"), style="color:red")
    }
  })

  output$HIModelfirstoutput<-renderText({
    # if (!(is.null(IsingplotInput()) & !(input$IsingplotInput==FALSE))) return ()
    "Error: Please run both models before plot"
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
      req(HIplotInput())
      ggsave(file, plot=HIplotInput())
  })

  # download option for table containing information of AE in the plot
  HItableInput<-function(){
    if ( (!(Hierv$doPlot==FALSE)) & (!(Isingv$doPlot==FALSE)) & (!(input$HIplotInput==FALSE))){
      HItable(hierdata=HIERDATA(),isingdata=ISINGDATA(), ptnum=input$HIplotptnum, param=input$HIplotparam)
    }
  }

  output$HItabledown<-renderUI({
    req(HIplotInput())
    if(!(input$HIplotInput==FALSE)){
      downloadButton('HItabledownload', "Download table for details of AEs shown in plot")
    } 
  })

  output$HItabledownload<-downloadHandler(
    filename <- function(){paste("Hier_Ising_table", ".csv")},
    content <-function(file){
      req(HItableInput())
      write.csv(HItableInput(), file, row.names = FALSE)
    }
  )

  ###############################################################################
  ###############################################################################
  # compare by cross validation error

  observeEvent(input$CVInput, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    CVv$doPlot <- input$CVInput
  })

  observeEvent((input$CVkfdInput), {
    CVv$doPlot <- FALSE
  })

  CVtableInput<-reactive({
    req(input$ADSLInput)
    req(input$ADAEInput)
    if (!(CVv$doPlot==0 )){
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
    } 
  })

  output$CVlosstable<-DT::renderDataTable({
    req(CVtableInput())
    CVtableInput()
  },
  caption = "Cross Validation Loss of Two Models"
  )

  output$CVtabledown<-renderUI({
    
    req(CVtableInput())
    downloadButton('CVtabledownload', "Download table of cross validation loss")
  })

  output$CVtabledownload<-downloadHandler(
    filename <- function(){paste("CVLoss", ".csv")},
    content <-function(file){
      req(CVtableInput())
      write.csv(CVtableInput(), file, row.names = FALSE)
    }
  )

}
)
