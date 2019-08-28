library(shiny)
# increase the maximum allowed size for uploading file
# options(shiny.maxRequestSize=100*1024^2)
# call the libarary FlagAE



#####################################################################################################
#######################################################################################################
# ADSL<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\demo_ADSL_TRTCTR.csv")
# ADAE<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\demo_ADAE_TRTCTR.csv")

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

  # AEdata<-function(){
  #  preprocess(adsl=ADSL, adae=ADAE)
  # }


#######################################################################################################
#######################################################################################################
  
  # output$fileloadreminder<-renderUI(
  #   HTML(
  #     paste(
  #       c('ADSL should include columns: USUBJID and TRTCTR.', 
  #         " ADAE should include columns: USUBJID, AEBODSYS, and AEDECOD"),
  #       collapse = "<br>"
  #     )
  #   )
  # )
  
  # get the ADSL.csv
  # ADSL is a reactive variable
  
  
  
  # ADSL<-reactive({
  #   ADSLread<-input$ADSLInput
  #   if (is.null(ADSLread))
  #     return(NULL)
  #   read.csv(ADSLread$datapath, header=TRUE)
  # })

  
  
  ADSL<-reactive({
    req(input$ADSLInput)
    return(read.csv(input$ADSLInput$datapath, header=TRUE))
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
  
  
  
  # ADAE<-reactive({
  #   ADAEread<-input$ADAEInput
  #   if (is.null(ADAEread))
  #     return(NULL)
  #   read.csv(ADAEread$datapath, header=TRUE)
  # })

  
  
  ADAE<-reactive({
    req(input$ADAEInput)
    return(read.csv(input$ADAEInput$datapath, header=TRUE))
  })
  
  # # reminder about ADSL dataset
  # output$ADAEreminder<-renderText({
  #   "ADAE should containing the following columns: "
  # })

  # get the AEdata from preprocess4 function in libarary FlagAE
  AEdata<-reactive({

    # ADSLread<-input$ADSLInput
    # ADAEread<-input$ADAEInput
    # if(is.null(ADSLread) | is.null(ADAEread)) return (NULL)
    req(input$ADSLInput)
    req(input$ADAEInput)
    if(( (('USUBJID' %in% names(ADSL()))) & ('TRTCTR' %in% names(ADSL()))
          & ('USUBJID' %in% names(ADAE())) & ('AEBODSYS' %in% names(ADAE()))
          & ('AEDECOD' %in% names(ADAE())) )) return (preprocess(adsl=ADSL(), adae=ADAE()))

    # preprocess(adsl=ADSL(), adae=ADAE())
  })


# ###############################################################################################################
# ###############################################################################################################
  # output subject and adverse event summary
  output$AEsummary<-DT::renderDataTable({

    # if (is.null (AEdata())) return ()
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


  # # output the Plot for fisher exact test
  # 
  # # take PTnumInput and confInput if user select to plot out Fisher Exact Test Plot
  # output$PTnumInput<-renderUI({
  #   #if(input$BCIInput==FALSE ) return (NULL)
  #   numericInput("PTnumInput", "number of adverse events to show", min = 1, value = 10)
  # })
  # 
  # output$confInput<-renderUI({
  #   #if(input$BCIInput==FALSE)  return (NULL)
  #   numericInput("confInput", "confidence level for Binomial CI",
  #                              value = 0.95, min=0, max=1, step = 0.025)
  # })
  # 
  # # create the plot with activation button
  # 
  # # the BCIv is to make sure that the plot and table will
  # # disappear and will not run before user click 'Run' button
  # BCIv <- reactiveValues(doPlot = FALSE)
  # 
  # observeEvent(input$BCIInput, {
  #   # 0 will be coerced to FALSE
  #   # 1+ will be coerced to TRUE
  #   BCIv$doPlot <- input$BCIInput
  # })
  # 
  # # my understanding is that this is to make sure that
  # # whenever user change PTnumInput or confInput, the plot
  # # will disappear and wont run before user click run button
  # observeEvent((input$PTnumInput & input$confInput), {
  #   BCIv$doPlot <- FALSE
  # })
  # 
  # BCIplotInput<-function(){
  # 
  #   if (!(BCIv$doPlot==0 | is.null(input$PTnumInput) | is.null(input$confInput))) {
  #     isolate({
  #       BCIplot(aedata=AEdata(), ptnum = input$PTnumInput, conf.level = input$confInput)
  #     })
  #   }
  # }
  # # plot out the plot
  # output$BCIplot<-renderPlot({
  #   req(BCIplotInput())
  #   if(!(BCIv$doPlot==FALSE)){
  #     BCIplotInput()
  #   }
  # })
  # # provide the download option for user
  # 
  # output$BCIplotdownjpeg<-renderUI({
  #   if(!(BCIv$doPlot==FALSE) & !(is.null(input$PTnumInput)) & !(is.null(input$confInput))){
  #     downloadButton('BCIplotdownloadjpeg', "Download the plot")
  #   }
  # })
  # 
  # output$BCIplotdownloadjpeg <- downloadHandler(
  #   filename <- "BCIplot.jpeg",
  #   content <- function(file) {
  #     # jpeg(file, width = 1100, height=720, quality = 450, pointsize = 9, res = 180)
  #     # BCIplotInput()
  #     # dev.off()
  #     req(BCIplotInput())
  #     ggsave(file, plot=BCIplotInput())
  # })
  # 
  # 
  # # create the table for details of AE selected in above plot
  # BCItableInput<-reactive({
  #   if (!(BCIv$doPlot==0 | is.null(input$PTnumInput) | is.null(input$confInput))) {
  #     topPTlist<-BCItable(aedata=AEdata(), ptnum=input$PTnumInput, conf.level = input$confInput)
  #   }
  # })
  # 
  # output$TopAE<-DT::renderDataTable({
  #   req(BCItableInput())
  #   BCItableInput()
  # },
  # caption = "Detail information of adverse events shown in plot above"
  # )
  # 
  # # create the download option for user to download the table
  # output$BCItabledown<-renderUI({
  #   if(!(BCIv$doPlot==0) & !(is.null(input$PTnumInput)) & !(is.null(input$confInput))){
  #     downloadButton('BCItabledownload', "Download the table")
  #   }
  # })
  # 
  # output$BCItabledownload<-downloadHandler(
  #   filename = function(){paste("BCItable", ".csv")},
  #   content =function(file){
  #     req(BCItableInput())
  #     write.csv(BCItableInput(), file, row.names = FALSE)
  #   })
  # 
  # 


 

}
)
