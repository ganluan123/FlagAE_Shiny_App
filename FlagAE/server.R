# increase the maximum allowed size for uploading file
options(shiny.maxRequestSize=100*1024^2)
# call the libarary FlagAE
library(FlagAE)
library(DT)
library(ggplot2)


server <- function(input, output) {

  ################################################################################
  ##############                                             #####################
  ############## get the summary of the adverase event data  #####################
  ##############                                             #####################
  ################################################################################
  # get the ADSL.csv
  # ADSL is a reactive variable
  ADSL<-reactive({
    ADSLread<-input$ADSLInput
    if (is.null(ADSLread))
      return(NULL)
    read.csv(ADSLread$datapath, header=TRUE)
  })

  # get the ADAE.csv
  # ADAE is a reactive variable
  ADAE<-reactive({
    ADAEread<-input$ADAEInput
    if (is.null(ADAEread))
      return(NULL)
    read.csv(ADAEread$datapath, header=TRUE)
  })

  # get the AEdata from preprocess4 function in libarary FlagAE
  AEdata<-reactive({

    ADSLread<-input$ADSLInput
    ADAEread<-input$ADAEInput
    if(is.null(ADSLread) | is.null(ADAEread)) return (NULL)

    preprocess(adsl=ADSL(), adae=ADAE())
  })

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
    #if(input$FETInput==FALSE ) return (NULL)
    numericInput("PTnumInput", "Insert the number of adverse events to show", value = 10)
  })

  output$confInput<-renderUI({
    #if(input$FETInput==FALSE)  return (NULL)
    numericInput("confInput", "Insert the confidence level for fisher exact test",
                               value = 0.95, min=0, max=1, step = 0.025)
  })

  # create the plot with activation button

  # the FETv is to make sure that the plot and table will
  # disappear and will not run before user click 'Run' button
  FETv <- reactiveValues(doPlot = FALSE)

  observeEvent(input$FETInput, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    FETv$doPlot <- input$FETInput
  })

  # my understanding is that this is to make sure that
  # whenever user change PTnumInput or confInput, the plot
  # will disappear and wont run before user click run button
  observeEvent((input$PTnumInput & input$confInput), {
    FETv$doPlot <- FALSE
  })

  FETplotInput<-function(){
    # if (input$FETInput==FALSE | is.null(input$PTnumInput) | is.null(input$confInput)) return ()
    # if (input$FETInput==0 | is.null(input$PTnumInput) | is.null(input$confInput)) return ()
    if (FETv$doPlot==0 | is.null(input$PTnumInput) | is.null(input$confInput)) return ()
    isolate({
    gci(aedata=AEdata(), ptnum = input$PTnumInput, conf.level = input$confInput)
    })
  }
  # plot out the plot
  output$FETplot<-renderPlot({
    FETplotInput()
  })
  # provide the download option for user
  output$FETplotdown<-renderUI({
    # if(!(input$FETInput==FALSE) & !(is.null(input$PTnumInput)) & !(is.null(input$confInput))){
    #   downloadButton('FETplotdownload', "Download the plot")
    # }

    if(!(FETv$doPlot==0) & !(is.null(input$PTnumInput)) & !(is.null(input$confInput))){
      downloadButton('FETplotdownload', "Download the plot")
    }

  })

  output$FETplotdownload <- downloadHandler(
    filename <- "test.jpeg",
    content <- function(file) {
      jpeg(file, width = 1100, height=720, quality = 450, pointsize = 9, res = 180)
      #tiff(file, width=6, height=4.8, res = 300)
      FETplotInput()
      dev.off()
    })

  # create the table for details of AE selected in above plot
  FETtableInput<-reactive({
    if (FETv$doPlot==0 | is.null(input$PTnumInput) | is.null(input$confInput)) return ()
    topPTlist<-gci3(aedata=AEdata(), ptnum=input$PTnumInput, conf.level = input$confInput)
    AEdata()[AEdata()$AEDECOD %in% topPTlist, ]
  })

  output$TopAE<-DT::renderDataTable({
    FETtableInput()
  },
  caption = "Detail information of adverse events shown in plot above"
  )

  # create the download option for user to download the table
  output$FETtabledown<-renderUI({
    if(!(FETv$doPlot==0) & !(is.null(input$PTnumInput)) & !(is.null(input$confInput))){
      downloadButton('FETtabledownload', "Download the table")
    }
  })

  output$FETtabledownload<-downloadHandler(
    filename <- function(){paste("test", ".csv")},
    content <-function(file){
      write.csv(FETtableInput(), file, row.names = FALSE)
    })

  ################################################################################
  ##############                                             #####################
  ##############        3 stage Hierarchical model           #####################
  ##############                                             #####################
  ################################################################################

  # give values for Gibbs samplinig parameters
  output$HierSampleInput<-renderUI({
    Samplelist<-tagList()
    # number of adaptation
    Samplelist[[1]]<-numericInput("HieradaptInput", "Adaptation", value = 10)
    # number of burn in
    Samplelist[[2]]<-numericInput("HierburnInput", "Burn In", value = 10)
    # number of iteration
    Samplelist[[3]]<-numericInput("HieriterInput", "Iterations", value = 10)
    # number of thin
    Samplelist[[4]]<-numericInput("HierthinInput", "Thin", value = 2)
    # number of chains
    Samplelist[[5]]<-numericInput("HierchainInput", "Chains", value = 2)

    Samplelist
  })

  # give values to whether to show the option to let the user to chose
  # to use different initials for different chains
  output$HierDiffInput<-renderUI({
    Chains<-input$HierchainInput
    if(is.null(Chains)) return(NULL)
    if(Chains<=1) return(NULL)
    checkboxInput("HierDiffInitInput", "Check to use DIFFERENT initials for different chains")
  })

  # give values for Hierarchical model Initials

  output$HierInitInput<-renderUI({

    Initallist<-tagList()
    Chains<-input$HierchainInput
    if(is.null(Chains)) return()

    # one chain
    if (Chains==1){
      Initallist[[1]]<-numericInput(inputId = "mu_gamma_0", label = "mu_gamma_0", value= 0.1)
      Initallist[[2]]<-numericInput(inputId = "tau_gamma_0", label = "tau_gamma_0", value= 0.1)
      Initallist[[3]]<-numericInput(inputId = "mu_theta_0", label = "mu_theta_0", value= 0.1)
      Initallist[[4]]<-numericInput(inputId = "tau_theta_0", label = "tau_theta_0", value= 0.1)
      Initallist[[5]]<-numericInput(inputId = "alpha_pi", label = "alpha_pi", value= 2)
      Initallist[[6]]<-numericInput(inputId = "beta_pi", label = "beta_pi", value= 2)
    }

    # same initials for all chains
    if(Chains>1 & !is.null(input$HierDiffInitInput)){
      if (input$HierDiffInitInput==FALSE){
        Initallist[[1]]<-numericInput(inputId = "mu_gamma_0", label = "mu_gamma_0", value= 0.1)
        Initallist[[2]]<-numericInput(inputId = "tau_gamma_0", label = "tau_gamma_0", value= 0.1)
        Initallist[[3]]<-numericInput(inputId = "mu_theta_0", label = "mu_theta_0", value= 0.1)
        Initallist[[4]]<-numericInput(inputId = "tau_theta_0", label = "tau_theta_0", value= 0.1)
        Initallist[[5]]<-numericInput(inputId = "alpha_pi", label = "alpha_pi", value= 2)
        Initallist[[6]]<-numericInput(inputId = "beta_pi", label = "beta_pi", value= 2)
      }

      # different initials for different chains
      if(input$HierDiffInitInput==TRUE){
        for(i in 1:Chains){
          Initallist[[((i-1)*6+1)]]<-numericInput(inputId = paste0("C", i, "_mu_gamma_0"), label =paste0("mu_gamma_0 for Chain ", i) , value= 0.1)
          Initallist[[((i-1)*6+2)]]<-numericInput(inputId = paste0("C", i, "_tau_gamma_0"), label =paste0("tau_gamma_0 for Chain ", i) , value= 0.1)
          Initallist[[((i-1)*6+3)]]<-numericInput(inputId = paste0("C", i, "_mu_theta_0"), label =paste0("mu_theta_0 for Chain ", i) , value= 0.1)
          Initallist[[((i-1)*6+4)]]<-numericInput(inputId = paste0("C", i, "_tau_theta_0"), label =paste0("tau_theta_0 for Chain ", i) , value= 0.1)
          Initallist[[((i-1)*6+5)]]<-numericInput(inputId = paste0("C", i, "_alpha_pi"), label =paste0("alpha_pi for Chain ", i) , value= 2)
          Initallist[[((i-1)*6+6)]]<-numericInput(inputId = paste0("C", i, "_beta_pi"), label =paste0("beta_pi for Chain ", i) , value= 2)
        }
      }
    }

    Initallist
  })

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

  observeEvent((input$HierSampleInput & input$HierInitInput), {
    Hierv$doPlot <- FALSE
  })

  #run the model
  Hiermodel<- function(){
    Chains<-input$HierchainInput
    if (Hierv$doPlot==0 ) return ()
    if (is.null(Chains)) return ()
    if (Chains>1 & is.null(input$HierDiffInitInput)) return ()


    isolate({
       #first create the INITS
      if(Chains==1){
        INITS_SingleChain<-list(mu.gamma.0=input$mu_gamma_0, tau.gamma.0=input$tau_gamma_0, mu.theta.0=input$mu_theta_0,
                                tau.theta.0=input$tau_theta_0, alpha.pi=input$alpha_pi, beta.pi=input$beta_pi)
        # run the model
        HierData<-Hier(aedata=AEdata(), inits=INITS_SingleChain, n_burn=input$HierburnInput,
                       n_iter=input$HieriterInput, thin=input$HierthinInput, n_adapt = input$HieradaptInput,
                       n_chain = 1)
      }
      if(Chains>1){
        INITS_MultChain<-list()
        # use same initials for all chains
        if(input$HierDiffInitInput==FALSE){
          for(i in 1:Chains){
            INITS_MultChain[[i]]<-list(mu.gamma.0=input$mu_gamma_0, tau.gamma.0=input$tau_gamma_0, mu.theta.0=input$mu_theta_0,
                                       tau.theta.0=input$tau_theta_0, alpha.pi=input$alpha_pi, beta.pi=input$beta_pi)
          }
        }

        # use different initials for different chains
        if(input$HierDiffInitInput==TRUE){
          for(i in 1:Chains){
            INITS_MultChain[[i]]<-list(mu.gamma.0=input[[paste0("C", i, "_mu_gamma_0")]],
                                            tau.gamma.0=input[[paste0("C", i, "_tau_gamma_0")]],
                                            mu.theta.0=input[[paste0("C", i, "_mu_theta_0")]],
                                            tau.theta.0=input[[paste0("C", i, "_tau_theta_0")]],
                                            alpha.pi=input[[paste0("C", i, "_alpha_pi")]],
                                            beta.pi=input[[paste0("C", i, "_beta_pi")]])
          }
        }

      # run the model
      HierData<-Hier(aedata=AEdata(), inits=INITS_MultChain, n_burn=input$HierburnInput,
                       n_iter=input$HieriterInput, thin=input$HierthinInput, n_adapt = input$HieradaptInput,
                       n_chain = Chains)
      }

      HierData

     })
  }

    output$Hiertable<-DT::renderDataTable({
      Hiermodel()
    })









}
