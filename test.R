testADSL<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\demo_ADSL.csv")

testADSL[,"TREATMENT"]



output$TreatcodeOutput<-renderUI({
  
  if(is.null(treatmentoptions())){
    return ()
  }
  
  checkboxGroupInput("TreatcodeInput", "Treatment codes",
                     choices= treatmentoptions() )
})
#observe({print(treatmentoptions())})



treatmentoptions<-reactive({
  if(is.null(input$TreatcolInput)){return(NULL)}
  
  #sort(unique(ADSL()[, input$TreatcolInput]))
  #sort(unique(ADSL()[, "TREATMENT"]))
  sort(unique(ADSL()[, NULL]))
})

observe({print(treatmentoptions())})