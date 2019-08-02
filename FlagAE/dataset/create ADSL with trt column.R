# Fed_ADAE
Fed_ADAE<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\Fed_ADAE.csv")
Fed_ADAE<-Fed_ADAE[Fed_ADAE$TRTEMFL=='Y',]
# get the "USUBJID", "AEBODSYS","AEDECOD", and "ACTARMCD" column
# # here "ACTARMCD" column is not required is not necessary 
# for further analysis, it is here only for generation purpose
Fed_ADAE<-Fed_ADAE[, c("USUBJID", "AEBODSYS", "AEDECOD", "ACTARMCD")]
write.csv(Fed_ADAE, "H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\Fed_ADAE_TRTCTR.csv", row.names = FALSE)


# Fed_ADSL
Fed_ADSL<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\Fed_ADSL.csv")
### select treated subjects
Fed_ADSL<-Fed_ADSL[Fed_ADSL$SAFFL == 'Y',]

# create the "TRTCTR" column 
Fed_ADSL$TRTCTR<-rep(NA, dim(Fed_ADSL)[1])
Fed_ADSL$TRTCTR[Fed_ADSL$ACTARMCD=='400MG'] <- 1
Fed_ADSL$TRTCTR[Fed_ADSL$ACTARMCD=='PLACEBO'|Fed_ADSL$ACTARMCD=='PL-400MG'|Fed_ADSL$ACTARMCD=='PL-500MG'] <- 0
Fed_ADSL <- Fed_ADSL[!is.na(Fed_ADSL$TRTCTR),]

# get "USUBJID","ACTARMCD", "TRTCTR" column
Fed_ADSL<-Fed_ADSL[, c("USUBJID", "ACTARMCD", 'TRTCTR')]
write.csv(Fed_ADSL, "H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\Fed_ADSL_TRTCTR.csv", row.names = FALSE)

##########################################################################################
##########################################################################################
# ABI_ADAE
ABI_ADAE<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\ABI_ADAE.csv")
ABI_ADAE<-ABI_ADAE[ABI_ADAE$TRTEMFL=='Y',]
# get the "USUBJID", "AEBODSYS","AEDECOD", and "TRT01A" column
# # here "ACTARMCD" column is not required is not necessary 
# for further analysis, it is here only for generation purpose
ABI_ADAE<-ABI_ADAE[, c("USUBJID", "AEBODSYS", "AEDECOD", "TRT01A")]
write.csv(ABI_ADAE, "H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\ABI_ADAE_TRTCTR.csv", row.names = FALSE)


# ABI_ADSL
ABI_ADSL<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\ABI_ADSL.csv")
### select treated subjects
ABI_ADSL<-ABI_ADSL[ABI_ADSL$SAFFL == 'Y',]

# create the "TRTCTR" column 
ABI_ADSL$TRTCTR<-rep(0, dim(ABI_ADSL)[1])
for (i in 1:dim(ABI_ADSL)[1]){
  if (grepl("ABI",ABI_ADSL[i,"TRT01A"])) ABI_ADSL[i,"TRTCTR"]<-1
}

# get "USUBJID","TRT01A", "TRTCTR" column
ABI_ADSL<-ABI_ADSL[, c("USUBJID", "TRT01A", 'TRTCTR')]
write.csv(ABI_ADSL, "H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\ABI_ADSL_TRTCTR.csv", row.names = FALSE)

##########################################################################################
##########################################################################################
# demo_ADAE
demo_ADAE<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\demo_ADAE.csv")
demo_ADAE<-demo_ADAE[demo_ADAE$TRTEMFL=='Y',]

demo_ADAE<-demo_ADAE[, 1:3]
write.csv(demo_ADAE, "H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\demo_ADAE_TRTCTR.csv", row.names = FALSE)


# demo_ADSL
demo_ADSL<-read.csv("H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\demo_ADSL.csv")
### select treated subjects
demo_ADSL<-demo_ADSL[demo_ADSL$SAFFL == 'Y',]

# create the "TRTCTR" column 
demo_ADSL$TRTCTR<-rep(0, dim(demo_ADSL)[1])
for (i in 1:dim(demo_ADSL)[1]){
  if (grepl("xyz",demo_ADSL[i,"TREATMENT"])) demo_ADSL[i,"TRTCTR"]<-1
}

# get "USUBJID","TREATMENT", "TRTCTR" column
demo_ADSL<-demo_ADSL[, c("USUBJID", "TREATMENT", 'TRTCTR')]
write.csv(demo_ADSL, "H:\\Safety data\\R Shiny App\\FlagAE\\dataset\\demo_ADSL_TRTCTR.csv", row.names = FALSE)
