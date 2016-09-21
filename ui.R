library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("Whether You Will Get Married Or Not"),
    sidebarPanel(
      sliderInput("PINCP", label = "Please Select Your Personal Income", 
                  value = 10000, min = min(ACS_pus_2$PINCP), 
                  max = max(ACS_pus_2$PINCP), step = 1000),
      numericInput("AGEP", label = "Please Select Your Age", 
                   value = 25, min = min(ACS_pus_2$AGEP), max = max(ACS_pus_2$AGEP), step = 1),
      selectInput("Race", label = "Please Select Your Race",
                  choices = c("White","African", "American Indian", "Alaska Native","not specified","Asian", 
                              "Pacific", "Some Other Race","Two or More Races ")),
      selectInput("Occupation", label = "Please Select Your Job Type",
                  choices = c("MGR","BUS", "FIN", "CMM", "ENG", "SCI", "CMS", "LGL", "EDU", 
                              "ENT", "MED","HLS","PRT","EAT","CLN","PRS","SAL","OFF","FFF",
                              "CON","EXT","RPR","PRD","TRN","MIL","UNEMP")),
      selectInput("Length.of.Work", label = "Please Select Your Race",
                  choices = c("50-52 Weeks","48-49 Weeks", "40-47 Weeks", "27-39 Weeks", "14-26 Weeks", "13 and less")),
      selectInput("SCHL_code", label = "Please Select Your Race",
                  choices = c("1.Less than K12", "2.K12", "3.Bachelor's","4.Master's", "5.Doctor's")),
      
      submitButton("Submit")),
    mainPanel(
      h3("Results of Prediction"),
      h4("The predicted probability that you may get married is"),
      verbatimTextOutput("Prediction"))
  )
)
