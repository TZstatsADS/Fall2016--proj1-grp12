class_model = glm(MAR_New ~ Occupation+Length.of.Work+PINCP+SCHL_code+AGEP+Race, data = ACS_pus_2,
                  family = "binomial",weights = Weight)

martiage = 0
occup = "MED"
lengthofwork = "50-52 Weeks"
income = 72000
education = "4.Master"
age = 28
race = "White"
## prediction method 
predict_marriage = function(occup,lengthofwork,income,education,age,race){ 
  inputdata = as.data.frame(t(as.vector(c(occup,lengthofwork,income,education,age,race))))                   
  names(inputdata) = c("Occupation","Length.of.Work","PINCP","SCHL_code","AGEP","Race")
  inputdata$PINCP = as.numeric(inputdata$PINCP)
  inputdata$AGEP = as.numeric(inputdata$AGEP)
  estim = predict(class_model,inputdata)
  out = 1/(1+exp(-estim+class_model$coefficient[1]))
  out
}
pred = predict_marriage("MED","50-52 Weeks",72000,"4.Master's",28,"Asian")

shinyServer(
  function(input,output){
    output$Prediction = renderPrint({
      occup1 = input$Occupation
      lengthofwork1 = input$Length.of.Work
      income1 = input$PINCP
      education1 = input$SCHL_code
      age1 = input$AGEP
      race1 = input$Race
      pred = predict_marriage(occup1,lengthofwork1,income1,education1,age1,race1)  
      paste(round(100*pred),"%")
    })
  }  
)
