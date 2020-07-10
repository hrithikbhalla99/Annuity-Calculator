#ANNUITY CALCULATOR
library(shiny)
library(dplyr)
epv=1
kpx=1
kpx1=1
kpxmale=1
kpxfemale=1
epvg=1
kpxg=1
kpxt=1
epvlsm=1
epvlsfm=1
kpxlsm=1
kpxlsfm=1
am92=read.csv("am92.csv")
pma92c20=read.csv("PMA92C20.csv")
pfa92c20=read.csv("PFA92C20.csv")
elt15males=read.csv("ELT15MALES.csv")
elt15females=read.csv("ELT15FEMALES.csv")

# Define UI for application that calculates EPV
ui=shinyUI(fluidPage(
  
  # Application title
  titlePanel("Annuity Calculator"),
  p(em("created by Hrithik Bhalla")),
  # Sidebar with a numeric input for amount, numeric input for interest, slider input for age and duration and male age and female age, radio button inputs for mortality and type of annuity and payable.
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "amount",label = "Annuity Amount (Rs.) :",value = 1),
      numericInput(inputId = "interest",label = "Interest rate (%) :",value = 4),
      radioButtons(inputId = "mortality",label = "Mortality Table (not applicable for Joint Whole Life and Last Survivor Whole Life Annuity) :",choices = c("AM92","PMA92C20","PFA92C20","ELT15MALES","ELT15FEMALES")),
      radioButtons(inputId = "type",label = "Type of Annuity :",choices = c("Whole Life Annuity","Temporary Annuity","Deferred Annuity","Guaranteed Annuity","Joint Whole Life Annuity for PMA92C20/PFA92C20","Joint Whole Life Annuity for ELT15MALES/ELT15FEMALES","Last Survivor Whole Life Annuity for PMA92C20/PFA92C20","Last Survivor Whole Life Annuity for ELT15MALES/ELT15FEMALES")),
      radioButtons(inputId="payable",label = "Annuity Payable :",choices = c("Advance","Arrear","Continuous")),
      sliderInput(inputId = "age",label = "Age of Annuitant :",value = 60,min = 0,max = 120),
      sliderInput(inputId = "duration",label="Duration (applicable for Temporary and Deferred annuity) :",min = 0,max=100,value=10),
      sliderInput(inputId = "maleage",label = "Age of Male Partner (applicable for Joint Annuity) :",value = 50,min = 0,max = 120),
      sliderInput(inputId = "femaleage",label = "Age of Female Partner (applicable for Joint Annuity) :",value = 30,min = 0,max = 120)
      
    ),
    
    # Show a text of generated distribution
    mainPanel(
      h3("Expected Present Value of Annuity (Rs.)"),
      textOutput(outputId = "annuitytable")
    )
  )
))



# Define server logic required to calculate EPV
server=shinyServer(function(input, output) {
  
  output$annuitytable <- renderText({
    
    
    
    # Calculate EPV based on input$type, input$age, input$interest and input$mortality from ui.R
    v= 1/(1+(input$interest/100))
  if (input$payable=="Advance"){  
    if (input$type=="Whole Life Annuity"){
      if (input$mortality=="AM92"){
        lim = 120 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a=(input$age + i)
          qx=filter(am92,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
          }
        epv * input$amount

      } else if (input$mortality=="PMA92C20"){
        lim = 120 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$age + i)
          qx = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv +(pvf * kpx)
        }
        epv* input$amount
        
      } else if (input$mortality=="PFA92C20"){
        lim = 120 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$age + i)
          qx = filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv +(pvf * kpx)
        }
        epv* input$amount
        
      } else if (input$mortality=="ELT15MALES"){
        lim = 108 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a = (input$age + i)
          qx = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv +(pvf * kpx)
        }
        epv* input$amount
        
      } else if (input$mortality=="ELT15FEMALES"){
        lim = 111 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$age + i)
          qx = filter(elt15females,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv +(pvf * kpx)
        }
        epv* input$amount
      }
      
    }else if (input$type=="Temporary Annuity"){
      if (input$mortality=="AM92"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(am92,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv* input$amount
        
      } else if (input$mortality=="PMA92C20"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv* input$amount
        
      } else if (input$mortality=="PFA92C20"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv* input$amount
        
      } else if (input$mortality=="ELT15MALES"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(elt15males,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv* input$amount
        
      } else if (input$mortality=="ELT15FEMALES"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(elt15females,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv* input$amount
      }
      
    }else if (input$type=="Deferred Annuity"){
      if (input$mortality=="AM92"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
        qx1=filter(am92,Age==a1)%>%select(qx)%>%unlist
        kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(am92,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv * v^input$duration *kpx1* input$amount
        
      } else if (input$mortality=="PMA92C20"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(pma92c20,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:(lim)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x+i)
          qx=filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv*(v^input$duration) * kpx1* input$amount
        
      } else if (input$mortality=="PFA92C20"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(pfa92c20,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:(lim)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x +i)
          qx=filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv*(v^input$duration) * kpx1* input$amount
        
      } else if (input$mortality=="ELT15MALES"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(elt15males,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 108 - x
        for (k in 0:(lim)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x+i)
          qx=filter(elt15males,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv*(v^input$duration) * kpx1* input$amount
        
      } else if (input$mortality=="ELT15FEMALES"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(elt15females,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 111 - x
        for (k in 0:(lim)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x+i)
          qx=filter(elt15females,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv*(v^input$duration) * kpx1* input$amount
      }
      
    }else if (input$type=="Joint Whole Life Annuity for PMA92C20/PFA92C20"){
      if (input$maleage>=input$femaleage){
      lim = 120 - input$maleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
            b= (input$femaleage + i)
          qxa = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          qxb = filter(pfa92c20,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        epv * input$amount 
        
      } else if (input$maleage<input$femaleage){
        lim = 120 - input$femaleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
            b= (input$femaleage + i)
          qxa = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          qxb = filter(pfa92c20,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        epv * input$amount 
      } 
      
    }else if (input$type=="Joint Whole Life Annuity for ELT15MALES/ELT15FEMALES"){
      if (input$maleage>=input$femaleage){
        lim = 108 - input$maleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          qxb = filter(elt15females,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        epv * input$amount 
        
      } else if (input$maleage<input$femaleage){
        lim = 111 - input$femaleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          qxb = filter(elt15females,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        epv * input$amount 
      } 
      
    }else if (input$type=="Last Survivor Whole Life Annuity for PMA92C20/PFA92C20"){
      lim = 120 - input$maleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$maleage + i)
        qxlsm = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
        kpxlsm = kpxlsm * (1 - qxlsm)
        epvlsm = epvlsm +(pvf * kpxlsm)
      }
      
      lim = 120 - input$femaleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$femaleage + i)
        qxlsfm = filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
        kpxlsfm = kpxlsfm * (1 - qxlsfm)
        epvlsfm = epvlsfm + (pvf * kpxlsfm)
      }
      
      if (input$maleage>=input$femaleage){
        lim = 120 - input$maleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          qxb = filter(pfa92c20,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        (epvlsm+epvlsfm-epv) * input$amount 
        
      } else if (input$maleage<input$femaleage){
        lim = 120 - input$femaleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          qxb = filter(pfa92c20,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        (epvlsm+epvlsfm-epv) * input$amount 
      } 
      
    }else if (input$type=="Last Survivor Whole Life Annuity for ELT15MALES/ELT15FEMALES"){
      lim = 108 - input$maleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$maleage + i)
        qxlsm = filter(elt15males,Age==a)%>%select(qx)%>%unlist
        kpxlsm = kpxlsm * (1 - qxlsm)
        epvlsm = epvlsm +(pvf * kpxlsm)
      }
      
      lim = 111 - input$femaleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$femaleage + i)
        qxlsfm = filter(elt15females,Age==a)%>%select(qx)%>%unlist
        kpxlsfm = kpxlsfm * (1 - qxlsfm)
        epvlsfm = epvlsfm + (pvf * kpxlsfm)
      }
      
      if (input$maleage>=input$femaleage){
        lim = 108 - input$maleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          qxb = filter(elt15females,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        (epvlsm+epvlsfm-epv) * input$amount 
        
      } else if (input$maleage<input$femaleage){
        lim = 111 - input$femaleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          qxb = filter(elt15females,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        (epvlsm+epvlsfm-epv) * input$amount 
      } 
      
    }else if (input$type=="Guaranteed Annuity"){
      if (input$mortality=="AM92"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(am92,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(am92,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=120-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(am92,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
            
        }
        ((epv * v^input$duration *kpx1) + epvg) * input$amount
        
      } else if (input$mortality=="PMA92C20"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(pma92c20,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=120-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(pma92c20,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
        }
        ((epv * v^input$duration *kpx1) + epvg) * input$amount
        
      } else if (input$mortality=="PFA92C20"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(pfa92c20,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=120-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(pfa92c20,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
        }
        ((epv * v^input$duration *kpx1) + epvg) * input$amount
        
      } else if (input$mortality=="ELT15MALES"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(elt15males,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 108 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(elt15males,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=108-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(elt15males,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
        }
        ((epv * v^input$duration *kpx1) + epvg) * input$amount
        
      } else if (input$mortality=="ELT15FEMALES"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(elt15females,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 111 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(elt15females,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=111-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(elt15females,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
        }
        ((epv * v^input$duration *kpx1) + epvg) * input$amount
      }
    }
    
  }else if (input$payable=="Arrear"){
    v= 1/(1+(input$interest/100))
    if (input$type=="Whole Life Annuity"){
      if (input$mortality=="AM92"){
        lim = 120 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a=(input$age + i)
          qx=filter(am92,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        (epv-1) * input$amount
        
      } else if (input$mortality=="PMA92C20"){
        lim = 120 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$age + i)
          qx = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv +(pvf * kpx)
        }
        (epv-1) * input$amount
        
      } else if (input$mortality=="PFA92C20"){
        lim = 120 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$age + i)
          qx = filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv +(pvf * kpx)
        }
        (epv-1) * input$amount
        
      } else if (input$mortality=="ELT15MALES"){
        lim = 108 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$age + i)
          qx = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv +(pvf * kpx)
        }
        (epv-1) * input$amount
        
      } else if (input$mortality=="ELT15FEMALES"){
        lim = 111 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$age + i)
          qx = filter(elt15females,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv +(pvf * kpx)
        }
        (epv-1) * input$amount
      }
      
    }else if (input$type=="Temporary Annuity"){
      if (input$mortality=="AM92"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(am92,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        for (j in 0:(lim)){
          t=input$age + j
          qxt= filter(am92,Age==t)%>%select(qx)%>%unlist
          kpxt = kpxt * (1 - qxt)
        }
        (epv-(1-(kpxt* v^input$duration)))* input$amount
        
      } else if (input$mortality=="PMA92C20"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        for (j in 0:(lim)){
          t=input$age + j
          qxt= filter(pma92c20,Age==t)%>%select(qx)%>%unlist
          kpxt = kpxt * (1 - qxt)
        }
        (epv-(1-(kpx*v^input$duration)))* input$amount
        
      } else if (input$mortality=="PFA92C20"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        for (j in 0:(lim)){
          t=input$age + j
          qxt= filter(pfa92c20,Age==t)%>%select(qx)%>%unlist
          kpxt = kpxt * (1 - qxt)
        }
        (epv-(1-(kpx*v^input$duration)))* input$amount
        
      } else if (input$mortality=="ELT15MALES"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(elt15males,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        for (j in 0:(lim)){
          t=input$age + j
          qxt= filter(elt15males,Age==t)%>%select(qx)%>%unlist
          kpxt = kpxt * (1 - qxt)
        }
        (epv-(1-(kpx*v^input$duration)))* input$amount
        
      } else if (input$mortality=="ELT15FEMALES"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(elt15females,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        for (j in 0:(lim)){
          t=input$age + j
          qxt= filter(elt15females,Age==t)%>%select(qx)%>%unlist
          kpxt = kpxt * (1 - qxt)
        }
        (epv-(1-(kpx * v^input$duration)))* input$amount
      }
      
    }else if (input$type=="Deferred Annuity"){
      if (input$mortality=="AM92"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(am92,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(am92,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv = (epv-1) * v^input$duration *kpx1
        (epv)* input$amount
        
      } else if (input$mortality=="PMA92C20"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(pma92c20,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:(lim)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x+i)
          qx=filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv = (epv-1) * v^input$duration *kpx1
        (epv)* input$amount
        
      } else if (input$mortality=="PFA92C20"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(pfa92c20,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:(lim)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x +i)
          qx=filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv = (epv-1) * v^input$duration *kpx1
        (epv)* input$amount
        
      } else if (input$mortality=="ELT15MALES"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(elt15males,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 108 - x
        for (k in 0:(lim)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x+i)
          qx=filter(elt15males,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv = (epv-1) * v^input$duration *kpx1
        (epv)* input$amount
        
      } else if (input$mortality=="ELT15FEMALES"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(elt15females,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 111 - x
        for (k in 0:(lim)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x+i)
          qx=filter(elt15females,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
          
        }
        epv = (epv-1) * v^input$duration *kpx1
        (epv)* input$amount
      }
      
    }else if (input$type=="Joint Whole Life Annuity for PMA92C20/PFA92C20"){
      if (input$maleage>=input$femaleage){
      lim = 120 - input$maleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$maleage + i)
          b= (input$femaleage + i)
        qxa = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
        qxb = filter(pfa92c20,Age==b)%>%select(qx)%>%unlist
        kpxmale = kpxmale * (1 - qxa)
        kpxfemale = kpxfemale * (1 - qxb)
        epv = epv + (pvf * kpxmale * kpxfemale )
        }
      (epv-1)* input$amount 
      
      } else if (input$maleage<input$femaleage){
        lim = 120 - input$femaleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          qxb = filter(pfa92c20,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        (epv-1) * input$amount 
      }
      
    }else if (input$type=="Joint Whole Life Annuity for ELT15MALES/ELT15FEMALES"){
      if (input$maleage>=input$femaleage){
        lim = 108 - input$maleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          qxb = filter(elt15females,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        (epv-1)* input$amount 
        
      } else if (input$maleage<input$femaleage){
        lim = 111 - input$femaleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          qxb = filter(elt15females,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        (epv-1) * input$amount 
      }
      
    }else if (input$type=="Last Survivor Whole Life Annuity for PMA92C20/PFA92C20"){
      lim = 120 - input$maleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$maleage + i)
        qxlsm = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
        kpxlsm = kpxlsm * (1 - qxlsm)
        epvlsm = epvlsm +(pvf * kpxlsm)
      }
      
      lim = 120 - input$femaleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$femaleage + i)
        qxlsfm = filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
        kpxlsfm = kpxlsfm * (1 - qxlsfm)
        epvlsfm = epvlsfm + (pvf * kpxlsfm)
      }
      
      if (input$maleage>=input$femaleage){
        lim = 120 - input$maleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          qxb = filter(pfa92c20,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        ((epvlsm-1)+(epvlsfm-1)-(epv-1)) * input$amount 
        
      } else if (input$maleage<input$femaleage){
        lim = 120 - input$femaleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          qxb = filter(pfa92c20,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        ((epvlsm-1)+(epvlsfm-1)-(epv-1)) * input$amount
      } 
      
    }else if (input$type=="Last Survivor Whole Life Annuity for ELT15MALES/ELT15FEMALES"){
      lim = 108 - input$maleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$maleage + i)
        qxlsm = filter(elt15males,Age==a)%>%select(qx)%>%unlist
        kpxlsm = kpxlsm * (1 - qxlsm)
        epvlsm = epvlsm +(pvf * kpxlsm)
      }
      
      lim = 111 - input$femaleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$femaleage + i)
        qxlsfm = filter(elt15females,Age==a)%>%select(qx)%>%unlist
        kpxlsfm = kpxlsfm * (1 - qxlsfm)
        epvlsfm = epvlsfm + (pvf * kpxlsfm)
      }
      
      if (input$maleage>=input$femaleage){
        lim = 108 - input$maleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          qxb = filter(elt15females,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        ((epvlsm-1)+(epvlsfm-1)-(epv-1)) * input$amount
        
      } else if (input$maleage<input$femaleage){
        lim = 111 - input$femaleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          qxb = filter(elt15females,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        ((epvlsm-1)+(epvlsfm-1)-(epv-1)) * input$amount
      } 
      
    }else if (input$type=="Guaranteed Annuity"){
      if (input$mortality=="AM92"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(am92,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(am92,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=120-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(am92,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
        }
        (((epv-1) * v^input$duration *kpx1) + (epvg-1)) * input$amount
        
      } else if (input$mortality=="PMA92C20"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(pma92c20,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=120-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(pma92c20,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
          
        }
        (((epv-1) * v^input$duration *kpx1) + (epvg-1)) * input$amount
        
      } else if (input$mortality=="PFA92C20"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(pfa92c20,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=120-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(pfa92c20,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
        }
        (((epv-1) * v^input$duration *kpx1) + (epvg-1)) * input$amount
      } else if (input$mortality=="ELT15MALES"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(elt15males,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 108 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(elt15males,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=108-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(elt15males,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
        }
        (((epv-1) * v^input$duration *kpx1) + (epvg-1)) * input$amount
        
      } else if (input$mortality=="ELT15FEMALES"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(elt15females,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 111 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(elt15females,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=111-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(elt15females,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
        }
        (((epv-1) * v^input$duration *kpx1) + (epvg-1)) * input$amount
      }
    } 
  }else if (input$payable=="Continuous"){
    v= 1/(1+(input$interest/100))
    if (input$type=="Whole Life Annuity"){
      if (input$mortality=="AM92"){
        lim = 120 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a=(input$age + i)
          qx=filter(am92,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        (epv-0.5) * input$amount
        
      } else if (input$mortality=="PMA92C20"){
        lim = 120 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$age + i)
          qx = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv +(pvf * kpx)
        }
        (epv-0.5) * input$amount
        
      } else if (input$mortality=="PFA92C20"){
        lim = 120 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$age + i)
          qx = filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv +(pvf * kpx)
        }
        (epv-0.5) * input$amount
        
      } else if (input$mortality=="ELT15MALES"){
        lim = 108 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$age + i)
          qx = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv +(pvf * kpx)
        }
        (epv-0.5) * input$amount
        
      } else if (input$mortality=="ELT15FEMALES"){
        lim = 111 - input$age
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$age + i)
          qx = filter(elt15females,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv +(pvf * kpx)
        }
        (epv-0.5) * input$amount
      }
      
    }else if (input$type=="Temporary Annuity"){
      if (input$mortality=="AM92"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(am92,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        for (j in 0:(lim)){
          t=input$age + j
          qxt= filter(am92,Age==t)%>%select(qx)%>%unlist
          kpxt = kpxt * (1 - qxt)
        }
        (epv-(0.5*(1-(kpxt* v^input$duration))))* input$amount
        
      } else if (input$mortality=="PMA92C20"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        for (j in 0:(lim)){
          t=input$age + j
          qxt= filter(pma92c20,Age==t)%>%select(qx)%>%unlist
          kpxt = kpxt * (1 - qxt)
        }
        (epv-(0.5*(1-(kpx*v^input$duration))))* input$amount
        
      } else if (input$mortality=="PFA92C20"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        for (j in 0:(lim)){
          t=input$age + j
          qxt= filter(pfa92c20,Age==t)%>%select(qx)%>%unlist
          kpxt = kpxt * (1 - qxt)
        }
        (epv-(0.5*(1-(kpx*v^input$duration))))* input$amount
        
      } else if (input$mortality=="ELT15MALES"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(elt15males,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        for (j in 0:(lim)){
          t=input$age + j
          qxt= filter(elt15males,Age==t)%>%select(qx)%>%unlist
          kpxt = kpxt * (1 - qxt)
        }
        (epv-(0.5*(1-(kpx*v^input$duration))))* input$amount
        
      } else if (input$mortality=="ELT15FEMALES"){
        lim= input$duration - 1
        for (k in 0:(lim-1)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(input$age+i)
          qx=filter(elt15females,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        for (j in 0:(lim)){
          t=input$age + j
          qxt= filter(elt15females,Age==t)%>%select(qx)%>%unlist
          kpxt = kpxt * (1 - qxt)
        }
        (epv-(0.5*(1-(kpx * v^input$duration))))* input$amount
      }
      
    }else if (input$type=="Deferred Annuity"){
      if (input$mortality=="AM92"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(am92,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(am92,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv = (epv-0.5) * v^input$duration *kpx1
        (epv)* input$amount
        
      } else if (input$mortality=="PMA92C20"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(pma92c20,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:(lim)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x+i)
          qx=filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv = (epv-0.5) * v^input$duration *kpx1
        (epv)* input$amount
        
      } else if (input$mortality=="PFA92C20"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(pfa92c20,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:(lim)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x +i)
          qx=filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv = (epv-0.5) * v^input$duration *kpx1
        (epv)* input$amount
        
      } else if (input$mortality=="ELT15MALES"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(elt15males,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 108 - x
        for (k in 0:(lim)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x+i)
          qx=filter(elt15males,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        epv = (epv-0.5) * v^input$duration *kpx1
        (epv)* input$amount
        
      } else if (input$mortality=="ELT15FEMALES"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(elt15females,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 111 - x
        for (k in 0:(lim)){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x+i)
          qx=filter(elt15females,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
          
        }
        epv = (epv-0.5) * v^input$duration *kpx1
        (epv)* input$amount
      }
      
    }else if (input$type=="Joint Whole Life Annuity for PMA92C20/PFA92C20"){
      if (input$maleage>=input$femaleage){
        lim = 120 - input$maleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          qxb = filter(pfa92c20,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        (epv-0.5)* input$amount 
        
      } else if (input$maleage<input$femaleage){
        lim = 120 - input$femaleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          qxb = filter(pfa92c20,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        (epv-0.5) * input$amount 
      }
      
    }else if (input$type=="Joint Whole Life Annuity for ELT15MALES/ELT15FEMALES"){
      if (input$maleage>=input$femaleage){
        lim = 108 - input$maleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          qxb = filter(elt15females,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        (epv-0.5)* input$amount 
        
      } else if (input$maleage<input$femaleage){
        lim = 111 - input$femaleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          qxb = filter(elt15females,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        (epv-0.5) * input$amount 
      }
      
    }else if (input$type=="Last Survivor Whole Life Annuity for PMA92C20/PFA92C20"){
      lim = 120 - input$maleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$maleage + i)
        qxlsm = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
        kpxlsm = kpxlsm * (1 - qxlsm)
        epvlsm = epvlsm +(pvf * kpxlsm)
      }
      
      lim = 120 - input$femaleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$femaleage + i)
        qxlsfm = filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
        kpxlsfm = kpxlsfm * (1 - qxlsfm)
        epvlsfm = epvlsfm + (pvf * kpxlsfm)
      }
      
      if (input$maleage>=input$femaleage){
        lim = 120 - input$maleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          qxb = filter(pfa92c20,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        ((epvlsm-0.5)+(epvlsfm-0.5)-(epv-0.5)) * input$amount 
        
      } else if (input$maleage<input$femaleage){
        lim = 120 - input$femaleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          qxb = filter(pfa92c20,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        ((epvlsm-0.5)+(epvlsfm-0.5)-(epv-0.5)) * input$amount
      } 
      
    }else if (input$type=="Last Survivor Whole Life Annuity for ELT15MALES/ELT15FEMALES"){
      lim = 108 - input$maleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$maleage + i)
        qxlsm = filter(elt15males,Age==a)%>%select(qx)%>%unlist
        kpxlsm = kpxlsm * (1 - qxlsm)
        epvlsm = epvlsm +(pvf * kpxlsm)
      }
      
      lim = 111 - input$femaleage
      for (k in 0:lim){
        pvf= (exp((k+1)*log(v)))
        for (i in 0:k)
          a = (input$femaleage + i)
        qxlsfm = filter(elt15females,Age==a)%>%select(qx)%>%unlist
        kpxlsfm = kpxlsfm * (1 - qxlsfm)
        epvlsfm = epvlsfm + (pvf * kpxlsfm)
      }
      
      if (input$maleage>=input$femaleage){
        lim = 108 - input$maleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          qxb = filter(elt15females,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        ((epvlsm-0.5)+(epvlsfm-0.5)-(epv-0.5)) * input$amount
        
      } else if (input$maleage<input$femaleage){
        lim = 111 - input$femaleage
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:k)
            a = (input$maleage + i)
          b= (input$femaleage + i)
          qxa = filter(elt15males,Age==a)%>%select(qx)%>%unlist
          qxb = filter(elt15females,Age==b)%>%select(qx)%>%unlist
          kpxmale = kpxmale * (1 - qxa)
          kpxfemale = kpxfemale * (1 - qxb)
          epv = epv + (pvf * kpxmale * kpxfemale )
        }
        ((epvlsm-0.5)+(epvlsfm-0.5)-(epv-0.5)) * input$amount
      } 
      
    }else if (input$type=="Guaranteed Annuity"){
      if (input$mortality=="AM92"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(am92,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(am92,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=120-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(am92,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
        }
        (((epv-0.5) * v^input$duration *kpx1) + (epvg-0.5)) * input$amount
        
      } else if (input$mortality=="PMA92C20"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(pma92c20,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(pma92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=120-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(pma92c20,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
          
        }
        (((epv-0.5) * v^input$duration *kpx1) + (epvg-0.5)) * input$amount
        
      } else if (input$mortality=="PFA92C20"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(pfa92c20,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 120 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(pfa92c20,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=120-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(pfa92c20,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
        }
        (((epv-0.5) * v^input$duration *kpx1) + (epvg-0.5)) * input$amount
      } else if (input$mortality=="ELT15MALES"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(elt15males,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 108 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(elt15males,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=108-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(elt15males,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
        }
        (((epv-0.5) * v^input$duration *kpx1) + (epvg-0.5)) * input$amount
        
      } else if (input$mortality=="ELT15FEMALES"){
        lim1 = input$duration - 1
        for (j in 0:lim1){
          a1 = (input$age + j)
          qx1=filter(elt15females,Age==a1)%>%select(qx)%>%unlist
          kpx1= kpx1 * (1 - qx1)
        }
        x = input$age + input$duration
        lim= 111 - x
        for (k in 0:lim){
          pvf= (exp((k+1)*log(v)))
          for (i in 0:(k))
            a=(x + i)
          qx=filter(elt15females,Age==a)%>%select(qx)%>%unlist
          kpx = kpx * (1 - qx)
          epv = epv + (pvf * kpx)
        }
        lim2=111-input$duration
        for (m in 0:lim2){
          pvf= (exp((m+1)*log(v)))
          for (n in 0:(m))
            ag=(input$duration + n)
          qxg=filter(elt15females,Age==ag)%>%select(qx)%>%unlist
          kpxg = kpxg * (1 - qxg)
          epvg = epvg + (pvf * kpxg)
        }
        (((epv-0.5) * v^input$duration *kpx1) + (epvg-0.5)) * input$amount
      }
    } 
  }    # Calculate EPV with all the inputs.
  })
  
})

shinyApp(ui=ui,server=server)

