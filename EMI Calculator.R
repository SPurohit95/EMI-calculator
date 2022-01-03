library('shiny')
library('tidyverse')
library('lubridate')
library('plotly')

ui<- fluidPage(
  
  ###### Put Title to the page  #########
  
  titlePanel("EMI Calculator", windowTitle = "EMI Calculator"),
  
  #### Take the Inputs from the values ######
  navbarPage("Select Type",
             tabPanel("Home",sidebarLayout(
               sidebarPanel(
                 helpText("Enter values"),
                 
                 sliderInput("h_loansId",label="Amount", min = 300000, max = 10000000, value = 600000 ),
                 
                 sliderInput("h_rateId",label="Rate of Interest per year", min= 5.5, max= 12, value= 7),
                 
                 numericInput("h_timeId", label = "Number of months",value = 12),
                 
                 dateInput("h_start_date",
                           "Start Date",
                           today(),
                           format = "yyyy-mm-dd"),
                 
                  actionButton("do1", "Generate!"),
                  width = 3
                  
                ),
               
               mainPanel(
                 splitLayout(
                         verbatimTextOutput("h_emi"),
                        plotlyOutput("h_desire_chart")),
                 width = 6,
                 dataTableOutput("main_data_h"),
                 plotlyOutput("Int_Prnp_h")
                 
               )
             )),
             
             tabPanel("Car",sidebarLayout(
               sidebarPanel(
                 helpText("Enter values"),
                 
                 sliderInput("c_loansId",label="Amount", min = 300000, max = 5000000, value = 600000 ),
                 
                 sliderInput("c_rateId",label="Rate of Interest per year", min= 7.5, max= 12, value= 9),
                 
                 dateInput("c_start_date",
                           "Start Date",
                           today(),
                           format = "yyyy-mm-dd"),
                 
                 numericInput("c_timeId", label = "Number of months",value = 12),
                 
                  actionButton("do2", "Generate!"),
                  width = 3
               ),
               
                  mainPanel(
                    splitLayout(verbatimTextOutput("c_emi"),
                                plotlyOutput("c_desire_chart")),
                      width = 6,
                    dataTableOutput("main_data_c"),
                    plotlyOutput("Int_Prnp_c")
               )
             
             )
             ),
           
            tabPanel("Personal",sidebarLayout(
             sidebarPanel(
               helpText("Enter values"),
               
              sliderInput("p_loansId",label="Amount", min = 30000, max = 1000000, value = 60000 ),
              
              sliderInput("p_rateId",label="Rate of Interest per year", min= 9.5, max= 16, value= 11),
              
              dateInput("p_start_date",
                        "Start Date",
                        today(),
                        format = "yyyy-mm-dd"),
              
              numericInput("p_timeId", label = "Number of months",value = 12),
              
               actionButton("do3", "Generate!"),
               width = 3
               
                ),
              mainPanel(
                splitLayout(verbatimTextOutput("p_emi"),
                           plotlyOutput("p_desire_chart")),
                width = 6,
                dataTableOutput("main_data_p"),
                plotlyOutput("Int_Prnp_p")
                      )
    
                 )
               ))  
  
    
  
)



server<- function(input,output)
{
  
  #Function for EMI calculation

    emi_calc= function(loan_amt, rate, tenure, start_date)
    {
      monthly_rate= rate/12
      r=(1 + monthly_rate)^ tenure - 1
      emi= loan_amt* monthly_rate*(r+1)/r

      interest = principal = payment = balance = balance_per = vector("numeric", tenure)

      out_principal= loan_amt

   #### Calculate EMI schedule #########

         for(i in 1:tenure)
        {
          intr = out_principal* monthly_rate
          if(out_principal < payment[i])
          {
            prnp = out_principal
            out_principal = 0
            final = TRUE

          }
          else
          {
            prnp = emi - intr
            out_principal = out_principal - prnp
            final = FALSE

          }

          interest[i]  = round(intr,2)
          principal[i] = round(prnp,2)
          payment[i] = round((prnp + intr),2)
          balance[i] = round(out_principal,2)
          balance_per[i]= round((balance[i]/loan_amt),2)

        }


    ##### Store in Dataframe #############
       data.frame('Month' = 1:tenure, 'Date' = ymd(start_date) %m+% months(1:tenure), 'Interest' = interest, 'Principal' = principal, 'Payment' = payment, 'Balance' = balance, "Loan Remaining in %"= balance_per)
      # return(final_df)

    }

    emi_calc1= function(loan_amt, rate, tenure, start_date)
    {
      monthly_rate= rate/12
      r=(1 + monthly_rate)^ tenure - 1
      emi= loan_amt* monthly_rate*(r+1)/r
      return(round(emi,2))
    }
    
    ######## For Home Loan Tab ############
    
    schedule_data_h <- eventReactive(input$do1, {
    schld_h<- emi_calc(loan_amt=input$h_loansId, rate=input$h_rateId/100, tenure=input$h_timeId, start_date=input$h_start_date)
    schld_h$Date <- format(schld_h$Date,'%Y-%m-%d')  
    return(schld_h)
    })
     
    output$main_data_h<- renderDataTable({
       schedule_data_h()
     })
    
    chart_data_h<- eventReactive(input$do1, {
      schld_h<- emi_calc(loan_amt=input$h_loansId, rate=input$h_rateId/100, tenure=input$h_timeId, start_date=input$h_start_date)
      name = c("Principal","Interest")
      start_bal = input$h_loansId
      final_bal = schld_h$Balance[input$tenure]
      principal = sum(schld_h$Principal)
      interest = sum(schld_h$Interest)
      values = c(principal , interest )
      dat = data.frame(cbind(name, values))
      dat$values <- as.numeric(dat$values)
      total <- sum(dat$values)
      dat$percents <- 100*(dat$values / total)
      dat = dat %>% dplyr::arrange(desc(percents))
      
      return(dat)
    })
    
    
    output$h_emi<- renderText(
      
       paste0("Emi=",emi_calc1(loan_amt=input$h_loansId, rate=input$h_rateId/100, tenure=input$h_timeId, start_date=input$h_start_date), '\n' ,"Total = ",(emi_calc1(loan_amt=input$h_loansId, rate=input$h_rateId/100, tenure=input$h_timeId, start_date=input$h_start_date)*input$h_timeId),
              '\n',"Interest=", round(((emi_calc1(loan_amt=input$h_loansId, rate=input$h_rateId/100, tenure=input$h_timeId, start_date=input$h_start_date)*input$h_timeId)-input$h_loansId)),2)
       )
    
    
    output$h_desire_chart <- renderPlotly({
      # store rendered pie_data() as a tibble
      data <- tibble(chart_data_h())
      fig <- plot_ly(data, labels = ~name, values = ~values, type = 'pie')
      fig <- fig %>% layout(title = paste0(input$type, ' Details', sep=""),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    
    intp_data_h<- eventReactive(input$do1, {
      intp_h<- emi_calc(loan_amt=input$h_loansId, rate=input$h_rateId/100, tenure=input$h_timeId, start_date=input$h_start_date)
      intp_h[, "year"] <- format(intp_h[,"Date"], "%Y")
      return(intp_h)
      })

    output$Int_Prnp_h <- renderPlotly(
      {
        data<- tibble(intp_data_h())
        fig<- plot_ly(data, x = ~year, y = ~Principal, type = 'bar', name = 'Principal') %>%
          add_trace(y = ~Interest, name = 'Interest') %>%
          add_trace(y= ~Balance, type ='scatter', mode='lines', name="Remaining balance")%>%
          layout(yaxis = list(title = 'Schedule'), barmode = 'stack')
      }
    )

    
    
    
    
    
    ####### For Car Loan Tab#################
    
    schedule_data_c <- eventReactive(input$do2, {
      schld_c<- emi_calc(loan_amt=input$c_loansId, rate=input$c_rateId/100, tenure=input$c_timeId, start_date=input$c_start_date)
      schld_c$Date <- format(schld_c$Date,'%Y-%m-%d')  
      return(schld_c)
    })
    
    output$main_data_c<- renderDataTable({
      schedule_data_c()
    })
    
    chart_data_c<- eventReactive(input$do2, {
      schld_c<- emi_calc(loan_amt=input$c_loansId, rate=input$c_rateId/100, tenure=input$c_timeId, start_date=input$c_start_date)
      name = c("Principal","Interest")
      start_bal = input$c_loansId
      final_bal = schld_c$Balance[input$tenure]
      principal = sum(schld_c$Principal)
      interest = sum(schld_c$Interest)
      values = c(principal , interest )
      dat = data.frame(cbind(name, values))
      dat$values <- as.numeric(dat$values)
      total <- sum(dat$values)
      dat$percents <- 100*(dat$values / total)
      dat = dat %>% dplyr::arrange(desc(percents))
      
      return(dat)
    })
    
    output$c_emi<- renderText(
      paste0("Emi=",emi_calc1(loan_amt=input$c_loansId, rate=input$c_rateId/100, tenure=input$c_timeId, start_date=input$c_start_date), '\n' ,"Total = ",(emi_calc1(loan_amt=input$c_loansId, rate=input$c_rateId/100, tenure=input$c_timeId, start_date=input$c_start_date)*input$c_timeId),
             '\n',"Interest=", round(((emi_calc1(loan_amt=input$c_loansId, rate=input$c_rateId/100, tenure=input$c_timeId, start_date=input$c_start_date)*input$c_timeId)-input$c_loansId)),2)
    )
    output$c_desire_chart <- renderPlotly({
      # store rendered pie_data() as a tibble
      data <- tibble(chart_data_c())
      fig <- plot_ly(data, labels = ~name, values = ~values, type = 'pie')
      fig <- fig %>% layout(title = paste0(input$type, ' Details', sep=""),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    
    intp_data_c<- eventReactive(input$do2, {
      intp_c<- emi_calc(loan_amt=input$c_loansId, rate=input$c_rateId/100, tenure=input$c_timeId, start_date=input$c_start_date)
      intp_c[, "year"] <- format(intp_c[,"Date"], "%Y")
      return(intp_c)
    })
    
    output$Int_Prnp_c <- renderPlotly(
      {
        data<- tibble(intp_data_c())
        fig<- plot_ly(data, x = ~year, y = ~Principal, type = 'bar', name = 'Principal') %>%
          add_trace(y = ~Interest, name = 'Interest') %>%
          add_trace(y= ~Balance, type ='scatter', mode='lines', name="Remaining balance")%>%
          layout(yaxis = list(title = 'Schedule'), barmode = 'stack')
      }
    )
    
    
  ############ For Personal Loan Tab ######################333  
    
    schedule_data_p <- eventReactive(input$do3, {
      schld_p<- emi_calc(loan_amt=input$p_loansId, rate=input$p_rateId/100, tenure=input$p_timeId, start_date=input$p_start_date)
      schld_p$Date <- format(schld_p$Date,'%Y-%m-%d')  
      return(schld_p)
    })
    
    output$main_data_p<- renderDataTable({
      schedule_data_p()
    })
    
    chart_data_p<- eventReactive(input$do1, {
      schld_p<- emi_calc(loan_amt=input$p_loansId, rate=input$p_rateId/100, tenure=input$p_timeId, start_date=input$p_start_date)
      name = c("Principal","Interest")
      start_bal = input$p_loansId
      final_bal = schld_p$Balance[input$tenure]
      principal = sum(schld_p$Principal)
      interest = sum(schld_p$Interest)
      values = c(principal , interest )
      dat = data.frame(cbind(name, values))
      dat$values <- as.numeric(dat$values)
      total <- sum(dat$values)
      dat$percents <- 100*(dat$values / total)
      dat = dat %>% dplyr::arrange(desc(percents))
      
      return(dat)
    })
    
    output$p_emi<- renderText(
      paste0("Emi=",emi_calc1(loan_amt=input$p_loansId, rate=input$p_rateId/100, tenure=input$p_timeId, start_date=input$p_start_date), '\n' ,"Total = ",(emi_calc1(loan_amt=input$p_loansId, rate=input$p_rateId/100, tenure=input$p_timeId, start_date=input$p_start_date)*input$p_timeId),
             '\n',"Interest=", round(((emi_calc1(loan_amt=input$p_loansId, rate=input$p_rateId/100, tenure=input$p_timeId, start_date=input$p_start_date)*input$p_timeId)-input$p_loansId)),2)
    )
    output$p_desire_chart <- renderPlotly({
      # store rendered pie_data() as a tibble
      data <- tibble(chart_data_p())
      fig <- plot_ly(data, labels = ~name, values = ~values, type = 'pie')
      fig <- fig %>% layout(title = paste0(input$type, ' Details', sep=""),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    
    intp_data_p<- eventReactive(input$do3, {
      intp_p<- emi_calc(loan_amt=input$p_loansId, rate=input$p_rateId/100, tenure=input$p_timeId, start_date=input$p_start_date)
      intp_p[, "year"] <- format(intp_p[,"Date"], "%Y")
      return(intp_p)
    })
    
    output$Int_Prnp_p <- renderPlotly(
      {
        data<- tibble(intp_data_p())
        fig<- plot_ly(data, x = ~year, y = ~Principal, type = 'bar', name = 'Principal') %>%
          add_trace(y = ~Interest, name = 'Interest') %>%
          add_trace(y= ~Balance, type ='scatter', mode='lines', name="Remaining balance")%>%
          layout(yaxis = list(title = 'Schedule'), barmode = 'stack')
      }
    )


  
}

shinyApp(ui = ui, server = server)
