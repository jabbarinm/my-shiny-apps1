#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui.npchart <- fluidPage(
  titlePanel("Statistical Quality Control Chart for NP (Number of Defective) by SAH Hashemian and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("m", "m:", value = 200, min = 1),
      textInput("b_values", "Values:", value = "23,15,17,15,41,0,25,31,29,0,8,16"),
      actionButton("plot_btn", "Draw Plot")
    ),
    
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("summary")
    )
  )
)

# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
server.npchart <- function(input, output) {
  
  observeEvent(input$plot_btn, {
    m <- input$m
    b_values <- as.numeric(unlist(strsplit(input$b_values, ",")))
    n <- length(b_values)
    
    b_values_n= b_values/m
    total <- sum(b_values_n)
    pihatbar <- 1/n * total
    
    clhat <- m*pihatbar
    a=m*pihatbar*(1-pihatbar)
    lclhat <- m*pihatbar - 3 * a ^ 0.5
    uclhat <- m*pihatbar + 3 * a ^ 0.5
    if (lclhat < 0) {
      lclhat <- 0
    }
    
    # ساخت دیتا فریم
    q <- data.frame(b = b_values , s = seq_along(b_values))
    
    # بررسی مقادیر
    co <- q$b <= uclhat
    q <- cbind(q, co)
    
    # رسم نمودار
    output$plot <- renderPlot({
      plot(x = q$s, y = q$b, type = "o", pch = 19, 
           col = ifelse(q$b < lclhat, 1, ifelse(q$b > uclhat, 1, 1)),
           ylim = c(0, max(q$b) + 0.05), 
           xlab = "Sample Number", ylab = "NP hat")
      
      abline(h = c(lclhat, uclhat), col = "black", lw = 2, lt = 2)
      abline(h = clhat, col = "green")
      
      points(x = q$s[q$b <= uclhat & q$b >= lclhat], 
             y = q$b[q$b <= uclhat & q$b >= lclhat], 
             col = "black", pch = 19)
      
      points(x = q$s[q$b > uclhat | q$b < lclhat], 
             y = q$b[q$b > uclhat | q$b < lclhat], 
             col = "red", pch = 19)
    })
    
    # نمایش مقادیر حدود اطمینان
    output$summary <- renderPrint({
      cat("Quality Control Limit:\n")
      cat("P hat bar:", pihatbar, "\n")
      cat("LCL:", lclhat, "\n")
      cat("CL:", clhat, "\n")
      cat("UCL:", uclhat, "\n")
    })
  })
}

shinyApp(ui.npchart,server.npchart)


