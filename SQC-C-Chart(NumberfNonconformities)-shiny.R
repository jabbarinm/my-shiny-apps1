library(shiny)

############################################
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Define UI for application that draws a histogram
ui.sqccchart <- fluidPage(
  titlePanel("Statistical Quality Control Chart for C (Number of NonConformities) by SAH Hashemian and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("b_values", "Values:", value = "21,24,16,12,15,5,28,20,31,35,20,24,16,19,10,17,13,22,18,39,30,24,16,19,17,15"),
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


# Define server logic required to draw a histogram
server.sqccchart <- function(input, output) {
  
  observeEvent(input$plot_btn, {
    b_values <- as.numeric(unlist(strsplit(input$b_values, ",")))
    n = length(b_values)
    
    C_bar = sum(b_values)/n
    clhat = C_bar
    lclhat = C_bar - 3 * C_bar ^ 0.5
    uclhat = C_bar + 3 * C_bar ^ 0.5
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
           xlab = "Sample Number", ylab = "C hat")
      
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
      cat("LCL:", lclhat, "\n")
      cat("CL:", clhat, "\n")
      cat("UCL:", uclhat, "\n")
    })
  })
}

shinyApp(ui.sqccchart,server.sqccchart)



