library(shiny)
###########################################
ui.sqcpchart <- fluidPage(
  titlePanel("Statistical Quality Control Chart for P (Proportion of Defective) by SAH Hashemian and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("pihat", "pihat:", value = 0.01, min = 0, step = 0.001),
      numericInput("n", "n:", value = 100, min = 1),
      textInput("b_values", "Values:", value = "4,5,3,7,2,1,4,3,0,2,6,3,1,0,1,3,2,2,1,1"),
      actionButton("plot_btn", "Draw Plot")
    ),
    
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("summary")
    )
  )
)

server.sqcpchart <- function(input, output) {
  
  observeEvent(input$plot_btn, {
    pihat <- input$pihat
    n <- input$n
    
    clhat <- pihat
    lclhat <- pihat - 3 * (pihat * (1 - pihat) / n) ^ 0.5
    uclhat <- pihat + 3 * (pihat * (1 - pihat) / n) ^ 0.5
    if (lclhat < 0) {
      lclhat <- 0
    }
    # تبدیل مقادیر b به عدد
    b_values <- as.numeric(unlist(strsplit(input$b_values, ",")))
    
    # ساخت دیتا فریم
    q <- data.frame(b = b_values / n, s = seq_along(b_values))
    
    # بررسی مقادیر
    co <- q$b <= uclhat
    q <- cbind(q, co)
    
    # رسم نمودار
    output$plot <- renderPlot({
      plot(x = q$s, y = q$b, type = "o", pch = 19, 
           col = ifelse(q$b < lclhat, 1, ifelse(q$b > uclhat, 1, 1)),
           ylim = c(0, max(q$b) + 0.05), 
           xlab = "Sample Number", ylab = "P hat")
      
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
shinyApp(ui.sqcpchart,server.sqcpchart)

