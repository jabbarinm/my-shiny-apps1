
library(shiny)
################################################

ui.sqcxbarsdchart <- fluidPage(
  titlePanel("Statistical Quality Control Chart for X-bar and S (Mean and Standard Deviation of Numeric Variable) by SAH Hashemian and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)"),
  numericInput("num_vectors", "n", value = 4, min = 1,max = 24),
  uiOutput("dynamic_vectors"),
  actionButton("create_df_btn", "Make Dataframe and Draw Plot"),
  tableOutput("df_output"),
  plotOutput("plot_xbar"),
  plotOutput("plot_Sbar"),
  verbatimTextOutput("summary")
)

server.sqcxbarsdchart <- function(input, output, session) {
  B4<-c(3.267,2.568,2.266,2.089,1.970,1.882,1.815,1.761,1.716,1.679,1.646,1.618,1.594,1.572,1.552,1.534,1.518,1.503,1.490,1.477,1.466,1.455,1.445,1.435)
  B3<-c(0,0,0,0,0.030,0.118,0.185,0.239,0.284,0.321,0.354,0.382,0.406,0.428,0.448,0.466,0.482,0.497,0.510,0.523,0.534,0.545,0.555,0.565)
  A3<-c(2.659,1954,1.628,1.427,1.287,1.182,1.099,1.032,0.975,0.927,0.886,0.850,0.817,0.789,0.763,0.739,0.718,0.698,0.680,0.663,0.647,0.633,0.619,0.606)
  output$dynamic_vectors <- renderUI({
    num_vectors <- input$num_vectors
    lapply(1:num_vectors, function(i) {
      textInput(paste("vector", i, sep = "_"), label = paste("Values", i), value = "72,70,69,74,71,73,75,70,76")
    })
  })
  observeEvent(input$create_df_btn, {
    vectors <- lapply(1:input$num_vectors, function(i) {
      as.numeric(unlist(strsplit(input[[paste("vector", i, sep = "_")]], ",")))
    })
    if (any(sapply(vectors, function(x) any(is.na(x))))) {
      showModal(modalDialog(
        title = "error",
        "Check Spaces between Values",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    df <- data.frame(vectors)
    names(df) <- paste("values", 1:input$num_vectors, sep = "_")
    total <- rowSums(df)
    xbar <- total / input$num_vectors
    xbarbar <- mean(xbar)
    S <- apply(df, 1, var)
    S <- S^0.5
    Sbar <- mean(S)
    n <- input$num_vectors
    BI4 <- B4[n-1]
    BI3 <- B3[n-1]
    A3 <- A3[n-1]
    LCLRbar <- Sbar * BI3
    UCLRbar <- Sbar * BI4
    CLRbar <- Sbar
    LCLxbarbar <- xbarbar - A3*Sbar
    UCLxbarbar <- xbarbar + A3*Sbar
    CLxbarbar <- xbarbar
    if (LCLRbar < 0) {
      LCLRbar <- 0
    }
    if (LCLxbarbar < 0) {
      LCLxbarbar <- 0
    }
    output$df_output <- renderTable({
      df
    })
    output$plot_xbar <- renderPlot({
      plot(1:length(xbar), xbar, type = "o", pch = 19, col = "black",
           ylim = c(min(xbar) - 0.05, max(xbar) + 0.05), 
           xlab = "Sample Number", ylab = "xbar")
      abline(h = xbarbar, col = "green", lty = 2)
      abline(h = LCLxbarbar, col = "black", lty = 2)
      abline(h = UCLxbarbar, col = "black", lty = 2)
      inside_limits_xbar <- which(xbar >= LCLxbarbar & xbar <= UCLxbarbar)
      points(inside_limits_xbar, xbar[inside_limits_xbar], col = "black", pch = 19)
      outside_limits_xbar <- which(xbar < LCLxbarbar | xbar > UCLxbarbar)
      points(outside_limits_xbar, xbar[outside_limits_xbar], col = "red", pch = 19)
    })
    output$plot_Sbar <- renderPlot({
      plot(1:length(S), S, type = "o", pch = 19, col = "black",
           ylim = c(min(S) - 0.05, max(S) + 0.05), 
           xlab = "Sample Number", ylab = "S")
      abline(h = CLRbar, col = "green", lty = 2)
      abline(h = LCLRbar, col = "black", lty = 2)
      abline(h = UCLRbar, col = "black", lty = 2)
      inside_limits_R <- which(S >= LCLRbar & S <= UCLRbar)
      points(inside_limits_R, S[inside_limits_R], col = "black", pch = 19)
      outside_limits_R <- which(S < LCLRbar | S > UCLRbar)
      points(outside_limits_R, S[outside_limits_R], col = "red", pch = 19)
    })
    output$summary <- renderPrint({
      cat("Quality Control Limit:\n")
      cat("LCL-xbar:", LCLxbarbar, "\n")
      cat("CL-xbar:", xbarbar, "\n")
      cat("UCL-xbar:", UCLxbarbar, "\n")
      cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%","\n")
      cat("LCL-S:", LCLRbar, "\n")
      cat("CL-S:", CLRbar, "\n")
      cat("UCL-S:", UCLRbar, "\n")
    })
  })
}

shinyApp(ui.sqcxbarsdchart,server.sqcxbarsdchart)