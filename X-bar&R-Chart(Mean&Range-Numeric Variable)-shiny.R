library(shiny)

#######################################################
ui.sqcxbarrchart <- fluidPage(
  titlePanel("Statistical Quality Control Chart for X-bar and R (Mean and Range of Numeric Variable) by SAH Hashemian and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)"),
  numericInput("num_vectors", "n", value = 4, min = 1,max = 24),
  uiOutput("dynamic_vectors"),
  actionButton("create_df_btn", "Make Dataframe and Draw Plot"),
  tableOutput("df_output"),
  plotOutput("plot_xbar"),
  plotOutput("plot_Rmat"),
  verbatimTextOutput("summary")
)

server.sqcxbarrchart <- function(input, output, session) {
  A2 <- c(1.88, 1.023, 0.729, 0.577, 0.483, 0.419, 0.373, 0.337, 0.308, 0.285, 0.266, 0.249, 0.235, 0.223, 0.212, 0.203, 0.194, 0.187, 0.180, 0.173, 0.167, 0.162, 0.157, 0.153)
  D3 <- c(0, 0, 0, 0, 0, 0.076, 0.136, 0.184, 0.223, 0.256, 0.283, 0.307, 0.328, 0.347, 0.363, 0.378, 0.391, 0.403, 0.415, 0.425, 0.434, 0.443, 0.451, 0.459)
  D4 <- c(3.267, 2.574, 2.282, 2.114, 2.004, 1.924, 1.864, 1.816, 1.777, 1.744, 1.717, 1.693, 1.672, 1.653, 1.637, 1.622, 1.608, 1.597, 1.585, 1.575, 11.566, 1.557, 1.548, 1.541)
  output$dynamic_vectors <- renderUI({
    num_vectors <- input$num_vectors
    lapply(1:num_vectors, function(i) {
      textInput(paste("vector", i, sep = "_"), label = paste("Values", i), value = "23,22,28,25,22,26,29,26,22,25,24,24,22,23,24,24,24,22,26,26")
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
    row_maxs <- apply(df, 1, max)
    row_mins <- apply(df, 1, min)
    R_mat <- row_maxs - row_mins
    Rbar <- mean(R_mat)
    n <- input$num_vectors
    DI3 <- D3[n-1]
    DI4 <- D4[n-1]
    AI2 <- A2[n-1]
    LCLRbar <- Rbar * DI3
    UCLRbar <- Rbar * DI4
    CLRbar <- Rbar
    LCLxbarbar <- xbarbar - AI2 * Rbar
    UCLxbarbar <- xbarbar + AI2 * Rbar
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
    output$plot_Rmat <- renderPlot({
      plot(1:length(R_mat), R_mat, type = "o", pch = 19, col = "black",
           ylim = c(min(R_mat) - 0.05, max(R_mat) + 0.05), 
           xlab = "Sample Number", ylab = "Ri")
      abline(h = Rbar, col = "green", lty = 2)
      abline(h = LCLRbar, col = "black", lty = 2)
      abline(h = UCLRbar, col = "black", lty = 2)
      inside_limits_R <- which(R_mat >= LCLRbar & R_mat <= UCLRbar)
      points(inside_limits_R, R_mat[inside_limits_R], col = "black", pch = 19)
      outside_limits_R <- which(R_mat < LCLRbar | R_mat > UCLRbar)
      points(outside_limits_R, R_mat[outside_limits_R], col = "red", pch = 19)
    })
    
    # نمایش مقادیر حدود اطمینان
    output$summary <- renderPrint({
      cat("Quality Control Limit:\n")
      cat("LCL-xbar:", LCLxbarbar, "\n")
      cat("CL-xbar:", xbarbar, "\n")
      cat("UCL-xbar:", UCLxbarbar, "\n")
      cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", "\n")
      cat("LCL-R:", LCLRbar, "\n")
      cat("CL-R:", CLRbar, "\n")
      cat("UCL-R:", UCLRbar, "\n")
    })
  })
}

shinyApp(ui.sqcxbarrchart,server.sqcxbarrchart)

