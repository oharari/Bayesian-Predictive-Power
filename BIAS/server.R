options(shiny.maxRequestSize=5000*1024^2) 
options(scipen=999)
rm(list = ls())

shinyServer(
  function(input, output, session){
    
    
    
    output$about = renderUI({
      withMathJax(includeMarkdown('About.Rmd'))
    })
    
    
    # output$about <- renderUI({  
    #   k <- knitr::knit(input = "About.Rmd", quiet = T)
    #   HTML(markdown::markdownToHTML(k, fragment.only = T))
    # })
    
    
    #**********************************************************************************  
    #**********************************************************************************
    #*********                           PPOS Calculations Tab                *********
    #**********************************************************************************  
    #**********************************************************************************
    
    output$P_success_response_select = renderUI({
      radioButtons("p_success_response_type",
                   "Select outcome type:",
                   c("Numeric", "Dichotomous", "Time-to-event"),
                   inline = F
      )
    })
    
    
    
    
    
    
    my_p_success_response = reactive({
      input$p_success_response_type
    })
    
    
    
    
    
    
    output$P_success_test_type = renderUI({
      radioButtons("p_success_test_type",
                   "Testing for:",
                   c("Superiority", "Non-inferiority"),
                   inline = T
      )
    })
    
    
    
    
    
    
    my_p_success_test = reactive({
      input$p_success_test_type
    })
    
    
    
    
    
    
    output$P_success_trial_type = renderUI({
      radioButtons("p_success_trial_type",
                   "Predictive power calculation:",
                   c("Within-trial", "Cross-trial"),
                   inline = T
      )
      
    })
    
    
    
    
    
    
    my_p_success_trial_type = reactive({
      input$p_success_trial_type
    })
    
    
    
    
    
    output$P_success_ctrl_ss = renderUI({
      numericInput("p_success_ctrl_samp_size",
                   "Control arm current sample size:",
                   value = 100,
                   min = 1, 
                   max = Inf, 
                   step = 1)
    })
    
    
    
    
    
    observeEvent(input$p_success_ctrl_samp_size, {
      if(is.na(input$p_success_ctrl_samp_size)){
        updateNumericInput(session, "p_success_ctrl_samp_size",
                           "Control arm current sample size:", 
                           value = 100,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      } else if(is.null(input$p_success_ctrl_samp_size)){
        updateNumericInput(session, "p_success_ctrl_samp_size",
                           "Control arm current sample size:", 
                           value = 100,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      } else if(!is.numeric(input$p_success_ctrl_samp_size)){
        updateNumericInput(session, "p_success_ctrl_samp_size",
                           "Control arm current sample size:", 
                           value = 100,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      } else if(input$p_success_ctrl_samp_size < 30){
        updateNumericInput(session, "p_success_ctrl_samp_size",
                           "Control arm current sample size:", 
                           value = 30,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      }
    })
    
    
    
    
    
    my_p_success_ctrl_samp_size = reactive({
      input$p_success_ctrl_samp_size
    })
    
    
    
    
    
    output$P_success_trt_ss = renderUI({
      numericInput("p_success_trt_samp_size",
                   "Treatment arm current sample size:",
                   value = 100,
                   min = 30, 
                   max = Inf, 
                   step = 1)
    })
    
    
    
    
    
    
    observeEvent(input$p_success_trt_samp_size, {
      if(is.na(input$p_success_trt_samp_size)){
        updateNumericInput(session, "p_success_trt_samp_size",
                           "Treatment arm current sample size:", 
                           value = 100,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      } else if(is.null(input$p_success_trt_samp_size)){
        updateNumericInput(session, "p_success_trt_samp_size",
                           "Treatment arm current sample size:", 
                           value = 100,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      } else if(!is.numeric(input$p_success_trt_samp_size)){
        updateNumericInput(session, "p_success_trt_samp_size",
                           "Treatment arm current sample size:", 
                           value = 100,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      } else if(input$p_success_trt_samp_size < 30){
        updateNumericInput(session, "p_success_trt_samp_size",
                           "Treatment arm current sample size:", 
                           value = 30,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      }
    })
    
    
    
    
    
    
    my_p_success_trt_samp_size = reactive({
      input$p_success_trt_samp_size
    })
    
    
    
    
    
    
    output$P_success_future_ctrl_ss = renderUI({
      numericInput("p_success_future_ctrl_samp_size",
                   "Control arm future sample size:",
                   value = 100,
                   min = 1, 
                   max = Inf, 
                   step = 1)
    })
    
    
    
    
    
    
    
    output$P_success_survival_future_ss = renderUI({
      numericInput("p_success_survival_future_ss",
                   "Combined future sample size:",
                   value = 200,
                   min = 30, 
                   max = Inf, 
                   step = 2)
    })
    
    
    
    
    
    output$P_success_ctrl_or_survival_future_ss = renderUI({
      if(my_p_success_response() == "Time-to-event"){
        uiOutput("P_success_survival_future_ss")
      } else{
        uiOutput("P_success_future_ctrl_ss")
      }
    })
    
    
    
    
    
    output$P_success_ctrl_or_survival_future_ss_tip = renderUI({
      if(my_p_success_response() == "Time-to-event"){
        txt = "Insert the overall number of patients yet to be enrolled."
      } else{
        txt = "Insert the number of patients yet to be enrolled to the control arm."
      }
      
      
      tipify(
        uiOutput("P_success_ctrl_or_survival_future_ss"),
        title = txt,
        options = list(container = "body")
      )
    })
    
    
    
    
    
    
    output$P_success_trt_or_survival_future_ss = renderUI({
      if(my_p_success_response() == "Time-to-event"){
        return()
      } else{
        uiOutput("P_success_future_trt_ss")
      }
    })
    
    
    
    
    
    output$P_success_trt_or_survival_future_ss_tip = renderUI({
      if(my_p_success_response() != "Time-to-event"){
        txt = "Insert the number of patients yet to be enrolled to the treatment arm."
      }
      
      
      tipify(
        uiOutput("P_success_trt_or_survival_future_ss"),
        title = txt,
        options = list(container = "body")
      )
    })
    
    
    
    
    
    
    observeEvent(input$p_success_survival_future_ss, {
      if(is.na(input$p_success_survival_future_ss)){
        updateNumericInput(session, "p_success_survival_future_ss",
                           "Combined future sample size:", 
                           value = 200,
                           min = 30, 
                           max = Inf, 
                           step = 2)
      } else if(is.null(input$p_success_survival_future_ss)){
        updateNumericInput(session, "p_success_survival_future_ss",
                           "Combined future sample size:", 
                           value = 200,
                           min = 30, 
                           max = Inf, 
                           step = 2)
      } else if(!is.numeric(input$p_success_survival_future_ss)){
        updateNumericInput(session, "p_success_survival_future_ss",
                           "Combined future sample size:", 
                           value = 200,
                           min = 30, 
                           max = Inf, 
                           step = 2)
      } else if(input$p_success_survival_future_ss < 30){
        updateNumericInput(session, "p_success_survival_future_ss",
                           "Combined future sample size:", 
                           value = 200,
                           min = 30, 
                           max = Inf, 
                           step = 2)
      }
    })
    
    
    
    
    
    
    
    my_p_success_survival_future_ss = reactive({
      input$p_success_survival_future_ss
    })
    
    
    
    
    
    
    
    observeEvent(input$p_success_future_ctrl_samp_size, {
      if(is.na(input$p_success_future_ctrl_samp_size)){
        updateNumericInput(session, "p_success_future_ctrl_samp_size",
                           "Control arm current sample size:", 
                           value = 100,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      } else if(is.null(input$p_success_future_ctrl_samp_size)){
        updateNumericInput(session, "p_success_future_ctrl_samp_size",
                           "Control arm current sample size:", 
                           value = 100,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      } else if(!is.numeric(input$p_success_future_ctrl_samp_size)){
        updateNumericInput(session, "p_success_future_ctrl_samp_size",
                           "Control arm current sample size:", 
                           value = 100,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      } else if(input$p_success_future_ctrl_samp_size < 30){
        updateNumericInput(session, "p_success_future_ctrl_samp_size",
                           "Control arm current sample size:", 
                           value = 30,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      }
    })
    
    
    
    
    
    my_p_success_future_ctrl_samp_size = reactive({
      input$p_success_future_ctrl_samp_size
    })
    
    
    
    
    
    output$P_success_future_trt_ss = renderUI({
      numericInput("p_success_future_trt_samp_size",
                   "Treatment arm future sample size:",
                   value = 100,
                   min = 1, 
                   max = Inf, 
                   step = 1)
    })
    
    
    
    
    
    
    observeEvent(input$p_success_future_trt_samp_size, {
      if(is.na(input$p_success_future_trt_samp_size)){
        updateNumericInput(session, "p_success_future_trt_samp_size",
                           "Treatment arm current sample size:", 
                           value = 100,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      } else if(is.null(input$p_success_future_trt_samp_size)){
        updateNumericInput(session, "p_success_future_trt_samp_size",
                           "Treatment arm current sample size:", 
                           value = 100,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      } else if(!is.numeric(input$p_success_future_trt_samp_size)){
        updateNumericInput(session, "p_success_future_trt_samp_size",
                           "Treatment arm current sample size:", 
                           value = 100,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      } else if(input$p_success_future_trt_samp_size < 30){
        updateNumericInput(session, "p_success_future_trt_samp_size",
                           "Treatment arm current sample size:", 
                           value = 30,
                           min = 30, 
                           max = Inf, 
                           step = 1)
      }
    })
    
    
    
    
    
    
    my_p_success_future_trt_samp_size = reactive({
      input$p_success_future_trt_samp_size
    })
    
    
    
    
    
    
    output$P_success_ctrl_mean = renderUI({
      numericInput("p_success_ctrl_mean",
                   "Control arm average response:",
                   value = 1,
                   min = -Inf, 
                   max = Inf, 
                   step = 1)
    })
    
    
    
    
    
    
    observeEvent(input$p_success_ctrl_mean, {
      if(is.na(input$p_success_ctrl_mean)){
        updateNumericInput(session, "p_success_ctrl_mean",
                           "Control arm average response:", 
                           value = 1,
                           min = -Inf, 
                           max = Inf, 
                           step = 1)
      } else if(is.null(input$p_success_ctrl_mean)){
        updateNumericInput(session, "p_success_ctrl_mean",
                           "Control arm average response:", 
                           value = 1,
                           min = -Inf, 
                           max = Inf, 
                           step = 1)
      } else if(!is.numeric(input$p_success_ctrl_mean)){
        updateNumericInput(session, "p_success_ctrl_mean",
                           "Control arm average response:", 
                           value = 1,
                           min = -Inf, 
                           max = Inf, 
                           step = 1)
      }
    })
    
    
    
    
    
    
    my_p_success_ctrl_mean = reactive({
      input$p_success_ctrl_mean
    })
    
    
    
    
    
    output$P_success_trt_mean = renderUI({
      numericInput("p_success_trt_mean",
                   "Treatment arm average response:",
                   value = 2,
                   min = -Inf, 
                   max = Inf, 
                   step = 1)
    })
    
    
    
    
    
    
    
    observeEvent(input$p_success_trt_mean, {
      if(is.na(input$p_success_trt_mean)){
        updateNumericInput(session, "p_success_trt_mean",
                           "Treatment arm average response:", 
                           value = 2,
                           min = -Inf, 
                           max = Inf, 
                           step = 1)
      } else if(is.null(input$p_success_trt_mean)){
        updateNumericInput(session, "p_success_trt_mean",
                           "Treatment arm average response:", 
                           value = 2,
                           min = -Inf, 
                           max = Inf, 
                           step = 1)
      } else if(!is.numeric(input$p_success_trt_mean)){
        updateNumericInput(session, "p_success_trt_mean",
                           "Treatment arm average response:", 
                           value = 2,
                           min = -Inf, 
                           max = Inf, 
                           step = 1)
      }
    })
    
    
    
    
    
    
    my_p_success_trt_mean = reactive({
      input$p_success_trt_mean
    })
    
    
    
    
    
    
    output$P_success_ctrl_sd = renderUI({
      if(my_p_success_response() == "Numeric"){
        numericInput("p_success_ctrl_sd",
                     "Control arm standard deviation:",
                     value = 5,
                     min = 1e-4, 
                     max = Inf, 
                     step = .1)
      }
    })
    
    
    
    
    
    
    
    observeEvent(input$p_success_ctrl_sd, {
      if(is.na(input$p_success_ctrl_sd)){
        updateNumericInput(session, "p_success_ctrl_sd",
                           "Control arm standard deviation:", 
                           value = 5,
                           min = 1e-4, 
                           max = Inf, 
                           step = .1)
      } else if(is.null(input$p_success_ctrl_sd)){
        updateNumericInput(session, "p_success_ctrl_sd",
                           "Control arm standard deviation:", 
                           value = 5,
                           min = 1e-4, 
                           max = Inf, 
                           step = .1)
      } else if(!is.numeric(input$p_success_ctrl_sd)){
        updateNumericInput(session, "p_success_ctrl_sd",
                           "Control arm standard deviation:", 
                           value = 5,
                           min = 1e-4, 
                           max = Inf, 
                           step = .1)
      } else if(input$p_success_ctrl_sd < 1e-4){
        updateNumericInput(session, "p_success_ctrl_sd",
                           "Control arm standard deviation:", 
                           value = 1e-4,
                           min = 1e-4, 
                           max = Inf, 
                           step = .1)
      }
    })
    
    
    
    
    
    
    my_p_success_ctrl_sd = reactive({
      input$p_success_ctrl_sd
    })
    
    
    
    
    
    output$P_success_trt_sd = renderUI({
      if(my_p_success_response() == "Numeric"){
        numericInput("p_success_trt_sd",
                     "Treatment arm standard deviation:",
                     value = 5,
                     min = 1e-4, 
                     max = Inf, 
                     step = .1)
      }
    })
    
    
    
    
    
    
    observeEvent(input$p_success_trt_sd, {
      if(is.na(input$p_success_trt_sd)){
        updateNumericInput(session, "p_success_trt_sd",
                           "Treatment arm standard deviation:", 
                           value = 5,
                           min = 1e-4, 
                           max = Inf, 
                           step = .1)
      } else if(is.null(input$p_success_trt_sd)){
        updateNumericInput(session, "p_success_trt_sd",
                           "Treatment arm standard deviation:", 
                           value = 5,
                           min = 1e-4, 
                           max = Inf, 
                           step = .1)
      } else if(!is.numeric(input$p_success_trt_sd)){
        updateNumericInput(session, "p_success_trt_sd",
                           "Treatment arm standard deviation:", 
                           value = 5,
                           min = 1e-4, 
                           max = Inf, 
                           step = .1)
      } else if(input$p_success_trt_sd < 1e-4){
        updateNumericInput(session, "p_success_trt_sd",
                           "Treatment arm standard deviation:", 
                           value = 1e-4,
                           min = 1e-4, 
                           max = Inf, 
                           step = .1)
      }
    })
    
    
    
    
    
    my_p_success_trt_sd = reactive({
      input$p_success_trt_sd
    })
    
    
    
    
    
    output$P_success_ctrl_n_cases = renderUI({
      numericInput("p_success_ctrl_n_cases",
                   "Control arm number of events:",
                   value = 10,
                   min = 0, 
                   max = my_p_success_ctrl_samp_size(), 
                   step = 1)
    })
    
    
    
    
    
    
    
    output$P_success_survival_n_events = renderUI({
      numericInput("p_success_survival_n_events",
                   "Number of events observed:",
                   value = round((my_p_success_ctrl_samp_size() + my_p_success_trt_samp_size())*.75),
                   min = 0, 
                   max = my_p_success_ctrl_samp_size(), 
                   step = 1)
    })
    
    
    
    
    
    
    
    observeEvent(input$p_success_ctrl_n_cases, {
      if(is.na(input$p_success_ctrl_n_cases)){
        updateNumericInput(session, "p_success_ctrl_n_cases",
                           "Control arm number of events:", 
                           value = 10,
                           min = 0, 
                           max = my_p_success_ctrl_samp_size(), 
                           step = 1)
      } else if(is.null(input$p_success_ctrl_n_cases)){
        updateNumericInput(session, "p_success_ctrl_n_cases",
                           "Control arm number of events:", 
                           value = 10,
                           min = 0, 
                           max = my_p_success_ctrl_samp_size(), 
                           step = 1)
      } else if(!is.numeric(input$p_success_ctrl_n_cases)){
        updateNumericInput(session, "p_success_ctrl_n_cases",
                           "Control arm number of events:", 
                           value = 10,
                           min = 0, 
                           max = my_p_success_ctrl_samp_size(), 
                           step = 1)
      } else if(input$p_success_ctrl_n_cases < 0){
        updateNumericInput(session, "p_success_ctrl_n_cases",
                           "Control arm number of events:", 
                           value = 10,
                           min = 0, 
                           max = my_p_success_ctrl_samp_size(), 
                           step = 1)
      }
    })
    
    
    
    
    
    
    observeEvent(input$p_success_survival_n_events, {
      if(is.na(input$p_success_survival_n_events)){
        updateNumericInput(session, "p_success_survival_n_events",
                           "Number of events observed:", 
                           value = round((my_p_success_ctrl_samp_size() + my_p_success_trt_samp_size())*.75),
                           min = 0, 
                           max = my_p_success_ctrl_samp_size() + my_p_success_trt_samp_size(), 
                           step = 1)
      } else if(is.null(input$p_success_survival_n_events)){
        updateNumericInput(session, "p_success_survival_n_events",
                           "Number of events observed:", 
                           value = round((my_p_success_ctrl_samp_size() + my_p_success_trt_samp_size())*.75),
                           min = 0, 
                           max = my_p_success_ctrl_samp_size() + my_p_success_trt_samp_size(), 
                           step = 1)
      } else if(!is.numeric(input$p_success_survival_n_events)){
        updateNumericInput(session, "p_success_survival_n_events",
                           "Number of events observed:", 
                           value = round((my_p_success_ctrl_samp_size() + my_p_success_trt_samp_size())*.75),
                           min = 0, 
                           max = my_p_success_ctrl_samp_size() + my_p_success_trt_samp_size(), 
                           step = 1)
      } else if(input$p_success_survival_n_events < 0){
        updateNumericInput(session, "p_success_survival_n_events",
                           "Number of events observed:", 
                           value = round((my_p_success_ctrl_samp_size() + my_p_success_trt_samp_size())*.75),
                           min = 0, 
                           max = my_p_success_ctrl_samp_size() + my_p_success_trt_samp_size(), 
                           step = 1)
      }
    })
    
    
    
    
    
    
    my_p_success_ctrl_n_cases = reactive({
      input$p_success_ctrl_n_cases
    })
    
    
    
    
    
    my_p_success_survival_n_events = reactive({
      input$p_success_survival_n_events
    })
    
    
    
    
    
    
    
    output$P_success_trt_n_cases = renderUI({
      numericInput("p_success_trt_n_cases",
                   "Treatment arm number of events:",
                   value = 5,
                   min = 0, 
                   max = my_p_success_trt_samp_size(), 
                   step = 1)
    })
    
    
    
    
    
    
    output$P_success_HR_estimate = renderUI({
      numericInput("p_success_HR_estimate",
                   "Hazard ratio estimate:",
                   value = 0.75,
                   min = 0, 
                   max = Inf, 
                   step = .05)
    })
    
    
    
    
    
    
    my_p_success_HR_estimate = reactive({
      input$p_success_HR_estimate
    })
    
    
    
    
    
    observeEvent(input$p_success_trt_n_cases, {
      if(is.na(input$p_success_trt_n_cases)){
        updateNumericInput(session, "p_success_trt_n_cases",
                           "Treatment arm number of events:", 
                           value = 5,
                           min = 0, 
                           max = my_p_success_trt_samp_size(), 
                           step = 1)
      } else if(is.null(input$p_success_trt_n_cases)){
        updateNumericInput(session, "p_success_trt_n_cases",
                           "Treatment arm number of events:", 
                           value = 5,
                           min = 0, 
                           max = my_p_success_trt_samp_size(), 
                           step = 1)
      } else if(!is.numeric(input$p_success_trt_n_cases)){
        updateNumericInput(session, "p_success_trt_n_cases",
                           "Treatment arm number of events:", 
                           value = 5,
                           min = 0, 
                           max = my_p_success_trt_samp_size(), 
                           step = 1)
      } else if(input$p_success_trt_n_cases < 0){
        updateNumericInput(session, "p_success_trt_n_cases",
                           "Treatment arm number of events:", 
                           value = 5,
                           min = 0, 
                           max = my_p_success_trt_samp_size(), 
                           step = 1)
      }
    })
    
    
    
    
    
    
    observeEvent(input$p_success_HR_estimate, {
      if(is.na(input$p_success_HR_estimate)){
        updateNumericInput(session, "p_success_HR_estimate",
                           "Hazard ratio estimate:",
                           value = 0.75,
                           min = 0, 
                           max = Inf, 
                           step = .05)
      } else if(is.null(input$p_success_HR_estimate)){
        updateNumericInput(session, "p_success_HR_estimate",
                           "Hazard ratio estimate:",
                           value = 0.75,
                           min = 0, 
                           max = Inf, 
                           step = .05)
      } else if(!is.numeric(input$p_success_HR_estimate)){
        updateNumericInput(session, "p_success_HR_estimate",
                           "Hazard ratio estimate:",
                           value = 0.75,
                           min = 0, 
                           max = Inf, 
                           step = .05)
      } else if(input$p_success_HR_estimate < 0){
        updateNumericInput(session, "p_success_HR_estimate",
                           "Hazard ratio estimate:",
                           value = 0.75,
                           min = 0, 
                           max = Inf, 
                           step = .05)
      }
    })
    
    
    
    
    
    
    my_p_success_trt_n_cases = reactive({
      input$p_success_trt_n_cases
    })
    
    
    
    
    
    
    output$ctrl_yBar_or_n_cases = renderUI({
      if(my_p_success_response() == "Numeric"){
        uiOutput("P_success_ctrl_mean")
      } else if(my_p_success_response() == "Dichotomous"){
        uiOutput("P_success_ctrl_n_cases")
      } else if(my_p_success_response() == "Time-to-event"){
        uiOutput("P_success_survival_n_events")
      }
    })
    
    
    
    
    
    output$ctrl_yBar_or_n_cases_tip = renderUI({
      txt = ifelse(my_p_success_response() == "Numeric",
                   "Insert the mean patient response for the control arm.",
                   ifelse(my_p_success_response() == "Dichotomous",
                          "Insert the number of events observed in the control arm.",
                          "Insert the number of non-censored events observed.")
      )
      
      tipify(
        uiOutput("ctrl_yBar_or_n_cases"),
        title = txt,
        options = list(container = "body")
      )
    })
    
    
    
    
    
    
    output$trt_yBar_or_n_cases = renderUI({
      if(my_p_success_response() == "Numeric"){
        uiOutput("P_success_trt_mean")
      } else if(my_p_success_response() == "Dichotomous"){
        uiOutput("P_success_trt_n_cases")
      } else if(my_p_success_response() == "Time-to-event"){
        uiOutput("P_success_HR_estimate")
      }
    })
    
    
    
    
    
    output$trt_yBar_or_n_cases_tip = renderUI({
      txt = ifelse(my_p_success_response() == "Numeric",
                   "Insert the mean patient response for the treatment arm.",
                   "Insert the number of events observed in the treatment arm.")
      
      tipify(
        uiOutput("trt_yBar_or_n_cases"),
        title = txt,
        options = list(container = "body")
      )
    })
    
    
    
    
    
    
    my_p_success_fav_string = reactive({
      if(my_p_success_response() %in% c("Dichotomous", "Time-to-event")){
        "Events are:"
      } else if(my_p_success_response() == "Numeric"){
        "Large outcome values are:"
      }
    })
    
    
    
    
    
    
    output$P_success_outcome_fav = renderUI({
      default = ifelse(my_p_success_response() == "Numeric",
                       "Favourable", "Unfavourable")
      
      radioButtons("p_success_out_fav",
                   my_p_success_fav_string(),
                   c("Favourable", "Unfavourable"),
                   selected = default,
                   inline = T)
    })
    
    
    
    
    
    
    
    
    output$P_success_outcome_fav_tip = renderUI({
      txt = ifelse(my_p_success_response() == "Numeric",
                   "Are large outcome values desirable?",
                   "Are events desirable?")
      
      tipify(
        uiOutput("P_success_outcome_fav"),
        title = txt,
        options = list(container = "body")
      )
    })
    
    
    
    
    
    my_p_success_is_fav = reactive({
      input$p_success_out_fav == "Favourable"
    })
    
    
    
    
    
    
    output$P_success_MID = renderUI({
      if(my_p_success_test() == "Non-inferiority"){
        numericInput("p_success_MID",
                     "Minimum important difference:",
                     value = 0,
                     min = 0, 
                     max = Inf, 
                     step = .1)
      }
    })
    
    
    
    
    
    
    
    observeEvent(input$p_success_MID, {
      if(is.na(input$p_success_MID)){
        updateNumericInput(session, "p_success_MID",
                           "Minimum important difference:", 
                           value = 0,
                           min = 0, 
                           max = Inf, 
                           step = .1)
      } else if(is.null(input$p_success_MID)){
        updateNumericInput(session, "p_success_MID",
                           "Minimum important difference:", 
                           value = 0,
                           min = 0, 
                           max = Inf, 
                           step = .1)
      } else if(!is.numeric(input$p_success_MID)){
        updateNumericInput(session, "p_success_MID",
                           "Minimum important difference:", 
                           value = 0,
                           min = 0, 
                           max = Inf, 
                           step = .1)
      } else if(input$p_success_MID < 0){
        updateNumericInput(session, "p_success_MID",
                           "Minimum important difference:", 
                           value = 0,
                           min = 0, 
                           max = Inf, 
                           step = .1)
      }
    })
    
    
    
    
    
    
    my_p_success_MID = reactive({
      if(my_p_success_test() == "Non-inferiority"){
        my_p_success_MID = input$p_success_MID
      } else{
        my_p_success_MID = 0
      }
    })
    
    
    
    
    
    
    output$P_success_target = renderUI({
      numericInput("p_success_target",
                   "Target Bayesian predictive power:",
                   value = 0.8,
                   min = 0.1, 
                   max = .99, 
                   step = .01)
    })
    
    
    
    
    
    
    observeEvent(input$p_success_target, {
      if(is.na(input$p_success_target)){
        updateNumericInput(session, "p_success_target",
                           "Target Bayesian predictive power:", 
                           value = 0.8,
                           min = 0.1, 
                           max = .99, 
                           step = .01)
      } else if(is.null(input$p_success_target)){
        updateNumericInput(session, "p_success_target",
                           "Target Bayesian predictive power:", 
                           value = 0.8,
                           min = 0.1, 
                           max = .99, 
                           step = .01)
      } else if(!is.numeric(input$p_success_target)){
        updateNumericInput(session, "p_success_target",
                           "Target Bayesian predictive power:", 
                           value = 0.8,
                           min = 0.1, 
                           max = .99, 
                           step = .01)
      } else if(input$p_success_target < .1){
        updateNumericInput(session, "p_success_target",
                           "Target Bayesian predictive power:", 
                           value = 0.1,
                           min = 0.1, 
                           max = .99, 
                           step = .01)
      } else if(input$p_success_target > .99){
        updateNumericInput(session, "p_success_target",
                           "Target Bayesian predictive power:", 
                           value = 0.99,
                           min = 0.1, 
                           max = .99, 
                           step = .01)
      }
    })
    
    
    
    
    
    
    my_p_success_target = reactive({
      input$p_success_target
    })
    
    
    
    
    
    
    
    output$P_success_alpha = renderUI({
      numericInput("p_success_alpha",
                   "Type I error rate:",
                   value = 0.025,
                   min = 0.001, 
                   max = .5, 
                   step = .01)
    })
    
    
    
    
    
    
    
    observeEvent(input$p_success_alpha, {
      if(is.na(input$p_success_alpha)){
        updateNumericInput(session, "p_success_alpha",
                           "Type I error rate:", 
                           value = 0.025,
                           min = 0.001, 
                           max = .5, 
                           step = .01)
      } else if(is.null(input$p_success_alpha)){
        updateNumericInput(session, "p_success_alpha",
                           "Type I error rate:", 
                           value = 0.025,
                           min = 0.001, 
                           max = .5, 
                           step = .01)
      } else if(!is.numeric(input$p_success_alpha)){
        updateNumericInput(session, "p_success_alpha",
                           "Type I error rate:", 
                           value = 0.025,
                           min = 0.001, 
                           max = .5, 
                           step = .01)
      } else if(input$p_success_alpha < .001){
        updateNumericInput(session, "p_success_alpha",
                           "Type I error rate:", 
                           value = 0.001,
                           min = 0.001, 
                           max = .5, 
                           step = .01)
      } else if(input$p_success_alpha > .5){
        updateNumericInput(session, "p_success_alpha",
                           "Type I error rate:", 
                           value = 0.5,
                           min = 0.001, 
                           max = .5, 
                           step = .01)
      }
    })
    
    
    
    
    
    
    
    my_p_success_alpha = reactive({
      input$p_success_alpha
    })
    
    
    
    
    
    
    my_p_success_trt_prop = reactive({
      my_p_success_trt_n_cases()/my_p_success_trt_samp_size()
    })
    
    
    
    
    
    my_p_success_ctrl_prop = reactive({
      my_p_success_ctrl_n_cases()/my_p_success_ctrl_samp_size()
    })
    
    
    
    
    
    my_p_success_is_viable = reactive({
      (!is.null(my_p_success_response())) &&
        (!is.na(my_p_success_response())) &&
        (!is.null(my_p_success_is_fav())) &&
        (!is.na(my_p_success_is_fav())) &&
        (!is.null(my_p_success_MID())) &&
        (!is.na(my_p_success_MID())) &&
        
        ((my_p_success_response() == "Numeric") &&
           (!is.null(my_p_success_ctrl_mean())) &&
           (!is.na(my_p_success_ctrl_mean())) &&
           (!is.null(my_p_success_trt_mean())) &&
           (!is.na(my_p_success_trt_mean())) &&
           (
             (!my_p_success_is_fav() &&
                (my_p_success_ctrl_mean() - my_p_success_trt_mean() + my_p_success_MID() > 0)) |
               (my_p_success_is_fav() &&
                  (my_p_success_ctrl_mean() - my_p_success_trt_mean() + my_p_success_MID() < 0)) 
           )) ||
        ((my_p_success_response() == "Dichotomous") &&
           (!is.null(my_p_success_ctrl_prop())) &&
           (!is.na(my_p_success_ctrl_prop())) &&
           (!is.null(my_p_success_trt_prop())) &&
           (!is.na(my_p_success_trt_prop())) &&
           (
             (!my_p_success_is_fav() &&
                (my_p_success_ctrl_prop() - my_p_success_trt_prop() + my_p_success_MID() > 0)) |
               (my_p_success_is_fav() &&
                  (my_p_success_ctrl_prop() - my_p_success_trt_prop() + my_p_success_MID() < 0)) 
           )) ||
        ((my_p_success_response() == "Time-to-event") &&
           (!is.null(my_p_success_survival_n_events())) &&
           (!is.na(my_p_success_survival_n_events())) &&
           (!is.null(my_p_success_HR_estimate())) &&
           (!is.na(my_p_success_HR_estimate())) &&
           (
             (!my_p_success_is_fav() &&
                (my_p_success_HR_estimate() + my_p_success_MID() > 0)) |
               (my_p_success_is_fav() &&
                  (my_p_success_HR_estimate() + my_p_success_MID() < 0)) 
           ))
    })
    
    
    
    
    
    
    output$p_success_eval_button = renderUI({
      actionButton(
        inputId = "p_success_eval", 
        label = "Calculate Sample Size",
        width = "100%",
        class = "butt1"
      )
    })
    
    
    
    
    
    
    
    p_success_min_samp_size_plot = eventReactive(input$p_success_eval,
                                                 {
                                                   if(my_p_success_response() == "Numeric"){
                                                     if(my_p_success_trial_type() == "Within-trial"){
                                                       min_samp_size_p_success_seamless_plot_continuous(my_p_success_MID(), 
                                                                                                        my_p_success_trt_sd(), 
                                                                                                        my_p_success_ctrl_sd(),  
                                                                                                        my_p_success_alpha(), 
                                                                                                        my_p_success_trt_samp_size(),
                                                                                                        my_p_success_ctrl_samp_size(),
                                                                                                        my_p_success_trt_mean(),
                                                                                                        my_p_success_ctrl_mean(),
                                                                                                        my_p_success_target(),
                                                                                                        my_p_success_is_fav())
                                                     } else{
                                                       min_samp_size_p_success_plot_continuous(my_p_success_MID(), 
                                                                                               my_p_success_trt_sd(), 
                                                                                               my_p_success_ctrl_sd(),  
                                                                                               my_p_success_alpha(), 
                                                                                               my_p_success_trt_samp_size(),
                                                                                               my_p_success_ctrl_samp_size(),
                                                                                               my_p_success_trt_mean(),
                                                                                               my_p_success_ctrl_mean(),
                                                                                               my_p_success_target(),
                                                                                               my_p_success_is_fav())
                                                     }
                                                   } else if(my_p_success_response() == "Dichotomous"){
                                                     if(my_p_success_trial_type() == "Within-trial"){
                                                       min_samp_size_p_success_seamless_plot_binomial(my_p_success_ctrl_n_cases(),
                                                                                                      my_p_success_trt_n_cases(),
                                                                                                      my_p_success_ctrl_samp_size(),
                                                                                                      my_p_success_trt_samp_size(),
                                                                                                      my_p_success_alpha(), 
                                                                                                      my_p_success_MID(), 
                                                                                                      my_p_success_target(),
                                                                                                      my_p_success_is_fav())
                                                     } else{
                                                       min_samp_size_p_success_plot_binomial(my_p_success_ctrl_n_cases(),
                                                                                             my_p_success_trt_n_cases(),
                                                                                             my_p_success_ctrl_samp_size(),
                                                                                             my_p_success_trt_samp_size(),
                                                                                             my_p_success_alpha(), 
                                                                                             my_p_success_MID(), 
                                                                                             my_p_success_target(),
                                                                                             my_p_success_is_fav())
                                                     }
                                                   } else if(my_p_success_response() == "Time-to-event"){
                                                     if(my_p_success_trial_type() == "Within-trial"){
                                                       min_samp_size_p_success_plot_survival_seamless(my_p_success_trt_samp_size(), 
                                                                                                      my_p_success_ctrl_samp_size(), 
                                                                                                      my_p_success_survival_n_events(), 
                                                                                                      my_p_success_HR_estimate(),
                                                                                                      my_p_success_MID(), 
                                                                                                      my_p_success_alpha(),
                                                                                                      my_p_success_is_fav(), 
                                                                                                      my_p_success_target())
                                                     } else{
                                                       min_samp_size_p_success_plot_survival(my_p_success_trt_samp_size(), 
                                                                                             my_p_success_ctrl_samp_size(), 
                                                                                             my_p_success_survival_n_events(), 
                                                                                             my_p_success_HR_estimate(),
                                                                                             my_p_success_MID(), 
                                                                                             my_p_success_alpha(),
                                                                                             my_p_success_is_fav(), 
                                                                                             my_p_success_target())
                                                     }
                                                   }
                                                 })
    
    
    
    
    
    output$p_success_plot = renderPlot({
      p_success_min_samp_size_plot()
    })
    
    
    
    
    
    
    my_p_success_calc = reactive({
      if(my_p_success_response() == "Numeric"){
        if(my_p_success_trial_type() == "Within-trial"){
          Non_inf_prob_continuous_seamless(my_p_success_MID(), 
                                           my_p_success_trt_sd(), 
                                           my_p_success_ctrl_sd(),  
                                           my_p_success_alpha(), 
                                           my_p_success_trt_samp_size(),
                                           my_p_success_ctrl_samp_size(),
                                           my_p_success_future_trt_samp_size(),
                                           my_p_success_future_ctrl_samp_size(),
                                           my_p_success_trt_mean(),
                                           my_p_success_ctrl_mean(),
                                           my_p_success_is_fav())
        } else{
          Non_inf_prob_continuous(my_p_success_MID(), 
                                  my_p_success_trt_sd(), 
                                  my_p_success_ctrl_sd(),  
                                  my_p_success_alpha(), 
                                  my_p_success_trt_samp_size(),
                                  my_p_success_ctrl_samp_size(),
                                  my_p_success_future_trt_samp_size(),
                                  my_p_success_future_ctrl_samp_size(),
                                  my_p_success_trt_mean(),
                                  my_p_success_ctrl_mean(),
                                  my_p_success_is_fav())
        }
      } else if(my_p_success_response() == "Dichotomous"){
        if(my_p_success_trial_type() == "Within-trial"){
          Non_inf_prob_of_success_binom_approx_seamless(my_p_success_future_ctrl_samp_size(),
                                                        my_p_success_future_trt_samp_size(),
                                                        my_p_success_ctrl_n_cases(),
                                                        my_p_success_trt_n_cases(),
                                                        my_p_success_ctrl_samp_size(),
                                                        my_p_success_trt_samp_size(),
                                                        my_p_success_alpha(), 
                                                        my_p_success_MID(), 
                                                        my_p_success_is_fav())
        } else{
          if(my_p_success_future_ctrl_samp_size() < 100 |
             my_p_success_future_ctrl_samp_size() < 100){
            Non_inf_prob_of_success_binom_sim(N = 1e5,
                                              my_p_success_future_ctrl_samp_size(),
                                              my_p_success_future_trt_samp_size(),
                                              my_p_success_ctrl_n_cases(),
                                              my_p_success_trt_n_cases(),
                                              my_p_success_ctrl_samp_size(),
                                              my_p_success_trt_samp_size(),
                                              my_p_success_alpha(), 
                                              my_p_success_MID(), 
                                              my_p_success_is_fav())
          } else{
            Non_inf_prob_of_success_binom_approx(my_p_success_future_ctrl_samp_size(),
                                                 my_p_success_future_trt_samp_size(),
                                                 my_p_success_ctrl_n_cases(),
                                                 my_p_success_trt_n_cases(),
                                                 my_p_success_ctrl_samp_size(),
                                                 my_p_success_trt_samp_size(),
                                                 my_p_success_alpha(), 
                                                 my_p_success_MID(), 
                                                 my_p_success_is_fav())
          }
        }
      } else if(my_p_success_response() == "Time-to-event"){
        if(my_p_success_trial_type() == "Within-trial"){
          Non_inf_prob_of_success_survival_seamless(my_p_success_ctrl_samp_size(), 
                                                    my_p_success_trt_samp_size(), 
                                                    my_p_success_survival_n_events(), 
                                                    my_p_success_survival_future_ss(),
                                                    my_p_success_HR_estimate(), 
                                                    my_p_success_MID(), 
                                                    my_p_success_alpha(),
                                                    my_p_success_is_fav())
        } else{
          Non_inf_prob_of_success_survival(my_p_success_ctrl_samp_size(),
                                           my_p_success_trt_samp_size(), 
                                           my_p_success_survival_n_events(), 
                                           my_p_success_survival_future_ss(),
                                           my_p_success_HR_estimate(), 
                                           my_p_success_MID(), 
                                           my_p_success_alpha(),
                                           my_p_success_is_fav())
        }
      }
    })
    
    
    
    
    
    
    
    output$PPOS_text = renderText({
      str = paste0("The Bayesian predictive power is ", 
                   sprintf("%.2f", 100*my_p_success_calc()), "%")
      
      HTML(paste0("<font size='4'><b>", str, "</b></font>"))
    })
    
    
    
    
    
    output$PPOS_vbox = renderValueBox({
      valueBox(
        value = tags$p(paste0("The Bayesian predictive power is ",
                              sprintf("%.1f", 100*my_p_success_calc()), "%"), 
                       style = "font-size: 75%;text-align: center;"),
        subtitle = NULL,
        color = "green"
      )
    })
    
    
    #**********************************************************************************  
    #**********************************************************************************
    
    
    
    
    
    
    #**********************************************************************************  
    #**********************************************************************************
    #*********                        PPOS Planning Tab                       *********
    #**********************************************************************************  
    #**********************************************************************************  
    output$ppos_planning_response_select = renderUI({
      radioButtons("ppos_planning_response_type",
                   "Select outcome type:",
                   c("Numeric", "Dichotomous", "Time-to-event"),
                   inline = T
      )
    })
    
    
    
    
    
    my_ppos_planning_response = reactive({
      input$ppos_planning_response_type
    })
    
    
    
    
    
    my_ppos_planning_fav_string = reactive({
      if(my_ppos_planning_response() != "Numeric"){
        "Events are:"
      } else{
        "Large outcome values are:"
      }
    })
    
    
    
    
    output$ppos_planning_outcome_fav = renderUI({
      default = ifelse(my_ppos_planning_response() == "Numeric",
                       "Favourable", "Unfavourable")
      
      radioButtons("ppos_planning_out_fav",
                   my_ppos_planning_fav_string(),
                   c("Favourable", "Unfavourable"),
                   selected = default,
                   inline = T
      )
    })
    
    
    
    
    
    
    output$ppos_planning_outcome_fav_tip = renderUI({
      txt = ifelse(my_ppos_planning_response() == "Numeric",
                   "Are large outcome values desirable?",
                   "Are events desirable?")
      
      tipify(
        uiOutput("ppos_planning_outcome_fav"),
        title = txt,
        options = list(container = "body")
      )
    })
    
    
    
    
    
    
    my_ppos_planning_is_fav = reactive({
      input$ppos_planning_out_fav == "Favourable"
    })
    
    
    
    
    
    
    
    output$ppos_planning_n_inputs = renderUI({
      sliderInput(inputId = "ppos_planning_n_inputs", 
                  label = "Number of active arms:",
                  value=4, min=2, max=9)
    }) 
    
    
    
    
    
    
    
    my_ppos_planning_n_arms = reactive({
      input$ppos_planning_n_inputs
    })
    
    
    
    
    
    
    output$ppos_planning_test_type = renderUI({
      radioButtons("ppos_planning_test_type",
                   "Testing for:",
                   c("Superiority", "Non-inferiority"),
                   inline = T
      )
    })
    
    
    
    
    
    
    my_ppos_planning_test_type = reactive({
      input$ppos_planning_test_type
    })
    
    
    
    
    
    
    output$ppos_planning_baseline_mean = renderUI({
      if(my_ppos_planning_response() == "Numeric"){
        numericInput("ppos_planning_ctrl_mean",
                     "Control arm mean response:",
                     value = 1,
                     min = -Inf, 
                     max = Inf, 
                     step = 1)
      } else if(my_ppos_planning_response() == "Dichotomous"){
        numericInput("ppos_planning_CER",
                     "Control event rate:",
                     value = 0.125,
                     min = 0, 
                     max = 1, 
                     step = .05)
      } else{
        return()
      }
    })
    
    
    
    
    
    output$ppos_planning_baseline_mean_tip = renderUI({
      txt = ifelse(my_ppos_planning_response() == "Numeric",
                   "Insert the assumed mean response for the control arm.",
                   paste0("Insert the assumed control event rate (CER). ",
                          "This is the proportion of events you expect ", 
                          "to observe among patients enrolled to the control arm.")
      )
      
      tipify(
        uiOutput("ppos_planning_baseline_mean"),
        title = txt,
        options = list(container = "body")
      )
    }) 
    
    
    
    
    
    my_ppos_planning_ctrl_mean = reactive({
      input$ppos_planning_ctrl_mean
    })
    
    
    
    
    
    my_ppos_planning_CER = reactive({
      input$ppos_planning_CER
    })
    
    
    
    
    
    
    
    observeEvent(input$ppos_planning_ctrl_mean, {
      if(is.na(input$ppos_planning_ctrl_mean)){
        updateNumericInput(session, "ppos_planning_ctrl_mean",
                           "Control arm mean response:",
                           value = 1,
                           min = -Inf, 
                           max = Inf, 
                           step = 1)
      } else if(is.null(input$ppos_planning_ctrl_mean)){
        updateNumericInput(session, "ppos_planning_ctrl_mean",
                           "Control arm mean response:",
                           value = 1,
                           min = -Inf, 
                           max = Inf, 
                           step = 1)
      } else if(!is.numeric(input$ppos_planning_ctrl_mean)){
        updateNumericInput(session, "ppos_planning_ctrl_mean",
                           "Control arm mean response:",
                           value = 1,
                           min = -Inf, 
                           max = Inf, 
                           step = 1)
      } 
    })
    
    
    
    
    
    
    observeEvent(input$ppos_planning_CER, {
      if(is.na(input$ppos_planning_CER)){
        updateNumericInput(session, "ppos_planning_CER",
                           "Control event rate:",
                           value = 0.125,
                           min = 0, 
                           max = 1, 
                           step = .05)
      } else if(is.null(input$ppos_planning_CER)){
        updateNumericInput(session, "ppos_planning_CER",
                           "Control event rate:",
                           value = 0.125,
                           min = 0, 
                           max = 1, 
                           step = .05)
      } else if(!is.numeric(input$ppos_planning_CER)){
        updateNumericInput(session, "ppos_planning_CER",
                           "Control event rate:",
                           value = 0.125,
                           min = 0, 
                           max = 1, 
                           step = .05)
      } else if(input$ppos_planning_CER < 0){
        updateNumericInput(session, "ppos_planning_CER",
                           "Control event rate:",
                           value = 0.125,
                           min = 0, 
                           max = 1, 
                           step = .05)
      } else if(input$ppos_planning_CER > 1){
        updateNumericInput(session, "ppos_planning_CER",
                           "Control event rate:",
                           value = 0.125,
                           min = 0, 
                           max = 1, 
                           step = .05)
      }
    })
    
    
    
    
    
    
    
    
    output$ppos_planning_ctrl_sd = renderUI({
      if(my_ppos_planning_response() == "Numeric"){
        numericInput("ppos_planning_ctrl_sd",
                     "Control arm response std. dev.:",
                     value = 1.2,
                     min = 1e-4, 
                     max = Inf, 
                     step = .1)
      }
    })
    
    
    
    
    
    
    my_ppos_planning_ctrl_sd = reactive({
      input$ppos_planning_ctrl_sd
    })
    
    
    
    
    
    
    observeEvent(input$ppos_planning_ctrl_sd, {
      if(is.na(input$ppos_planning_ctrl_sd)){
        updateNumericInput(session, "ppos_planning_ctrl_sd",
                           "Control arm response std. dev.:",
                           value = 1.2,
                           min = 1e-4, 
                           max = Inf, 
                           step = .1)
      } else if(is.null(input$ppos_planning_ctrl_sd)){
        updateNumericInput(session, "ppos_planning_ctrl_sd",
                           "Control arm response std. dev.:",
                           value = 1.2,
                           min = 1e-4, 
                           max = Inf, 
                           step = .1)
      } else if(!is.numeric(input$ppos_planning_ctrl_sd)){
        updateNumericInput(session, "ppos_planning_ctrl_sd",
                           "Control arm response std. dev.:",
                           value = 1.2,
                           min = 1e-4, 
                           max = Inf, 
                           step = .1)
      } else if(input$ppos_planning_ctrl_sd <= 0){
        updateNumericInput(session, "ppos_planning_ctrl_sd",
                           "Control arm response std. dev.:",
                           value = 1.2,
                           min = 1e-4, 
                           max = Inf, 
                           step = .1)
      } 
    })
    
    
    
    
    
    
    
    output$ppos_planning_alpha = renderUI({
      numericInput("ppos_planning_alpha",
                   "Type I error rate:",
                   value = 0.025,
                   min = 0.001, 
                   max = .5, 
                   step = .01)
    })
    
    
    
    
    
    
    
    observeEvent(input$ppos_planning_alpha, {
      if(is.na(input$ppos_planning_alpha)){
        updateNumericInput(session, "ppos_planning_alpha",
                           "Type I error rate:", 
                           value = 0.025,
                           min = 0.001, 
                           max = .5, 
                           step = .01)
      } else if(is.null(input$ppos_planning_alpha)){
        updateNumericInput(session, "ppos_planning_alpha",
                           "Type I error rate:", 
                           value = 0.025,
                           min = 0.001, 
                           max = .5, 
                           step = .01)
      } else if(!is.numeric(input$ppos_planning_alpha)){
        updateNumericInput(session, "ppos_planning_alpha",
                           "Type I error rate:", 
                           value = 0.025,
                           min = 0.001, 
                           max = .5, 
                           step = .01)
      } else if(input$ppos_planning_alpha < .001){
        updateNumericInput(session, "ppos_planning_alpha",
                           "Type I error rate:", 
                           value = 0.001,
                           min = 0.001, 
                           max = .5, 
                           step = .01)
      } else if(input$ppos_planning_alpha > .5){
        updateNumericInput(session, "ppos_planning_alpha",
                           "Type I error rate:", 
                           value = 0.5,
                           min = 0.001, 
                           max = .5, 
                           step = .01)
      }
    })
    
    
    
    
    
    
    
    my_ppos_planning_alpha = reactive({
      input$ppos_planning_alpha
    })
    
    
    
    
    
    
    output$ppos_planning_MID = renderUI({
      if(my_ppos_planning_test_type() != "Superiority"){
        numericInput("ppos_planning_MID",
                     "Minimum important difference:",
                     value = 0,
                     min = 0, 
                     max = Inf, 
                     step = .1)
      }
    })
    
    
    
    
    
    
    
    my_ppos_planning_MID = reactive({
      if(my_ppos_planning_test_type() == "Non-inferiority"){
        input$ppos_planning_MID
      } else{
        0
      }
    })
    
    
    
    
    
    
    observeEvent(input$ppos_planning_MID, {
      if(is.na(input$ppos_planning_MID)){
        updateNumericInput(session, "ppos_planning_MID",
                           "Minimum important difference:",
                           value = 0,
                           min = 0, 
                           max = Inf, 
                           step = .1)
      } else if(is.null(input$ppos_planning_MID)){
        updateNumericInput(session, "ppos_planning_MID",
                           "Minimum important difference:",
                           value = 0,
                           min = 0, 
                           max = Inf, 
                           step = .1)
      } else if(!is.numeric(input$ppos_planning_MID)){
        updateNumericInput(session, "ppos_planning_MID",
                           "Minimum important difference:",
                           value = 0,
                           min = 0, 
                           max = Inf, 
                           step = .1)
      } else if(input$ppos_planning_MID <= 0){
        updateNumericInput(session, "ppos_planning_MID",
                           "Minimum important difference:",
                           value = 0,
                           min = 0, 
                           max = Inf, 
                           step = .1)
      } 
    })
    
    
    
    
    
    
    output$ppos_planning_samp_sizes = renderUI({
      lapply(1:my_ppos_planning_n_arms(), 
             function(i){
               list(numericInput(paste0("ppos_planning_samp_size", i), 
                                 label = paste0("Arm ", i,":"), 
                                 value = 250, 
                                 min = 0, max = Inf, step = 1)
               )
               
             }) #end of lapply    
      
    })
    
    
    
    
    
    
    
    my_ppos_planning_samp_sizes = reactive({
      sapply(1:my_ppos_planning_n_arms(),
             function(i){
               as.numeric(input[[paste0("ppos_planning_samp_size", i)]])
             }
      )
    })
    
    
    
    
    
    
    my_ppos_planning_ss_test = reactive({
      sapply(1:my_ppos_planning_n_arms(),
             function(i){
               x = my_ppos_planning_samp_sizes()[i]
               is.null(x)|| is.na(x) || !is.numeric(x) || x <= 0
             })
    })
    
    
    
    
    
    
    output$ppos_planning_future_samp_sizes = renderUI({
      lapply(1:my_ppos_planning_n_arms(), 
             function(i){
               list(numericInput(paste0("ppos_planning_future_samp_size", i), 
                                 label = ifelse(my_ppos_planning_response() == "Time-to-event",
                                                paste0("Arm ", i," + Control:"),
                                                paste0("Arm ", i,":")), 
                                 value = ifelse(my_ppos_planning_response() == "Time-to-event", 1500, 750), 
                                 min = 0, max = Inf, step = 1)
               )
               
             }) #end of lapply    
      
    })
    
    
    
    
    
    
    output$ppos_planning_future_samp_sizes_tip = renderUI({
      txt = ifelse(my_p_success_response() != "Time-to-event",
                   paste0("Insert the number of patients planned to be enrolled ", 
                          "to each of the treatment arms in your follow-up trial."),
                   "Insert the overall number of patients yet to be enrolled for each of the control comparisons."
                   )

      
      tipify(
        uiOutput("ppos_planning_future_samp_sizes"),
        title = txt,
        options = list(container = "body")
      )
    })
    
    
    
    
    
    my_ppos_planning_future_samp_sizes = reactive({
      sapply(1:my_ppos_planning_n_arms(),
             function(i){
               as.numeric(input[[paste0("ppos_planning_future_samp_size", i)]])
             }
      )
    })
    
    
    
    
    
    
    my_ppos_planning_future_ss_test = reactive({
      sapply(1:my_ppos_planning_n_arms(),
             function(i){
               x = my_ppos_planning_future_samp_sizes()[i]
               is.null(x)|| is.na(x) || !is.numeric(x) || x < 0
             })
    })
    
    
    
    
    
    
    output$ppos_planning_means = renderUI({
      lapply(1:my_ppos_planning_n_arms(), 
             function(i){
               list(numericInput(paste0("ppos_planning_mean", i), 
                                 label = paste0("Arm ", i,":"), 
                                 value = 1 + .075*(i-1), 
                                 min = -Inf, max = Inf, step = 1)
               )
               
             }) #end of lapply    
      
    })
    
    
    
    
    
    
    
    my_ppos_planning_means = reactive({
      sapply(1:my_ppos_planning_n_arms(), 
             function(i) {
               as.numeric(input[[paste0("ppos_planning_mean", i)]])
             }
      )
    })
    
    
    
    
    
    
    my_ppos_planning_means_test = reactive({
      sapply(1:my_ppos_planning_n_arms(),
             function(i){
               x = my_ppos_planning_means()[i]
               my_ppos_planning_response() == "Numeric" && 
                 (is.null(x) || is.na(x) || !is.numeric(x) || x == '')
             })
    })
    
    
    
    
    
    
    output$ppos_planning_sds = renderUI({
      if(my_ppos_planning_response() == "Numeric"){
        lapply(1:my_ppos_planning_n_arms(), 
               function(i){
                 list(numericInput(paste0("ppos_planning_sd", i), 
                                   label = paste0("Arm ", i,":"), 
                                   value = 1.2, 
                                   min = 1e-3, 
                                   max = Inf, 
                                   step = .1)
                 )
                 
               }) #end of lapply    
      } else if(my_ppos_planning_response() == "Time-to-event"){
        lapply(1:my_ppos_planning_n_arms(),
               function(i){
                 list(numericInput(paste0("ppos_planning_event_rate", i),
                                   label = paste0("Arm ", i, " + Control:"),
                                   value = 0.75,
                                   min = 0, 
                                   max = 1, 
                                   step = .05)
                 )
               })
      }
      
    })
    
    
    
    
    
    
    output$ppos_planning_sds_tip = renderUI({
      
      txt = ifelse(my_ppos_planning_response() == "Numeric",
                    "Insert the assumed response standard deviation for each of the arms.",
                    "Insert the expected proportion of non-censored patient outcomes for each of the control comparisons."
      )
      
      tipify(
        uiOutput("ppos_planning_sds"),
        title = txt,
        options = list(container = "body")
      )
    })
    
    
    
    
    
    
    my_ppos_planning_sds = reactive({
      sapply(1:my_ppos_planning_n_arms(), 
             function(i) {
               as.numeric(input[[paste0("ppos_planning_sd", i)]])
             }
      )
    })
    
    
    
    
    
    
    my_ppos_planning_sds_test = reactive({
      sapply(1:my_ppos_planning_n_arms(),
             function(i){
               x = my_ppos_planning_sds()[i]
               my_ppos_planning_response() == "Numeric" && 
                 (is.null(x)|| is.na(x) || !is.numeric(x) || x <= 0)
             })
    })
    
    
    
    
    
    
    my_ppos_planning_event_rates = reactive({
      sapply(1:my_ppos_planning_n_arms(), 
             function(i) {
               as.numeric(input[[paste0("ppos_planning_event_rate", i)]])
             }
      )
    })
    
    
    
    
    
    
    my_ppos_planning_event_rates_test = reactive({
      sapply(1:my_ppos_planning_n_arms(),
             function(i){
               x = my_ppos_planning_event_rates()[i]
               my_ppos_planning_response() == "Time-to-event" && 
                 (is.null(x)|| is.na(x) || !is.numeric(x) || x <= 0 || x > 1)
             })
    })
    
    
    
    
    
    output$ppos_planning_RRRs = renderUI({
      lapply(1:my_ppos_planning_n_arms(), 
             function(i){
               list(numericInput(paste0("ppos_planning_RRR", i), 
                                 label = paste0("Arm ", i,":"), 
                                 value = min(.15*(i-1), .75),
                                 min = -Inf, 
                                 max = 1, 
                                 step = .1)
               )
               
             }) #end of lapply    
      
    })
    
    
    
    
    
    
    my_ppos_planning_RRRs = reactive({
      sapply(1:my_ppos_planning_n_arms(), 
             function(i) {
               as.numeric(input[[paste0("ppos_planning_RRR", i)]])
             }
      )
    })
    
    
    
    
    
    
    my_ppos_planning_RRRs_test = reactive({
      sapply(1:my_ppos_planning_n_arms(),
             function(i){
               x = my_ppos_planning_RRRs()[i]
               my_ppos_planning_response() == "Dichotomous" && 
                 (is.null(x)|| is.na(x) || !is.numeric(x) || x > 1)
             })
    })
    
    
    
    
    
    
    output$ppos_planning_HRs = renderUI({
      lapply(1:my_ppos_planning_n_arms(), 
             function(i){
               list(numericInput(paste0("ppos_planning_HR", i), 
                                 label = paste0("Arm ", i,":"), 
                                 value = max(1 - .1*(i-1), .1),
                                 min = 0, 
                                 max = Inf, 
                                 step = .1)
               )
               
             }) #end of lapply    
      
    })
    
    
    
    
    
    
    my_ppos_planning_HRs = reactive({
      sapply(1:my_ppos_planning_n_arms(), 
             function(i) {
               as.numeric(input[[paste0("ppos_planning_HR", i)]])
             }
      )
    })
    
    
    
    
    
    
    my_ppos_planning_HRs_test = reactive({
      sapply(1:my_ppos_planning_n_arms(),
             function(i){
               x = my_ppos_planning_HRs()[i]
               my_ppos_planning_response() == "Time-to-event" && 
                 (is.null(x)|| is.na(x) || !is.numeric(x) || x < 0)
             })
    })
    
    
    
    
    
    
    
    
    output$ppos_planning_means_or_RRRs = renderUI({
      if(my_ppos_planning_response() == "Numeric"){
        uiOutput("ppos_planning_means")
      } else if(my_ppos_planning_response() == "Dichotomous") {
        uiOutput("ppos_planning_RRRs")
      } else if(my_ppos_planning_response() == "Time-to-event") {
        uiOutput("ppos_planning_HRs")
      }
    })
    
    
    
    
    
    
    output$ppos_planning_means_or_RRRs_tip = renderUI({
      txt = ifelse(my_ppos_planning_response() == "Numeric",
                   "Insert the assumed mean response for each of the arms.",
                   ifelse(my_ppos_planning_response() == "Dichotomous",
                          paste0("Insert the assumed relative risk reduction (RRR) for each of the arms. ",
                                 "For example, if the CER is 20%, an event rate of 10% among patients",
                                 " enrolled to arm 1 means an RRR of 50%."),
                          paste0("Insert the assumed hazard ratio (HR) for each of the arms. ",
                                 "For example, if the HR is 0.8, the hazard among patients in arm 1",
                                 " is 20% lower than the hazard among patients in the control arm.")
                   )
      )
      tipify(
        uiOutput("ppos_planning_means_or_RRRs"),
        title = txt,
        options = list(container = "body")
      )
    }) 
    
    
    
    
    
    
    
    my_ppos_planning_problem = reactive({
      (sum(my_ppos_planning_ss_test()) > 0) ||
        (sum(my_ppos_planning_future_ss_test()) > 0) ||
        (sum(my_ppos_planning_means_test()) > 0) ||
        (sum(my_ppos_planning_sds_test()) > 0) ||
        (sum(my_ppos_planning_RRRs_test()) > 0) ||
        (sum(my_ppos_planning_HRs_test()) > 0) ||
        (sum(my_ppos_planning_event_rates_test()) > 0)
    })
    
    
    
    
    
    
    
    output$ppos_planning_alert_all = renderText({
      if(sum(as.numeric(my_ppos_planning_ss_test())) > 0) {
        createAlert(session, "ppos_planning_alert", "ppos_planning_Alert", title = "Oops",
                    content = "All sample sizes must be positive integers", append = FALSE)
      } else if(sum(as.numeric(my_ppos_planning_future_ss_test())) > 0) {
        createAlert(session, "ppos_planning_alert", "ppos_planning_Alert", title = "Oops",
                    content = "All sample sizes must be non-negative integers", append = FALSE)
      } else if(sum(as.numeric(my_ppos_planning_means_test())) > 0){
        createAlert(session, "ppos_planning_alert", "ppos_planning_Alert", title = "Oops",
                    content = "All means must be numeric", append = FALSE)
      } else if(sum(as.numeric(my_ppos_planning_sds_test())) > 0){
        createAlert(session, "ppos_planning_alert", "ppos_planning_Alert", title = "Oops",
                    content = "All standard seviations must be positive numbers", append = FALSE)
      } else if(sum(as.numeric(my_ppos_planning_RRRs_test())) > 0){
        createAlert(session, "ppos_planning_alert", "ppos_planning_Alert", title = "Oops",
                    content = "All RRRs must be numbers smaller than 1", append = FALSE)
      } else if(sum(as.numeric(my_ppos_planning_HRs_test())) > 0){
        createAlert(session, "ppos_planning_alert", "ppos_planning_Alert", title = "Oops",
                    content = "All HRs must be numbers greater than 0", append = FALSE)
      } else if(sum(my_ppos_planning_event_rates_test()) > 0){
        createAlert(session, "ppos_planning_alert", "ppos_planning_Alert", title = "Oops",
                    content = "All event rates must be numbers between zero and 1", append = FALSE)
      } else {
        closeAlert(session, "ppos_planning_Alert")
        return()
      }
    })
    
    
    
    
    
    
    
    output$ppos_planning_trial_type = renderUI({
      radioButtons("ppos_planning_trial_type",
                   "Predictive power calculation:",
                   c("Within-trial", "Cross-trial"),
                   inline = T)
    })
    
    
    
    
    
    
    my_ppos_planning_trial_type = reactive({
      input$ppos_planning_trial_type
    })
    
    
    
    
    
    
    my_ppos_planning_is_seamless = reactive({
      my_ppos_planning_trial_type() == "Within-trial"
    })
    
    
    
    
    
    
    output$ppos_planning_cutoff = renderUI({
      numericInput("ppos_planning_cutoff",
                   "Predictive power cutoff:",
                   value = .6,
                   min = 0, 
                   max = 1, 
                   step = .05)
      
    })
    
    
    
    
    
    
    my_ppos_planning_cutoff = reactive({
      input$ppos_planning_cutoff
    })
    
    
    
    
    
    
    observeEvent(input$ppos_planning_cutoff, {
      if(is.na(input$ppos_planning_cutoff)){
        updateNumericInput(session, "ppos_planning_cutoff",
                           "BPP cutoff:",
                           value = .6,
                           min = 0, 
                           max = 1, 
                           step = .05)
      } else if(is.null(input$ppos_planning_cutoff)){
        updateNumericInput(session, "ppos_planning_cutoff",
                           "BPP cutoff:",
                           value = .6,
                           min = 0, 
                           max = 1, 
                           step = .05)
      } else if(!is.numeric(input$ppos_planning_cutoff)){
        updateNumericInput(session, "ppos_planning_cutoff",
                           "BPP cutoff:",
                           value = .6,
                           min = 0, 
                           max = 1, 
                           step = .05)
      } else if(input$ppos_planning_cutoff < 0){
        updateNumericInput(session, "ppos_planning_cutoff",
                           "BPP cutoff:",
                           value = .6,
                           min = 0, 
                           max = 1, 
                           step = .05)
      } else if(input$ppos_planning_cutoff > 1){
        updateNumericInput(session, "ppos_planning_cutoff",
                           "BPP cutoff:",
                           value = .6,
                           min = 0, 
                           max = 1, 
                           step = .05)
      }  
    })
    
    
    
    
    
    
    
    output$ppos_planning_ctrl_ss = renderUI({
      numericInput("ppos_planning_ctrl_samp_size",
                   "Control arm initial sample size:",
                   value = 250,
                   min = 1, 
                   max = Inf, 
                   step = 1)
    })
    
    
    
    
    
    observeEvent(input$ppos_planning_ctrl_samp_size, {
      if(is.na(input$ppos_planning_ctrl_samp_size)){
        updateNumericInput(session, "ppos_planning_ctrl_samp_size",
                           "Control arm initial sample size:",
                           value = 250,
                           min = 1, 
                           max = Inf, 
                           step = 1)
      } else if(is.null(input$ppos_planning_ctrl_samp_size)){
        updateNumericInput(session, "ppos_planning_ctrl_samp_size",
                           "Control arm initial sample size:",
                           value = 250,
                           min = 1, 
                           max = Inf, 
                           step = 1)
      } else if(!is.numeric(input$ppos_planning_ctrl_samp_size)){
        updateNumericInput(session, "ppos_planning_ctrl_samp_size",
                           "Control arm initial sample size:",
                           value = 250,
                           min = 1, 
                           max = Inf, 
                           step = 1)
      } else if(input$ppos_planning_ctrl_samp_size < 30){
        updateNumericInput(session, "ppos_planning_ctrl_samp_size",
                           "Control arm initial sample size:",
                           value = 250,
                           min = 1, 
                           max = Inf, 
                           step = 1)
      }
    })
    
    
    
    
    
    my_ppos_planning_ctrl_samp_size = reactive({
      input$ppos_planning_ctrl_samp_size
    })
    
    
    
    
    
    
    
    output$ppos_planning_ctrl_future_ss = renderUI({
      if(my_ppos_planning_response() != "Time-to-event"){
        numericInput("ppos_planning_ctrl_future_samp_size",
                     "Control arm future sample size:",
                     value = 750,
                     min = 1, 
                     max = Inf, 
                     step = 1)
      }
    })
    
    
    
    
    
    
    observeEvent(input$ppos_planning_ctrl_future_samp_size, {
      if(is.na(input$ppos_planning_ctrl_future_samp_size)){
        updateNumericInput(session, "ppos_planning_ctrl_future__samp_size",
                           "Control arm future sample size:",
                           value = 750,
                           min = 1, 
                           max = Inf, 
                           step = 1)
      } else if(is.null(input$ppos_planning_ctrl_future_samp_size)){
        updateNumericInput(session, "ppos_planning_ctrl_future_samp_size",
                           "Control arm future sample size:",
                           value = 750,
                           min = 1, 
                           max = Inf, 
                           step = 1)
      } else if(!is.numeric(input$ppos_planning_ctrl_future_samp_size)){
        updateNumericInput(session, "ppos_planning_ctrl_future_samp_size",
                           "Control arm future sample size:",
                           value = 750,
                           min = 1, 
                           max = Inf, 
                           step = 1)
      } else if(input$ppos_planning_ctrl_future_samp_size < 30){
        updateNumericInput(session, "ppos_planning_ctrl_future_samp_size",
                           "Control arm future sample size:",
                           value = 750,
                           min = 1, 
                           max = Inf, 
                           step = 1)
      }
    })
    
    
    
    
    
    
    
    my_ppos_planning_ctrl_future_ss = reactive({
      input$ppos_planning_ctrl_future_samp_size
    })
    
    
    
    
    
    
    my_ppos_planning_probs_of_selection = reactive({
      if(my_ppos_planning_response() == "Numeric"){
        if(my_ppos_planning_is_seamless()){
          PPOS_selection_continuous_seamless(my_ppos_planning_ctrl_mean(), 
                                             my_ppos_planning_means(), 
                                             my_ppos_planning_ctrl_sd(), 
                                             my_ppos_planning_sds(),
                                             my_ppos_planning_ctrl_samp_size(), 
                                             my_ppos_planning_samp_sizes(), 
                                             my_ppos_planning_ctrl_future_ss(), 
                                             my_ppos_planning_future_samp_sizes(),
                                             my_ppos_planning_alpha(), 
                                             my_ppos_planning_MID(), 
                                             my_ppos_planning_is_fav(), 
                                             my_ppos_planning_cutoff())
        } else {
          PPOS_selection_continuous(my_ppos_planning_ctrl_mean(), 
                                    my_ppos_planning_means(), 
                                    my_ppos_planning_ctrl_sd(), 
                                    my_ppos_planning_sds(),
                                    my_ppos_planning_ctrl_samp_size(), 
                                    my_ppos_planning_samp_sizes(), 
                                    my_ppos_planning_ctrl_future_ss(), 
                                    my_ppos_planning_future_samp_sizes(),
                                    my_ppos_planning_alpha(), 
                                    my_ppos_planning_MID(), 
                                    my_ppos_planning_is_fav(), 
                                    my_ppos_planning_cutoff())
        }
      } else if(my_ppos_planning_response() == "Dichotomous"){
        if(my_ppos_planning_is_seamless()){
          PPOS_selection_binomial_seamless(my_ppos_planning_CER(), 
                                           my_ppos_planning_RRRs(), 
                                           my_ppos_planning_ctrl_samp_size(), 
                                           my_ppos_planning_samp_sizes(), 
                                           my_ppos_planning_ctrl_future_ss(), 
                                           my_ppos_planning_future_samp_sizes(),
                                           my_ppos_planning_alpha(), 
                                           my_ppos_planning_MID(), 
                                           my_ppos_planning_is_fav(), 
                                           my_ppos_planning_cutoff())
        } else {
          PPOS_selection_binomial(my_ppos_planning_CER(), 
                                  my_ppos_planning_RRRs(), 
                                  my_ppos_planning_ctrl_samp_size(), 
                                  my_ppos_planning_samp_sizes(), 
                                  my_ppos_planning_ctrl_future_ss(), 
                                  my_ppos_planning_future_samp_sizes(),
                                  my_ppos_planning_alpha(), 
                                  my_ppos_planning_MID(), 
                                  my_ppos_planning_is_fav(), 
                                  my_ppos_planning_cutoff())
        }
      } else if(my_ppos_planning_response() == "Time-to-event"){
        if(my_ppos_planning_is_seamless()){
          PPOS_selection_survival_seamless(my_ppos_planning_HRs(), 
                                           my_ppos_planning_ctrl_samp_size(), 
                                           my_ppos_planning_samp_sizes(), 
                                           my_ppos_planning_future_samp_sizes(),
                                           my_ppos_planning_event_rates(),
                                           my_ppos_planning_alpha(), 
                                           my_ppos_planning_MID(), 
                                           my_ppos_planning_is_fav(), 
                                           my_ppos_planning_cutoff())
        } else {
          PPOS_selection_survival(my_ppos_planning_HRs(), 
                                  my_ppos_planning_ctrl_samp_size(), 
                                  my_ppos_planning_samp_sizes(), 
                                  my_ppos_planning_future_samp_sizes(),
                                  my_ppos_planning_event_rates(),
                                  my_ppos_planning_alpha(), 
                                  my_ppos_planning_MID(), 
                                  my_ppos_planning_is_fav(), 
                                  my_ppos_planning_cutoff())
        }
      }
    })
    
    
    
    
    
    
    my_ppos_planning_table = reactive({
      ppos_selection_probs_table(my_ppos_planning_probs_of_selection(),
                                 my_ppos_planning_plot_data()$Assurance)
    })
    
    
    
    
    
    output$ppos_planning_table_out = function(){
      my_ppos_planning_table()
    }
    
    
    
    
    
    
    
    my_ppos_planning_plot_data = reactive({
      if(my_ppos_planning_response() == "Numeric"){
        PPOS_continuous_selection_probs_plot_data(cuts = seq(0, 1, length = 200), 
                                                  my_ppos_planning_ctrl_mean(), 
                                                  my_ppos_planning_means(), 
                                                  my_ppos_planning_ctrl_sd(), 
                                                  my_ppos_planning_sds(),
                                                  my_ppos_planning_ctrl_samp_size(), 
                                                  my_ppos_planning_samp_sizes(), 
                                                  my_ppos_planning_ctrl_future_ss(), 
                                                  my_ppos_planning_future_samp_sizes(),
                                                  my_ppos_planning_alpha(), 
                                                  my_ppos_planning_MID(), 
                                                  my_ppos_planning_is_fav(), 
                                                  my_ppos_planning_cutoff(),
                                                  my_ppos_planning_is_seamless())
      } else if(my_ppos_planning_response() == "Dichotomous"){
        PPOS_binomial_selection_probs_plot_data(cuts = seq(0, 1, length = 200), 
                                                my_ppos_planning_CER(), 
                                                my_ppos_planning_RRRs(), 
                                                my_ppos_planning_ctrl_samp_size(), 
                                                my_ppos_planning_samp_sizes(), 
                                                my_ppos_planning_ctrl_future_ss(), 
                                                my_ppos_planning_future_samp_sizes(),
                                                my_ppos_planning_alpha(), 
                                                my_ppos_planning_MID(), 
                                                my_ppos_planning_is_fav(), 
                                                my_ppos_planning_cutoff(),
                                                my_ppos_planning_is_seamless())
      } else if(my_ppos_planning_response() == "Time-to-event"){
        PPOS_survival_selection_probs_plot_data(cuts = seq(0, 1, length = 200), 
                                                my_ppos_planning_HRs(), 
                                                my_ppos_planning_ctrl_samp_size(), 
                                                my_ppos_planning_samp_sizes(), 
                                                my_ppos_planning_future_samp_sizes(),
                                                my_ppos_planning_event_rates(),
                                                my_ppos_planning_alpha(), 
                                                my_ppos_planning_MID(), 
                                                my_ppos_planning_is_fav(), 
                                                my_ppos_planning_cutoff(),
                                                my_ppos_planning_is_seamless())
      }
    })
    
    
    
    
    
    
    my_ppos_planning_plot = reactive({
      PPOS_selection_probs_plot(my_ppos_planning_plot_data())
    })
    
    
    
    
    
    
    
    output$ppos_planning_curves_or_dens_select = renderUI({
      radioButtons(inputId = "ppos_planning_curves_or_dens",
                   label = "",
                   choices = c("Selection probability curves",
                               "Selectivity plot",
                               "BPP distribution plots"),
                   inline = T)
    })
    
    
    
    
    
    
    my_ppos_power_density_plot_data = reactive({
      if(my_ppos_planning_response() == "Numeric"){
        Power_density_normal_data(cuts = seq(0, 1, length = 100), 
                                  my_ppos_planning_ctrl_mean(), 
                                  my_ppos_planning_means(), 
                                  my_ppos_planning_ctrl_sd(), 
                                  my_ppos_planning_sds(),
                                  my_ppos_planning_ctrl_samp_size(), 
                                  my_ppos_planning_samp_sizes(), 
                                  my_ppos_planning_ctrl_future_ss(), 
                                  my_ppos_planning_future_samp_sizes(),
                                  my_ppos_planning_alpha(), 
                                  my_ppos_planning_MID(), 
                                  my_ppos_planning_is_fav(), 
                                  my_ppos_planning_cutoff(),
                                  my_ppos_planning_is_seamless())
      } else if(my_ppos_planning_response() == "Dichotomous"){
        Power_density_binomial_data(cuts = seq(0, 1, length = 100), 
                                    my_ppos_planning_CER(), 
                                    my_ppos_planning_RRRs(), 
                                    my_ppos_planning_ctrl_samp_size(), 
                                    my_ppos_planning_samp_sizes(), 
                                    my_ppos_planning_ctrl_future_ss(), 
                                    my_ppos_planning_future_samp_sizes(),
                                    my_ppos_planning_alpha(), 
                                    my_ppos_planning_MID(), 
                                    my_ppos_planning_is_fav(), 
                                    my_ppos_planning_cutoff(),
                                    my_ppos_planning_is_seamless())
      } else if(my_ppos_planning_response() == "Time-to-event"){
        Power_density_survival_data(cuts = seq(0, 1, length = 100), 
                                    my_ppos_planning_HRs(), 
                                    my_ppos_planning_ctrl_samp_size(), 
                                    my_ppos_planning_samp_sizes(), 
                                    my_ppos_planning_future_samp_sizes(),
                                    my_ppos_planning_event_rates(),
                                    my_ppos_planning_alpha(), 
                                    my_ppos_planning_MID(), 
                                    my_ppos_planning_is_fav(), 
                                    my_ppos_planning_cutoff(),
                                    my_ppos_planning_is_seamless())
      }
    })
    
    
    
    
    
    
    my_ppos_power_density_plot = reactive({
      Power_density_plot(my_ppos_power_density_plot_data())
    })
    
    
    
    
    
    
    output$ppos_planning_plot = renderPlot({
      if(input$ppos_planning_curves_or_dens == "Selection probability curves"){
        my_ppos_planning_plot()
      } else if(input$ppos_planning_curves_or_dens == "BPP distribution plots"){
        my_ppos_power_density_plot()
      } else{
        my_ppos_planning_Selectivity_plot()
      }
    })
    
    
    
    
    
    
    output$ppos_planning_n_trts_to_select = renderUI({
      selectInput("ppos_planning_n_trts_to_select",
                  "Number of arms to advance:",
                  1:(my_ppos_planning_n_arms()-1),
                  min(2, my_ppos_planning_n_arms()-1),
                  width = "100%")
    })
    
    
    
    
    
    
    
    my_ppos_planning_n_trts_to_select = reactive({
      input$ppos_planning_n_trts_to_select
    })
    
    
    
    
    
    
    my_ppos_planning_Selectivity_df = reactive({
      Selectivity_df(my_ppos_planning_plot_data(),
                     my_ppos_planning_n_trts_to_select())
    })
    
    
    
    
    
    my_ppos_planning_Selectivity_plot = reactive({
      Selectivity_plot(my_ppos_planning_Selectivity_df())$p
    })
    
    
    
    
    
    output$discriminativity_vbox = renderValueBox({
      valueBox(
        value = tags$p(paste0("The recommended  cutoff is ",
                              sprintf("%.2f", 
                                      Selectivity_plot(my_ppos_planning_Selectivity_df())$opt_cut)), 
                       style = "font-size: 75%;text-align: center;"),
        subtitle = NULL,
        color = "green"
      )
    })
    
    
    
    
    
    
    output$x = renderText({
      if(my_ppos_planning_response() == "Numeric"){
        "Mean Response"
      } else if(my_ppos_planning_response() == "Dichotomous"){
        "Relative Risk Reduction"
      } else if(my_ppos_planning_response() == "Time-to-event"){
        "Hazard Ratio"
      }
    })
    
    
    
    
    
    output$xx = renderText({
      if(my_ppos_planning_response() == "Numeric"){
        "Standard Deviation"
      } else if(my_ppos_planning_response() == "Time-to-event"){
        "Event Rate"
      }
    })
    
    
    
    
    
    output$y = renderText({
      if(my_p_superiority_response() == "Numeric"){
        "Mean Response"
      } else
        "Number of Events"
    })
    
    
    
    
    output$yy = renderText({
      if(my_p_superiority_response() == "Numeric"){
        "Standard Deviation"
      } 
    })
    
    
    
    
    
    #**********************************************************************************  
    #**********************************************************************************
    
    
    
    
    
    
    
    #**********************************************************************************  
    #**********************************************************************************
    #*********                       Posterior Superiority Tab                *********
    #**********************************************************************************  
    #**********************************************************************************  
    output$P_superiority_response_select = renderUI({
      radioButtons("p_superiority_response_type",
                   "Select outcome type:",
                   c("Numeric", "Dichotomous")#, 
                   #inline=T
      )
    })
    
    
    
    
    
    
    my_p_superiority_response = reactive({
      input$p_superiority_response_type
    })
    
    
    
    
    
    my_p_superiority_fav_string = reactive({
      if(my_p_superiority_response() == "Dichotomous"){
        "Events are:"
      } else{
        "Large outcome values are:"
      }
    })
    
    
    
    
    output$P_superiority_outcome_fav = renderUI({
      default = ifelse(my_p_superiority_response() == "Numeric",
                       "Favourable", "Unfavourable")
      
      radioButtons("p_superiority_out_fav",
                   my_p_superiority_fav_string(),
                   c("Favourable", "Unfavourable"),
                   selected = default#, 
                   #inline=T
      )
    })
    
    
    
    
    
    
    
    my_p_superiority_out_fav = reactive({
      input$p_superiority_out_fav == "Favourable"
    })
    
    
    
    
    
    
    output$P_superiority_outcome_fav_tip = renderUI({
      txt = ifelse(my_p_superiority_response() == "Numeric",
                   "Are large outcome values desirable?",
                   "Are events desirable?")
      
      tipify(
        uiOutput("P_superiority_outcome_fav"),
        title = txt,
        options = list(container = "body")
      )
    })
    
    
    
    
    
    
    
    output$P_superiority_n_inputs = renderUI({
      sliderInput(inputId = "p_superiority_n_inputs", 
                  label = "Number of arms:",
                  value = 4, min = 2, max = 9,
                  ticks = T)
    }) 
    
    
    
    
    
    
    
    my_n_arms = reactive({
      input$p_superiority_n_inputs
    })
    
    
    
    
    
    
    
    output$p_superiority_samp_sizes = renderUI({
      lapply(1:my_n_arms(), 
             function(i){
               list(numericInput(paste0("p_superiority_samp_size", i), 
                                 label = paste0("Arm ", i,":"), 
                                 value = 50, 
                                 min = 0, max = Inf, step = 1)
               )
               
             }) #end of lapply    
      
    })
    
    
    
    
    
    
    
    my_p_superiority_samp_sizes = reactive({
      sapply(1:my_n_arms(),
             function(i) {
               as.numeric(input[[paste0("p_superiority_samp_size", i)]])
             }
      )
    })
    
    
    
    
    
    
    my_p_superiority_ss_test = reactive({
      sapply(1:my_n_arms(),
             function(i){
               x = my_p_superiority_samp_sizes()[i]
               is.null(x)|| is.na(x) || !is.numeric(x) || x < 0
             })
    })
    
    
    
    
    
    my_p_superiority_means_test = reactive({
      sapply(1:my_n_arms(),
             function(i){
               x = my_p_superiority_means()[i]
               my_p_superiority_response() == "Numeric" && 
                 (is.null(x) || is.na(x) || !is.numeric(x) || x == '')
             })
    })
    
    
    
    
    
    
    my_p_superiority_sds_test = reactive({
      sapply(1:my_n_arms(),
             function(i){
               x = my_p_superiority_sds()[i]
               my_p_superiority_response() == "Numeric" && 
                 (is.null(x)|| is.na(x) || !is.numeric(x) || x <= 0)
             })
    })
    
    
    
    
    
    my_p_superiority_n_cases_test = reactive({
      sapply(1:my_n_arms(),
             function(i){
               x = my_p_superiority_n_cases()[i]
               y = my_p_superiority_samp_sizes()[i]
               my_p_superiority_response() != "Numeric" && 
                 (is.null(x)|| is.na(x) || !is.numeric(x) || x < 0 || x > y)
             })
    })
    
    
    
    
    
    
    my_p_superiority_problem = reactive({
      (sum(my_p_superiority_ss_test()) > 0) ||
        (sum(my_p_superiority_means_test()) > 0) ||
        (sum(my_p_superiority_sds_test()) > 0) ||
        (sum(my_p_superiority_n_cases_test()) > 0)
    })
    
    
    
    
    
    
    output$p_superiority_alert_all = renderText({
      inputs = my_p_superiority_samp_sizes()
      
      if(sum(as.numeric(my_p_superiority_ss_test())) > 0) {
        createAlert(session, "p_superiority_alert", "SuperiorityAlert", title = "Oops",
                    content = "All sample sizes must be non-negative integers", append = FALSE)
      } else if(sum(as.numeric(my_p_superiority_means_test())) > 0){
        createAlert(session, "p_superiority_alert", "SuperiorityAlert", title = "Oops",
                    content = "All means must be numeric", append = FALSE)
      } else if(sum(as.numeric(my_p_superiority_sds_test())) > 0){
        createAlert(session, "p_superiority_alert", "SuperiorityAlert", title = "Oops",
                    content = "All standard seviations must be positive numbers", append = FALSE)
      } else if(sum(as.numeric(my_p_superiority_n_cases_test())) > 0){
        createAlert(session, "p_superiority_alert", "SuperiorityAlert", title = "Oops",
                    content = "All numbers of events must be non-negative integers and cannot exceed ?panel.grid.minorthe arm size", append = FALSE)
      }  else {
        closeAlert(session, "SuperiorityAlert")
        return()
      }
    })
    
    
    
    
    output$p_superiority_button = renderUI({
      actionButton(
        inputId = "p_superiority_eval", 
        label = "Calculate Probabilities",
        width = "100%",
        class = "butt1"
      )
    })
    
    
    
    
    
    
    
    output$p_superiority_means = renderUI({
      lapply(1:my_n_arms(), 
             function(i){
               list(numericInput(paste0("p_superiority_mean", i), 
                                 label = paste0("Arm ", i,":"), 
                                 value = .1*(i-1), 
                                 min = -Inf, max = Inf, step = 1)
               )
               
             }) #end of lapply    
      
    })
    
    
    
    
    
    
    
    my_p_superiority_means = reactive({
      sapply(1:my_n_arms(), 
             function(i) {
               as.numeric(input[[paste0("p_superiority_mean", i)]])
             }
      )
    })
    
    
    
    
    
    
    
    output$p_superiority_sds = renderUI({
      lapply(1:my_n_arms(), 
             function(i){
               list(numericInput(paste0("p_superiority_sd", i), 
                                 label = paste0("Arm ", i,":"), 
                                 value = 1, 
                                 min = 1e-3, max = Inf, step = .1)
               )
               
             }) #end of lapply    
      
    })
    
    
    
    
    
    
    
    my_p_superiority_sds = reactive({
      sapply(1:my_n_arms(), 
             function(i) {
               as.numeric(input[[paste0("p_superiority_sd", i)]])
             }
      )
    })
    
    
    
    
    
    
    output$p_superiority_n_cases = renderUI({
      lapply(1:my_n_arms(), 
             function(i){
               list(numericInput(paste0("p_superiority_n_cases", i), 
                                 label = paste0("Arm ", i,":"), 
                                 value = 6 + 2*(i-1), 
                                 min = 0, 
                                 max = as.numeric(input[[paste0("p_superiority_samp_size", i)]]), 
                                 step = 1)
               )
               
             }) #end of lapply    
      
    })
    
    
    
    
    
    
    my_p_superiority_n_cases = reactive({
      sapply(1:my_n_arms(), 
             function(i) {
               as.numeric(input[[paste0("p_superiority_n_cases", i)]])
             }
      )
    })
    
    
    
    
    
    
    output$p_superiority_means_or_cases = renderUI({
      if(my_p_superiority_response() == "Numeric"){
        uiOutput("p_superiority_means")
      } else{
        uiOutput("p_superiority_n_cases")
      }
    })
    
    
    
    
    
    
    output$p_superiority_means_or_cases_tip = renderUI({
      txt = ifelse(my_p_superiority_response() == "Numeric",
                   "Insert the mean response for each of the arms.",
                   "Insert the number of events observed for each of the arms.")
      
      tipify(
        uiOutput("p_superiority_means_or_cases"),
        title = txt,
        options = list(container = "body")
      )
    })
    
    
    
    
    
    
    
    my_p_superiority_post_dens_df = reactive({
      if(my_p_superiority_response() == "Numeric"){
        normal_posterior_density_df(my_p_superiority_samp_sizes(), 
                                    my_p_superiority_means(),
                                    my_p_superiority_sds())
      } else{
        beta_posterior_density_df(my_p_superiority_samp_sizes(), 
                                  my_p_superiority_n_cases())
      }
    })
    
    
    
    
    
    
    my_p_superiority_post_dens_plot = reactive({
      posterior_dens_plot(my_p_superiority_post_dens_df(), 
                          my_p_superiority_response())
    })
    
    
    
    
    
    
    output$p_superiority_dens_plot = renderPlot({
      my_p_superiority_post_dens_plot()
    })
    
    
    
    
    
    my_p_superiority_CrI_plot = reactive({
      if(my_p_superiority_response() == "Numeric"){
        normal_CrI_plot(my_p_superiority_samp_sizes(), 
                        my_p_superiority_means(),
                        my_p_superiority_sds())
      } else{
        beta_CrI_plot(my_p_superiority_samp_sizes(), 
                      my_p_superiority_n_cases())
      }
    })
    
    
    
    
    
    
    output$p_superiority_CrI_plot = renderPlot({
      my_p_superiority_CrI_plot()
    })
    
    
    
    
    
    
    my_p_superiority_sample = eventReactive(input$p_superiority_eval, {
      if(my_p_superiority_response() == "Numeric"){
        normal_posterior_samp(my_p_superiority_samp_sizes(), 
                              my_p_superiority_means(),
                              my_p_superiority_sds(),
                              1e5)
      } else{
        beta_posterior_samp(my_p_superiority_samp_sizes(), 
                            my_p_superiority_n_cases(),
                            1e5)
      }
    })
    
    
    
    
    
    
    my_p_superiority_table = reactive({
      prob_of_superiority_table(my_p_superiority(),
                                my_p_superiority_RAR_power())
    })
    
    
    
    
    
    
    output$p_superiority_table_out = function(){ 
      my_p_superiority_table()
    }
    
    
    
    
    
    my_p_superiority = reactive({
      posterior_superiority(my_p_superiority_sample(), 
                            my_p_superiority_out_fav())
    })
    
    
    
    
    
    
    my_p_superiority_plot = reactive({
      if(!is.na(my_p_superiority_RAR_power())){
        post_superiority_barplot_new(my_p_superiority(),
                                     my_p_superiority_RAR_power())
      } else{
        post_superiority_barplot(my_p_superiority())
      }
    })
    
    
    
    
    
    output$p_superiority_barplot = renderPlot({
      my_p_superiority_plot()
    }, height = 375)
    
    
    
    
    
    
    output$p_superiority_RAR_switch = renderUI({
      materialSwitch(inputId = "p_superiority_is_RAR", 
                     label = tags$b("Response Adaptive Randomization"), 
                     status = "success")
    })
    
    
    
    
    output$p_superiority_RAR_power = renderUI({
      shinyWidgets::sliderTextInput(inputId = "p_superiority_RAR_power", 
                                    label = "Response adaptive randomization power:",
                                    choices = c("0", "1/6", "1/3", "1/2", "2/3", "5/6", "1"),
                                    selected = "1/2",
                                    grid = T)
    })
    
    
    
    
    
    
    my_p_superiority_RAR_power = reactive({
      if(!input$p_superiority_is_RAR){
        return(NA)
      } else{
        x = input$p_superiority_RAR_power
        
        if(x == '0'){
          return(0)
        } else if(x == '1'){
          return(1)
        } else{
          vec = as.numeric(unlist(strsplit(x, "/")))
          return(vec[1]/vec[2])
        }
      }
    })
    
    
    #**********************************************************************************  
    #**********************************************************************************   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    observe({
      if(is.null(my_p_success_response())||
         my_p_success_response() != "Numeric") {
        shinyjs::hide(selector = ".rowhide1", anim = T, 
                      animType = "fade", time = .5)
      } else{
        shinyjs::show(selector = ".rowhide1", anim = T, 
                      animType = "fade", time = .5)
      }
    })
    
    
    
    
    
    observe({
      if(is.null(my_p_success_test())||
         my_p_success_test() == "Superiority") {
        shinyjs::hide(selector = ".rowhide2", anim = T, 
                      animType = "fade", time = .5)
      } else{
        shinyjs::show(selector = ".rowhide2", anim = T, 
                      animType = "fade", time = .5)
      }
    })
    
    
    
    
    
    observe({
      if(is.null(input$p_success_eval) ||
         !(input$p_success_eval)) {
        shinyjs::hide(selector = ".rowhide3", anim = T, 
                      animType = "fade", time = .5)
      } else{
        shinyjs::show(selector = ".rowhide3", anim = T, 
                      animType = "fade", time = .5)
      }
    })
    
    
    
    
    
    
    observe({
      req(input$p_success_response_type,
          input$p_success_test_type,
          input$p_success_ctrl_samp_size,
          input$p_success_trt_samp_size,
          input$p_success_ctrl_mean,
          input$p_success_trt_mean
      )
      
      if(is.null(my_p_success_is_viable()) ||
         is.na(my_p_success_is_viable()) ||
         my_p_success_is_viable() == FALSE) {
        shinyjs::hide(selector = ".rowhide4", anim = T, 
                      animType = "fade", time = .5)
      } else{
        shinyjs::show(selector = ".rowhide4", anim = T, 
                      animType = "fade", time = .5)
      }
    })
    
    
    
    
    
    observe({
      if(is.null(my_p_superiority_response())||
         my_p_superiority_response() == "Dichotomous") {
        shinyjs::hide(selector = ".rowhide5", anim = T, 
                      animType = "fade", time = .5)
      } else{
        shinyjs::show(selector = ".rowhide5", anim = T, 
                      animType = "fade", time = .5)
      }
    })
    
    
    
    
    
    observe({
      req(input$p_superiority_n_inputs, 
          input$p_superiority_response_type)
      
      if(is.null(my_p_superiority_problem()) ||
         is.na(my_p_superiority_problem()) ||
         as.numeric(my_p_superiority_problem()) > 0) {
        shinyjs::hide(selector = ".rowhide6", anim = T, 
                      animType = "fade", time = .5)
      } else{
        shinyjs::show(selector = ".rowhide6", anim = T, 
                      animType = "fade", time = .5)
      }
    })
    
    
    
    
    
    observe({
      if(is.null(input$p_superiority_eval) ||
         !input$p_superiority_eval){
        shinyjs::hide(selector = ".rowhide7", anim = T, 
                      animType = "fade", time = .5)
      } else{
        shinyjs::show(selector = ".rowhide7", anim = T, 
                      animType = "fade", time = .5)
      }
    })
    
    
    
    
    observe({
      if(is.null(my_ppos_planning_response())||
         my_ppos_planning_response() == "Dichotomous") {
        shinyjs::hide(selector = ".rowhide8", anim = T, 
                      animType = "fade", time = .5)
      } else{
        shinyjs::show(selector = ".rowhide8", anim = T, 
                      animType = "fade", time = .5)
      }
    })
    
    
    
    
    
    
    observe({
      req(input$ppos_planning_n_inputs, 
          input$ppos_planning_response_type)
      
      if(is.null(my_ppos_planning_problem()) ||
         is.na(my_ppos_planning_problem()) ||
         as.numeric(my_ppos_planning_problem()) > 0) {
        shinyjs::hide(selector = ".rowhide9", anim = T, 
                      animType = "fade", time = .5)
      } else{
        shinyjs::show(selector = ".rowhide9", anim = T, 
                      animType = "fade", time = .5)
      }
    })
    
    
    
    
    
    observe({
      if(is.null(input$p_superiority_is_RAR) ||
         is.na(input$p_superiority_is_RAR) ||
         (!input$p_superiority_is_RAR)){
        shinyjs::hide(selector = ".rowhide10", anim = T, 
                      animType = "fade", time = .5)
      } else{
        shinyjs::show(selector = ".rowhide10", anim = T, 
                      animType = "fade", time = .5)
      }
    })
    
    
    
    
  }
)





