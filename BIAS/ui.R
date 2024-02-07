ui = tagList(
  tags$head(tags$link(rel = "icon", type = "image/png", href = "flavicon.png")),
  
  tags$style("html,body{background-color: white;}
             .container{
             width: 100%;
             margin: 0 auto;
             padding: 0;
             }
             #myimg{
             width:30%;
             }
             @media screen and (min-width: 1280px){
             .container{
             width: 1280px;
             }
             }"),
  
  tags$div(class="container",
           
           dashboardPage(
             title = "BIAS: Bayesian Interim Analysis Software",
             
             header = dashboardHeader(
               # title = tags$a(href = 'https://www.mteksciences.com/',
               #                tags$img(src = "MTEK logo for apps 175 by 50 - v2.0.png",
               #                         height = "50px",
               #                         width = "175px",
               #                         align = "center"))#,
               
               title = tags$a(href = 'https://www.cytel.com/',
                              tags$img(src = "Cytel_logo_white_eps.png",
                                       height = "40px",
                                       width = "120px",
                                       align = "center"))
               
               # enable_rightsidebar = F,
               # rightSidebarIcon = "gears"
             ),
             
             
             
             dashboardSidebar(
               # tags$style(HTML("
               #                 .main-sidebar{
               #                 width: 230px;
               #                 }
               #                 ")),
               sidebarMenu(
                 menuItem("Home", tabName = "Home", icon = icon("home")),
                 menuItem("BPP Trial Design", tabName = "PPOS_planning", icon = icon("fas fa-chart-line")),
                 menuItem("BPP Calculations", tabName = "Probability_of_Success", icon = icon("fas fa-calculator")),
                 menuItem("Posterior Superiority", tabName = "Posterior_Superiority", icon = icon("fas fa-infinity"))
               )
             ),
             
             
             dashboardBody(
               tags$img(
                 src = "background.png",
                 style = 'position: absolute',
                 width = "1024px",
                 vspace = "240px"
               ),
               
               tags$head(tags$style(HTML('.info-box {min-height: 50px;} .info-box-icon {height: 50px; line-height: 50px;} .info-box-content {padding-top: 2.5px; padding-bottom: 0px;}'))),
               
               ## to not show error message in shiny
               tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
               tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
               
               
               shinyjs::useShinyjs(),
               
               
               # Also add some custom CSS to make the title background area the same
               # color as the rest of the header.
               tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #2C3E50;
                              font-weight: bold; 
                              font-size: 24px; 
                              font-family: Helvetica;
                              padding-top: .5%;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #D3D3D3;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              padding-top: .55%;
                              padding-bottom: .55%;
                              background-color: #2C3E50;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              padding-top: 5%;
                              background-color: #2C3E50;
                              font-weight: bold;
                              }


                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #D3D3D3;
                              border-left-color: #ff0000;
                              font-weight: bold;
                              }

                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #D3D3D3;
                              color: #000000;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #000000;
                              color: #FFFFFF;
                              }

                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: black;
                              }
                              
                              /* body */
                                .content-wrapper, .right-side {
                              background-color: #EBF5FB;
                              }
                              '))),
               
               
               
               tags$script(HTML('
                                $(document).ready(function() {
                                $("header").find("nav").append(\'<span class="myClass"> BIAS: Bayesian Interim Analysis Software </span>\');
                                })
                                ')),
               
               
               
               
               tags$style(HTML(
                 '.myClass { 
                 font-size: 22px;
                 line-height: 50px;
                 text-align: left;
                 font-family: Helvetica;
                 overflow: hidden;
                 color: #FFFFFF;
                                }
                 ')),
               
               
               
               #displaying tick marks on integer values only on sliderInput 
               tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
               
               
               
               tabItems(
                 
                 tabItem(tabName = "Home",
                         fluidRow(
                           shinydashboardPlus::box(
                             id = 'box0',
                             solidHeader = T,
                             collapsible = F, 
                             closable = F,
                             width = 12,
                             uiOutput("about")
                           ),
                           tags$head(tags$style('#box0 .box-header{ display: none}'))
                         )
                 ),
                 
                 
                 
                 
                 
                 
                 
                 tabItem(
                   tabName = "PPOS_planning",
                   
                   fluidRow(
                     shinydashboardPlus::box(
                       id = 'box1',
                       solidHeader = T,
                       collapsible = F, 
                       closable = F,
                       width = 6,
                       div(style="height: 175px;",
                           uiOutput("ppos_planning_response_select"),
                           br(),
                           setSliderColor(c("#2C3E50", "#2C3E50", "#2C3E50"), c(1, 2, 3)),
                           uiOutput("ppos_planning_n_inputs")
                       ),
                       
                       bsTooltip("ppos_planning_response_select", 
                                 paste0("Select your outcome type of interest. Use an appropriate ", 
                                        "transformation if your numeric outcome is unlikely to be normal."),
                                 options = list(container = "body")
                       ),
                       
                       bsTooltip("ppos_planning_n_inputs", 
                                 paste0("How many treatment arms do you plan to test against ", 
                                        "the control in your early (phase II) trial?"),
                                 options = list(container = "body"))
                     ),
                     tags$head(tags$style('#box1 .box-header{ display: none}')),
                     
                     shinydashboardPlus::box(
                       id = 'box2',
                       solidHeader = T,
                       collapsible = F, 
                       closable = F,
                       width = 6,
                       div(style="height: 175px;",
                           tags$style(HTML(".radio-inline {margin-right: 75px;}")),
                           uiOutput("ppos_planning_outcome_fav_tip"),
                           uiOutput("ppos_planning_test_type"),
                           uiOutput("ppos_planning_trial_type")
                       ),
                       
                       bsTooltip("ppos_planning_test_type", 
                                 paste0("Are you testing for superiority of the different treatments to the control ", 
                                        "or non-inferiority? Non-inferiority testing is done to show that a treatment ", 
                                        "is no worse than the control by more than a set clinically important difference."),
                                 options = list(container = "body")),
                       
                       bsTooltip("ppos_planning_trial_type", 
                                 paste0("Within trial predictive power is calculated when the follow-up ", 
                                        "(phase III) analysis will include all the ", 
                                        "data collected (phase II and III). Cross trial predictive power is calculated ", 
                                        "when the follow-up analysis ", 
                                        "will only consider future (phase III) patient outcomes."),
                                 options = list(container = "body"))
                     ),
                     tags$head(tags$style('#box2 .box-header{ display: none}'))
                     
                     
                   ),
                   
                   
                   
                   
                   
                   fluidRow(
                     
                     shinydashboardPlus::box(
                       id = 'box3',
                       solidHeader = T,
                       collapsible = F,
                       closable = F,
                       width = 4,
                       div(style="height: 150px;",
                           uiOutput("ppos_planning_ctrl_ss"),
                           uiOutput("ppos_planning_ctrl_future_ss")
                       ),
                       
                       bsTooltip("ppos_planning_ctrl_ss", 
                                 "Insert the number of patients enrolled to the control arm in your early (phase II) trial.",
                                 options = list(container = "body")),
                       
                       bsTooltip("ppos_planning_ctrl_future_ss", 
                                 "Insert the number of patients planned to be enrolled to the control arm in your follow-up trial.",
                                 options = list(container = "body"))
                     ),
                     tags$head(tags$style('#box3 .box-header{ display: none}')),
                     
                     conditionalPanel(
                       condition = "input.ppos_planning_response_type != 'Time-to-event'",
                       shinydashboardPlus::box(
                         id = 'box4',
                         solidHeader = T,
                         collapsible = F,
                         closable = F,
                         width = 4,
                         div(style="height: 150px;",
                             uiOutput("ppos_planning_baseline_mean_tip"),
                             uiOutput("ppos_planning_ctrl_sd")
                         ),
                         
                         bsTooltip("ppos_planning_ctrl_sd", 
                                   "Insert the assumed response standard deviation for the control arm.",
                                   options = list(container = "body"))
                       ),
                       tags$head(tags$style('#box4 .box-header{ display: none}'))
                     ),
                     
                     
                     shinydashboardPlus::box(
                       id = 'box5',
                       solidHeader = T,
                       collapsible = F,
                       closable = F,
                       width = 4,
                       div(style="height: 150px;",
                           uiOutput("ppos_planning_alpha"),
                           uiOutput("ppos_planning_MID")
                       ),
                       
                       bsTooltip("ppos_planning_alpha", 
                                 "The significance level at which the (one-sided) test will be conducted at the end of the follow-up trial.",
                                 options = list(container = "body")),
                       
                       bsTooltip("ppos_planning_MID", 
                                 paste0("The minimum clinically important difference (MID) between ", 
                                        "treatment and control in a non-inferiority test.",
                                        " the conclusion from rejecting the null hypothesis in this ", 
                                        "kind of test is that the treatment is worse than the control ", 
                                        "by more than the specified MID."),
                                 options = list(container = "body"))
                     ),
                     tags$head(tags$style('#box5 .box-header{ display: none}'))
                     
                   ),
                   
                   
                   fluidRow(
                     
                     shinydashboardPlus::box(
                       title = 'Initial Sample Size',
                       solidHeader = T,
                       closable = F,
                       collapsible = F,
                       width = 3,
                       uiOutput("ppos_planning_samp_sizes"),
                       
                       bsTooltip("ppos_planning_samp_sizes", 
                                 paste0("Insert the number of patients enrolled to each of ", 
                                        "the treatment arms in your early (phase II) trial."),
                                 options = list(container = "body"))
                     ),
                     
                     shinydashboardPlus::box(
                       title = 'Future Sample Size',
                       solidHeader = T,
                       closable = F,
                       collapsible = F,
                       width = 3,
                       uiOutput("ppos_planning_future_samp_sizes_tip")#,
                       
                       # bsTooltip("ppos_planning_future_samp_sizes", 
                       #           paste0("Insert the number of patients planned to be enrolled ", 
                       #                  "to each of the treatment arms in your follow-up trial."),
                       #           options = list(container = "body"))
                     ),
                     
                     
                     shinydashboardPlus::box(
                       title = textOutput('x'),
                       solidHeader = T,
                       closable = F,
                       collapsible = F,
                       width = 3,
                       uiOutput("ppos_planning_means_or_RRRs_tip")
                     ),
                     
                     conditionalPanel(
                       condition = "input.ppos_planning_response_type != 'Dichotomous'",
                       shinydashboardPlus::box(
                         title = textOutput('xx'),
                         solidHeader = T,
                         closable = F,
                         collapsible = F,
                         width = 3,
                         uiOutput("ppos_planning_sds_tip"),
                         
                         bsTooltip("ppos_planning_sds",
                                   "Insert the assumed response standard deviation for each of the arms.",
                                   options = list(container = "body"))
                       )
                     )
                     
                   ),
                   
                   
                   fluidRow(
                     textOutput("ppos_planning_alert_all"),
                     
                     bsAlert("ppos_planning_alert")
                   ),
                   
                   
                   
                   fluidRow(
                     shinydashboardPlus::box(
                       id = 'box6',
                       solidHeader = T,
                       collapsible = F,
                       closable = F,
                       width = 8,
                       div(style="height: 525px;",
                           fluidRow(
                             column(width = 6,
                                    uiOutput("ppos_planning_n_trts_to_select")
                             ),
                             column(width = 6,
                                    uiOutput("ppos_planning_cutoff")
                             )
                           ),
                           uiOutput("ppos_planning_curves_or_dens_select"),
                           tags$style(HTML(".radio-inline {margin-right: 30px;}")),
                           plotOutput("ppos_planning_plot")
                       ),
                       
                       bsTooltip("ppos_planning_curves_or_dens_select", 
                                 paste0("Selection probability curves show the probability of each arm ", 
                                        "qualifying to the follow-up trial as a function of the cutoff set. ",
                                        "Selectivity plot shows the probability of selecting the preferred ", 
                                        "subset of treatments as a function of said cutoff. ",
                                        "BPP distribution plots display the distribution of the predictive power at the ", 
                                        "end of the phase II trial."),
                                 options = list(container = "body")),
                       
                       bsTooltip("ppos_planning_cutoff", 
                                 paste0("For a treatment to qualify to the follow-up trial, its predictive power", 
                                        " at the end of the phase II trial must exceed a set cutoff (between 0 and 1)."),
                                 options = list(container = "body")),
                       
                       bsTooltip("ppos_planning_n_trts_to_select", 
                                 "Select the number of treatments you intend to advance to a follow-up trial.",
                                 options = list(container = "body"))
                     ),
                     tags$head(tags$style('#box6 .box-header{ display: none}')),
                     
                     shinydashboardPlus::box(
                       id = 'box7',
                       solidHeader = T,
                       collapsible = F,
                       closable = F,
                       width = 4,
                       div(style="height: 525px;",
                           tableOutput("ppos_planning_table_out")
                       )
                     ),
                     tags$head(tags$style('#box7 .box-header{ display: none}'))
                   ),
                   
                   fluidRow(
                     column(2),
                     valueBoxOutput("discriminativity_vbox",
                                    width = 8)
                   )
                   
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 tabItem(tabName = "Probability_of_Success",
                         
                         fluidRow(
                           
                           shinydashboardPlus::box(
                             id = 'box8',
                             solidHeader = T,
                             collapsible = F, 
                             closable = F,
                             width = 4,
                             div(style="height: 150px;",
                                 uiOutput("P_success_response_select"),
                                 uiOutput("P_success_trial_type")
                             ),
                             
                             bsTooltip("P_success_response_select", 
                                       paste0("Select your outcome type of interest. Use an appropriate ", 
                                              "transformation if your numeric outcome is unlikely to be normal."),
                                       options = list(container = "body")),
                             
                             bsTooltip("P_success_trial_type", 
                                       paste0("Within trial predictive power is calculated when the follow-up ", 
                                              "(phase III) analysis will include all the ", 
                                              "data collected (phase II and III). Cross trial predictive power is calculated ", 
                                              "when the follow-up analysis ", 
                                              "will only consider future (phase III) patient outcomes."),
                                       options = list(container = "body"))
                           ),
                           tags$head(tags$style('#box8 .box-header{ display: none}')),
                           
                           shinydashboardPlus::box(
                             id = 'box9',
                             solidHeader = T,
                             collapsible = F, 
                             closable = F,
                             width = 4,
                             div(style="height: 150px;",
                                 uiOutput("P_success_ctrl_ss"),
                                 uiOutput("P_success_trt_ss")
                             ),
                             
                             bsTooltip("P_success_ctrl_ss", 
                                       "Insert the number of control arm patient outcomes already available for analysis.",
                                       options = list(container = "body")),
                             
                             bsTooltip("P_success_trt_ss", 
                                       "Insert the number of treatment arm patient outcomes already available for analysis.",
                                       options = list(container = "body"))
                           ),
                           tags$head(tags$style('#box9 .box-header{ display: none}')),
                           
                           
                           shinydashboardPlus::box(
                             id = 'box10',
                             solidHeader = T,
                             collapsible = F, 
                             closable = F,
                             width = 4,
                             div(style="height: 150px;",
                                 uiOutput("P_success_ctrl_or_survival_future_ss_tip"),
                                 uiOutput("P_success_trt_or_survival_future_ss_tip")
                             )
                           ),
                           tags$head(tags$style('#box10 .box-header{ display: none}'))
                         ),
                         
                         
                         
                         fluidRow(
                           
                           shinydashboardPlus::box(
                             id = 'box11',
                             solidHeader = T,
                             collapsible = F, 
                             closable = F,
                             width = 4,
                             div(style="height: 150px;",
                                 uiOutput("P_success_outcome_fav_tip"),
                                 br(),
                                 uiOutput("P_success_test_type")
                             ),
                             
                             bsTooltip("P_success_test_type", 
                                       paste0("Are you testing for superiority of the treatment to the control ", 
                                              "or non-inferiority? Non-inferiority testing is done to show that ", 
                                              "a treatment is no worse than the control by more than a set clinically important difference."),
                                       options = list(container = "body"))
                           ),
                           tags$head(tags$style('#box11 .box-header{ display: none}')),
                           
                           
                           shinydashboardPlus::box(
                             id = 'box12',
                             solidHeader = T,
                             collapsible = F, 
                             closable = F,
                             width = 4,
                             div(style="height: 150px;",
                                 uiOutput("ctrl_yBar_or_n_cases_tip"),
                                 uiOutput("trt_yBar_or_n_cases_tip")
                             )
                           ),
                           tags$head(tags$style('#box12 .box-header{ display: none}')),
                           
                           
                           shinydashboardPlus::box(class = "rowhide1",
                                                   id = 'box13',
                                   solidHeader = T,
                                   collapsible = F, 
                                   closable = F,
                                   width = 4,
                                   div(style="height: 150px;",
                                       uiOutput("P_success_ctrl_sd"),
                                       uiOutput("P_success_trt_sd")
                                   ),
                                   
                                   bsTooltip("P_success_ctrl_sd", 
                                             "Insert the response standard deviation for the control arm.",
                                             options = list(container = "body")),
                                   
                                   bsTooltip("P_success_trt_sd", 
                                             "Insert the response standard deviation for the treatment arm.",
                                             options = list(container = "body"))
                           ),
                           tags$head(tags$style('#box13 .box-header{ display: none}'))
                           
                         ),
                         
                         
                         
                         fluidRow(
                           
                           shinydashboardPlus::box(
                             id = 'box14',
                             solidHeader = T,
                             collapsible = F, 
                             closable = F,
                             width = 4,
                             div(style="height: 150px;",
                                 uiOutput("P_success_alpha"),
                                 uiOutput("P_success_target")
                             ),
                             bsTooltip("P_success_alpha", 
                                       paste0("The significance level at which the (one-sided) ", 
                                              "test will be conducted at the end of the follow-up trial."),
                                       options = list(container = "body")),
                             
                             bsTooltip("P_success_target", 
                                       paste0("Specify your target Bayesian Predictive Power to allow the ", 
                                              "software to calculate the sample size required to meet it. ",
                                              "This is the probability of rejecting the null hypothesis at the specified ", 
                                              "significance level when all patient outcomes have been observed, ",
                                              "given the data collected thus far."),
                                       options = list(container = "body"))
                           ),
                           tags$head(tags$style('#box14 .box-header{ display: none}')),
                           
                           
                           
                           shinydashboardPlus::box(class = "rowhide2",
                                                   id = 'box15',
                                   solidHeader = T,
                                   collapsible = F, 
                                   closable = F,
                                   width = 4,
                                   div(style="height: 150px;",
                                       uiOutput("P_success_MID")
                                   ),
                                   
                                   bsTooltip("P_success_MID", 
                                             paste0("The minimum clinically important difference (MID) between ", 
                                                    "treatment and control in a non-inferiority test. ",
                                                    "The conclusion from rejecting the null hypothesis in this ", 
                                                    "kind of test is that the treatment is worse than the control ", 
                                                    "by more than the specified MID."),
                                             options = list(container = "body"))
                           ),
                           tags$head(tags$style('#box15 .box-header{ display: none}'))
                           
                         ),
                         
                         
                         
                         
                         
                         fluidRow(
                           column(width = 2),
                           
                           valueBoxOutput("PPOS_vbox", width = 8)
                           
                         ),
                         
                         
                         tags$head(tags$style(".butt1{background-color: #2C3E50;}
                                   .butt1{color: white;}
                                   .butt1{}
                                   .butt1{font-weight: bold;}
                                   .butt1{font-family: Helvetica}
                                   .butt1{text-align: center}")),
                         
                         
                         fluidRow(class = "rowhide4",
                                  shinydashboardPlus::box(
                                    id = 'box16',
                                    solidHeader = T,
                                    collapsible = F,
                                    closable = F,
                                    width = 12,
                                    tags$head(tags$style(".butt2{background-color: #337ab7;}
                                                .butt1{color: white;}
                                                .butt1{width: 100%;}
                                                .butt1{font-weight: bold;}")),
                                    uiOutput("p_success_eval_button"),
                                    
                                    bsTooltip("p_success_eval_button", 
                                              paste0("Calculate the future sample size required to meet your target Predictive Power. ",
                                                     "This is the probability of rejecting the null hypothesis at the specified ", 
                                                     "significance level when all patient outcomes have been observed, ",
                                                     "given the data collected thus far."),
                                              options = list(container = "body"))
                                  ),
                                  tags$head(tags$style('#box16 .box-header{ display: none}'))
                         ),
                         
                         
                         
                         
                         fluidRow(class ="rowhide3",
                                  
                                  shinydashboardPlus::box(
                                    id = 'box17',
                                    solidHeader = T,
                                    collapsible = F, 
                                    closable = F,
                                    width = 12,
                                    shinycssloaders::withSpinner(plotOutput("p_success_plot"), 
                                                                 type = 6, 
                                                                 color = "#2C3E50")
                                  ),
                                  tags$head(tags$style('#box17 .box-header{ display: none}'))
                         )
                         
                         
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 tabItem(tabName = "Posterior_Superiority",
                         
                         fluidRow(
                           shinydashboardPlus::box(
                             id = 'box18',
                             solidHeader = T,
                             collapsible = F, 
                             closable = F,
                             width = 4,
                             div(style="height: 80px;",
                                 uiOutput("P_superiority_response_select")
                             ),
                             
                             bsTooltip("P_superiority_response_select", 
                                       "Select your outcome type of interest.",
                                       options = list(container = "body"))
                           ),
                           tags$head(tags$style('#box18 .box-header{ display: none}')),
                           
                           shinydashboardPlus::box(
                             id = 'box19',
                             solidHeader = T,
                             collapsible = F, 
                             closable = F,
                             width = 4,
                             div(style="height: 80px;",
                                 uiOutput("P_superiority_outcome_fav_tip")
                             )
                           ),
                           tags$head(tags$style('#box19 .box-header{ display: none}')),
                           
                           shinydashboardPlus::box(
                             id = 'box20',
                             solidHeader = T,
                             collapsible = F,
                             closable = F,
                             width = 4,
                             div(style="height: 80px;",
                                 uiOutput("P_superiority_n_inputs")
                             ),
                             
                             bsTooltip("P_superiority_n_inputs", 
                                       "The number of treatments competing directly against each other.",
                                       options = list(container = "body"))
                           ),
                           tags$head(tags$style('#box20 .box-header{ display: none}'))
                           
                         ),
                         
                         fluidRow(
                           shinydashboardPlus::box(
                             title = 'Sample Size',
                             solidHeader = T,
                             collapsible = F,
                             closable = F,
                             width = 4,
                             uiOutput("p_superiority_samp_sizes"),
                             
                             bsTooltip("p_superiority_samp_sizes", 
                                       "Insert the number of patient outcomes per arm.",
                                       options = list(container = "body"))
                           ),
                           
                           shinydashboardPlus::box(
                             title = textOutput('y'),
                             solidHeader = T,
                             collapsible = F,
                             closable = F,
                             width = 4,
                             uiOutput("p_superiority_means_or_cases_tip")
                           ),
                           
                           conditionalPanel(
                             condition = "input.p_superiority_response_type == 'Numeric'",
                             shinydashboardPlus::box(
                               title = textOutput('yy'),
                               class = "rowhide5",
                               solidHeader = T,
                               collapsible = F,
                               closable = F,
                               width = 4,
                               uiOutput("p_superiority_sds")
                             ),
                             
                             bsTooltip("p_superiority_sds", 
                                       "Insert the response standard deviation for each of the arms.",
                                       options = list(container = "body"))
                           )
                         ),
                         
                         
                         fluidRow(
                           textOutput("p_superiority_alert_all"),
                           
                           bsAlert("p_superiority_alert"),
                           
                           shinydashboardPlus::box(
                             id = 'box21',
                             class = "rowhide6",
                             solidHeader = T,
                             collapsible = F,
                             closable = F,
                             width = 12,
                             div(style="height: 33px;",
                                 shinycssloaders::withSpinner(
                                   uiOutput("p_superiority_button"), 
                                   type = 6, 
                                   color = "#2C3E50")
                             ),
                             
                             bsTooltip("p_superiority_button", 
                                       paste0("Calculate the posterior probability of superiority for all treatment arms. ",
                                              "This is the probability of a given arm being superior to all other arms, ",
                                              "based on the data collected thus far."),
                                       options = list(container = "body"))
                           ),
                           tags$head(tags$style('#box21 .box-header{ display: none}'))
                           
                           
                         ),
                         
                         
                         fluidRow(class = "rowhide6",
                                  shinydashboardPlus::box(
                                    id = 'box22',
                                    solidHeader = T,
                                    collapsible = F,
                                    closable = F,
                                    width = 12,
                                    div(style="height: 21px;",
                                        tags$style(".material-switch input:checked~.state label:after {background-color: #2C3E50 !important;}"),
                                        shinycssloaders::withSpinner(
                                          uiOutput("p_superiority_RAR_switch"),
                                          type = 6, 
                                          color = "#2C3E50")
                                    ),
                                    
                                    bsTooltip("p_superiority_RAR_switch", 
                                              paste0("Switch on for allocation probabilities in a response ", 
                                                     "adaptive randomization design to be calculated. ",
                                                     "Response adaptive randomization will divert a larger ",
                                                     "number of patients to treatment arms that show greater promise."),
                                              options = list(container = "body"))
                                    
                                  ),
                                  tags$head(tags$style('#box22 .box-header{ display: none}'))
                         ),
                         
                         
                         fluidRow(class = "rowhide10",
                                  shinydashboardPlus::box(
                                    id = 'box23',
                                    solidHeader = T,
                                    collapsible = F,
                                    closable = F,
                                    width = 12,
                                    div(style="height: 80px;",
                                        shinycssloaders::withSpinner(
                                          uiOutput("p_superiority_RAR_power"), 
                                          type = 6, 
                                          color = "#2C3E50")
                                    ),
                                    
                                    bsTooltip("p_superiority_RAR_power", 
                                              paste0("The power to which the probabilities of superiority", 
                                                     " are raised to give the allocation probabilities. ",
                                                     "The larger the power is, the greater the impact the ",
                                                     "data will have on future treatment allocation of patients."),
                                              options = list(container = "body"))
                                  ),
                                  tags$head(tags$style('#box23 .box-header{ display: none}'))
                         ),
                         
                         
                         fluidRow(class = "rowhide7",
                                  shinydashboardPlus::box(id = 'box24',
                                    solidHeader = T,
                                    collapsible = F,
                                    closable = F,
                                    width = 6,
                                    div(style="height: 375px;",
                                        shinycssloaders::withSpinner(
                                          plotOutput("p_superiority_barplot"), 
                                          type = 6, 
                                          color = "#2C3E50")
                                    )
                                  ),
                                  tags$head(tags$style('#box24 .box-header{ display: none}')),
                                  
                                  shinydashboardPlus::box(
                                    id = 'box25',
                                    solidHeader = T,
                                    collapsible = F,
                                    closable = F,
                                    width = 6,
                                    div(style="height: 375px;",
                                        shinycssloaders::withSpinner(
                                          tableOutput("p_superiority_table_out"), 
                                          type = 6, 
                                          color = "#2C3E50")
                                    )
                                  ),
                                  tags$head(tags$style('#box25 .box-header{ display: none}'))
                         ),
                         
                         
                         fluidRow(
                           shinydashboardPlus::box(
                             id = 'box26',
                             solidHeader = T,
                             closable = F,
                             collapsible = F,
                             width = 6,
                             plotOutput("p_superiority_dens_plot")
                           ),
                           tags$head(tags$style('#box26 .box-header{ display: none}')),
                           
                           shinydashboardPlus::box(
                             id = 'box27',
                             solidHeader = T,
                             closable = F,
                             collapsible = F,
                             width = 6,
                             plotOutput("p_superiority_CrI_plot")
                           ),
                           tags$head(tags$style('#box27 .box-header{ display: none}'))
                         )
                         
                 )
                 
                 
                 
                 
                 
                 
                 
                 
               )
               
               
               
               
             )
             
           ),
           
           
           
           
           # rightsidebar = rightSidebar(),
           
           title = "BIAS: Bayesian Interim Analysis Software"
           
           
           
           
           
  )
  
  
)




