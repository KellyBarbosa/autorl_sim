ui <- tagList(
  useShinyjs(),
  shinyjs::inlineCSS(css),
  tags$head(tags$script(js)),
  navbarPage(
    theme = shinytheme("lumen"),
    "AutoRL - Sim",
    id = "navbar",
    collapsible = TRUE,
    tabPanel(
      title = "Home",
      icon = icon("house",  lib = "font-awesome"),
      div(imageOutput("logo"),  class = "text-center"),
      div(class = "image-container",
          div(imageOutput("route"), class = "text-center", style = "margin-top: -290px;"),   
          div(class = "button-container",
              actionButton("start", "Start", class = "btn-primary", style = "color: #7fffff; background: rgba(41, 45, 37, 0.8); border: 2px #7fffff solid; font-size: 22px; width: 100px; height: 50px;")
          ))),
    tabPanel(
      "TSP",
      sidebarPanel(width = 3,
                   h3("TSP", class = "text-center reduce-margin"),
                   class = "limit_sidebarPanel",
                   selectInput(
                     inputId = "input_problem_name_tsp",
                     label = "TSPLIB instances: ",
                     choices = c(
                       "eil51",
                       "berlin52",
                       "st70",
                       "eil76",
                       "pr76",
                       "rat99",
                       "kroA100",
                       "eil101",
                       "bier127",
                       "ch130",
                       "ch150",
                       "a280",
                       "lin318",
                       "d1655"
                     )
                   ),
                   numericInput(
                     "learning_rate_tsp",
                     "Enter the learning rate: ",
                     0.75,
                     min = 0.01,
                     max = 1,
                     step = 0.01
                   ),
                   numericInput(
                     "discount_factor_tsp",
                     "Enter the discount factor: ",
                     0.15,
                     min = 0.01,
                     max = 1,
                     step = 0.01
                   ),
                   numericInput(
                     "e_greedy_tsp",
                     "Enter the e-greedy: ",
                     0.01,
                     min = 0.01,
                     max = 1,
                     step = 0.01
                   ),
                   numericInput(
                     "episodes_tsp",
                     "Enter the number of episodes: ",
                     1000,
                     min = 2,
                     max = 10000,
                     step = 1
                   ),
                   div(class = "text-center",
                       actionButton("random_tsp", "Generate random values", style= "margin-bottom: 5px; background-color: #87CEEB; border: 1px solid #5892A9;"),
                       actionButton("btn_tsp", "Start experiments", class = "btn-primary"))),
      mainPanel(width = 8,
                tabsetPanel(
                  tabPanel(
                    "Distance Graph",
                    column(class = "increase-margin", width = 12, align = "center",
                           plotOutput(
                             "distance_graph_tsp",
                             dblclick = "distance_graph_tsp_dblclick",
                             brush = brushOpts(id = "distance_graph_tsp_brush", resetOnNew = TRUE), height = "475px"),
                           help_text(),
                           downloadButton("download_graph_tsp", "Download graph", class = "btn-success"),
                           downloadButton("download_graph_zoom_tsp", "Download clipping", class = "btn-success"))),
                  tabPanel(
                    "Route Graph",
                    column(class = "increase-margin", width = 12, align = "center",
                           plotOutput(
                             "route_graph_tsp",
                             dblclick = "route_graph_tsp_dblclick",
                             brush = brushOpts(id = "route_graph_tsp_brush", resetOnNew = TRUE), width = "600px", height = "475px"),
                           help_text(),
                           downloadButton("download_route_graph_tsp", "Download graph", class = "btn-success"),
                           downloadButton("download_route_zoom_graph_tsp", "Download clipping", class = "btn-success"))),
                  tabPanel(
                    "Results",
                    class = "increase-margin",
                    h5(verbatimTextOutput("problem_name_tsp")),
                    h5(verbatimTextOutput("problem_type_tsp")),
                    h5(verbatimTextOutput("dist_avg_tsp")),
                    h5(verbatimTextOutput("dist_min_tsp")),
                    h5(verbatimTextOutput("ep_dist_min_tsp")),
                    h5(verbatimTextOutput("tsplib_tsp")),
                    h5(verbatimTextOutput("error_tsp")),
                    h5(verbatimTextOutput("time_tsp")),
                    h5(verbatimTextOutput("alpha_tsp")),
                    h5(verbatimTextOutput("gamma_tsp")),
                    h5(verbatimTextOutput("e_greedy_tsp")),
                    h5(verbatimTextOutput("episodes_tsp")),
                    column(width = 12,  align = "center",
                           radioButtons("format_tsp", "Report type",
                                        c("PDF report with graphs", "Simple PDF report", "Simple CSV report"), inline = TRUE),
                           downloadButton("download_data_tsp", "Download experiment data", class = "btn-success")))))),
    tabPanel(
      "ATSP",
      sidebarPanel(width = 3,
                   h3("ATSP", class = "text-center reduce-margin"),
                   class = "limit_sidebarPanel",
                   selectInput(
                     inputId = "input_problem_name_atsp",
                     label = "TSPLIB instances: ",
                     choices = c(
                       "ftv33",
                       "p43",
                       "ftv44",
                       "ftv47",
                       "ry48p",
                       "ft53",
                       "ftv64",
                       "ft70"
                     )
                   ),
                   numericInput(
                     "learning_rate_atsp",
                     "Enter the learning rate: ",
                     0.75,
                     min = 0.01,
                     max = 1,
                     step = 0.01
                   ),
                   numericInput(
                     "discount_factor_atsp",
                     "Enter the discount factor: ",
                     0.15,
                     min = 0.01,
                     max = 1,
                     step = 0.01
                   ),
                   numericInput(
                     "e_greedy_atsp",
                     "Enter the e-greedy: ",
                     0.01,
                     min = 0.01,
                     max = 1,
                     step = 0.01
                   ),
                   numericInput(
                     "episodes_atsp",
                     "Enter the number of episodes: ",
                     1000,
                     min = 2,
                     max = 10000,
                     step = 1
                   ),
                   div(class = "text-center",
                       actionButton("random_atsp", "Generate random values", style= "margin-bottom: 5px; background-color: #87CEEB; border: 1px solid #5892A9;"),
                       actionButton("btn_atsp", "Start experiments", class = "btn-primary"))),
      mainPanel(width = 8,
                tabsetPanel(
                  tabPanel(
                    "Distance Graph",
                    column(class = "increase-margin", width = 12, align = "center",
                           plotOutput(
                             "distance_graph_atsp",
                             dblclick = "distance_graph_atsp_dblclick",
                             brush = brushOpts(id = "distance_graph_atsp_brush", resetOnNew = TRUE), height = "475px"),
                           help_text(),
                           downloadButton("download_graph_atsp", "Download graph", class = "btn-success"),
                           downloadButton("download_graph_zoom_atsp", "Download clipping", class = "btn-success"))),
                  tabPanel(
                    "Results",
                    class = "increase-margin",
                    h5(verbatimTextOutput("problem_name_atsp")),
                    h5(verbatimTextOutput("problem_type_atsp")),
                    h5(verbatimTextOutput("dist_avg_atsp")),
                    h5(verbatimTextOutput("dist_min_atsp")),
                    h5(verbatimTextOutput("ep_dist_min_atsp")),
                    h5(verbatimTextOutput("tsplib_atsp")),
                    h5(verbatimTextOutput("error_atsp")),
                    h5(verbatimTextOutput("time_atsp")),
                    h5(verbatimTextOutput("alpha_atsp")),
                    h5(verbatimTextOutput("gamma_atsp")),
                    h5(verbatimTextOutput("e_greedy_atsp")),
                    h5(verbatimTextOutput("episodes_atsp")),
                    column(width = 12,  align = "center",
                           radioButtons("format_atsp", "Report type",
                                        c("PDF report with graphs", "Simple PDF report", "Simple CSV report"), inline = TRUE),
                           downloadButton("download_data_atsp", "Download experiment data", class = "btn-success")))))),
    tabPanel(
      "SOP",
      sidebarPanel(width = 3,
                   h3("SOP", class = "text-center reduce-margin"),
                   class = "limit_sidebarPanel",
                   selectInput(
                     inputId = "input_problem_name_sop",
                     label = "TSPLIB instances: ",
                     choices = c(
                       "br17.10",
                       "br17.12",
                       "p43.1",
                       "p43.2",
                       "p43.3",
                       "p43.4",
                       "ry48p.1",
                       "ry48p.2",
                       "ry48p.3",
                       "ry48p.4",
                       "ft53.1",
                       "ft53.2",
                       "ft53.3",
                       "ft53.4",
                       "ft70.1",
                       "ft70.2",
                       "ft70.3",
                       "ft70.4"
                     )
                   ),
                   numericInput(
                     "learning_rate_sop",
                     "Enter the learning rate: ",
                     0.75,
                     min = 0.01,
                     max = 1,
                     step = 0.01
                   ),
                   numericInput(
                     "discount_factor_sop",
                     "Enter the discount factor: ",
                     0.15,
                     min = 0.01,
                     max = 1,
                     step = 0.01
                   ),
                   numericInput(
                     "e_greedy_sop",
                     "Enter the e-greedy: ",
                     0.01,
                     min = 0.01,
                     max = 1,
                     step = 0.01
                   ),
                   numericInput(
                     "episodes_sop",
                     "Enter the number of episodes: ",
                     1000,
                     min = 2,
                     max = 10000,
                     step = 1
                   ),
                   div(class = "text-center",
                       actionButton("random_sop", "Generate random values", style= "margin-bottom: 5px; background-color: #87CEEB; border: 1px solid #5892A9;"),
                       actionButton("btn_sop", "Start experiments", class = "btn-primary"))),
      mainPanel(width = 8,
                tabsetPanel(
                  tabPanel(
                    "Distance Graph",
                    column(class = "increase-margin",width = 12, align = "center",
                           plotOutput(
                             "distance_graph_sop",
                             dblclick = "distance_graph_sop_dblclick",
                             brush = brushOpts(id = "distance_graph_sop_brush", resetOnNew = TRUE), height = "475px"),
                           help_text(),
                           downloadButton("download_graph_sop", "Download graph", class = "btn-success"),
                           downloadButton("download_graph_zoom_sop", "Download clipping", class = "btn-success"))),
                  tabPanel(
                    "Results",
                    class = "increase-margin",
                    h5(verbatimTextOutput("problem_name_sop")),
                    h5(verbatimTextOutput("problem_type_sop")),
                    h5(verbatimTextOutput("dist_avg_sop")),
                    h5(verbatimTextOutput("dist_min_sop")),
                    h5(verbatimTextOutput("ep_dist_min_sop")),
                    h5(verbatimTextOutput("tsplib_sop")),
                    h5(verbatimTextOutput("error_sop")),
                    h5(verbatimTextOutput("time_sop")),
                    h5(verbatimTextOutput("alpha_sop")),
                    h5(verbatimTextOutput("gamma_sop")),
                    h5(verbatimTextOutput("e_greedy_sop")),
                    h5(verbatimTextOutput("episodes_sop")),
                    column(width = 12,  align = "center",
                           radioButtons("format_sop", "Report type",
                                        c("PDF report with graphs", "Simple PDF report", "Simple CSV report"), inline = TRUE),
                           downloadButton("download_data_sop", "Download experiment data", class = "btn-success")))))),
    tabPanel(
      "TSP - AutoML",
      helpText(
        "Warning: Experiments performed with AutoML may take longer than usual.",
        class = "text-danger text-center",
        style = "margin-top:-15px; font-size: 16px;"),
      sidebarPanel(width = 3,
                   h3("TSP - AutoML", class = "text-center reduce-margin"),
                   class = "limit_sidebarPanel",
                   selectInput(
                     inputId = "input_problem_name_tsp_automl",
                     label = "TSPLIB instances: ",
                     choices = c(
                       "eil51",
                       "berlin52",
                       "st70",
                       "eil76",
                       "pr76",
                       "rat99",
                       "kroA100",
                       "eil101",
                       "bier127",
                       "ch130",
                       "ch150",
                       "a280",
                       "lin318",
                       "d1655"
                     )
                   ),
                   div(class = "text-center",
                       actionButton("btn_tsp_automl", "Start experiments", class = "btn-primary"))),
      mainPanel(width = 8,
                tabsetPanel(
                  tabPanel(
                    "Distance Graph",
                    column(class = "increase-margin", width = 12, align = "center",
                           plotOutput(
                             "distance_graph_tsp_automl",
                             dblclick = "distance_graph_tsp_automl_dblclick",
                             brush = brushOpts(id = "distance_graph_tsp_automl_brush", resetOnNew = TRUE), height = "475px"),
                           help_text(),
                           downloadButton("download_graph_tsp_automl", "Download graph", class = "btn-success"),
                           downloadButton("download_graph_zoom_tsp_automl", "Download clipping", class = "btn-success"))),
                  tabPanel(
                    "Route Graph",            
                    column(class = "increase-margin", width = 12, align = "center",
                           plotOutput(
                             "route_graph_tsp_automl",
                             dblclick = "route_graph_tsp_automl_dblclick",
                             brush = brushOpts(id = "route_graph_tsp_automl_brush", resetOnNew = TRUE), width = "600px", height = "475px"),
                           help_text(),
                           downloadButton("download_route_graph_tsp_automl", "Download graph", class = "btn-success"),
                           downloadButton(
                             "download_route_zoom_graph_tsp_automl",
                             "Download clipping",
                             class = "btn-success"))),
                  tabPanel("Contour Graph",
                           column(width = 12,  align = "center",
                                  plotOutput("graph_tsp_contour_automl", width = "500px", height = "525px"),
                                  downloadButton("download_graph_tsp_contour_automl", "Download graph", class = "btn-success"))),
                  tabPanel("Surface Graph",
                           column(width = 12,  align = "center",
                                  plotOutput("graph_tsp_surface_automl", width = "600px", height = "500px"),
                                  downloadButton("download_graph_tsp_surface_automl", "Download graph", class = "btn-success"))),
                  tabPanel(
                    "Results",
                    class = "increase-margin",
                    h5(verbatimTextOutput("problem_name_tsp_automl")),
                    h5(verbatimTextOutput("problem_type_tsp_automl")), 
                    h5(verbatimTextOutput("dist_avg_tsp_automl")),
                    h5(verbatimTextOutput("dist_min_tsp_automl")),
                    h5(verbatimTextOutput("ep_dist_min_tsp_automl")),
                    h5(verbatimTextOutput("epoch_min_dist_tsp_automl")),
                    h5(verbatimTextOutput("tsplib_tsp_automl")),
                    h5(verbatimTextOutput("error_tsp_automl")),
                    h5(verbatimTextOutput("time_tsp_automl")),
                    h5(verbatimTextOutput("alpha_tsp_automl")),
                    h5(verbatimTextOutput("gamma_tsp_automl")),
                    h5(verbatimTextOutput("e_greedy_tsp_automl")),
                    h5(verbatimTextOutput("episodes_tsp_automl")),
                    h5(verbatimTextOutput("epochs_tsp_automl")),
                    h5(verbatimTextOutput("info_tsp_automl")),
                    h5(verbatimTextOutput("model_summary_tsp_automl")),
                    h5(verbatimTextOutput("anova_data_tsp_automl")),
                    h5(verbatimTextOutput("pks_tsp_automl")),
                    column(width = 12,  align = "center",
                           radioButtons(
                             "format_tsp_automl",
                             "Report type",
                             c("PDF report with graphs", "Simple report"), inline = TRUE),
                           downloadButton(
                             "download_data_tsp_automl",
                             "Download experiment data",
                             class = "btn-success"
                           )))))),
    tabPanel(
      "ATSP - AutoML",
      helpText(
        "Warning: Experiments performed with AutoML may take longer than usual.",
        class = "text-danger text-center",
        style = "margin-top:-15px; font-size: 16px;"
      ),
      sidebarPanel(width = 3, 
                   h3("ATSP - AutoML", class = "text-center reduce-margin"),
                   class = "limit_sidebarPanel",
                   selectInput(
                     inputId = "input_problem_name_atsp_automl",
                     label = "TSPLIB instances: ",
                     choices = c(
                       "ftv33",
                       "p43",
                       "ftv44",
                       "ftv47",
                       "ry48p",
                       "ft53",
                       "ftv64",
                       "ft70"
                     )
                   ),
                   div(class = "text-center",
                       actionButton("btn_atsp_automl", "Start experiments", class = "btn-primary"))),
      mainPanel(width = 8,
                tabsetPanel(
                  tabPanel(
                    "Distance Graph",
                    column(class = "increase-margin", width = 12, align = "center",
                           plotOutput(
                             "distance_graph_atsp_automl",
                             dblclick = "distance_graph_atsp_automl_dblclick",
                             brush = brushOpts(id = "distance_graph_atsp_automl_brush", resetOnNew = TRUE), height = "475px"),
                           help_text(),
                           downloadButton("download_graph_atsp_automl", "Download graph", class = "btn-success"),
                           downloadButton("download_graph_zoom_atsp_automl", "Download clipping", class = "btn-success"))),
                  tabPanel("Contour Graph",
                           column(width = 12,  align = "center",
                                  plotOutput("graph_atsp_contour_automl", width = "500px", height = "525px"),
                                  downloadButton("download_graph_atsp_contour_automl", "Download graph", class = "btn-success"))),
                  tabPanel(
                    "Surface Graph",
                    column(width = 12,  align = "center",
                           plotOutput("graph_atsp_surface_automl", width = "600px", height = "500px"),
                           downloadButton("download_graph_atsp_surface_automl", "Download graph", class = "btn-success"))),
                  tabPanel(
                    "Results",
                    class = "increase-margin",
                    h5(verbatimTextOutput("problem_name_atsp_automl")),
                    h5(verbatimTextOutput("problem_type_atsp_automl")),
                    h5(verbatimTextOutput("dist_avg_atsp_automl")),
                    h5(verbatimTextOutput("dist_min_atsp_automl")),
                    h5(verbatimTextOutput("ep_dist_min_atsp_automl")),
                    h5(verbatimTextOutput("epoch_min_dist_atsp_automl")),
                    h5(verbatimTextOutput("tsplib_atsp_automl")),
                    h5(verbatimTextOutput("error_atsp_automl")),
                    h5(verbatimTextOutput("time_atsp_automl")),
                    h5(verbatimTextOutput("alpha_atsp_automl")),
                    h5(verbatimTextOutput("gamma_atsp_automl")),
                    h5(verbatimTextOutput("e_greedy_atsp_automl")),
                    h5(verbatimTextOutput("episodes_atsp_automl")),
                    h5(verbatimTextOutput("epochs_atsp_automl")),
                    h5(verbatimTextOutput("info_atsp_automl")),
                    h5(verbatimTextOutput("resumo_atsp_automl")),
                    h5(verbatimTextOutput("dadosAnova_atsp_automl")),
                    h5(verbatimTextOutput("pks_atsp_automl")),
                    column(width = 12,  align = "center",
                           radioButtons(
                             "format_atsp_automl",
                             "Report type",
                             c("PDF report with graphs", "Simple report"), inline = TRUE),
                           downloadButton(
                             "download_data_atsp_automl",
                             "Download experiment data",
                             class = "btn-success")))))),
    tabPanel(
      "SOP - AutoML",
      helpText(
        "Warning: Experiments performed with AutoML may take longer than usual.",
        class = "text-danger text-center",
        style = "margin-top:-15px; font-size: 16px;"
      ),
      sidebarPanel(width = 3,
                   h3("SOP - AutoML", class = "text-center reduce-margin"),
                   class = "limit_sidebarPanel",
                   selectInput(
                     inputId = "input_problem_name_sop_automl",
                     label = "TSPLIB instances: ",
                     choices = c(
                       "br17.10",
                       "br17.12",
                       "p43.1",
                       "p43.2",
                       "p43.3",
                       "p43.4",
                       "ry48p.1",
                       "ry48p.2",
                       "ry48p.3",
                       "ry48p.4",
                       "ft53.1",
                       "ft53.2",
                       "ft53.3",
                       "ft53.4",
                       "ft70.1",
                       "ft70.2",
                       "ft70.3",
                       "ft70.4"
                     )
                   ),
                   div(class = "text-center",
                       actionButton("btn_sop_automl", "Start experiments", class = "btn-primary"))),
      mainPanel(width = 8,
                tabsetPanel(
                  tabPanel(
                    "Distance Graph",            
                    column(class = "increase-margin", width = 12, align = "center",
                           plotOutput(
                             "distance_graph_sop_automl",
                             dblclick = "distance_graph_sop_automl_dblclick",
                             brush = brushOpts(id = "distance_graph_sop_automl_brush", resetOnNew = TRUE), height = "475px"),
                           help_text(),
                           downloadButton("download_graph_sop_automl", "Download graph", class = "btn-success"),
                           downloadButton("download_graph_zoom_sop_automl", "Download clipping", class = "btn-success"))),
                  tabPanel("Contour Graph",
                           column(width = 12, align = "center",
                                  plotOutput("graph_sop_contour_automl", width = "500px", height = "525px"),
                                  downloadButton("download_graph_sop_contour_automl", "Download graph", class = "btn-success"))),
                  tabPanel("Surface Graph",
                           column(width = 12, align = "center",
                                  plotOutput("graph_sop_surface_automl", width = "600px", height = "500px"),
                                  downloadButton("download_graph_sop_surface_automl", "Download graph", class = "btn-success"))),
                  tabPanel(
                    "Results",
                    class = "increase-margin",
                    h5(verbatimTextOutput("problem_name_sop_automl")),
                    h5(verbatimTextOutput("problem_type_sop_automl")),
                    h5(verbatimTextOutput("dist_avg_sop_automl")),
                    h5(verbatimTextOutput("dist_min_sop_automl")),
                    h5(verbatimTextOutput("ep_dist_min_sop_automl")),
                    h5(verbatimTextOutput("epoch_min_dist_sop_automl")),
                    h5(verbatimTextOutput("tsplib_sop_automl")),
                    h5(verbatimTextOutput("error_sop_automl")),
                    h5(verbatimTextOutput("time_sop_automl")),
                    h5(verbatimTextOutput("alpha_sop_automl")),
                    h5(verbatimTextOutput("gamma_sop_automl")),
                    h5(verbatimTextOutput("e_greedy_sop_automl")),
                    h5(verbatimTextOutput("episodes_sop_automl")),
                    h5(verbatimTextOutput("epochs_sop_automl")),
                    h5(verbatimTextOutput("info_sop_automl")),
                    h5(verbatimTextOutput("resumo_sop_automl")),
                    h5(verbatimTextOutput("dadosAnova_sop_automl")),
                    h5(verbatimTextOutput("pks_sop_automl")),
                    column(width = 12,  align = "center",
                           radioButtons(
                             "format_sop_automl",
                             "Report type",
                             c("PDF report with graphs", "Simple report"), inline = TRUE),
                           downloadButton(
                             "download_data_sop_automl",
                             "Download experiment data",
                             class = "btn-success"
                           )))))),
    
    navbarMenu("Free module", 
               icon = icon("street-view",  lib = "font-awesome"),
               tabPanel(
                 "Without AutoML",
                 sidebarPanel(width = 3,
                              h3("TSP/ATSP/SOP", class = "text-center reduce-margin"),
                              class = "limit_sidebarPanel",
                              textInput(inputId = "input_problem_name_user", "Instance name: ", value = "", width = NULL, placeholder = NULL),
                              radioButtons("type_user", "Instance type:",
                                           choices = list("TSP" = "TSP", "ATSP" = "ATSP", "SOP" = "SOP"),
                                           selected = "TSP", inline = TRUE),
                              radioButtons("type_data_user", "Data type:",
                                           choices = list("Euclidean 2d" = 1, "Matrix" = 2),
                                           selected = 1, inline = TRUE),
                              uiOutput("condicional_size_ui_user"),
                              radioButtons("check_best_value_user", "Is there already a known optimal value?",
                                           choices = list("Yes" = 1, "No" = 2),
                                           selected = 2, inline = TRUE),
                              uiOutput("condicional_best_value_ui_user"),
                              div(fileInput("file_user", "Upload data file (.txt):", multiple = FALSE, accept = ".txt"), style="margin-bottom: -20px;"),
                              numericInput(
                                "learning_rate_user",
                                "Enter the learning rate: ",
                                0.75,
                                min = 0.01,
                                max = 1,
                                step = 0.01
                              ),
                              numericInput(
                                "discount_factor_user",
                                "Enter the discount factor: ",
                                0.15,
                                min = 0.01,
                                max = 1,
                                step = 0.01
                              ),
                              numericInput(
                                "e_greedy_user",
                                "Enter the e-greedy: ",
                                0.01,
                                min = 0.01,
                                max = 1,
                                step = 0.01
                              ),
                              numericInput(
                                "episodes_user",
                                "Enter the number of episodes: ",
                                1000,
                                min = 2,
                                max = 10000,
                                step = 1
                              ),
                              div(class = "text-center",
                                  actionButton("random_user", "Generate random values", style= "margin-bottom: 5px; background-color: #87CEEB; border: 1px solid #5892A9;"),
                                  actionButton("btn_user", "Start experiments", class = "btn-primary"))),
                 mainPanel(width = 8, 
                           tabsetPanel(id = "tabs_user",
                                       tabPanel("Distance Graph", 
                                                column(class = "increase-margin", width = 12, align = "center",
                                                       plotOutput(
                                                         "distance_graph_user",
                                                         dblclick = "distance_graph_user_dblclick",
                                                         brush = brushOpts(id = "distance_graph_user_brush", resetOnNew = TRUE), height = "475px"),
                                                       help_text(),
                                                       downloadButton("download_graph_user", "Download graph", class = "btn-success"),
                                                       downloadButton("download_graph_zoom_user", "Download clipping", class = "btn-success"))),
                                       tabPanel("Route Graph", 
                                                column(class = "increase-margin", width = 12, align = "center", plotOutput(
                                                  "route_graph_user",
                                                  dblclick = "route_graph_user_dblclick",
                                                  brush = brushOpts(id = "route_graph_user_brush", resetOnNew = TRUE), width = "600px", height = "475px"), help_text(),
                                                  downloadButton("download_route_graph_user", "Download graph", class = "btn-success"),
                                                  downloadButton("download_route_zoom_graph_user", "Download clipping", class = "btn-success")
                                                )),
                                       tabPanel("Results", class = "increase-margin",
                                                h5(verbatimTextOutput("problem_name_user")),
                                                h5(verbatimTextOutput("problem_type_user")),
                                                h5(verbatimTextOutput("dist_avg_user")),
                                                h5(verbatimTextOutput("dist_min_user")),
                                                h5(verbatimTextOutput("ep_dist_min_user")),
                                                h5(verbatimTextOutput("time_user")),
                                                h5(verbatimTextOutput("alpha_user")),
                                                h5(verbatimTextOutput("gamma_user")),
                                                h5(verbatimTextOutput("e_greedy_user")),
                                                h5(verbatimTextOutput("episodes_user")),
                                                h5(verbatimTextOutput("otimo_user")),
                                                h5(verbatimTextOutput("error_user")),
                                                column(width = 12,  align = "center",
                                                       radioButtons("format_user", "Report type",
                                                                    c("PDF report with graphs", "Simple PDF report", "Simple CSV report"), inline = TRUE),
                                                       downloadButton("download_data_user", "Download experiment data", class = "btn-success")))))),
               
               tabPanel("With AutoML", 
                        helpText(
                          "Warning: Experiments performed with AutoML may take longer than usual.",
                          class = "text-danger text-center",
                          style = "margin-top:-15px; font-size: 16px;"),
                        sidebarPanel(width = 3,
                                     h3("TSP/ATSP/SOP", class = "text-center reduce-margin"),
                                     h3("AutoML", class = "text-center reduce-margin"),
                                     class = "limit_sidebarPanel",
                                     textInput(inputId = "input_problem_name_user_automl", "Instance name: ", value = "", width = NULL, placeholder = NULL),
                                     radioButtons("type_user_automl", "Instance type:",
                                                  choices = list("TSP" = "TSP", "ATSP" = "ATSP", "SOP" = "SOP"),
                                                  selected = "TSP", inline = TRUE),
                                     radioButtons("type_data_user_automl", "Data type:",
                                                  choices = list("Euclidean 2d" = 1, "Matrix" = 2),
                                                  selected = 1, inline = TRUE),
                                     uiOutput("condicional_size_ui_user_automl"),
                                     radioButtons("check_best_value_user_automl", "Is there already a known optimal value?",
                                                  choices = list("Yes" = 1, "No" = 2),
                                                  selected = 2, inline = TRUE),
                                     uiOutput("condicional_best_value_ui_user_automl"),
                                     div(fileInput("file_user_automl", "Upload data file (.txt):", multiple = FALSE, accept = ".txt"), style="margin-bottom: -15px;"),      
                                     div(class = "text-center",
                                         actionButton("btn_user_automl", "Start experiments", class = "btn-primary"))),
                        mainPanel(width = 8,
                                  tabsetPanel(id = "tabs_user_automl",
                                              tabPanel(
                                                "Distance Graph",
                                                column(class = "increase-margin", width = 12, align = "center",
                                                       plotOutput(
                                                         "distance_graph_user_automl",
                                                         dblclick = "distance_graph_user_automl_dblclick",
                                                         brush = brushOpts(id = "distance_graph_user_automl_brush", resetOnNew = TRUE), height = "475px"),
                                                       help_text(),
                                                       downloadButton("download_graph_user_automl", "Download graph", class = "btn-success"),
                                                       downloadButton("download_graph_zoom_user_automl", "Download clipping", class = "btn-success"))),
                                              tabPanel(
                                                "Route Graph",            
                                                column(class = "increase-margin", width = 12, align = "center",
                                                       plotOutput(
                                                         "route_graph_user_automl",
                                                         dblclick = "route_graph_user_automl_dblclick",
                                                         brush = brushOpts(id = "route_graph_user_automl_brush", resetOnNew = TRUE), width = "600px", height = "475px"),
                                                       help_text(),
                                                       downloadButton("download_route_graph_user_automl", "Download graph", class = "btn-success"),
                                                       downloadButton(
                                                         "download_route_zoom_graph_user_automl",
                                                         "Download clipping",
                                                         class = "btn-success"))),
                                              tabPanel("Contour Graph",
                                                       column(width = 12,  align = "center",
                                                              plotOutput("graph_user_contour_automl", width = "500px", height = "525px"),
                                                              downloadButton("download_graph_user_contour_automl", "Download graph", class = "btn-success"))),
                                              tabPanel("Surface Graph",
                                                       column(width = 12,  align = "center",
                                                              plotOutput("graph_user_surface_automl", width = "600px", height = "500px"),
                                                              downloadButton("download_graph_user_surface_automl", "Download graph", class = "btn-success"))),
                                              tabPanel(
                                                "Results",
                                                class = "increase-margin",
                                                h5(verbatimTextOutput("problem_name_user_automl")),
                                                h5(verbatimTextOutput("problem_type_user_automl")), 
                                                h5(verbatimTextOutput("dist_avg_user_automl")),
                                                h5(verbatimTextOutput("dist_min_user_automl")),
                                                h5(verbatimTextOutput("ep_dist_min_user_automl")),
                                                h5(verbatimTextOutput("epoch_min_dist_user_automl")),
                                                h5(verbatimTextOutput("time_user_automl")),
                                                h5(verbatimTextOutput("alpha_user_automl")),
                                                h5(verbatimTextOutput("gamma_user_automl")),
                                                h5(verbatimTextOutput("e_greedy_user_automl")),
                                                h5(verbatimTextOutput("episodes_user_automl")),
                                                h5(verbatimTextOutput("epochs_user_automl")),
                                                h5(verbatimTextOutput("info_user_automl")),
                                                h5(verbatimTextOutput("resumo_user_automl")),
                                                h5(verbatimTextOutput("dadosAnova_user_automl")),
                                                h5(verbatimTextOutput("pks_user_automl")),
                                                h5(verbatimTextOutput("otimo_user_automl")),
                                                h5(verbatimTextOutput("error_user_automl")),
                                                column(width = 12,  align = "center",
                                                       radioButtons(
                                                         "format_user_automl",
                                                         "Report type",
                                                         c("PDF report with graphs", "Simple PDF report"), inline = TRUE),
                                                       downloadButton(
                                                         "download_data_user_automl",
                                                         "Download experiment data",
                                                         class = "btn-success"
                                                       )))))
               )
    ),
    navbarMenu("More",icon = icon("arrows-to-dot",  lib = "font-awesome"),
               tabPanel(
                 title = "Additional Information",
                 icon = icon("circle-info",  lib = "font-awesome"),
                 h3("AutoRL - Sim"),
                 hr(),
                 HTML(paste0("<h4><strong>Reinforcement Learning:</strong> Reinforcement learning is one of the artificial intelligence techniques in which an agent learns while interacting with the environment. It is a paradigm in which an agent takes actions in an environment to maximize cumulative reward over time. In AutoRL-Sim, two classic reinforcement learning algorithms are applied, SARSA and Q-learning.</h4>")),
                 HTML(paste0("<h4><strong>Parameters of Reinforcement Learning:</strong></h4>")),
                 tags$ul(tags$li(
                   HTML(paste0("<h5><strong>Learning Rate:</strong> The learning rate, ranging from 0 to 1, determines the speed at which the machine learning model adapts to the data. Smaller values make the adjustment slower, while larger values make it faster.</h5>"))),
                   tags$li(
                     HTML(paste0("<h5><strong>Discount Factor:</strong> The discount factor, also ranging from 0 to 1, is used to weigh future rewards in a decision-making process. Larger values assign more importance to future rewards, while smaller values give them less importance.</h5>"))),
                   tags$li(HTML(paste0("<h5><strong>E-greedy:</strong> The e-greedy, like the previous parameters, varies between 0 and 1 and is an action selection strategy used in reinforcement learning algorithms. It controls the probability of choosing a random action instead of the best-known action. Larger values make it more likely to choose the best action, while smaller values make it more likely to choose a random action.</h5>")))),
                 HTML(paste0("<h4><strong>Automated Machine Learning:</strong> Automated Machine Learning (AutoML) aims to optimize results and reduce the human effort required in the initial setup of experiments. This method employs algorithms that automate machine learning tasks, such as tuning hyperparameters, to improve model performance. When AutoML is applied to Reinforcement Learning, it can be referred to as AutoRL or Automated Reinforcement Learning.</h4>")),
                 HTML(paste0("<h4><strong>Response Surface Methodology:</strong> The Response Surface Methodology (RSM) is a mathematical technique for modeling problems using polynomial equations. In AutoRL-Sim, RSM is applied to adjust the parameters learning rate and discount factor.</h4>")),
                 HTML(paste0("<h4><strong>Problems addressed:</strong></h4>")),
                 tags$ul(tags$li(
                    HTML(paste0("<h5><strong>Traveling Salesman Problem:</strong> The Traveling Salesman Problem (TSP) is one of the most emblematic challenges in the field of combinatorial optimization. In this problem, the goal is to find the lowest cost for a salesman to visit a set of cities, passing through each city exactly once and returning to the starting city. The challenge is to find the most efficient route to minimize the total cost.</h5>")),
                 HTML(paste0("<h5><strong>Asymmetric Traveling Salesman Problem:</strong> The Asymmetric Traveling Salesman Problem (ATSP) is a variation of the Traveling Salesman Problem. In the ATSP, the travel cost between two cities can vary depending on the direction of travel adopted.</h5>")),
                 HTML(paste0("<h5><strong>Sequential ordering problem:</strong> The Sequential ordering problem (SOP) is a variation of the Asymmetric traveling salesman problem. The SOP differs from the ATSP in that it has precedence constraints for the displacement.</h5>")),
                 )),
                 
                 h4(
                   "For a deeper understanding, please refer to the following works: ",
                   tags$ul(tags$li(a("Transfer Reinforcement Learning for Combinatorial Optimization Problems", 
                   href = "https://doi.org/10.3390/a17020087", target = '_blank')),
                       tags$li(a("Tuning of reinforcement learning parameters applied to SOP using the Scott-Knott method", href= "https://doi.org/10.1007/s00500-019-04206-w", target = '_blank')),
                       tags$li(a("A Response Surface Model Approach to Parameter Estimation of Reinforcement Learning for the Travelling Salesman Problem", href = "https://doi.org/10.1007/s40313-018-0374-y", target = '_blank')),
                       tags$li(a("AutoRL-TSP-RSM: sistema de aprendizado por reforço automatizado com metodologia de superfície de resposta para o problema do caixeiro viajante", href = "https://doi.org/10.5335/rbca.v13i3.12653", target = '_blank'))
                       )
                   )),
               
               tabPanel(
                 "About",
                 icon = icon("glyphicon glyphicon-question-sign",  lib = "glyphicon"),
                 h3("AutoRL - Sim"),
                 hr(),
                 h4(
                   "AutoRL - Sim (Automated Reinforcement Learning Simulator for Combinatorial Optimization Problems) is a simulation environment developed during the Bachelor's Degree in Computer Engineering at",
                   a("UFRB", href = "https://ufrb.edu.br/portal/", target = '_blank') ,
                   ". The goal of this environment is to provide easy access to conducting experiments in Combinatorial Optimization using Reinforcement Learning and Automated Machine Learning."
                 ),
                 h4(
                   "This simulation environment features three types of combinatorial optimization problems:"
                 ),
                 tags$ul(tags$li(
                   h5("Symmetric traveling salesman problem (TSP)")
                 ),
                 tags$li(
                   h5("Asymmetric traveling salesman problem (ATSP)")
                 ),
                 tags$li(h5(
                   "Sequential ordering problem (SOP)"
                 ))),
                 h4(
                   "The data used is provided by the library",
                   a("TSPLIB", href = "http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/", target =
                       '_blank') ,
                   ". The TSPLIB provides data for conducting experiments and the best known results of the instances made available."
                 ),
                 hr(),
                 h4(HTML(paste0("<strong>Developed by:</strong> Gleice Kelly Barbosa Souza"))),
                 h4(
                  fluidRow(
                    column(1, 
                      a(
                        "LinkedIn",
                        href = "https://www.linkedin.com/in/gleice-souza/",
                        target = '_blank'
                      )),
                    column(1, 
                      a(
                        "GitHub",
                        href = "https://github.com/KellyBarbosa",
                        target = '_blank'
                      )),
                    column(1, 
                      a(
                        "Contact", 
                         href = "mailto:kelly.189@hotmail.com"
                      )),
                    column(1, 
                      a(
                      "ORCID",
                      href = "https://orcid.org/0009-0001-3679-3298",
                      target = '_blank'
                      )))),
                 h4(HTML(paste0("<strong>Advisor:</strong> Prof. Dr. André Luiz Carvalho Ottoni"))),
                 h4(
                  fluidRow(
                    column(1, 
                      a(
                     "Contact",
                     href = "mailto:andre.ottoni@ufrb.edu.br",
                     target = '_blank'
                   )),
                   column(1, 
                      a(
                     "ORCID",
                     href = "https://orcid.org/0000-0003-2136-9870",
                     target = '_blank'
                   )))),
                 div(imageOutput("footer"), style = "margin-bottom: -250px;"),
                 h4(
                   "This application was developed in 2023. For more information, please contact us!"
                 ))))
)