tsp_atsp_sop_user <- function(input, output, session){
  user_data <- reactiveValues()
  file_data <- reactiveValues()
  dist_user_ranges <- reactiveValues(x = NULL, y = NULL)
  user_route_ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$condicional_size_ui_user <- renderUI({
    if (input$type_data_user == 2) {
      numericInput(
        "matrix_size_user",
        "Instance dimension: ",
        3,
        min = 1,
        max = 10000,
        step = 1
      )
    }
  })
  
  output$condicional_best_value_ui_user <- renderUI({
    if (input$check_best_value_user == 1) {
      numericInput(
        "best_value_user",
        "Known optimal value: ",
        1,
        min = 1,
        max = 1000000,
        step = 1
      )
    }
  })  
  
  observeEvent(input$type_data_user, {
    clear_file_input()
  })
  
  observeEvent(input$file_user, {
    file_data$data <- input$file_user
  })
  
  observeEvent(input$btn_user, {
    disable_buttons(c("random_user", "btn_user", "download_graph_user", "download_graph_zoom_user","download_route_graph_user", "download_route_zoom_graph_user", "format_user", "download_data_user"))
    disable_tabs(c("TSP", "ATSP", "SOP", "TSP - AutoML", "ATSP - AutoML", "SOP - AutoML", "With AutoML"))
    
    if (validate(c(input$learning_rate_user, input$discount_factor_user, input$e_greedy_user), input$episodes_user) && validate_user(input$input_problem_name_user, if (input$check_best_value_user == 1) input$best_value_user else 99999, input$type_data_user, file_data$data, input$matrix_size_user)) {
      
      if (input$check_best_value_user == 1) { 
        inputs <- c(input$input_problem_name_user, input$learning_rate_user, input$discount_factor_user, input$e_greedy_user, input$episodes_user, input$type_user, input$best_value_user)
      } else {
        inputs <- c(input$input_problem_name_user, input$learning_rate_user, input$discount_factor_user, input$e_greedy_user, input$episodes_user, input$type_user)
      }
      
      if(input$type_data_user == 2) {
        req(file_data$data)
        req(input$matrix_size_user)
        
        content <- paste(readLines(file_data$data$datapath), collapse = " ") # Lê o arquivo em uma única linha
        
        if (length(content) < 1) {
          return(NULL)
        }
        
        file_data$data <- as.data.frame(parse_matrix_line(content, input$matrix_size_user)) # Converte a linha de texto em matriz
      } else {
        if (is.null(file_data$data)) {
          return(NULL)
        }
        file_data$data <- read.table(file_data$data$datapath, header = FALSE)
      }
      
      show_loading_modal()
      
      if(!identical(inputs[6], "SOP")) {
        result <- process_tsp_atsp_user(file_data$data, input$learning_rate_user, input$discount_factor_user, input$e_greedy_user, input$episodes_user, if (identical(inputs[6], "TSP")) 'tsp' else 'atsp', if (input$type_data_user == 1) 'euc2d' else 'matrix')
      } else {
        result <- process_sop(file_data$data, input$learning_rate_user, input$discount_factor_user, input$e_greedy_user, input$episodes_user)
      }
      
      clear_file_input()
      
      show_loading_modal(FALSE)
      
      user_data$dist_graph <- result$dist_graph
      user_data$min_distance <- result$min_distance
      user_data$avg_distance <- result$avg_distance
      user_data$ep_dist_min <- result$ep_dist_min
      user_data$xRoute <- result$xRoute
      user_data$yRoute <- result$yRoute
      user_data$time <- result$time
      user_data$units <- result$units      
      
      # Route Graph
      output$route_graph_user <- renderPlot({
        plot_route_graph(user_data, inputs[1], inputs[6], user_data$min_distance) +
          coord_cartesian(xlim = user_route_ranges$x, ylim = user_route_ranges$y, expand = FALSE)
      }, res = 100)
      
      observeEvent(input$route_graph_user_dblclick, {
        brush <- input$route_graph_user_brush
        if (!is.null(brush)) {
          user_route_ranges$x <- c(brush$xmin, brush$xmax)
          user_route_ranges$y <- c(brush$ymin, brush$ymax)
          
        } else {
          user_route_ranges$x <- NULL
          user_route_ranges$y <- NULL
        }
      })
      
      output$download_route_graph_user <- downloadHandler(
        filename = function() {
          paste("route_graph_", inputs[1], ".pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_route_graph(user_data, inputs[1], inputs[6], user_data$min_distance),
                 device = "pdf")
        }
      )
      
      output$download_route_zoom_graph_user <- downloadHandler(
        filename = function() {
          paste("route_graph_clipping_", inputs[1], ".pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_route_graph(user_data, inputs[1], inputs[6], user_data$min_distance) +
                   coord_cartesian(xlim = user_route_ranges$x, ylim = user_route_ranges$y, expand = FALSE),
                 device = "pdf")
        }
      )
      
      # Distance Graph
      output$distance_graph_user <- renderPlot({
        plot_dist_graph(user_data$dist_graph, inputs[5], inputs[1], inputs[6]) +
          coord_cartesian(xlim = dist_user_ranges$x, ylim = dist_user_ranges$y, expand = FALSE)
      }, res = 100)
      
      observeEvent(input$distance_graph_user_dblclick, {
        brush <- input$distance_graph_user_brush
        if (!is.null(brush)) {
          dist_user_ranges$x <- c(brush$xmin, brush$xmax)
          dist_user_ranges$y <- c(brush$ymin, brush$ymax)
          
        } else {
          dist_user_ranges$x <- NULL
          dist_user_ranges$y <- NULL
        }
      })
      
      output$download_graph_user <- downloadHandler(
        filename = function() {
          paste("distance_graph_", inputs[1], ".pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_dist_graph(user_data$dist_graph, inputs[5], inputs[1], inputs[6]),
                 device = "pdf")
        }
      )
      
      output$download_graph_zoom_user <- downloadHandler(
        filename = function() {
          paste("distance_graph_clipping_", inputs[1], ".pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_dist_graph(user_data$dist_graph, inputs[5], inputs[1], inputs[6]) + 
                   coord_cartesian(xlim = dist_user_ranges$x, ylim = dist_user_ranges$y, expand = FALSE),
                 device = "pdf")
        }
      )
      
      # Experiment data
      if (input$check_best_value_user == 1) {
        output_text_elements <- c(
          problem_name_user = paste("Problem name:", inputs[1]),
          problem_type_user = paste("Problem type:", inputs[6]),
          dist_min_user = paste("Minimum distance:", user_data$min_distance),
          ep_dist_min_user = paste("Episode of minimum distance:", user_data$ep_dist_min),
          dist_avg_user = paste("Average distance:", round(user_data$avg_distance, 2)),
          gamma_user = paste("Discount factor:", inputs[3]),
          alpha_user = paste("Learning rate:", inputs[2]),
          e_greedy_user = paste("E-greedy policy:", inputs[4]),
          episodes_user = paste("Number of episodes:", inputs[5]),
          optimal_user = paste("Optimal distance:", inputs[7]),
          error_user = paste("Percentage relative error: ", round(relative_error(as.numeric(inputs[7]), user_data$min_distance), 2), "%", sep = ""),
          time_user = paste("Runtime:", round(user_data$time, 2), user_data$units)
        )
        
        fields_list <- c(
          "Problem type",
          "Problem",
          "Average distance",
          "Minimum distance",
          "Episode of minimum distance",
          "Optimal distance",
          "Percentage relative error",
          "Discount factor",
          "Learning rate",
          "E-greedy policy",
          "Number of episodes",
          "Runtime"
        )
        
        data_list <- c(
          inputs[6],
          inputs[1],
          round(user_data$avg_distance, 2),
          user_data$min_distance,
          user_data$ep_dist_min,
          inputs[7],
          paste(round(relative_error(as.numeric(inputs[7]), user_data$min_distance), 2), "%", sep = ""),
          inputs[3],
          inputs[2],
          inputs[4],
          inputs[5],
          paste(round(user_data$time, 2), user_data$units)
        ) 
      } else {
        output_text_elements <- c(
          problem_name_user = paste("Problem name:", inputs[1]),
          problem_type_user = paste("Problem type:", inputs[6]),
          dist_min_user = paste("Minimum distance:", user_data$min_distance),
          ep_dist_min_user = paste("Episode of minimum distance:", user_data$ep_dist_min),
          dist_avg_user = paste("Average distance:", round(user_data$avg_distance, 2)),
          gamma_user = paste("Discount factor:", inputs[3]),
          alpha_user = paste("Learning rate:", inputs[2]),
          e_greedy_user = paste("E-greedy policy:", inputs[4]),
          episodes_user = paste("Number of episodes:", inputs[5]),
          time_user = paste("Runtime:", round(user_data$time, 2), user_data$units)
        )
        
        fields_list <- c(
          "Problem type",
          "Problem",
          "Average distance",
          "Minimum distance",
          "Episode of minimum distance",
          "Discount factor",
          "Learning rate",
          "E-greedy policy",
          "Number of episodes",
          "Runtime"
        )
        
        data_list <- c(
          inputs[6],
          inputs[1],
          round(user_data$avg_distance, 2),
          user_data$min_distance,
          user_data$ep_dist_min,
          inputs[3],
          inputs[2],
          inputs[4],
          inputs[5],
          paste(round(user_data$time, 2), user_data$units)
        )
      }
      
      render_text_elements(output, output_text_elements)
      
      observeEvent(input$format_user, {
        if(identical(input$format_user, "Simple PDF report")) { 
          output$download_data_user <- download_data(fields_list, data_list, inputs[1], 'PDF', user_data, FALSE, FALSE, FALSE, FALSE, FALSE, if (input$check_best_value_user == 1) TRUE else FALSE)
        } else if (identical(input$format_user, "Simple CSV report")) {
          output$download_data_user <- download_data(fields_list, data_list, inputs[1], 'CSV', user_data, FALSE, FALSE, FALSE, FALSE, FALSE, if (input$check_best_value_user == 1) TRUE else FALSE)
        } else {
          output$download_data_user <- download_data(fields_list, data_list, inputs[1], 'PDF', user_data, TRUE, if (identical(inputs[6], "TSP")) TRUE else FALSE, FALSE, FALSE, FALSE, if (input$check_best_value_user == 1) TRUE else FALSE)
        }    
      })
      
    }
    
    enable_buttons(c("random_user", "btn_user", "download_graph_user", "download_graph_zoom_user","download_route_graph_user", "download_route_zoom_graph_user", "format_user", "download_data_user"))
    enable_tabs(c("TSP", "ATSP", "SOP", "TSP - AutoML", "ATSP - AutoML", "SOP - AutoML", "With AutoML"))
  })
  
  clear_file_input <- function(){
    file_data$data <- NULL
    reset("file_user")
  }
}
