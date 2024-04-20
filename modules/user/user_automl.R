tsp_atsp_sop_user_automl <- function(input, output, session){
  user_data_automl <- reactiveValues()
  file_data_automl <- reactiveValues()
  dist_user_ranges_automl <- reactiveValues(x = NULL, y = NULL)
  user_route_ranges_automl <- reactiveValues(x = NULL, y = NULL)
  
  output$condicional_size_ui_user_automl <- renderUI({
    if (input$type_data_user_automl == 2) {
      numericInput(
        "matrix_size_user_automl",
        "Instance dimension: ",
        3,
        min = 1,
        max = 10000,
        step = 1
      )
    }
  })
  
  output$condicional_best_value_ui_user_automl <- renderUI({
    if (input$check_best_value_user_automl == 1) {
      numericInput(
        "best_value_user_automl",
        "Known optimal value: ",
        1,
        min = 1,
        max = 1000000,
        step = 1
      )
    }
  })
  
  observeEvent(input$type_user_automl, {
    clear_file_input()
  })
  
  observeEvent(input$file_user_automl, {
    file_data_automl$data <- input$file_user_automl
  })
  
  observeEvent(input$btn_user_automl, {
    disable_buttons(c("btn_user_automl", "download_graph_user_automl", "download_graph_zoom_user_automl", "download_route_graph_user_automl","download_route_zoom_graph_user_automl", "format_user_automl", "download_data_user_automl", "download_graph_user_contour_automl", "download_graph_user_surface_automl"))
    disable_tabs(c("TSP", "ATSP", "SOP", "TSP - AutoML", "ATSP - AutoML", "SOP - AutoML", "Without AutoML"))
    
    if (validate_user(input$input_problem_name_user_automl, if (input$check_best_value_user_automl == 1) input$best_value_user_automl else 99999, input$type_data_user_automl, file_data_automl$data, input$matrix_size_user_automl)) {
      
      if (input$check_best_value_user_automl == 1) {
        inputs <- c(input$input_problem_name_user_automl, input$type_user_automl, input$best_value_user_automl)
      } else { 
        inputs <- c(input$input_problem_name_user_automl, input$type_user_automl)
      }
      
      if(input$type_data_user_automl == 2) {
        req(file_data_automl$data)
        req(input$matrix_size_user_automl)
        
        content <- paste(readLines(file_data_automl$data$datapath), collapse = " ") # Lê o arquivo em uma única linha
        
        if (length(content) < 1) {
          return(NULL)
        }
        
        file_data_automl$data <- as.data.frame(parse_matrix_line(content, input$matrix_size_user_automl)) # Converte a linha de texto em matriz
      } else {
        if (is.null(file_data_automl$data)) {
          return(NULL)
        }
        file_data_automl$data <- read.table(file_data_automl$data$datapath, header = FALSE)
      }
      
      show_loading_modal()
      
      if(!identical(inputs[2], "SOP")) {
        result <- process_tsp_atsp_user_automl(file_data_automl$data, if (identical(inputs[2], "TSP")) 'tsp' else 'atsp', if (input$type_data_user_automl == 1) 'euc2d' else 'matrix')
      } else {
        result <- process_sop_automl(file_data_automl$data)
      }
      
      clear_file_input()
      
      show_loading_modal(FALSE)
      
      user_data_automl$episodes <- result$episodes
      user_data_automl$dist_graph <- result$dist_graph
      user_data_automl$alpha <- result$alpha
      user_data_automl$gamma <- result$gamma
      user_data_automl$e_greedy <- result$e_greedy
      user_data_automl$min_distance <- result$min_distance
      user_data_automl$avg_distance <- result$avg_distance
      user_data_automl$ep_dist_min <- result$ep_dist_min
      user_data_automl$epoch_min_dist <- result$epoch_min_dist
      user_data_automl$epochs <- result$epochs
      user_data_automl$xRoute <- result$xRoute
      user_data_automl$yRoute <- result$yRoute
      user_data_automl$model <- result$model
      user_data_automl$time <- result$time
      user_data_automl$units <- result$units
      user_data_automl$df <- result$df
      user_data_automl$normal <- result$normal
      user_data_automl$model_summary <- result$model_summary
      user_data_automl$anova_data <- result$anova_data
      user_data_automl$pks <- result$pks
      
      # Route Graph
      output$route_graph_user_automl <- renderPlot({
        plot_route_graph(user_data_automl, inputs[1], inputs[2], user_data_automl$min_distance) +
          coord_cartesian(xlim = user_route_ranges_automl$x, ylim = user_route_ranges_automl$y, expand = FALSE)
      }, res = 100)
      
      observeEvent(input$route_graph_user_automl_dblclick, {
        brush <- input$route_graph_user_automl_brush
        if (!is.null(brush)) {
          user_route_ranges_automl$x <- c(brush$xmin, brush$xmax)
          user_route_ranges_automl$y <- c(brush$ymin, brush$ymax)
          
        } else {
          user_route_ranges_automl$x <- NULL
          user_route_ranges_automl$y <- NULL
        }
      })
      
      output$download_graph_user_automl <- downloadHandler(
        filename = function() {
          paste("route_graph_", inputs[1], "_automl.pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_route_graph(user_data_automl, inputs[1], inputs[2], user_data_automl$min_distance),
                 device = "pdf")
        }
      )
      
      output$download_graph_zoom_user_automl <- downloadHandler(
        filename = function() {
          paste("route_graph_clipping_", inputs[1], "_automl.pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_route_graph(user_data_automl, inputs[1], inputs[2], user_data_automl$min_distance) +
                   coord_cartesian(xlim = user_route_ranges_automl$x, ylim = user_route_ranges_automl$y, expand = FALSE),
                 device = "pdf")
        }
      )
      
      # Distance Graph
      output$distance_graph_user_automl <- renderPlot({
        plot_dist_graph(user_data_automl$dist_graph, user_data_automl$episodes, inputs[1], inputs[2]) +
          coord_cartesian(xlim = dist_user_ranges_automl$x, ylim = dist_user_ranges_automl$y, expand = FALSE)
      }, res = 100)
      
      observeEvent(input$distance_graph_user_automl_dblclick, {
        brush <- input$distance_graph_user_automl_brush
        if (!is.null(brush)) {
          dist_user_ranges_automl$x <- c(brush$xmin, brush$xmax)
          dist_user_ranges_automl$y <- c(brush$ymin, brush$ymax)
          
        } else {
          dist_user_ranges_automl$x <- NULL
          dist_user_ranges_automl$y <- NULL
        }
      })
      
      output$download_route_graph_user_automl <- downloadHandler(
        filename = function() {
          paste("distance_graph_", inputs[1], "_automl.pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_dist_graph(user_data_automl$dist_graph, user_data_automl$episodes, inputs[1], inputs[2]),
                 device = "pdf")
        }
      )
      
      output$download_route_zoom_graph_user_automl <- downloadHandler(
        filename = function() {
          paste("distance_graph_clipping_", inputs[1], "_automl.pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_dist_graph(user_data_automl$dist_graph, user_data_automl$episodes, inputs[1], inputs[2]) + 
                   coord_cartesian(xlim = dist_user_ranges_automl$x, ylim = dist_user_ranges_automl$y, expand = FALSE),
                 device = "pdf")
        }
      )
      
      # Contour graph
      output$graph_user_contour_automl <- renderPlot({
        plot_contour_graph(user_data_automl$model)
      })
      
      output$download_graph_user_contour_automl <- download_rsm_graph(inputs[1], user_data_automl$model, 'CONT')
      
      # Surface graph
      output$graph_user_surface_automl <- renderPlot({
        plot_surface_graph(user_data_automl$model)
      })
      
      output$download_graph_user_surface_automl <- download_rsm_graph(inputs[1], user_data_automl$model, 'SUP')
      
      # Experiment data
      if (input$check_best_value_user_automl == 1) {
        output_text_elements <- c(
          problem_name_user_automl = paste("Problem name:", inputs[1]),
          problem_type_user_automl = paste("Problem type:", inputs[2]),
          dist_min_user_automl = paste("Minimum distance:", user_data_automl$min_distance),
          ep_dist_min_user_automl = paste("Episode of minimum distance:", user_data_automl$ep_dist_min),
          epoch_min_dist_user_automl = paste("Epoch of minimum distance:", user_data_automl$epoch_min_dist),
          dist_avg_user_automl = paste("Average distance:", round(user_data_automl$avg_distance, 2)),
          gamma_user_automl = paste("Discount factor:", round(user_data_automl$gamma, 2)),
          alpha_user_automl = paste("Learning rate:", round(user_data_automl$alpha, 2)),
          e_greedy_user_automl = paste("E-greedy policy:", user_data_automl$e_greedy),
          episodes_user_automl = paste("Number of episodes:", user_data_automl$episodes),
          epochs_user_automl = paste("Number of epochs:", user_data_automl$epochs),
          otimo_user_automl = paste("Optimal distance:", inputs[3]),
          error_user_automl = paste("Percentage relative error: ", round(relative_error(as.numeric(inputs[3]), user_data_automl$min_distance), 2), "%", sep = ""),
          time_user_automl = paste("Runtime:", round(user_data_automl$time, 2), user_data_automl$units)
        )
        
        fields_list <- c(
          "Problem type",
          "Problem",
          "Average distance",
          "Minimum distance",
          "Episode of minimum distance",
          "Epoch of minimum distance",
          "Optimal distance",
          "Percentage relative error",
          "Discount factor",
          "Learning rate",
          "E-greedy policy",
          "Number of episodes",
          "Number of epochs",
          "Runtime"
        )
        
        data_list <- c(
          inputs[2],
          inputs[1],
          round(user_data_automl$avg_distance, 2),
          user_data_automl$min_distance,
          user_data_automl$ep_dist_min,
          user_data_automl$epoch_min_dist,
          inputs[3],
          paste(round(relative_error(as.numeric(inputs[3]), user_data_automl$min_distance), 2), "%", sep = ""),
          round(user_data_automl$gamma, 2),
          round(user_data_automl$alpha, 2),
          user_data_automl$e_greedy,
          user_data_automl$episodes,
          user_data_automl$epochs,
          paste(round(user_data_automl$time, 2), user_data_automl$units)
        )
      } else {
        output_text_elements <- c(
          problem_name_user_automl = paste("Problem name:", inputs[1]),
          problem_type_user_automl = paste("Problem type:", inputs[2]),
          dist_min_user_automl = paste("Minimum distance:", user_data_automl$min_distance),
          ep_dist_min_user_automl = paste("Episode of minimum distance:", user_data_automl$ep_dist_min),
          epoch_min_dist_user_automl = paste("Epoch of minimum distance:", user_data_automl$epoch_min_dist),
          dist_avg_user_automl = paste("Average distance:", round(user_data_automl$avg_distance, 2)),
          gamma_user_automl = paste("Discount factor:", round(user_data_automl$gamma, 2)),
          alpha_user_automl = paste("Learning rate:", round(user_data_automl$alpha, 2)),
          e_greedy_user_automl = paste("E-greedy policy:", user_data_automl$e_greedy),
          episodes_user_automl = paste("Number of episodes:", user_data_automl$episodes),
          epochs_user_automl = paste("Number of epochs:", user_data_automl$epochs),
          time_user_automl = paste("Runtime:", round(user_data_automl$time, 2), user_data_automl$units)
        )
        
        fields_list <- c(
          "Problem type",
          "Problem",
          "Average distance",
          "Minimum distance",
          "Episode of minimum distance",
          "Epoch of minimum distance",
          "Discount factor",
          "Learning rate",
          "E-greedy policy",
          "Number of episodes",
          "Number of epochs",
          "Runtime"
        )
        
        data_list <- c(
          inputs[2],
          inputs[1],
          round(user_data_automl$avg_distance, 2),
          user_data_automl$min_distance,
          user_data_automl$ep_dist_min,
          user_data_automl$epoch_min_dist,
          round(user_data_automl$gamma, 2),
          round(user_data_automl$alpha, 2),
          user_data_automl$e_greedy,
          user_data_automl$episodes,
          user_data_automl$epochs,
          paste(round(user_data_automl$time, 2), user_data_automl$units)
        )
      }
      
      
      output$model_summary_user_automl <- renderPrint({
        cat('Summary:\n')
        print(user_data_automl$model_summary)
      })
      
      output$anova_data_user_automl <- renderPrint({
        cat('Anova:\n\n')
        print(user_data_automl$anova_data)
      })
      
      output$pks_user_automl <- renderPrint({
        cat('Kolmogorov-Smirnov test:\n')
        print(user_data_automl$pks)
      })
      
      render_text_elements(output, output_text_elements)
      
      output$info_user_automl <- renderText({
        if (user_data_automl$normal) normal_residuals() else non_normal_residuals()
      })
      
      observeEvent(input$format_user_automl, {
        if(identical(input$format_user_automl, "Simple report")) { 
          output$download_data_user_automl <- download_data(fields_list, data_list, inputs[1], 'PDF', user_data_automl, FALSE, FALSE, FALSE, FALSE, TRUE, if (input$check_best_value_user_automl == 1) TRUE else FALSE)
        } else {
          output$download_data_user_automl <- download_data(fields_list, data_list, inputs[1], 'PDF', user_data_automl, TRUE, if (identical(inputs[2], "TSP")) TRUE else FALSE, TRUE, TRUE, TRUE, if (input$check_best_value_user_automl == 1) TRUE else FALSE)
        }    
      }) 
      
    }
    
    enable_buttons(c("btn_user_automl", "download_graph_user_automl", "download_graph_zoom_user_automl", "download_route_graph_user_automl","download_route_zoom_graph_user_automl", "format_user_automl", "download_data_user_automl", "download_graph_user_contour_automl", "download_graph_user_surface_automl"))
    enable_tabs(c("TSP", "ATSP", "SOP", "TSP - AutoML", "ATSP - AutoML", "SOP - AutoML", "Without AutoML"))
  })
  
  clear_file_input <- function(){
    file_data_automl$data <- NULL
    reset("file_user_automl")
  }
}