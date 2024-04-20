atsp <- function(input, output, session){
  
  atsp_data <- reactiveValues()
  dist_atsp_ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$btn_atsp, {
    disable_buttons(c("random_atsp", "btn_atsp", "download_graph_atsp", "download_graph_zoom_atsp","format_atsp", "download_data_atsp"))
    disable_tabs(c("TSP", "SOP", "TSP - AutoML", "ATSP - AutoML", "SOP - AutoML", "Without AutoML", "With AutoML"))
    
    if(validate(c(input$learning_rate_atsp, input$discount_factor_atsp, input$e_greedy_atsp), input$episodes_atsp )) {
      
      show_loading_modal()
      
      inputs <- c(input$input_problem_name_atsp, input$learning_rate_atsp, input$discount_factor_atsp, input$e_greedy_atsp, input$episodes_atsp)
      
      best_tsplib <- get_best(inputs[1])
      
      result <- process_tsp(get_data(inputs[1]), inputs[1], as.numeric(inputs[2]), as.numeric(inputs[3]), as.numeric(inputs[4]), as.numeric(inputs[5]))
      
      show_loading_modal(FALSE)
      
      atsp_data$dist_graph <- result$dist_graph
      atsp_data$min_distance <- result$min_distance
      atsp_data$avg_distance <- result$avg_distance
      atsp_data$ep_dist_min <- result$ep_dist_min
      atsp_data$time <- result$time
      atsp_data$units <- result$units
      
      # Distance Graph
      output$distance_graph_atsp <- renderPlot({
        plot_dist_graph(atsp_data$dist_graph, inputs[5],inputs[1], "ATSP") +
          coord_cartesian(xlim = dist_atsp_ranges$x, ylim = dist_atsp_ranges$y, expand = FALSE)
      }, res = 100)
      
      observeEvent(input$distance_graph_atsp_dblclick, {
        brush <- input$distance_graph_atsp_brush
        if (!is.null(brush)) {
          dist_atsp_ranges$x <- c(brush$xmin, brush$xmax)
          dist_atsp_ranges$y <- c(brush$ymin, brush$ymax)
          
        } else {
          dist_atsp_ranges$x <- NULL
          dist_atsp_ranges$y <- NULL
        }
      })
      
      output$download_graph_atsp <- downloadHandler(
        filename = function() {
          paste("distance_graph_", inputs[1], ".pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_dist_graph(atsp_data$dist_graph, inputs[5], inputs[1], "ATSP"),
                 device = "pdf")
        }
      )
      
      output$download_graph_zoom_atsp <- downloadHandler(
        filename = function() {
          paste("distance_graph_clipping_", inputs[1], ".pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_dist_graph(atsp_data$dist_graph, inputs[5], inputs[1], "ATSP") +
                   coord_cartesian(xlim = dist_atsp_ranges$x, ylim = dist_atsp_ranges$y, expand = FALSE),
                 device = "pdf")
        }
      )
      
      # Experiment data
      output_text_elements <- c(
        problem_name_atsp = paste("Problem name:", inputs[1]),
        problem_type_atsp = "Problem type: ATSP",
        dist_min_atsp = paste("Minimum distance:", atsp_data$min_distance),
        ep_dist_min_atsp = paste("Episode of minimum distance:", atsp_data$ep_dist_min),
        dist_avg_atsp = paste("Average distance:", round(atsp_data$avg_distance, 2)),
        gamma_atsp = paste("Discount factor:", inputs[3]),
        alpha_atsp = paste("Learning rate:", inputs[2]),
        e_greedy_atsp = paste("E-greedy policy:", inputs[4]),
        episodes_atsp = paste("Number of episodes:", inputs[5]),
        tsplib_atsp = paste("Optimal distance presented by TSPLIB:", best_tsplib),
        error_atsp = paste("Percentage relative error: ", round(relative_error(best_tsplib, atsp_data$min_distance), 2), "%", sep = ""),
        time_atsp = paste("Runtime:", round(atsp_data$time, 2), atsp_data$units)
      )
      
      render_text_elements(output, output_text_elements)
      
      
      fields_list <- c(
        "Problem type",
        "Problem",
        "Average distance",
        "Minimum distance",
        "Episode of minimum distance",
        "Optimal distance presented by TSPLIB",
        "Percentage relative error",
        "Discount factor",
        "Learning rate",
        "E-greedy policy",
        "Number of episodes",
        "Runtime"
      )
      data_list <- c(
        "ATSP",
        inputs[1],
        round(atsp_data$avg_distance, 2),
        atsp_data$min_distance,
        atsp_data$ep_dist_min,
        best_tsplib,
        paste(round(relative_error(best_tsplib, atsp_data$min_distance), 2), "%", sep = ""),
        inputs[3],
        inputs[2],
        inputs[4],
        inputs[5],
        paste(round(atsp_data$time, 2), atsp_data$units)
      )
      
      observeEvent(input$format_atsp, {
        if(identical(input$format_atsp, "Simple PDF report")) { 
          output$download_data_atsp <- download_data(fields_list, data_list, inputs[1], 'PDF', atsp_data, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
        } else if (identical(input$format_atsp, "Simple CSV report")) {
          output$download_data_atsp <- download_data(fields_list, data_list, inputs[1], 'CSV', atsp_data, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
        } else {
          output$download_data_atsp <- download_data(fields_list, data_list, inputs[1], 'PDF', atsp_data, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
        }    
      })
      
    }
    
    enable_buttons(c("random_atsp", "btn_atsp", "download_graph_atsp", "download_graph_zoom_atsp","format_atsp", "download_data_atsp"))
    enable_tabs(c("TSP", "SOP", "TSP - AutoML", "ATSP - AutoML", "SOP - AutoML", "Without AutoML", "With AutoML"))
  })
}