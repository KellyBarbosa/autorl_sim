tsp <- function(input, output, session){
  tsp_data <- reactiveValues()
  dist_tsp_ranges <- reactiveValues(x = NULL, y = NULL)
  tsp_route_ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$btn_tsp, {
    disable_buttons(c("random_tsp", "btn_tsp", "download_graph_tsp", "download_graph_zoom_tsp","download_route_graph_tsp", "download_route_zoom_graph_tsp", "format_tsp", "download_data_tsp"))
    disable_tabs(c("ATSP", "SOP", "TSP - AutoML", "ATSP - AutoML", "SOP - AutoML", "Without AutoML", "With AutoML"))
    
    if(validate(c(input$learning_rate_tsp, input$discount_factor_tsp, input$e_greedy_tsp), input$episodes_tsp)) {
      
      show_loading_modal()
      
      inputs <- c(input$input_problem_name_tsp, input$learning_rate_tsp, input$discount_factor_tsp, input$e_greedy_tsp, input$episodes_tsp)
      
      best_tsplib <- get_best(inputs[1])
      
      result <- process_tsp(get_data(tolower(inputs[1])), inputs[1], as.numeric(inputs[2]), as.numeric(inputs[3]), as.numeric(inputs[4]), as.numeric(inputs[5]))
      
      show_loading_modal(FALSE)
      
      tsp_data$dist_graph <- result$dist_graph
      tsp_data$min_distance <- result$min_distance
      tsp_data$avg_distance <- result$avg_distance
      tsp_data$ep_dist_min <- result$ep_dist_min
      tsp_data$xRoute <- result$xRoute
      tsp_data$yRoute <- result$yRoute
      tsp_data$time <- result$time
      tsp_data$units <- result$units
      
      # Route Graph
      output$route_graph_tsp <- renderPlot({
        plot_route_graph(tsp_data, inputs[1], "TSP", tsp_data$min_distance) +
          coord_cartesian(xlim = tsp_route_ranges$x, ylim = tsp_route_ranges$y, expand = FALSE)
      }, res = 100)
      
      observeEvent(input$route_graph_tsp_dblclick, {
        brush <- input$route_graph_tsp_brush
        if (!is.null(brush)) {
          tsp_route_ranges$x <- c(brush$xmin, brush$xmax)
          tsp_route_ranges$y <- c(brush$ymin, brush$ymax)
          
        } else {
          tsp_route_ranges$x <- NULL
          tsp_route_ranges$y <- NULL
        }
      })
      
      output$download_route_graph_tsp <- downloadHandler(
        filename = function() {
          paste("route_graph_", inputs[1], ".pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_route_graph(tsp_data, inputs[1], "TSP", tsp_data$min_distance),
                 device = "pdf")
        }
      )
      
      output$download_route_zoom_graph_tsp <- downloadHandler(
        filename = function() {
          paste("route_graph_clipping_", inputs[1], ".pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_route_graph(tsp_data, inputs[1], "TSP", tsp_data$min_distance) +
                   coord_cartesian(xlim = tsp_route_ranges$x, ylim = tsp_route_ranges$y, expand = FALSE),
                 device = "pdf")
        }
      )
      
      # Distance Graph
      output$distance_graph_tsp <- renderPlot({
        plot_dist_graph(tsp_data$dist_graph, inputs[5], inputs[1], "TSP") +
          coord_cartesian(xlim = dist_tsp_ranges$x, ylim = dist_tsp_ranges$y, expand = FALSE)
      }, res = 100)
      
      observeEvent(input$distance_graph_tsp_dblclick, {
        brush <- input$distance_graph_tsp_brush
        if (!is.null(brush)) {
          dist_tsp_ranges$x <- c(brush$xmin, brush$xmax)
          dist_tsp_ranges$y <- c(brush$ymin, brush$ymax)
          
        } else {
          dist_tsp_ranges$x <- NULL
          dist_tsp_ranges$y <- NULL
        }
      })
      
      output$download_graph_tsp <- downloadHandler(
        filename = function() {
          paste("distance_graph_", inputs[1], ".pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_dist_graph(tsp_data$dist_graph, inputs[5], inputs[1], "TSP"),
                 device = "pdf")
        }
      )
      
      output$download_graph_zoom_tsp <- downloadHandler(
        filename = function() {
          paste("distance_graph_clipping_", inputs[1], ".pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_dist_graph(tsp_data$dist_graph, inputs[5], inputs[1], "TSP") + 
                   coord_cartesian(xlim = dist_tsp_ranges$x, ylim = dist_tsp_ranges$y, expand = FALSE),
                 device = "pdf")
        }
      )
      
      # Experiment data
      output_text_elements <- c(
        problem_name_tsp = paste("Problem name:", inputs[1]),
        problem_type_tsp = "Problem type: TSP",
        dist_min_tsp = paste("Minimum distance:", tsp_data$min_distance),
        ep_dist_min_tsp = paste("Episode of minimum distance:", tsp_data$ep_dist_min),
        dist_avg_tsp = paste("Average distance:", round(tsp_data$avg_distance, 2)),
        gamma_tsp = paste("Discount factor:", inputs[3]),
        alpha_tsp = paste("Learning rate:", inputs[2]),
        e_greedy_tsp = paste("E-greedy policy:", inputs[4]),
        episodes_tsp = paste("Number of episodes:", inputs[5]),
        tsplib_tsp = paste("Optimal distance presented by TSPLIB:", best_tsplib),
        error_tsp = paste("Percentage relative error: ", round(relative_error(best_tsplib, tsp_data$min_distance), 2), "%", sep = ""),
        time_tsp = paste("Runtime:", round(tsp_data$time, 2), tsp_data$units)
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
        "TSP",
        inputs[1],
        round(tsp_data$avg_distance, 2),
        tsp_data$min_distance,
        tsp_data$ep_dist_min,
        best_tsplib,
        paste(round(relative_error(best_tsplib, tsp_data$min_distance), 2), "%", sep = ""),
        inputs[3],
        inputs[2],
        inputs[4],
        inputs[5],
        paste(round(tsp_data$time, 2), tsp_data$units)
      )
      
      observeEvent(input$format_tsp, {
        if(identical(input$format_tsp, "Simple PDF report")) { 
          output$download_data_tsp <- download_data(fields_list, data_list, inputs[1], 'PDF', tsp_data, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
        } else if (identical(input$format_tsp, "Simple CSV report")) {
          output$download_data_tsp <- download_data(fields_list, data_list, inputs[1], 'CSV', tsp_data, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
        } else {
          output$download_data_tsp <- download_data(fields_list, data_list, inputs[1], 'PDF', tsp_data, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)
        }    
      })
      
    }
    
    enable_buttons(c("random_tsp", "btn_tsp", "download_graph_tsp", "download_graph_zoom_tsp","download_route_graph_tsp", "download_route_zoom_graph_tsp", "format_tsp", "download_data_tsp"))
    enable_tabs(c("ATSP", "SOP", "TSP - AutoML", "ATSP - AutoML", "SOP - AutoML", "Without AutoML", "With AutoML"))
  })
  
}