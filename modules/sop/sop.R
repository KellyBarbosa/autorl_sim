sop <- function(input, output, session){
  sop_data <- reactiveValues()
  dist_sop_ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$btn_sop, {
    disable_buttons(c("random_sop", "btn_sop", "download_graph_sop", "download_graph_zoom_sop","format_sop", "download_data_sop"))
    disable_tabs(c("TSP", "ATSP", "TSP - AutoML", "ATSP - AutoML", "SOP - AutoML", "Without AutoML", "With AutoML"))
    
    if(validate(c(input$learning_rate_sop, input$discount_factor_sop, input$e_greedy_sop), input$episodes_sop)) {
      
      show_loading_modal()
      
      inputs <- c(input$input_problem_name_sop, input$learning_rate_sop, input$discount_factor_sop, input$e_greedy_sop, input$episodes_sop)
      
      best_tsplib <- get_best(inputs[1])
      
      result <- process_sop(get_data(inputs[1]), as.numeric(inputs[2]), as.numeric(inputs[3]), as.numeric(inputs[4]), as.numeric(inputs[5]))
      
      show_loading_modal(FALSE)
      
      sop_data$dist_graph <- result$dist_graph
      sop_data$min_distance <- result$min_distance
      sop_data$avg_distance <- result$avg_distance
      sop_data$ep_dist_min <- result$ep_dist_min
      sop_data$time <- result$time
      sop_data$units <- result$units 
      
      # Distance Graph
      output$distance_graph_sop <- renderPlot({
        plot_dist_graph(sop_data$dist_graph, inputs[5], inputs[1], "SOP") +
          coord_cartesian(xlim = dist_sop_ranges$x, ylim = dist_sop_ranges$y, expand = FALSE)
      }, res = 100)
      
      observeEvent(input$distance_graph_sop_dblclick, {
        brush <- input$distance_graph_sop_brush
        if (!is.null(brush)) {
          dist_sop_ranges$x <- c(brush$xmin, brush$xmax)
          dist_sop_ranges$y <- c(brush$ymin, brush$ymax)
          
        } else {
          dist_sop_ranges$x <- NULL
          dist_sop_ranges$y <- NULL
        }
      })
      
      output$download_graph_sop <- downloadHandler(
        filename = function() {
          paste("distance_graph_", inputs[1], ".pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_dist_graph(sop_data$dist_graph, inputs[5], inputs[1], "SOP"),
                 device = "pdf")
        }
      )
      
      output$download_graph_zoom_sop <- downloadHandler(
        filename = function() {
          paste("distance_graph_clipping_", inputs[1], ".pdf", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot_dist_graph(sop_data$dist_graph, inputs[5], inputs[1], "SOP") +
                   coord_cartesian(xlim = dist_sop_ranges$x, ylim = dist_sop_ranges$y, expand = FALSE),
                 device = "pdf")
        }
      )
      
      # Experiment data
      output_text_elements <- c(
        problem_name_sop = paste("Problem name:", inputs[1]),
        problem_type_sop = "Problem type: SOP",
        dist_min_sop = paste("Minimum distance:", sop_data$min_distance),
        ep_dist_min_sop = paste("Episode of minimum distance:", sop_data$ep_dist_min),
        dist_avg_sop = paste("Average distance:", round(sop_data$avg_distance, 2)),
        gamma_sop = paste("Discount factor:", inputs[3]),
        alpha_sop = paste("Learning rate:", inputs[2]),
        e_greedy_sop = paste("E-greedy policy:", inputs[4]),
        episodes_sop = paste("Number of episodes:", inputs[5]),
        tsplib_sop = paste("Optimal distance presented by TSPLIB:", best_tsplib),
        error_sop = paste("Percentage relative error: ", round(relative_error(best_tsplib, sop_data$min_distance), 2), "%", sep = ""),
        time_sop = paste("Runtime:", round(sop_data$time, 2), sop_data$units)
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
        "SOP",
        inputs[1],
        round(sop_data$avg_distance, 2),
        sop_data$min_distance,
        sop_data$ep_dist_min,
        best_tsplib,
        paste(round(relative_error(best_tsplib, sop_data$min_distance), 2), "%", sep = ""),
        inputs[3],
        inputs[2],
        inputs[4],
        inputs[5],
        paste(round(sop_data$time, 2), sop_data$units)
      )
      
      observeEvent(input$format_sop, {
        if(identical(input$format_sop, "Simple PDF report")) { 
          output$download_data_sop <- download_data(fields_list, data_list, inputs[1], 'PDF', sop_data, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
        } else if (identical(input$format_sop, "Simple CSV report")) {
          output$download_data_sop <- download_data(fields_list, data_list, inputs[1], 'CSV', sop_data, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
        } else {
          output$download_data_sop <- download_data(fields_list, data_list, inputs[1], 'PDF', sop_data, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
        }    
      })
      
      
    }
    
    enable_buttons(c("random_sop", "btn_sop", "download_graph_sop", "download_graph_zoom_sop","format_sop", "download_data_sop"))
    enable_tabs(c("TSP", "ATSP", "TSP - AutoML", "ATSP - AutoML", "SOP - AutoML", "Without AutoML", "With AutoML"))
  })
  
}