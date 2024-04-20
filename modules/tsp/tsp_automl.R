tsp_automl <- function(input, output, session){
  
  tsp_data_automl <- reactiveValues()
  dist_tsp_ranges_automl <- reactiveValues(x = NULL, y = NULL)
  tsp_route_ranges_automl <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$btn_tsp_automl, {
    
    show_loading_modal()
    
    disable_buttons(c("btn_tsp_automl", "download_graph_tsp_automl", "download_graph_zoom_tsp_automl", "download_route_graph_tsp_automl","download_route_zoom_graph_tsp_automl", "format_tsp_automl", "download_data_tsp_automl", "download_graph_tsp_contour_automl", "download_graph_tsp_surface_automl"))
    disable_tabs(c("TSP", "ATSP", "SOP", "ATSP - AutoML", "SOP - AutoML", "Without AutoML", "With AutoML"))
    
    input_automl <- c(input$input_problem_name_tsp_automl)
    
    best_tsplib <- get_best(input_automl[1])
    
    result <- process_tsp_automl(get_data(tolower(input_automl[1])), input_automl[1])
    
    show_loading_modal(FALSE)
    
    tsp_data_automl$episodes <- result$episodes
    tsp_data_automl$dist_graph <- result$dist_graph
    tsp_data_automl$alpha <- result$alpha
    tsp_data_automl$gamma <- result$gamma
    tsp_data_automl$e_greedy <- result$e_greedy
    tsp_data_automl$min_distance <- result$min_distance
    tsp_data_automl$avg_distance <- result$avg_distance
    tsp_data_automl$ep_dist_min <- result$ep_dist_min
    tsp_data_automl$epoch_min_dist <- result$epoch_min_dist
    tsp_data_automl$epochs <- result$epochs
    tsp_data_automl$xRoute <- result$xRoute
    tsp_data_automl$yRoute <- result$yRoute
    tsp_data_automl$model <- result$model
    tsp_data_automl$time <- result$time
    tsp_data_automl$units <- result$units
    tsp_data_automl$df <- result$df
    tsp_data_automl$normal <- result$normal
    tsp_data_automl$model_summary <- result$model_summary
    tsp_data_automl$anova_data <- result$anova_data
    tsp_data_automl$pks <- result$pks
    
    # Route Graph
    output$route_graph_tsp_automl <- renderPlot({
      plot_route_graph(tsp_data_automl, input_automl[1], "TSP", tsp_data_automl$min_distance) +
        coord_cartesian(xlim = tsp_route_ranges_automl$x, ylim = tsp_route_ranges_automl$y, expand = FALSE)
    }, res = 100)
    
    observeEvent(input$route_graph_tsp_automl_dblclick, {
      brush <- input$route_graph_tsp_automl_brush
      if (!is.null(brush)) {
        tsp_route_ranges_automl$x <- c(brush$xmin, brush$xmax)
        tsp_route_ranges_automl$y <- c(brush$ymin, brush$ymax)
        
      } else {
        tsp_route_ranges_automl$x <- NULL
        tsp_route_ranges_automl$y <- NULL
      }
    })
    
    output$download_route_graph_tsp_automl <- downloadHandler(
      filename = function() {
        paste("route_graph_", input_automl[1], "_automl.pdf", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_route_graph(tsp_data_automl, input_automl[1], "TSP", tsp_data_automl$min_distance),
               device = "pdf")
      }
    )
    
    output$download_route_zoom_graph_tsp_automl <- downloadHandler(
      filename = function() {
        paste("route_graph_clipping_", input_automl[1], "_automl.pdf", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_route_graph(tsp_data_automl, input_automl[1], "TSP", tsp_data_automl$min_distance) +
                 coord_cartesian(xlim = tsp_route_ranges_automl$x, ylim = tsp_route_ranges_automl$y, expand = FALSE),
               device = "pdf")
      }
    )
    
    # Distance Graph
    output$distance_graph_tsp_automl <- renderPlot({
      plot_dist_graph(tsp_data_automl$dist_graph, tsp_data_automl$episodes, input_automl[1], "TSP") +
        coord_cartesian(xlim = dist_tsp_ranges_automl$x, ylim = dist_tsp_ranges_automl$y, expand = FALSE)
    }, res = 100)
    
    observeEvent(input$distance_graph_tsp_automl_dblclick, {
      brush <- input$distance_graph_tsp_automl_brush
      if (!is.null(brush)) {
        dist_tsp_ranges_automl$x <- c(brush$xmin, brush$xmax)
        dist_tsp_ranges_automl$y <- c(brush$ymin, brush$ymax)
        
      } else {
        dist_tsp_ranges_automl$x <- NULL
        dist_tsp_ranges_automl$y <- NULL
      }
    })
    
    output$download_graph_tsp_automl <- downloadHandler(
      filename = function() {
        paste("distance_graph_", input_automl[1], "_automl.pdf", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_dist_graph(tsp_data_automl$dist_graph, tsp_data_automl$episodes, input_automl[1], "TSP"),
               device = "pdf")
      }
    )
    
    output$download_graph_zoom_tsp_automl <- downloadHandler(
      filename = function() {
        paste("distance_graph_clipping_", input_automl[1], "_automl.pdf", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_dist_graph(tsp_data_automl$dist_graph, tsp_data_automl$episodes, input_automl[1], "TSP") + 
                 coord_cartesian(xlim = dist_tsp_ranges_automl$x, ylim = dist_tsp_ranges_automl$y, expand = FALSE),
               device = "pdf")
      }
    )
    
    # Contour graph
    output$graph_tsp_contour_automl <- renderPlot({
      plot_contour_graph(tsp_data_automl$model)
    })
    
    output$download_graph_tsp_contour_automl <- download_rsm_graph(input_automl[1], tsp_data_automl$model, 'CONT')
    
    # Surface graph
    output$graph_tsp_surface_automl <- renderPlot({
      plot_surface_graph(tsp_data_automl$model)
    })
    
    output$download_graph_tsp_surface_automl <- download_rsm_graph(input_automl[1], tsp_data_automl$model, 'SUP')
    
    # Experiment data
    output_text_elements <- c(
      problem_name_tsp_automl = paste("Problem name:", input_automl[1]),
      problem_type_tsp_automl = "Problem type: TSP",
      dist_min_tsp_automl = paste("Minimum distance:", tsp_data_automl$min_distance),
      ep_dist_min_tsp_automl = paste("Episode of minimum distance:", tsp_data_automl$ep_dist_min),
      epoch_min_dist_tsp_automl = paste("Epoch of minimum distance:", tsp_data_automl$epoch_min_dist),
      dist_avg_tsp_automl = paste("Average distance:", round(tsp_data_automl$avg_distance, 2)),
      gamma_tsp_automl = paste("Discount factor:", round(tsp_data_automl$gamma, 2)),
      alpha_tsp_automl = paste("Learning rate:", round(tsp_data_automl$alpha, 2)),
      e_greedy_tsp_automl = paste("E-greedy policy:", tsp_data_automl$e_greedy),
      episodes_tsp_automl = paste("Number of episodes:", tsp_data_automl$episodes),
      epochs_tsp_automl = paste("Number of epochs:", tsp_data_automl$epochs),
      tsplib_tsp_automl = paste("Optimal distance presented by TSPLIB:", best_tsplib),
      error_tsp_automl = paste("Percentage relative error: ", round(relative_error(best_tsplib, tsp_data_automl$min_distance), 2), "%", sep = ""),
      time_tsp_automl = paste("Runtime:", round(tsp_data_automl$time, 2), tsp_data_automl$units)
    )
    
    output$model_summary_tsp_automl <- renderPrint({
      cat('Summary:\n')
      print(tsp_data_automl$model_summary)
    })
    
    output$anova_data_tsp_automl <- renderPrint({
      cat('Anova:\n\n')
      print(tsp_data_automl$anova_data)
    })
    
    output$pks_tsp_automl <- renderPrint({
      cat('Kolmogorov-Smirnov test:\n')
      print(tsp_data_automl$pks)
    })
    
    render_text_elements(output, output_text_elements)
    
    output$info_tsp_automl <- renderText({
      if (tsp_data_automl$normal) normal_residuals() else non_normal_residuals()
    })
    
    fields_list <- c(
      "Problem type",
      "Problem",
      "Average distance",
      "Minimum distance",
      "Episode of minimum distance",
      "Epoch of minimum distance",
      "Optimal distance presented by TSPLIB",
      "Percentage relative error",
      "Discount factor",
      "Learning rate",
      "E-greedy policy",
      "Number of episodes",
      "Number of epochs",
      "Runtime"
    )
    
    data_list <- c(
      "TSP",
      input_automl[1],
      round(tsp_data_automl$avg_distance, 2),
      tsp_data_automl$min_distance,
      tsp_data_automl$ep_dist_min,
      tsp_data_automl$epoch_min_dist,
      best_tsplib,
      paste(round(relative_error(best_tsplib, tsp_data_automl$min_distance), 2), "%", sep = ""),
      round(tsp_data_automl$gamma, 2),
      round(tsp_data_automl$alpha, 2),
      tsp_data_automl$e_greedy,
      tsp_data_automl$episodes,
      tsp_data_automl$epochs,
      paste(round(tsp_data_automl$time, 2), tsp_data_automl$units)
    )
    
    observeEvent(input$format_tsp_automl, {
      if(identical(input$format_tsp_automl, "Simple report")) { 
        output$download_data_tsp_automl <- download_data(fields_list, data_list, input_automl[1], 'PDF', tsp_data_automl, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
      } else {
        output$download_data_tsp_automl <- download_data(fields_list, data_list, input_automl[1], 'PDF', tsp_data_automl, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
      }    
    }) 
    
    enable_buttons(c("btn_tsp_automl", "download_graph_tsp_automl", "download_graph_zoom_tsp_automl", "download_route_graph_tsp_automl","download_route_zoom_graph_tsp_automl", "format_tsp_automl", "download_data_tsp_automl", "download_graph_tsp_contour_automl", "download_graph_tsp_surface_automl"))
    enable_tabs(c("TSP", "ATSP", "SOP", "ATSP - AutoML", "SOP - AutoML", "Without AutoML", "With AutoML"))
  })
}