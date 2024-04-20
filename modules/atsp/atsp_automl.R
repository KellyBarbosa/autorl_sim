atsp_automl <- function(input, output, session){
  atsp_data_automl <- reactiveValues()
  dist_atsp_ranges_automl <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$btn_atsp_automl, {
    
    show_loading_modal()
    
    disable_buttons(c("btn_atsp_automl", "download_graph_atsp_automl", "download_graph_zoom_atsp_automl", "download_graph_atsp_contour_automl", "download_graph_atsp_surface_automl", "format_atsp_automl","download_data_atsp_automl"))
    disable_tabs(c("TSP", "ATSP", "SOP", "TSP - AutoML", "SOP - AutoML", "Without AutoML", "With AutoML"))
    
    input_automl <- c(input$input_problem_name_atsp_automl)
    
    best_tsplib <- get_best(input_automl[1])
    
    result <- process_tsp_automl(get_data(input_automl[1]), input_automl[1])
    
    show_loading_modal(FALSE)
    
    atsp_data_automl$episodes <- result$episodes
    atsp_data_automl$dist_graph <- result$dist_graph
    atsp_data_automl$alpha <- result$alpha
    atsp_data_automl$gamma <- result$gamma
    atsp_data_automl$e_greedy <- result$e_greedy
    atsp_data_automl$min_distance <- result$min_distance
    atsp_data_automl$avg_distance <- result$avg_distance
    atsp_data_automl$ep_dist_min <- result$ep_dist_min
    atsp_data_automl$epoch_min_dist <- result$epoch_min_dist
    atsp_data_automl$epochs <- result$epochs
    atsp_data_automl$model <- result$model
    atsp_data_automl$time <- result$time
    atsp_data_automl$units <- result$units 
    atsp_data_automl$df <- result$df
    atsp_data_automl$normal <- result$normal
    atsp_data_automl$model_summary <- result$model_summary
    atsp_data_automl$anova_data <- result$anova_data
    atsp_data_automl$pks <- result$pks
    
    # Distance Graph
    output$distance_graph_atsp_automl <- renderPlot({
      plot_dist_graph(atsp_data_automl$dist_graph, atsp_data_automl$episodes, input_automl[1], "ATSP") +
        coord_cartesian(xlim = dist_atsp_ranges_automl$x, ylim = dist_atsp_ranges_automl$y, expand = FALSE)
    }, res = 100)
    
    observeEvent(input$distance_graph_atsp_automl_dblclick, {
      brush <- input$distance_graph_atsp_automl_brush
      if (!is.null(brush)) {
        dist_atsp_ranges_automl$x <- c(brush$xmin, brush$xmax)
        dist_atsp_ranges_automl$y <- c(brush$ymin, brush$ymax)
        
      } else {
        dist_atsp_ranges_automl$x <- NULL
        dist_atsp_ranges_automl$y <- NULL
      }
    })
    
    output$download_graph_atsp_automl <- downloadHandler(
      filename = function() {
        paste("distance_graph_", input_automl[1], "_automl.pdf", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_dist_graph(atsp_data_automl$dist_graph, atsp_data_automl$episodes, input_automl[1], "ATSP"),
               device = "pdf")
      }
    )
    
    output$download_graph_zoom_atsp_automl <- downloadHandler(
      filename = function() {
        paste("distance_graph_clipping_", input_automl[1], "_automl.pdf", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_dist_graph(atsp_data_automl$dist_graph, atsp_data_automl$episodes, input_automl[1], "ATSP") + 
                 coord_cartesian(xlim = dist_atsp_ranges_automl$x, ylim = dist_atsp_ranges_automl$y, expand = FALSE),
               device = "pdf")
      }
    )
    
    # Contour graph
    output$graph_atsp_contour_automl <- renderPlot({
      plot_contour_graph(atsp_data_automl$model)
    })
    
    output$download_graph_atsp_contour_automl <- download_rsm_graph(input_automl[1], atsp_data_automl$model, 'CONT')
    
    # Surface graph
    output$graph_atsp_surface_automl <- renderPlot({
      plot_surface_graph(atsp_data_automl$model)
    })
    
    output$download_graph_atsp_surface_automl <- download_rsm_graph(input_automl[1], atsp_data_automl$model, 'SUP')
    
    # Experiment data
    output_text_elements <- c(
      problem_name_atsp_automl = paste("Problem name:", input_automl[1]),
      problem_type_atsp_automl = "Problem type: ATSP",
      dist_min_atsp_automl = paste("Minimum distance:", atsp_data_automl$min_distance),
      ep_dist_min_atsp_automl = paste("Episode of minimum distance:", atsp_data_automl$ep_dist_min),
      epoch_min_dist_atsp_automl = paste("Epoch of minimum distance:", atsp_data_automl$epoch_min_dist),
      dist_avg_atsp_automl = paste("Average distance:", round(atsp_data_automl$avg_distance, 2)),
      gamma_atsp_automl = paste("Discount factor:", round(atsp_data_automl$gamma, 2)),
      alpha_atsp_automl = paste("Learning rate:", round(atsp_data_automl$alpha, 2)),
      e_greedy_atsp_automl = paste("E-greedy policy:", atsp_data_automl$e_greedy),
      episodes_atsp_automl = paste("Number of episodes:", atsp_data_automl$episodes),
      epochs_atsp_automl = paste("Number of epochs:", atsp_data_automl$epochs),
      tsplib_atsp_automl = paste("Optimal distance presented by TSPLIB:", best_tsplib),
      error_atsp_automl = paste("Percentage relative error: ", round(relative_error(best_tsplib, atsp_data_automl$min_distance), 2), "%", sep = ""),
      time_atsp_automl = paste("Runtime:", round(atsp_data_automl$time, 2), atsp_data_automl$units)
    )
    
    output$model_summary_atsp_automl <- renderPrint({
      cat('Summary:\n')
      print(atsp_data_automl$model_summary)
    })
    
    output$anova_data_atsp_automl <- renderPrint({
      cat('Anova:\n\n')
      print(atsp_data_automl$anova_data)
    })
    
    output$pks_atsp_automl <- renderPrint({
      cat('Kolmogorov-Smirnov test:\n')
      print(atsp_data_automl$pks)
    })
    
    render_text_elements(output, output_text_elements)
    
    output$info_atsp_automl <- renderText({
      if (atsp_data_automl$normal) normal_residuals() else non_normal_residuals()
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
      "ATSP",
      input_automl[1],
      round(atsp_data_automl$avg_distance, 2),
      atsp_data_automl$min_distance,
      atsp_data_automl$ep_dist_min,
      atsp_data_automl$epoch_min_dist,
      best_tsplib,
      paste(round(relative_error(best_tsplib, atsp_data_automl$min_distance), 2), "%", sep = ""),
      round(atsp_data_automl$gamma, 2),
      round(atsp_data_automl$alpha, 2),
      atsp_data_automl$e_greedy,
      atsp_data_automl$episodes,
      atsp_data_automl$epochs,
      paste(round(atsp_data_automl$time, 2), atsp_data_automl$units)
    )
    
    observeEvent(input$format_atsp_automl, {
      if(identical(input$format_atsp_automl, "Simple report")) { 
        output$download_data_atsp_automl <- download_data(fields_list, data_list, input_automl[1], 'PDF', atsp_data_automl, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
      } else {
        output$download_data_atsp_automl <- download_data(fields_list, data_list, input_automl[1], 'PDF', atsp_data_automl, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
      }    
    })      
    enable_buttons(c("btn_atsp_automl", "download_graph_atsp_automl", "download_graph_zoom_atsp_automl", "download_graph_atsp_contour_automl", "download_graph_atsp_surface_automl","format_atsp_automl","download_data_atsp_automl"))
    enable_tabs(c("TSP", "ATSP", "SOP", "TSP - AutoML", "SOP - AutoML", "Without AutoML", "With AutoML"))
  })
}