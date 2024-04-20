sop_automl <- function(input, output, session){
  
  sop_data_automl <- reactiveValues()
  dist_sop_ranges_automl <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$btn_sop_automl, {
    
    show_loading_modal()
    
    disable_buttons(c("btn_sop_automl", "download_graph_sop_automl", "download_graph_zoom_sop_automl", "download_graph_sop_contour_automl", "download_graph_sop_surface_automl", "format_sop_automl","download_data_sop_automl"))
    disable_tabs(c("TSP", "ATSP", "SOP", "TSP - AutoML", "ATSP - AutoML", "Without AutoML", "With AutoML"))
    
    input_automl <- c(input$input_problem_name_sop_automl)
    
    best_tsplib <- get_best(input_automl[1])
    
    result <- process_sop_automl(get_data(input_automl[1]))
    
    show_loading_modal(FALSE)
    
    sop_data_automl$episodios <- result$episodios
    sop_data_automl$dist_graph <- result$dist_graph
    sop_data_automl$alpha <- result$alpha
    sop_data_automl$gamma <- result$gamma
    sop_data_automl$e_greedy <- result$e_greedy
    sop_data_automl$min_distance <- result$min_distance
    sop_data_automl$avg_distance <- result$avg_distance
    sop_data_automl$ep_dist_min <- result$ep_dist_min
    sop_data_automl$epoch_min_dist <- result$epoch_min_dist
    sop_data_automl$epocas <- result$epocas
    sop_data_automl$model <- result$model
    sop_data_automl$time <- result$time
    sop_data_automl$units <- result$units
    sop_data_automl$df <- result$df
    sop_data_automl$normal <- result$normal
    sop_data_automl$model_summary <- result$model_summary
    sop_data_automl$anova_data <- result$anova_data
    sop_data_automl$pks <- result$pks
    
    # Distance Graph
    output$distance_graph_sop_automl <- renderPlot({
      plot_dist_graph(sop_data_automl$dist_graph, sop_data_automl$episodios, input_automl[1], "SOP") +
        coord_cartesian(xlim = dist_sop_ranges_automl$x, ylim = dist_sop_ranges_automl$y, expand = FALSE)
    }, res = 100)
    
    observeEvent(input$distance_graph_sop_automl_dblclick, {
      brush <- input$distance_graph_sop_automl_brush
      if (!is.null(brush)) {
        dist_sop_ranges_automl$x <- c(brush$xmin, brush$xmax)
        dist_sop_ranges_automl$y <- c(brush$ymin, brush$ymax)
        
      } else {
        dist_sop_ranges_automl$x <- NULL
        dist_sop_ranges_automl$y <- NULL
      }
    })
    
    output$download_graph_sop_automl <- downloadHandler(
      filename = function() {
        paste("distance_graph_", input_automl[1], "_automl.pdf", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_dist_graph(sop_data_automl$dist_graph, sop_data_automl$episodios, input_automl[1], "SOP"),
               device = "pdf")
      }
    )
    
    output$download_graph_zoom_sop_automl <- downloadHandler(
      filename = function() {
        paste("distance_graph_clipping_", input_automl[1], "_automl.pdf", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_dist_graph(sop_data_automl$dist_graph, sop_data_automl$episodios, input_automl[1], "SOP") + 
                 coord_cartesian(xlim = dist_sop_ranges_automl$x, ylim = dist_sop_ranges_automl$y, expand = FALSE),
               device = "pdf")
      }
    )
    
    # Contour graph      
    output$graph_sop_contour_automl <- renderPlot({
      plot_contour_graph(sop_data_automl$model)
    })
    
    output$download_graph_sop_contour_automl <- download_rsm_graph(input_automl[1], sop_data_automl$model, 'CONT')
    
    
    # Surface graph
    output$graph_sop_surface_automl <- renderPlot({
      plot_surface_graph(sop_data_automl$model)
    })
    
    output$download_graph_sop_surface_automl <- download_rsm_graph(input_automl[1], sop_data_automl$model, 'SUP')
    
    # Experiment data
    output_text_elements <- c(
      problem_name_sop_automl = paste("Problem name:", input_automl[1]),
      problem_type_sop_automl = "Problem type: SOP",
      dist_min_sop_automl = paste("Minimum distance:", sop_data_automl$min_distance),
      ep_dist_min_sop_automl = paste("Episode of minimum distance:", sop_data_automl$ep_dist_min),
      epoch_min_dist_sop_automl = paste("Epoch of minimum distance:", sop_data_automl$epoch_min_dist),
      dist_avg_sop_automl = paste("Average distance:", round(sop_data_automl$avg_distance, 2)),
      gamma_sop_automl = paste("Discount factor:", round(sop_data_automl$gamma, 2)),
      alpha_sop_automl = paste("Learning rate:", round(sop_data_automl$alpha, 2)),
      e_greedy_sop_automl = paste("E-greedy policy:", sop_data_automl$e_greedy),
      episodes_sop_automl = paste("Number of episodes:", sop_data_automl$episodios),
      epochs_sop_automl = paste("Number of epochs:", sop_data_automl$epocas),
      tsplib_sop_automl = paste("Optimal distance presented by TSPLIB:", best_tsplib),
      error_sop_automl = paste("Percentage relative error: ", round(relative_error(best_tsplib, sop_data_automl$min_distance), 2), "%", sep = ""),
      time_sop_automl = paste("Runtime:", round(sop_data_automl$time, 2), sop_data_automl$units)
    )
    
    output$model_summary_sop_automl <- renderPrint({
      cat('Summary:\n')
      print(sop_data_automl$model_summary)
    })
    
    output$anova_data_sop_automl <- renderPrint({
      cat('Anova:\n\n')
      print(sop_data_automl$anova_data)
    })
    
    output$pks_sop_automl <- renderPrint({
      cat('Kolmogorov-Smirnov test:\n')
      print(sop_data_automl$pks)
    })
    
    render_text_elements(output, output_text_elements)
    
    output$info_sop_automl <- renderText({
      if (sop_data_automl$normal) normal_residuals() else non_normal_residuals()
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
      "SOP",
      input_automl[1],
      round(sop_data_automl$avg_distance, 2),
      sop_data_automl$min_distance,
      sop_data_automl$ep_dist_min,
      sop_data_automl$epoch_min_dist,
      best_tsplib,
      paste(round(relative_error(best_tsplib, sop_data_automl$min_distance), 2), "%", sep = ""),
      round(sop_data_automl$gamma, 2),
      round(sop_data_automl$alpha, 2),
      sop_data_automl$e_greedy,
      sop_data_automl$episodios,
      sop_data_automl$epocas,
      paste(round(sop_data_automl$time, 2), sop_data_automl$units)
    )
    
    observeEvent(input$format_sop_automl, {
      if(identical(input$format_sop_automl, "Simple report")) { 
        output$download_data_sop_automl <- download_data(fields_list, data_list, input_automl[1], 'PDF', sop_data_automl, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
      } else {
        output$download_data_sop_automl <- download_data(fields_list, data_list, input_automl[1], 'PDF', sop_data_automl, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
      }    
    })
    
    enable_buttons(c("btn_sop_automl", "download_graph_sop_automl", "download_graph_zoom_sop_automl", "download_graph_sop_contour_automl", "download_graph_sop_surface_automl", "format_sop_automl","download_data_sop_automl"))
    enable_tabs(c("TSP", "ATSP", "SOP", "TSP - AutoML", "ATSP - AutoML", "Without AutoML", "With AutoML"))
  })
}