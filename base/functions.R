plot_dist_graph <- function(distance, episodes, problem_name, type) {
  ggplot(data.frame(x = 1:episodes, y = distance),
         aes(x = x, y = y)) +
    geom_line(color = "steelblue", linewidth = 1) +
    labs(x = "Episode", y = "Distance", title = paste(problem_name, "-", type)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), plot.margin = unit(c(0, 1, 0, 1), "cm"))
}

plot_route_graph <- function(data, problem_name, type, distance) {
  ggplot(data.frame(x = data$xRoute, y = data$yRoute), aes(x = x, y = y)) +
    geom_path(color = "blue") +
    geom_point(color = "blue", shape = 19) +
    labs(x = "Latitude", y = "Longitude", title = paste("Best found route for ", problem_name, " - ",type,": ", distance, sep = "")) +
    theme_minimal() +  
    theme(plot.title = element_text(hjust = 0.5))
}

get_data <- function(problem_name) {
  load("base/data.rdata")
  
  table <- data[[problem_name]]
  
  adjusted_data <- data.frame(do.call("rbind", strsplit(as.character(table[[1]]), ",")))
  
  return(adjusted_data)
}

get_best <- function(problem_name){
  load("base/data.rdata")
  
  table <- data$tsplib
  
  adjusted_data <- data.frame(do.call("rbind", strsplit(as.character(table[[1]]), ",")))
  
  value <- adjusted_data[adjusted_data$X1 == problem_name, "X2"]
  
  return(as.numeric(value))
}

relative_error <- function(real_value, experimental_value) {
  value <- abs((real_value - experimental_value)/real_value) * 100
  return(value)
}

show_modal <- function(title_msg, text){
  showModal(modalDialog(
    title = title_msg,
    text,
    footer = modalButton("Close"),
    size = "m",
    easyClose = F
  ))
}

show_loading_modal <- function(display = TRUE) {
  if (display) {
    showModal(modalDialog(
      title = "Experiment in progress",
      "Please wait while we process the data...",
      footer = modalButton("Ok"),
      size = "m",
      easyClose = F
    ))
  } else {
    removeModal()
  }
}

show_modal_user <- function(){
  showModal(modalDialog(
    title = "Data entry",
    size = "l",
    footer = modalButton("Close"),
    easyClose = F,
    fluidPage(
      helpText("Attention, the operation of this module depends entirely on how the data is provided. So, fill in the information carefully!",
               class = "text-danger",
               style = "font-size: 20px;"),
      hr(),
      h4("Pay attention to the following details in the information that will be provided below:"
      ),
      tags$ul(tags$li(
        h5("The separator used is the space.")
      ),
      tags$li(
        h5("No titles are provided in the columns and there are no texts within the data document.")
      ),
      tags$li(
        h5("The decimal separator used is the period.")
      )),
      hr(),
      h4("For data arranged in 'Matrix' format, the following inputs are accepted:"),
      div(class = "text-center",
          renderImage({
            list(src = "img/matriz1.png",
                 contentType = "image/png",
                 width = "500px",
                 height = "400px",
                 alt = "Logos")
          }, deleteFile = F),
          h4("ou"),
          renderImage({
            list(src = "img/matriz2.png",
                 contentType = "image/png",
                 width = "500px",
                 height = "250px",
                 alt = "Logos")
          }, deleteFile = F)),
      div(h4("In the example above, the data belongs to an instance containing 17 vertices."), style = "margin-top: -150px;"),
      hr(),
      h4("For data arranged in 'Euclidean' format, the following inputs are accepted:"),
      fluidRow(class = "text-center",
               column( width = 6,
                       renderImage({
                         list(src = "img/euc2d1.png",
                              contentType = "image/png",
                              width = "180px",
                              height = "400px",
                              alt = "Logos")
                       }, deleteFile = F)
               ),
               column(width = 6,
                      renderImage({
                        list(src = "img/euc2d2.png",
                             contentType = "image/png",
                             width = "180px",
                             height = "400px",
                             alt = "Logos")
                      }, deleteFile = F)
               )
      ),
      div(h4("In the example above, the data belongs to an instance containing 20 vertices.")),
    )
  ))
}

help_text <- function(){
  helpText("Tip: Select a region and double-click to zoom in. Double-click to reset the zoom.",
           style = "color:#5b5b5b;")
}

validate <- function(params, episodes){
  if(episodes <= 1 || is.na(episodes)){
    show_modal("Incorrect information", "Review the provided data and try again!")
    return(FALSE)
  }
  for(p in params) {
    if(is.na(p) || p < 0 || p > 1) {
      show_modal("Incorrect information", "Review the provided data and try again!")
      return(FALSE)
    }
  }
  return(TRUE)
}

validate_user <- function(problem_name, best_value, type, file, matrix_dimension){
  if((best_value <= 0 || is.na(best_value)) || (trimws(problem_name) == "")){
    show_modal("Incorrect information", "Review the provided data and try again!")
    return(FALSE)
  }
  
  if((type == 2) && (matrix_dimension <= 2 || is.na(matrix_dimension))){
    show_modal("Invalid dimension", "The instance must have at least 3 vertices!")
    return(FALSE)
  }
  
  if(is.null(file)) {
    show_modal("File not loaded", "Please insert a file and try again!") 
    return(FALSE)
  } 
  
  if(!identical(tools::file_ext(file$datapath), "txt")){
    show_modal("Extension not allowed", "Please select a .txt file and try again!") 
    return(FALSE)
  }
  
  return(TRUE)
}

render_text_elements <- function(output, elements) {
  lapply(names(elements), function(name) {
    output[[name]] <- renderText({
      if(grepl("tempo", name, fixed=TRUE)){
        if(grepl("secs", elements[name], fixed=TRUE)){
          elements[name] <- gsub("secs", "seconds", elements[name])
        }
        if(grepl("mins", elements[name], fixed=TRUE)){
          elements[name] <- gsub("mins", "minutes", elements[name])
        }
        if(grepl("hours", elements[name], fixed=TRUE)){
          elements[name] <- gsub("hours", "hours", elements[name])
        }
      } 
      elements[name]
    })
  })
}

plot_contour_graph <- function(model) {
  par(mar = c(5.1, 4.1, 1.5, 2.1))
  suppressWarnings({
    contour(model, Gamma ~ Alpha, image = TRUE, img.col = rainbow(100), cex.lab = 1.4, cex.axis = 1.4, labcex = 1.4, xlabs = c("Learning Rate", "Discount Factor"))
  })
}

plot_surface_graph <- function(model) {
  par(mar = c(1, 0, 0, 0))  # mar = c(bottom, left, top, right)
  persp(model, ~Alpha+Gamma, col = rainbow(100), theta = 30, phi = 35, contours = list(z="top", col="blue"), cex.lab = 1.4, cex.axis = 1.4, zlab = "Distance", xlabs = c("Learning Rate", "Discount Factor"))
}

download_rsm_graph <- function(problem_name, model, type){
  if(type == 'CONT'){
    graph_type <- 'contour_plot_'
    func_content <- function(file) {
      pdf(file, width = 6)
      contour(model, Gamma ~ Alpha, image=TRUE, img.col = rainbow(100),cex.lab =1.4, cex.axis=1.4, labcex=1.4, xlabs = c("Learning Rate", "Discount Factor"))
      dev.off()
    }
  } else {
    graph_type <- 'surface_plot_'
    func_content <- function(file) {
      pdf(file, width = 20, height = 20)
      persp(model,~Alpha+Gamma, col=rainbow(100),theta = 30, phi = 35, contours = list(z="top", col="blue"), cex.lab =3, cex.axis=3, zlab = "Distance", xlabs = c("Learning Rate", "Discount Factor"))        
      dev.off()
    }
  }
  
  downloadHandler(
    filename = function() {
      paste(graph_type, problem_name, ".pdf", sep = "")
    },
    content = func_content
  )
}

download_data <- function(col_names, col_data, problem_name, extension, graph_data, plot_dist, plot_route, plot_contour, plot_surface, automl, best_value){
  return(
    downloadHandler(
      filename = function() {
        file_prefix <- if (extension == 'CSV' || !plot_dist) "simple_report_" else "complete_report_"
        file_extension <- if (extension == 'CSV') '.csv' else ( if (automl) '_automl.pdf' else '.pdf' )
        paste(file_prefix, problem_name, file_extension, sep = "")
      },
      content = function(file) { 
        data <- data.frame(
          col_names,
          col_data
        )
        names(data) <- NULL
        if(extension == 'CSV') csv_document(file, data) else pdf_document(file, data, graph_data, plot_dist, plot_route, plot_contour, plot_surface, automl, best_value)
      }
    )
  )
}

pdf_document <- function(file, data, graph_data, plot_dist, plot_route, plot_contour, plot_surface, automl, best_value){
  rmarkdown::render(
    input = "base/report/template.Rmd",
    output_file = file,
    params = list(
      data_table = data,
      graph_data = graph_data,
      plot_dist = plot_dist,
      plot_route = plot_route, 
      plot_contour = plot_contour, 
      plot_surface = plot_surface,
      automl = automl,
      best_value = best_value
    )
  )
}

csv_document <- function(file, data){
  write.csv(data, file, row.names = FALSE)
}

normal_residuals <- function(){
  paste("The residues were normal. Therefore, the parameters used were generated through the response surface methodology.")
}

non_normal_residuals <- function(){
  paste("The residues were not normal. Therefore, the parameters used were the ones that showed the best result during the initial learning phase.")
}

disable_buttons <- function(elements){
  for (element in elements) {
    disable(element)
  }
}

enable_buttons <- function(elements){
  for (element in elements) {
    enable(element)
  }
}

disable_tabs <- function(elements){
  for (element in elements) {
    disable(selector =  paste0('.navbar-nav a[data-value="', element, '"], .navmenu a[data-value="', element, '"]'))
  }
}

enable_tabs <- function(elements){
  for (element in elements) {
    enable(selector =  paste0('.navbar-nav a[data-value="', element, '"], .navmenu a[data-value="', element, '"]'))
  }
}

disable_all_buttons <- function(){
  disable(selector = ".shiny-download-link")
}

get_random_value <- function(){
  value <- round(runif(1), 3)
  if(value != 0) return(value) else get_random_value()
}

parse_matrix_line <- function(content, size) {
  data <- as.numeric(strsplit(content, " ")[[1]])
  data <- data[!is.na(data)] 
  return(matrix(data, nrow=size, ncol=size, byrow=TRUE))
}

show_images <- function(output, session) {
  output$logo <- renderImage({
    list(src = "img/logo.png",
         contentType = "image/png",
         width = session$clientData$output_image1_width,
         height = session$clientData$output_image1_height,
         alt = "Logo")
  }, deleteFile = F)
  
  output$route <- renderImage({
    list(src = "img/route.png",
         contentType = "image/png",
         width = session$clientData$output_image1_width,
         height = session$clientData$output_image1_height,
         alt = "Rota")
  }, deleteFile = F)
  
  output$footer <- renderImage({
    list(src = "img/footer.png",
         contentType = "image/png",
         width = "425px",
         height = "150px",
         alt = "Logos")
  }, deleteFile = F)
}

run_app <- function(){
  runIn <- readline(prompt = "Where do you want to open the application (RStudio/Browser)? ")

  if(runIn == "Browser"){
    runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))
  } else if (runIn == "RStudio") {
    runGadget(ui, server, viewer = dialogViewer("AutoRL - Sim", width = 1200, height = 800))
  } else {
    run_app()
  }
}