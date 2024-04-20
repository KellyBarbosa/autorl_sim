server <- function(input, output, session) {
  
  show_images(output, session)
  
  observe({   
    if(input$btn_tsp[1] == 0){
      disable_all_buttons()
    }
        
    if (identical(input$type_user, "TSP") && input$type_data_user == 1) {
      showTab(inputId = "tabs_user", target = "Route Graph")
    } else {
      hideTab(inputId = "tabs_user", target = "Route Graph")
    }
    
    if (identical(input$type_user_automl, "TSP") && input$type_data_user_automl == 1) {
      showTab(inputId = "tabs_user_automl", target = "Route Graph")
    } else {
      hideTab(inputId = "tabs_user_automl", target = "Route Graph")
    }
  })

  observeEvent(input$start, {
    updateNavbarPage(session, "navbar", selected = "TSP")
  })

  observeEvent(input$random_tsp, {
    updateNumericInput(session, "learning_rate_tsp", value = get_random_value())
    updateNumericInput(session, "discount_factor_tsp", value = get_random_value())
    updateNumericInput(session, "e_greedy_tsp", value = get_random_value())
    updateNumericInput(session, "episodes_tsp", value = sample.int(10000, 1))
  })

  observeEvent(input$random_atsp, {
    updateNumericInput(session, "learning_rate_atsp", value = get_random_value())
    updateNumericInput(session, "discount_factor_atsp", value = get_random_value())
    updateNumericInput(session, "e_greedy_atsp", value = get_random_value())
    updateNumericInput(session, "episodes_atsp", value = sample.int(10000, 1))
  })

  observeEvent(input$random_sop, {
    updateNumericInput(session, "learning_rate_sop", value = get_random_value())
    updateNumericInput(session, "discount_factor_sop", value = get_random_value())
    updateNumericInput(session, "e_greedy_sop", value = get_random_value())
    updateNumericInput(session, "episodes_sop", value = sample.int(10000, 1))
  })
  
  observeEvent(input$random_user, {
    updateNumericInput(session, "learning_rate_user", value = get_random_value())
    updateNumericInput(session, "discount_factor_user", value = get_random_value())
    updateNumericInput(session, "e_greedy_user", value = get_random_value())
    updateNumericInput(session, "episodes_user", value = sample.int(10000, 1))
  })
  
  observeEvent(input$navbar, {
    if(identical(input$navbar, "Without AutoML") || identical(input$navbar, "With AutoML")){
      show_modal_user()
    }
  })
  
  observeEvent(input$tab_closed, {
    stopApp()
  })
  
  tsp(input, output, session)
  tsp_automl(input, output, session)
  
  atsp(input, output, session)
  atsp_automl(input, output, session)
  
  sop(input, output, session)
  sop_automl(input, output, session)
  
  tsp_atsp_sop_user(input, output, session)
  tsp_atsp_sop_user_automl(input, output, session)
}