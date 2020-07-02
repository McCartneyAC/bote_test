#' main server of app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @return shiny server
#' @export


server <- function(input, output, session) {



  # Data Input --------------------------------------------------------------

  datasetInput <- reactive({
    infile <- input$FileInput
    if (is.null(infile))
      return(NULL)
    dat<-use(infile$datapath)
    names(dat) <-  gsub(" ", "_", names(dat), fixed = TRUE)
    return(dat)
    #readr::read_csv(infile$datapath)
  })

  # Update elements of UI for features of data input
  observeEvent(datasetInput(), {
    updateSelectInput(session, "responsevar", choices = names(datasetInput()))
  })
  observeEvent(datasetInput(), {
    updateSelectInput(session, "clust", choices = names(datasetInput()))
  })
  observeEvent(datasetInput(), {
    updateSelectInput(session, "indevars", choices = names(datasetInput()))
  })
  observeEvent(indvariable(), {
    updateSelectInput(session, "restricted", choices = indvariable())
  })
  observeEvent(datasetInput(), {
    updateSelectInput(session, "instrument", choices = names(datasetInput()))
  })


  # Data Table --------------------------------------------------------------
  output$reg_data_table = DT::renderDataTable(datasetInput())



  # Describe the Data Set ---------------------------------------------------
  desc <- reactive({
    if (is.null(datasetInput()))
      return(NULL)
    psych::describe(datasetInput(), fast = T) %>%
      add_rownames(var = "Variable") %>%
      mutate(mean = round(mean, 2)) %>%
      mutate(sd = round(sd, 2)) %>%
      mutate(se = round(se, 2)) %>%
      mutate(min = round(min, 2)) %>%
      mutate(max = round(max, 2)) %>%
      mutate(range = round(range, 2))
  })

  # description table (psych::describe)
  output$description =  DT::renderDataTable(desc())


  # Correlation Table -------------------------------------------------------
  output$cors <- renderPlot({
    datasetInput() %>%
      select_if(is_extant) %>%
      select_if(is_numeric) %>%
      sjp.corr(
        data = .,
        sort.corr = T,
        decimals = 2,
        na.deletion = "pairwise",
        show.p = FALSE
      ) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  }, height = 600
  )




  # Generate a Regression Formula -------------------------------------------

  feats <- reactive({
    if (length(input$indevars != 1)) {
      paste(input$indevars, collapse = " + ")
    } else {
      paste(input$indevars)
    }
  })

  regFormula <- reactive({
    if (input$clstr == "Fixed Effects"){
      as.formula(paste(input$responsevar, ' ~ ', feats()," + factor(", cluster_var(), ")"))
    } else {
      as.formula(paste(input$responsevar, ' ~ ', feats()))
    }
  })

  # Make the Formula Visual
  output$regformula<-reactive({
    if (is.null(indvariable()) | is.null(depvariable()))
      return(NULL)
    paste(deparse(regFormula(), width.cutoff = 500), collapse="")
  })

  output$restricted_reg_formula <- reactive({
    if (is.null(indvariable()) | is.null(depvariable()))
      return(NULL)
    paste(deparse(fullformula(), width.cutoff = 500), collapse="")
  })

  # creates the actual model summary object.
  output$model <- renderPrint({
    if (is.null(indvariable()) | is.null(depvariable()))
      return(NULL)
    summary(model())
  })
  # passes model summary object into the Sjplot model summary HTML thing.
  output$tabmodel <- renderUI({
    modeltab <- tab_model(model())
    HTML(modeltab$knitr)
  })

  # Clustering Issues

  cluster_var <- reactive({
    if (input$clstr == "No Clustering") {
      NULL
    } else {
      input$clust
    }
  })

  # Model Building
  linear <- reactive ({
    if (input$rbst & (input$clstr == "Cluster Standard Errors")) {
      lm_robust(regFormula(), clusters = cluster_var(), data = datasetInput())
    } else {
      lm(regFormula(), data = datasetInput())
    }
  })

  logistic <- reactive({
    if (input$rbst) {
      robust::glmRob(
        regFormula(),
        data = datasetInput(),
        family = binomial(),
        method = "cubif"
      )
    } else {
      glm(regFormula(), data = datasetInput(), family = "binomial")
    }
  })


  model <- reactive({
    if (input$rgrssn == "logistic") {
      logistic()
    } else {
      linear()
    }
  })


  # Plots (all of them) -----------------------------------------------------

  # Variables Plots
  output$var_matrix <-renderPlot(
    datasetInput() %>%
      select_if(is_extant) %>%
      select_if(is_numeric) %>%
      gather(key = "var", value = "value", reg_variables_choice()) %>%
      ggplot(aes(x = value)) +
      geom_point(aes_string(y = reg_variables_choice())) +
      facet_wrap(~ var, scales = "free") +
      theme_light()
  )

  # Marginal Effects Plot:
  #
  #   if(is.null(datasetInput())){
  #     return(NULL)
  #   }
  # else{
  output$marginal <- renderPlot(
    plot_model(model(), vline.color = "grey", show.values = TRUE, value.offset = .3)+
      theme_light()
  )

  #}




  # can this be exported to a sourced .R file to clean up the code?
  xrange <- reactive({
    datasetInput() %>%
      select_(input$indevars) %>%
      range()
  })
  yrange <- reactive({
    datasetInput() %>%
      select_(input$responsevar) %>%
      range()
  })
  indvariable <- reactive({
    input$indevars
  })
  indvariable1<-reactive({
    input$indevars[1]
  })
  indvariable2<-reactive({
    input$indevars[2]
  })
  depvariable <- reactive({
    input$responsevar
  })
  model_predicted <- reactive({
    predict(model())   # Save the predicted values
  })
  model_residuals <-  reactive({
    residuals(model()) # Save the residual values
  })
  reg_variables_choice <- reactive({
    input$reg_variables_choice
  })



  # Dossier Code
  reg_dossier_id <- reactive({
    input$reg_dossier_ID
  })
  reg_dossier_choice <- reactive({
    input$reg_dossier_choice
  })
  id_var <- reactive({
    if (is.null(datasetInput())) {
      return(NULL)
    } else {
      datasetInput() %>%
        dplyr::select(reg_dossier_id()) %>%
        as.list()
    }
  })
  observeEvent(datasetInput(), {
    updateSelectInput(session, "reg_dossier_ID", choices = names(datasetInput()))
  })
  observeEvent(reg_dossier_id(), {
    updateSelectInput(session, "reg_dossier_choice", choices = id_var())
  })

  output$dossier <- DT::renderDataTable(
    datasetInput() %>%
      filter(reg_dossier_id() == reg_dossier_choice()) %>%
      t()  %>%
      as.data.frame() %>%
      as_tibble() %>%
      tibble::rownames_to_column(var = "variable")
  )


  #refactor this you fool.
  output$bivariate <- renderPlot(if (input$rgrssn == "linear") {
    #why won't this let me refactor it ????
    datasetInput() %>%
      ggplot(aes_string(x = indvariable1(), y = depvariable())) +
      geom_point() +
      geom_smooth(method = "lm") +
      theme_light()
  } else if (input$rgrssn == "logistic") {
    datasetInput() %>%
      ggplot(aes_string(x = indvariable1(), y = depvariable())) +
      geom_point() +
      geom_smooth(method = "glm",
                  method.args = list(family = "binomial")) +
      theme_light()
  } else {
    print(NULL)
  })

  output$bivar_resid <-  renderPlot(if (input$rgrssn == "linear") {
    datasetInput() %>%
      ggplot(aes_string(x = indvariable1(), y = model_residuals())) +
      geom_point() +
      geom_smooth(method = "lm") +
      theme_light()
  } else if (input$rgrssn == "logistic") {
    datasetInput() %>%
      ggplot(aes_string(x = indvariable1(), y = depvariable())) +
      geom_point() +
      annotate("text", x = mean(xrange()), y = mean(yrange()), label = "Error is not normally distributed in logistic regression.") +
      theme_light()
  } else {
    print(NULL)
  })

  output$trivariate <- renderPlot(if (input$rgrssn == "linear") {
    datasetInput() %>%
      ggplot(aes_string(
        x = indvariable1(),
        y = depvariable()
      )) +
      geom_point(alpha = 0.6, aes_string(color = indvariable2())) +
      theme_light() +
      geom_smooth(method = "lm")
  } else if (input$rgrssn == "logistic") {
    datasetInput() %>%
      ggplot(aes_string(
        x = indvariable1(),
        y = depvariable()
      )) +
      geom_point(alpha = 0.6, aes_string(color = indvariable2())) +
      geom_smooth(method = "glm",
                  method.args = list(family = "binomial")) +
      theme_light()
  } else {
    print(NULL)
  })

  # DIagnostic Plots! library(Lindia)

  output$hist_resid <-renderPlot(
    gg_reshist(model())+
      theme_light()
  )

  output$resid_v_fitted<-renderPlot(
    gg_resfitted(model())+
      theme_light()
  )

  output$normal_qq<-renderPlot(
    gg_qqplot(model())+
      theme_light()
  )


  output$cooks_d <- renderPlot(
    gg_cooksd(model()) +
      theme_light()
  )

  # AV PLOT CONSTRUCTION
  # Hey COOL : ( mod_vars = all.vars( formula(fit1) )[-1] ) grabs all the X variables.

  restricted_choice <- reactive({
    input$restricted
  })

  rstrctd_list<-reactive({
    indvariable()[indvariable() != restricted_choice()]
  })
  rstrctdfeats<- reactive({
    paste(rstrctd_list(), collapse = " + ")
  })
  fullformula <-  reactive({
    as.formula(paste(depvariable(), ' ~ ', rstrctdfeats()))
  })

  partialformula <-reactive({
    as.formula(paste(restricted_choice(), ' ~ ', rstrctdfeats()))
  })

  # Model for Y ~ all but chosen X val
  partiallinear <- reactive ({
    if (input$rbst & (input$clstr == "Cluster Standard Errors")) {
      lm_robust(partialformula(), clusters = cluster_var(), data = datasetInput())
    } else {
      lm(partialformula(), data = datasetInput())
    }
  })
  partiallogistic <- reactive({
    if (input$rbst) {
      robust::glmRob(
        partialformula(),
        data = datasetInput(),
        family = binomial(),
        method = "cubif"
      )
    } else {
      glm(partialformula(), data = datasetInput(), family = "binomial")
    }
  })
  partialdmodel <- reactive({
    if (input$rgrssn == "logistic") {
      partiallogistic()
    } else {
      partiallinear()
    }
  })

  # model for x_chosen ~ all other x values
  fulllinear <- reactive ({
    if (input$rbst & (input$clstr == "Cluster Standard Errors")) {
      lm_robust(fullformula(), clusters = cluster_var(), data = datasetInput())
    } else {
      lm(fullformula(), data = datasetInput())
    }
  })

  fulllogistic <- reactive({
    if (input$rbst) {
      robust::glmRob(
        fullformula(),
        data = datasetInput(),
        family = binomial(),
        method = "cubif"
      )
    } else {
      glm(fullformula(), data = datasetInput(), family = "binomial")
    }
  })


  fullmodel <- reactive({
    if (input$rgrssn == "logistic") {
      fulllogistic()
    } else {
      fulllinear()
    }
  })


  # final choice
  output$avplot <- renderPlot(
    gg_added_var(extended = fullmodel(),  partial= partialdmodel())
  )



  # details for brushing leverage -------------------------------------------

  vals <- reactive({
    keeprows = rep(TRUE, nrow(datasetInput()))
  })


  output$brush_plot <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- bind_cols(datasetInput(), vals$keeprows)
    exclude <- bind_cols(datasetInput(), vals$keeprows)

    ggplot(keep, aes(y = depvariable(), x = indvariable1())) + geom_point() +
      geom_smooth(method = "lm", fullrange = TRUE, color = "black") +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
      coord_cartesian(xlim = xrange() , ylim = yrange())
  })



  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    res <- nearPoints(datasetInput(), input$plot1_click, allRows = TRUE)

    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })

  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(datasetInput(), input$plot1_brush, allRows = TRUE)

    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })

  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(datasetInput()))
  })






}
