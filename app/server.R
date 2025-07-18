server <- function(input, output, session) {#
  
  # Help button ----
  observeEvent(input$help, {
    introjs(session, events = list(onbeforechange = readCallback("switchTabs")))
  })

  #### Presentation ----
  
  #** Recap Presentation slides ----
  output$slides <- renderSlickR({
    slickR(recap_slides) + settings(dots = TRUE)
  })
  
  #### Introduction ----
  
  # Downloading Student Handout ----
  
  # Hide download button until report is generated
  handout <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
  output$handoutbuilt <- reactive({
    return(file.exists("report.docx"))
  })
  outputOptions(output, 'handoutbuilt', suspendWhenHidden= FALSE)
  
  handout_file <- "Student_handout.docx"
  
  output$stud_dl <-  downloadHandler(
    filename = function() {
      handout_file
    },
    content = function(file) {
      file.copy("report.docx", file)
    }
  )
  
  #### Activity A ----
  
  # Objective 1 ----
  # Case study sites datatable ----
  output$table01 <- DT::renderDT(
    sites_df[, c(1:3)], selection = "single", options = list(stateSave = TRUE, dom = 't'), server = FALSE
  ) 
  
  observe({
    if(input$row_num != "") {
      dt_proxy <- dataTableProxy("table01")
      selectRows(dt_proxy, input$row_num)
    }
  })
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  siteID <- reactiveValues(lab = NULL)
  
  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  
  
  # Select NEON DT rows ----
  lake_data <- reactiveValues(df = NULL,
                              wtemp = NULL,
                              do = NULL,
                              turb = NULL)
  site_photo_file <- reactiveValues(img = NULL)

  observeEvent(input$table01_rows_selected, {
    
    siteID$lab <- input$table01_rows_selected
    
    row_selected = sites_df[input$table01_rows_selected, ]
    proxy <- leafletProxy('sitemap')
    proxy %>%
      addAwesomeMarkers(layerId = as.character(row_selected$SiteID),
                        lng=row_selected$Longitude,
                        lat=row_selected$Latitude,
                        icon = my_icon)
    
    # Reset previously selected marker
    if(!is.null(prev_row()))
    {
      proxy %>%
        removeMarker(layerId = as.character(prev_row()$SiteID))
    }
    # set new value to reactiveVal
    prev_row(row_selected)
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Loading case study data",
                 detail = "This may take a while. This window will disappear
                     when it is loaded.", value = 0.33)
    
    #retrieve site photooutput$display.image <- renderImage({
    site_photo_file$img <- paste("www/",row_selected$SiteID,".jpg",sep="")
    
    #show site info
    output$site_info <- renderText({
      module_text[row_selected$SiteID, ]
    })
    
    #show site photo caption
    output$site_photo_info <- renderText({
      module_text[paste0(row_selected$SiteID,"_photo"), ]
    })
    
    progress$set(value = 1)
    
  })
  
  # Site map ----
  output$sitemap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = sites_df,
                 layerId = ~SiteID, clusterOptions = markerClusterOptions(),
                 label = ~SiteName, icon = ~siteIcons[1])
    
  })
  
  # Show reservoir image ----
  output$site_photo <- renderImage({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in the table.")
    )
    list(src = site_photo_file$img,
         alt = "Image failed to render.",
         height = 403.2,
         width = 302.4)
  }, deleteFile = FALSE)
  
  # Objective 2
  
  # variable description table
  output$var_desc <- renderDT({
    row_selected = sites_df[input$table01_rows_selected, ]
    var_desc <- site_vars[which(site_vars$site_id == row_selected$SiteID), c("variable_name", "variable_description")]
    colnames(var_desc) <- c("Variable name", "Description")
    datatable(var_desc, rownames = FALSE, options = list(pageLength = 4))
  })
  
  # Read in site data 
  site_data <- reactive({ # view_var
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    row_selected = sites_df[input$table01_rows_selected, ]
    site_id <- row_selected$SiteID
    
    if(site_id == "cann"){
      df <- cann_data
      
      autocorrelation_data <- df %>%
        select(datetime, chla) %>%
        mutate(chla = na.approx(chla, na.rm = F)) %>% 
        mutate(chla_lag = lag(chla)) %>%
        filter(complete.cases(.))
    }
    
    return(list(data = df,
                ac = autocorrelation_data))
  })
  
  # Select variable for plotting/data table
  site_DT <- reactive({ # view_var
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    read_var <- site_vars$variable_id[which(site_vars$variable_name == input$view_var)][1]
    df <- site_data()$data[,c("datetime",read_var)]
    df[, -1] <- signif(df[, -1], 4)
    names(df)[ncol(df)] <- read_var
    
    return(list(data = df))
  })
  
  
  # Site data datatable ----
  output$site_datatable <- DT::renderDT({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    df <- site_DT()$data
   return(df)
  })
  
  # Site data plot ----
  output$var_plot <- renderPlotly({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    units <- site_vars$variable_unit[which(site_vars$variable_name == input$view_var)][1]
    
    p <- ggplot() +
      geom_point(data = site_DT()$data, aes_string(names(site_DT()$data)[1], names(site_DT()$data)[2]), color = "black") +
      ylab(paste0(input$view_var, " (", units, ")")) +
      xlab("Time") +
      theme_minimal(base_size = 12)
    
    return(ggplotly(p, dynamicTicks = TRUE, source = "A"))
    
  })
  
  # Variable description ----
  output$txt_out <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    out_txt <- site_vars$variable_description[which(site_vars$variable_name == input$view_var)][1]
    return(out_txt)
  })
  
  # Output stats ----
  output$out_stats <- renderText({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    sum_stat <- summary(site_DT()$data)
    ridx <- grep(input$stat_calc, sum_stat[, ncol(sum_stat)])
    out_stat <- sum_stat[ridx, ncol(sum_stat)]
    
    return(out_stat)
  })
  
  # Table for stats
  stat_ans <- reactiveValues(dt = stat_table) # %>% formatStyle(c(1:3), border = '1px solid #ddd'))
  
  output$stat_tab <- DT::renderDT(
    stat_ans$dt, 
    selection = "none", class = "cell-border stripe",
    options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t"),
    server = FALSE, escape = FALSE, editable = FALSE
  )
  
  # Comparison plot ----
  output$xy_plot <- renderPlotly({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    validate(
      need(input$x_var != "",
           message = "Please select an X variable.")
    )
    
    row_selected = sites_df[input$table01_rows_selected, ]
    site_id <- row_selected$SiteID
    
    ref <- site_vars$variable_id[which(site_vars$variable_name == input$x_var)][1]
    
    if(site_id == "cann"){
      plot_data <- site_data()$data[,c("datetime",ref,"chla")]
      x_units <- site_vars$variable_unit[which(site_vars$variable_name == input$x_var)]
      target = "chlorophyll-a"
      y_units = "mg/L"
    }
    
    validate(
      need(nrow(plot_data) > 0, message = "No variables at matching timesteps. Please select different  X-Y variables.")
    )
    
    p <- ggplot(plot_data, aes_string(names(plot_data)[2], names(plot_data)[3])) +
      geom_point() +
      xlab(paste0(input$x_var, " (", x_units, ")")) +
      ylab(paste0(target, " (", y_units, ")")) +
      theme_minimal(base_size = 12)
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Table for relationships
  rel_ans <- reactiveValues(dt = rel_table) # %>% formatStyle(c(1:3), border = '1px solid #ddd'))
  
  output$rel_tab <- DT::renderDT(
    rel_ans$dt, 
    selection = "none", class = "cell-border stripe",
    options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t"),
    server = FALSE, escape = FALSE, editable = FALSE
  )
  
  # Objective 3 ----
  
  #** ARIMA model slides ----
  output$model_slides <- renderSlickR({
    slickR(arima_slides) + settings(dots = TRUE)
  })
  
  # Plot 1-day lag
  plot.lag <- reactiveValues(main=NULL)
  
  observe({
    
    output$lag_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(input$plot_lag > 0,
             message = "Click 'Plot lag scatterplot'")
      )
      
      df <- site_data()$ac
      
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        x_lab = "1-day lag of chlorophyll-a (mg/L)"
        y_lab = "chlorophyll-a (mg/L)"
      }
      
      p <- ggplot(data = df, aes(x = chla_lag, y = chla))+
        geom_point()+
        xlab(x_lab)+
        ylab(y_lab)+
        geom_abline(slope = 1, intercept = 0, linetype = 2)+
        theme_bw()
      
      plot.lag$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download scatterplot of lagged chl-a
  output$save_lag_plot <- downloadHandler(
    filename = function() {
      paste("Q8-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.lag$main, device = device)
    }
  )
  
  # PACF plot ----
  plot.pacf <- reactiveValues(main=NULL)
  
  observe({
    
    output$pacf_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(input$plot_pacf > 0,
             message = "Click 'Plot PACF'")
      )
      
      df <- site_data()$ac
      
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        pacf_list <- acf(df$chla, type = c("partial"), plot = FALSE)
      }
      
      pacf_plot_data <- tibble(Lag = pacf_list$lag,
                               Partial_ACF = round(pacf_list$acf, 2))
      
      p <- ggplot(data = pacf_plot_data, aes(x = Lag, y = Partial_ACF))+
        geom_bar(stat = "identity", color = "#446c84", fill = "#cee3f1")+
        xlab("Lag in days")+
        ylab("Partial autocorrelation of target data")+
        theme_bw()
      
      plot.pacf$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download scatterplot of pacf
  output$save_pacf_plot <- downloadHandler(
    filename = function() {
      paste("Q9-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.pacf$main, device = device)
    }
  )
  
  # Differencing plot ----
  plot.diff <- reactiveValues(main=NULL)
  
  observe({
    
    output$diff_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(input$plot_diff > 0,
             message = "Click 'Plot differenced data'")
      )
      
      df <- site_data()$ac
      
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        diff_data <- diff(df$chla)
        y_lab = "chlorophyll-a (mg/L)"
      }
      
      diff_plot_data <- tibble(datetime = df$datetime,
                               undiff = df$chla,
                               diff = c(NA,diff_data)) %>%
        pivot_longer(undiff:diff, names_to = "series_name", values_to = "value") %>%
        mutate(series_name = ifelse(series_name == "undiff","undifferenced data","differenced data"))
      
      p <- ggplot(data = diff_plot_data, aes(x = datetime, y = value, group = series_name, color = series_name))+
        geom_line()+
        xlab("datetime")+
        ylab(y_lab)+
        labs(color = "")+
        theme_bw()+
        scale_color_manual(values = c("undifferenced data" = "#ccd9e0",
                                      "differenced data" = "#0d3658"))
      
      plot.diff$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download scatterplot of diff
  output$save_diff_plot <- downloadHandler(
    filename = function() {
      paste("Q10-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.diff$main, device = device)
    }
  )
  
  # Objective 4 ----
  
  # make multi-select list
  multi.select <- reactiveValues(lst=NULL)
  
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(site_data()$data),
           message = "Please select a site in Objective 1.")
    )
    
    row_selected = sites_df[input$table01_rows_selected, ]
    site_id <- row_selected$SiteID
    
    if(site_id == "cann"){
      selected_site_vars <- site_vars %>%
        filter(site_id == "cann")
      select_list <- unique(selected_site_vars$variable_id)
      names(select_list) = unique(selected_site_vars$variable_name)
    }
    
    multi.select$lst <- select_list[-1]
    
  })
  
  # made header text for multi-select list
  output$dropdown_txt <- renderText({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(site_data()$data),
           message = "Please select a site in Objective 1.")
    )
    
    return("Please select up to 3 predictors from the dropdown menu.")
    
  })
  
  # Update the selectInput with the named list
  observe({
    updateSelectInput(session, "select", choices = multi.select$lst)
  })
  
  
  # Plot of regressors
  plot.stand <- reactiveValues(main=NULL,
                               stand.tracker = 0)
  
  observe({
    
    input$select
    
    output$standardize_plot <- renderPlotly({
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        
        var_names <- site_vars[,c("variable_id","variable_name","variable_unit")]
        
        col_names <- c(input$select)
        
        plot_data <- cann_model_data %>%
          dplyr::slice_head(prop = .7) %>% # using a 70:30 split here
          select(all_of(col_names)) %>%
          pivot_longer(cols = everything(), names_to = "variable_id", values_to = "obs") %>%
          left_join(., site_vars, by = "variable_id") %>%
          mutate(plot_labels = paste(variable_name, paste0("(",variable_unit,")"), sep = " "))
        
      }
      
      p <- ggplot(data = plot_data)+
        geom_density(aes(x = obs, color = plot_labels, fill = plot_labels), alpha = 0.5)+
        facet_wrap(facets = vars(plot_labels), nrow = 1, scales = "free_x")+
        theme_bw()+
        theme(legend.position = "none")+
        xlab("observed value")
      
      plot.stand$main <- p
      plot.stand$stand.tracker <- 0
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  observe({
    
    input$standardize_data
    
    output$standardize_plot <- renderPlotly({
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        
        var_names <- site_vars[,c("variable_id","variable_name","variable_unit")]
        
        col_names <- c(input$select)
        
        plot_data_stand <- cann_model_data %>%
          dplyr::slice_head(prop = .7) %>% # using a 70:30 split here
          select(all_of(col_names)) %>%
          pivot_longer(cols = everything(), names_to = "variable_id", values_to = "obs") %>%
          left_join(., site_vars, by = "variable_id") %>%
          mutate(plot_labels = paste(variable_name, paste0("(",variable_unit,")"), sep = " ")) %>%
          group_by(plot_labels) %>%
          mutate(zscore = as.numeric(scale(obs))) %>%
          ungroup()
        
      }
      
      p <- ggplot(data = plot_data_stand)+
        geom_density(aes(x = zscore, color = plot_labels, fill = plot_labels), alpha = 0.5)+
        facet_wrap(facets = vars(plot_labels), nrow = 1)+
        theme_bw()+
        theme(legend.position = "none")+
        xlab("standardized value")
      
      plot.stand$main <- p
      plot.stand$stand.tracker = 1
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
    
  })
  
  # Download standardized regressors
  output$save_standardize_plot <- downloadHandler(
    filename = function() {
      paste("Q13-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.stand$main, device = device)
    }
  )
  
  # Fit ARIMA with selected variables
  actA.arima <- reactiveValues(arima=NULL)
  
  observe({
    input$fit_arima
    output$arima_order <- renderText({
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        
        col_names <- c("datetime","chla",input$select)
        
        model_df4 <- as_tsibble(cann_model_data) %>%
          dplyr::slice_head(prop = .7) %>% # using a 70:30 split here
          tsibble::fill_gaps() %>%
          select(all_of(col_names)) %>%
          mutate(across(input$select, list(zscore = ~as.numeric(scale(.)))))
        
        reg_cols <- paste0(input$select,"_zscore")
        
        if(length(input$select) == 0){
          #my.arima <- model_df4 %>%
          #model(`ARIMA` = fable::ARIMA("chla"))
          order_txt <- "Please select at least one predictor to fit the ARIMA."
        } else if(length(input$select) == 1){
          my.formula <- formula(paste0("chla ~ ",reg_cols[1]))
          actA.arima$arima <- model_df4 %>%
            model(`ARIMA` = fable::ARIMA(formula = my.formula))
        } else if(length(input$select) == 2){
          my.formula <- formula(paste0("chla ~ ",reg_cols[1],"+",reg_cols[2]))
          actA.arima$arima <- model_df4 %>%
            model(`ARIMA` = fable::ARIMA(formula = my.formula))
        } else if(length(input$select) == 3){
          my.formula <- formula(paste0("chla ~ ",reg_cols[1],"+",reg_cols[2],"+",reg_cols[3]))
          actA.arima$arima <- model_df4 %>%
            model(`ARIMA` = fable::ARIMA(formula = my.formula))
        }
        
      }
      
      if(length(input$select) >= 1 & length(input$select) <= 3){
        order <- strsplit(as.character(actA.arima$arima$ARIMA), split = " ")[[1]][3]
        selected_site_vars <- site_vars %>%
          filter(site_id == "cann")
        predictors <- selected_site_vars$variable_name[which(selected_site_vars$variable_id %in% input$select)]
        predictors_list = ""
        for (i in 1:length(predictors)){
          if(i == 1){
            predictors_list <- paste(predictors_list, predictors[i], sep = ": ")
          } else{
            predictors_list <- paste(predictors_list, predictors[i], sep = " and ")
          }
        }
        order_txt <- paste0("Fitted an ARIMA model using predictors ",predictors_list," with order ",order,".")
      } else if(length(input$select) > 3){
        order_txt <- "Please select no more than three predictors to fit the ARIMA."
      }
      
      return(order_txt)
      })
  })
  
  # Reset ARIMA order text when change selected variables
  observe({
    input$select
    output$arima_order <- renderText({
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      
      return("Please click 'Fit ARIMA'.")
      
      })
  })
  
  # ARIMA plot ----
  plot.arima <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima
    
    output$arima_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      validate(
        need(input$fit_arima > 0,
             message = "Click 'Fit ARIMA'")
      )

    
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        plot_data <- as_tsibble(cann_model_data) %>%
          dplyr::slice_head(prop = .7) %>% # using a 70:30 split here
          select(datetime, chla) 
        colnames(plot_data) <- c("datetime","target")
        y_lab = "chlorophyll-a (mg/L)"
      }
      
      fitted_values <- fitted(actA.arima$arima)
      
      p <- ggplot()+
        xlab("datetime")+
        ylab(y_lab)+
        geom_point(data = plot_data, aes(x = datetime, y = target, color = "obs"))+
        geom_line(data = fitted_values, aes(x = datetime, y = .fitted, group = .model, color = .model))+
        labs(color = NULL, fill = NULL)+
        scale_color_manual(values = c("obs" = "#0d3658",.model = "#446c84"))+
        theme_classic()

      plot.arima$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  observe({
    input$select
    
    output$arima_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      validate(
        need(input$fit_arima > 0,
             message = "Click 'Fit ARIMA'")
      )
      
      p <- ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 6,
                 label = "Looks like you've chosen new regressors!\nPlease click 'Fit ARIMA' to regenerate this plot.") + 
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line = element_blank())
      
      plot.arima$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download scatterplot of arima
  output$save_arima_plot <- downloadHandler(
    filename = function() {
      paste("Q12-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.arima$main, device = device)
    }
  )
  
  # Model coefficient table output
  coeff.table <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima
    
    output$coeff_table <- renderDT({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(input$fit_arima > 0,
             message = "Click 'Fit ARIMA'")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      model_coeffs_info <- model_coeffs_key %>%
        select(term, coeff_description)
      t <- coefficients(actA.arima$arima %>% select(`ARIMA`))[,c(2:4)]
      t[,c(2:3)] <- round(t[,c(2:3)], digits = 3)
      t <- left_join(t,model_coeffs_info, by = "term")
      
      coeff.table$main <- t
      
      return(t)
      
    },colnames = c("","Model term","Estimate", "Standard error","Term description"))
    
  })
  
  # Objective 5 ----
  plot.train.test <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima
    
    output$train_test_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 4.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 4.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 4.")
      )
      validate(
        need(!is.null(actA.arima$arima),
             message = "Please fit an ARIMA model in Objective 4.")
      )
      
      
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        train_data <- as_tsibble(cann_model_data) %>%
          dplyr::slice_head(prop = .7) %>% # using a 70:30 split here
          select(datetime, chla) %>%
          rename(target = chla) %>%
          mutate(set = "training data")
        
        test_data <- as_tsibble(cann_model_data) %>%
          dplyr::slice_tail(prop = .3) %>% # using a 70:30 split here
          select(datetime, chla) %>%
          rename(target = chla) %>%
          mutate(set = "testing data")
        
        plot_data <- bind_rows(train_data, test_data)
        
        y_lab = "chlorophyll-a (mg/L)"
        
        train_test_dates <- train_data %>%
          pull(datetime)
        train_test_line <- last(train_test_dates)
      }
      
      fitted_values <- fitted(actA.arima$arima)
      
      p <- ggplot()+
        xlab("datetime")+
        ylab(y_lab)+
        geom_point(data = plot_data, aes(x = datetime, y = target, color = set))+
        geom_line(data = fitted_values, aes(x = datetime, y = .fitted, group = .model, color = .model))+
        geom_vline(xintercept = train_test_line)+
        labs(color = NULL, fill = NULL)+
        scale_color_manual(values = c("training data" = "#cee3f1",.model = "#446c84","testing data" = "#0d3658"))+
        theme_classic()
      
      plot.train.test$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  observe({
    input$select
    
    output$train_test_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 4.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 4.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 4.")
      )
      validate(
        need(!is.null(actA.arima$arima),
             message = "Please fit an ARIMA model in Objective 4.")
      )
      
      p <- ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 6,
                 label = "Looks like you've chosen new regressors!\nPlease click 'Fit ARIMA' in Objective 4 to regenerate this plot.") + 
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line = element_blank())
      
      plot.train.test$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download scatterplot of arima
  output$save_train_test_plot <- downloadHandler(
    filename = function() {
      paste("QXX-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.train.test$main, device = device)
    }
  )
  
  # predictions on testing data plot
  plot.test.pred <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima
    
    output$test_pred_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 4.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 4.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 4.")
      )
      validate(
        need(!is.null(actA.arima$arima),
             message = "Please fit an ARIMA model in Objective 4.")
      )
      validate(
        need(input$generate_pred > 0,
             message = "Click 'Generate predictions'")
      )
      
      
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        
        test_data <- as_tsibble(cann_model_data) %>%
          dplyr::slice_tail(prop = .3) %>% # using a 70:30 split here
          select(datetime, chla) %>%
          rename(target = chla) %>%
          mutate(set = "testing data")
        
        train_data <- as_tsibble(cann_model_data) %>%
          dplyr::slice_head(prop = .7) %>% # using a 70:30 split here
          select(datetime, chla) %>%
          rename(target = chla) %>%
          mutate(set = "training data") 
        
        plot_data <- bind_rows(train_data, test_data)
        
        y_lab = "chlorophyll-a (mg/L)"
        
        train_test_dates <- train_data %>%
          pull(datetime)
        train_test_line <- last(train_test_dates)
        
        col_names <- c("datetime","chla",input$select)
        
        new_data <- as_tsibble(cann_model_data) %>%
          filter(!datetime %in% train_data$datetime) %>% # using a 70:30 split here
          tsibble::fill_gaps() %>%
          select(all_of(col_names)) %>%
          mutate(across(input$select, list(zscore = ~as.numeric(scale(.)))))
        
      }
      
      fitted_values <- fitted(actA.arima$arima)
      pred <- forecast(actA.arima$arima, new_data = new_data)
      
      
      p <- ggplot()+
        xlab("datetime")+
        ylab(y_lab)+
        geom_point(data = plot_data, aes(x = datetime, y = target, color = set))+
        geom_line(data = pred, aes(x = datetime, y = .mean, group = .model, color = .model))+
        geom_vline(xintercept = train_test_line)+
        labs(color = NULL, fill = NULL)+
        scale_color_manual(values = c("training data" = "#cee3f1",.model = "#446c84","testing data" = "#0d3658"))+
        theme_classic()
      
      plot.test.pred$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  observe({
    input$select
    
    output$test_pred_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 4.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 4.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 4.")
      )
      validate(
        need(!is.null(actA.arima$arima),
             message = "Please fit an ARIMA model in Objective 4.")
      )
      validate(
        need(input$generate_pred > 0,
             message = "Click 'Generate predictions'")
      )
      
      p <- ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 6,
                 label = "Looks like you've chosen new regressors!\nPlease click 'Fit ARIMA' in Objective 4 to regenerate this plot.") + 
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line = element_blank())
      
      plot.test.pred$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download scatterplot of arima
  output$save_test_pred_plot <- downloadHandler(
    filename = function() {
      paste("QXX-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.test.pred$main, device = device)
    }
  )
  
  #** Uncertainty slides ----
  output$uc_slides <- renderSlickR({
    slickR(uc_slides) + settings(dots = TRUE)
  })
  
  # Residuals plot
  plot.resid <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima
    
    output$resid_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 4.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 4.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 4.")
      )
      validate(
        need(!is.null(actA.arima$arima),
             message = "Please fit an ARIMA model in Objective 4.")
      )
      validate(
        need(input$view_resid > 0,
             message = "Click 'View residuals'")
      )
      
      
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        x_lab = "chlorophyll-a (mg/L)"
      }
      resid <- residuals(actA.arima$arima)
      sd.resid <- round(sd(resid$.resid, na.rm = TRUE),3)
      
      p <- ggplot(data = resid)+
        xlab(x_lab)+
        geom_histogram(aes(x = .resid), color = "#0d3658", fill = "#cee3f1")+
        theme_bw()+
        ggtitle(paste0("Model residuals: Std. Dev. = ",sd.resid))
      
      plot.resid$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  observe({
    input$select
    
    output$resid_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 4.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 4.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 4.")
      )
      validate(
        need(!is.null(actA.arima$arima),
             message = "Please fit an ARIMA model in Objective 4.")
      )
      validate(
        need(input$generate_pred > 0,
             message = "Click 'Generate predictions'")
      )
      
      p <- ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 4,
                 label = "You've chosen new regressors!\nClick 'Fit ARIMA' in Obj. 4 \nto regenerate this plot.") + 
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line = element_blank())
      
      plot.resid$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download scatterplot of arima
  output$save_resid_plot <- downloadHandler(
    filename = function() {
      paste("QXX-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.resid$main, device = device)
    }
  )
  
  # Predictions with uncertainty plot
  # Residuals plot
  plot.uc <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima
    
    output$uc_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 4.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 4.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 4.")
      )
      validate(
        need(!is.null(actA.arima$arima),
             message = "Please fit an ARIMA model in Objective 4.")
      )
      validate(
        need(input$add_uc > 0,
             message = "Click 'Add uncertainty'")
      )
      
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        
        test_data <- as_tsibble(cann_model_data) %>%
          dplyr::slice_tail(prop = .3) %>% # using a 70:30 split here
          select(datetime, chla) %>%
          rename(target = chla) %>%
          mutate(set = "testing data")
        
        train_data <- as_tsibble(cann_model_data) %>%
          dplyr::slice_head(prop = .7) %>% # using a 70:30 split here
          select(datetime, chla) %>%
          rename(target = chla) %>%
          mutate(set = "training data") 
        
        plot_data <- bind_rows(train_data, test_data)
        
        y_lab = "chlorophyll-a (mg/L)"
        
        train_test_dates <- train_data %>%
          pull(datetime)
        train_test_line <- last(train_test_dates)
        
        col_names <- c("datetime","chla",input$select)
        
        new_data <- as_tsibble(cann_model_data) %>%
          filter(!datetime %in% train_data$datetime) %>% # using a 70:30 split here
          tsibble::fill_gaps() %>%
          select(all_of(col_names)) %>%
          mutate(across(input$select, list(zscore = ~as.numeric(scale(.)))))
        
      }
      
      fitted_values <- fitted(actA.arima$arima)
      pred <- forecast(actA.arima$arima, new_data = new_data) %>%
        hilo()
      
      
      p <- ggplot()+
        xlab("datetime")+
        ylab(y_lab)+
        geom_point(data = plot_data, aes(x = datetime, y = target, color = set))+
        geom_ribbon(data = pred, aes(x = datetime, ymin = `95%`$lower, ymax = `95%`$upper), color = "#DDE4E1", fill = "#DDE4E1",
                    alpha = 0.5)+
        geom_line(data = pred, aes(x = datetime, y = .mean, group = .model, color = .model))+
        geom_vline(xintercept = train_test_line)+
        labs(color = NULL, fill = NULL)+
        scale_color_manual(values = c("training data" = "#cee3f1",.model = "#446c84","testing data" = "#0d3658"))+
        theme_classic()
      
      plot.uc$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  observe({
    input$select
    
    output$uc_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 4.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 4.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 4.")
      )
      validate(
        need(!is.null(actA.arima$arima),
             message = "Please fit an ARIMA model in Objective 4.")
      )
      validate(
        need(input$generate_pred > 0,
             message = "Click 'Generate predictions'")
      )
      
      p <- ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 4,
                 label = "Looks like you've chosen new regressors!\nPlease click 'Fit ARIMA' in Obj. 4 to regenerate this plot.") + 
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line = element_blank())
      
      plot.uc$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download scatterplot of arima
  output$save_uc_plot <- downloadHandler(
    filename = function() {
      paste("QXX-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.uc$main, device = device)
    }
  )
  
  #** Model assessment slides ----
  output$ign_slides <- renderSlickR({
    slickR(ign_slides) + settings(dots = TRUE)
  })
  
  # Calculate RMSE
  rmse.text <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima
    
    output$rmse_text <- renderText({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 4.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 4.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 4.")
      )
      validate(
        need(!is.null(actA.arima$arima),
             message = "Please fit an ARIMA model in Objective 4.")
      )
      validate(
        need(input$assess_mod > 0,
             message = "Click 'Calculate RMSE and ignorance score'")
      )
      
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        
        col_names <- c("datetime","chla",input$select)
        
        train_data <- as_tsibble(cann_model_data) %>%
          dplyr::slice_head(prop = .7)
        
        new_data <- as_tsibble(cann_model_data) %>%
          filter(!datetime %in% train_data$datetime) %>% # using a 70:30 split here
          tsibble::fill_gaps() %>%
          select(all_of(col_names)) %>%
          mutate(across(input$select, list(zscore = ~as.numeric(scale(.)))))
        
        acc <- forecast(actA.arima$arima, new_data = new_data) %>%
          accuracy(data = new_data)
        
        rmse <- acc$RMSE
      
      }
      
      rmse_out <- paste0("RMSE: ",round(mean(rmse, na.rm = TRUE), 2))
      
      rmse.text$main <- rmse_out
      
      return(rmse_out)
      
    })
    
  })
  
  observe({
    input$select
    
    output$rmse_text <- renderText({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 4.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 4.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 4.")
      )
      validate(
        need(!is.null(actA.arima$arima),
             message = "Please fit an ARIMA model in Objective 4.")
      )
      validate(
        need(input$assess_mod > 0,
             message = "Click 'Calculate RMSE and ignorance score'")
      )
      
      rmse_out <- paste0("You've selected new regressors! Please click 'Fit ARIMA' in Obj. 4 to recalculate the ignorance score!")
      
      rmse.text$main <- rmse_out
      
      return(rmse_out)
      
    })
    
  })
  
  # Calculate ignorance
  ign.text <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima
    
    output$ign_text <- renderText({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 4.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 4.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 4.")
      )
      validate(
        need(!is.null(actA.arima$arima),
             message = "Please fit an ARIMA model in Objective 4.")
      )
      validate(
        need(input$assess_mod > 0,
             message = "Click 'Calculate RMSE and ignorance score'")
      )
      
      row_selected = sites_df[input$table01_rows_selected, ]
      site_id <- row_selected$SiteID
      
      if(site_id == "cann"){
        
        col_names <- c("datetime","chla",input$select)
        
        train_data <- as_tsibble(cann_model_data) %>%
          dplyr::slice_head(prop = .7)
        
        new_data <- as_tsibble(cann_model_data) %>%
          filter(!datetime %in% train_data$datetime) %>% # using a 70:30 split here
          tsibble::fill_gaps() %>%
          select(all_of(col_names)) %>%
          mutate(across(input$select, list(zscore = ~as.numeric(scale(.)))))
        
        pred <- forecast(actA.arima$arima, new_data = new_data) 
        
        dist_params <- distributional::parameters(pred$chla)
        
        ign <- scoringRules::logs_norm(new_data$chla, dist_params$mu, dist_params$sigma)
        
      }
      
      ign_out <- paste0("Ignorance score: ",round(mean(ign, na.rm = TRUE), 2))
      
      ign.text$main <- ign_out
      
      return(ign_out)
      
    })
    
  })
  
  observe({
    input$select
    
    output$ign_text <- renderText({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(site_data()$data),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(input$select),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 4.")
      )
      validate(
        need(length(input$select) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 4.")
      )
      validate(
        need(plot.stand$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 4.")
      )
      validate(
        need(!is.null(actA.arima$arima),
             message = "Please fit an ARIMA model in Objective 4.")
      )
      validate(
        need(input$assess_mod > 0,
             message = "Click 'Calculate RMSE and ignorance score'")
      )
      
      ign_out <- paste0("You've selected new regressors! Please click 'Fit ARIMA' in Obj. 4 to recalculate the ignorance score!")
      
      ign.text$main <- ign_out
      
      return(ign_out)
      
    })
    
  })
  
  # Objective 6
  
  #** data standards slides ----
  output$data_std_slides <- renderSlickR({
    slickR(data_slides) + settings(dots = TRUE)
  })
  
  # data format table
  dat.format.table <- reactiveValues(dt = data_format_table) 
  
  output$format_table <- DT::renderDT(
    datatable(data_format_table,
              rownames = FALSE,
              colnames = c("","","",""),
              selection = "none", 
              class = "cell-border stripe",
              options = list(searching = FALSE,paging = FALSE, ordering= FALSE, dom = "t"),
              editable = FALSE
    ) %>%
      formatStyle('RowName',fontWeight = "bold")
  )
  
  renderDT(
    datatable(
      data,
      rownames = FALSE, # Hide the default row names
      options = list(
        columnDefs = list(
          list(targets = 0, visible = FALSE) # Hide the dummy column 'RowName'
        )
      )
    ) %>% formatStyle('RowName', fontWeight = 'bold')
  )
  
  # download datasets provided with module
  actB_data <- reactive({
    out <- get(paste0(input$actB_dataset,"Data"))
    out
  })
  
  output$preview_actB_dataset <- renderTable({
    if(input$actB_dataset == "CanningRiverKentStWeir"){
      head(actB_data()) 
    } else {
      head(actB_data()) %>%
        mutate(datetime = format(datetime, "%Y-%m-%d"))
    }
  })
  
  output$download_actB_dataset <- downloadHandler(
    filename = function() {
      paste0(input$actB_dataset, "Data.csv")
    },
    content = function(file) {
      vroom::vroom_write(actB_data(), file, delim = ",")
    }
  )
  
  # download metadata provided with module
  actB_metadata <- reactive({
    out <- get(paste0(input$actB_dataset,"Metadata"))
    out
  })
  
  output$download_actB_metadata <- downloadHandler(
    filename = function() {
      paste0(input$actB_dataset, "Metadata.csv")
    },
    content = function(file) {
      vroom::vroom_write(actB_metadata(), file, delim = ",")
    }
  )
  
  # display site info if user chooses data provided within module
  #show site info
  output$actB_site_info <- renderText({
    module_text[input$actB_dataset, ]
  })
  
  # validation toggle
  valid <- reactiveValues(main = TRUE)
  
  # user input data upload and validation
  stand.data <- reactive({
    req(input$upload_data)
    
    ext <- tools::file_ext(input$upload_data$name)
    switch(ext,
           csv = vroom::vroom(input$upload_data$datapath, delim = ","),
           validate("Invalid file! Please upload a .csv file.")
    )
    
    dat <- readr::read_csv(input$upload_data$datapath, guess_max = 1e6, show_col_types = FALSE) 
    
    IsDate <- function(mydate, date.format = "%y-%m-%d") {
      tryCatch(!is.na(as.Date(x = mydate, format = date.format)),  
               error = function(err) {FALSE})  
    }
    
    if(!"site_id" %in% names(dat)){
      valid$main <- FALSE
      validate("'site_id' column is missing!")
    } 
    
    if(!"datetime" %in% names(dat)){
      valid$main <- FALSE
      validate("'datetime' column is missing!")
    } 
    
    if(!"variable" %in% names(dat)){
      valid$main <- FALSE
      validate("'variable' column is missing!")
    } 
    
    if(!"observation" %in% names(dat)){
      valid$main <- FALSE
      validate("'observation' column is missing!")
    } 
    
    if(!class(dat$site_id) == "character"){
      valid$main <- FALSE
      validate("'site_id' column is not in character format!")
    } 
    
    if(!"Date" %in% class(dat$datetime)){
      valid$main <- FALSE
      validate("'datetime' column is not in Date format!")
    } 
    
    if(!class(dat$variable) == "character"){
      valid$main <- FALSE
      validate("'variable' column is not in character format!")
    } 
    
    if(!class(dat$observation) == "numeric"){
      valid$main <- FALSE
      validate("'observation' column is not in numeric format!")
    } 
    
    check_num_sites <- length(unique(dat$site_id))
    if(check_num_sites > 1){
      valid$main <- FALSE
      validate("only one unique value of 'site_id' is supported at this time!")
    }
    
    check_num_variables <- length(unique(dat$variable))
    if(check_num_variables > 10){
      valid$main <- FALSE
      validate("more than 10 unique values of 'variable' detected in data!")
    }
    
    check_variable_spaces <- sapply(dat$variable, function(x) grepl(" ", x))
    if(any(check_variable_spaces)){
      valid$main <- FALSE
      validate("'variable' column values contain spaces!")
    }
    
    check_num_timesteps <- length(unique(dat$datetime))
    if(check_num_timesteps < 70){
      valid$main <- FALSE
      validate("at least 70 timesteps are required to fit and assess models!")
    }
    
    check_date_format <- IsDate(dat$datetime)
    if(any(!check_date_format)){
      valid$main <- FALSE
      validate("'datetime' column cannot be translated to yyyy-mm-dd format!")
    } 
    
    wide_dat <- dat %>%
      pivot_wider(names_from = "variable", values_from = "observation")
    
    diff_dates <- as.numeric(diff(wide_dat$datetime), units = "days")
    
    if(any(diff_dates < 1)){
      valid$main <- FALSE
      validate("sub-daily dates detected in data!")
    }
    
    valid$main <- TRUE
    
    return(dat)
  })
  
  # table of head of user data
  output$stand_data <- renderTable({
    table_dat <- stand.data() %>%
      mutate(datetime = format(datetime, "%Y-%m-%d"))
    head(table_dat, input$n)
  })
  
  # alert user that gaps are being filled
  output$gap_text <- renderText({
    
    validate(
      need(!is.null(input$upload_data),
           message = "Please upload your data above.")
    )
    validate(
      need(valid$main == TRUE,
           message = "Please correct your data format and try again.")
    )
    
    dat <- stand.data()
    wide_dat <- dat %>%
      pivot_wider(names_from = "variable", values_from = "observation")

    model_df <- as_tsibble(wide_dat) %>%
      fill_gaps()

    n_gaps = nrow(model_df) - nrow(wide_dat)

    if(nrow(model_df) > nrow(wide_dat)){
      return(paste0("Data gaps detected! Filling ",n_gaps," gap(s) with NA values!"))
    } else {
      return("No data gaps detected!")
    }
  })
  
  # plot variables in user data
  # Site data plot ----
  output$user_data_plot <- renderPlotly({
    
    validate(
      need(!is.null(input$upload_data),
           message = "Please upload your data.")
    )
    validate(
      need(valid$main == TRUE,
           message = "Please correct your data format and try again.")
    )
    
    plot_dat <- stand.data() 
    
    p <- ggplot(data = plot_dat) +
      geom_point(aes(x = datetime, y = observation, color = variable))+
      facet_wrap(facets = vars(variable), scales = "free_y")+
      theme_bw(base_size = 10)
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Objective 7
  
  # make multi-select list
  multi.select2 <- reactiveValues(lst=NULL)
  multi.select3 <- reactiveValues(lst=NULL)
  
  observe({
    
    validate(
      need(!is.null(input$upload_data),
           message = "Please upload your data in Objective 6.")
    )
    validate(
      need(valid$main == TRUE,
           message = "Please correct your data format in Objective 6.")
    )
    
    dat <- stand.data()
    
    tar_select_list <- unique(dat$variable)
    names(tar_select_list) = unique(dat$variable)
    
    multi.select2$lst <- tar_select_list

  })
  
  observe({
    
    validate(
      need(!is.null(input$upload_data),
           message = "Please upload your data in Objective 6.")
    )
    validate(
      need(valid$main == TRUE,
           message = "Please correct your data format in Objective 6.")
    )
    validate(
      need(!is.null(input$select_tar_actB),
           message = "Please select your target variable from the dropdown menu.")
    )
    
    dat <- stand.data()
    
    reg_dat <- dat %>%
      filter(!variable == input$select_tar_actB)
    reg_select_list <- unique(reg_dat$variable)
    names(reg_select_list) = unique(reg_dat$variable)
    
    multi.select3$lst <- reg_select_list
    
  })
  
  # made header text for multi-select lists
  output$tar_dropdown_actB <- renderText({
    
    validate(
      need(!is.null(input$upload_data),
           message = "Please upload your data in Objective 6.")
    )
    validate(
      need(valid$main == TRUE,
           message = "Please correct your data format in Objective 6.")
    )
    
    return("Please select your target variable from the dropdown menu.")
    
  })
  
  output$reg_dropdown_actB <- renderText({
    
    validate(
      need(!is.null(input$upload_data),
           message = "Please upload your data in Objective 6.")
    )
    validate(
      need(valid$main == TRUE,
           message = "Please correct your data format in Objective 6.")
    )
    validate(
      need(!is.null(input$select_tar_actB),
           message = "Before selecting predictors, please select your target variable from the dropdown menu.")
    )
    
    return("Please select predictors from the dropdown menu.")
    
  })
  
  # Update the selectInput with the named list
  observe({
    updateSelectInput(session, "select_tar_actB", choices = multi.select2$lst)
  })
  
  observe({
    updateSelectInput(session, "select_reg_actB", choices = multi.select3$lst)
  })
  
  
  # Plot of regressors
  plot.stand2 <- reactiveValues(main=NULL,
                               stand.tracker = 0)
  
  observe({
    
    input$select_reg_actB
    
    output$standardize_plot2 <- renderPlotly({
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      
      dat <- stand.data()
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
      
      col_names <- c(input$select_reg_actB)
        
      plot_data <- wide_dat %>%
        dplyr::slice_head(prop = .7) %>% # using a 70:30 split here
        select(all_of(col_names)) %>%
        pivot_longer(cols = everything(), names_to = "variable_id", values_to = "obs") 
        
      p <- ggplot(data = plot_data)+
        geom_density(aes(x = obs, color = variable_id, fill = variable_id), alpha = 0.5)+
        facet_wrap(facets = vars(variable_id), nrow = 1, scales = "free_x")+
        theme_bw()+
        theme(legend.position = "none")+
        xlab("observed value")
      
      plot.stand2$main <- p
      plot.stand2$stand.tracker <- 0
      
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
    
  })
  
  observe({
    
    input$standardize_data2
    
    output$standardize_plot2 <- renderPlotly({
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      
      dat <- stand.data()
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
      
      col_names <- c(input$select_reg_actB)
      
      plot_data_stand <- wide_dat %>%
        dplyr::slice_head(prop = .7) %>% # using a 70:30 split here
        select(all_of(col_names)) %>%
        pivot_longer(cols = everything(), names_to = "variable_id", values_to = "obs") %>%
        group_by(variable_id) %>%
        mutate(zscore = as.numeric(scale(obs))) %>%
        ungroup()
      
      p <- ggplot(data = plot_data_stand)+
        geom_density(aes(x = zscore, color = variable_id, fill = variable_id), alpha = 0.5)+
        facet_wrap(facets = vars(variable_id), nrow = 1)+
        theme_bw()+
        theme(legend.position = "none")+
        xlab("standardized value")
      
      plot.stand2$main <- p
      plot.stand2$stand.tracker = 1
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
    
  })
  
  # Download standardized regressors
  output$save_standardize_plot2 <- downloadHandler(
    filename = function() {
      paste("QXX-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.stand2$main, device = device)
    }
  )
  
  # Fit ARIMA with selected variables
  actB.arima <- reactiveValues(arima=NULL,
                               nrow_model_df = 31)
  
  observe({
    input$prop

      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      
      dat <- stand.data()
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
      
      col_names <- c("datetime",input$select_tar_actB,input$select_reg_actB)
      
      model_df4 <- as_tsibble(wide_dat) %>%
        dplyr::slice_head(prop = input$prop) %>% # using a 70:30 split here
        tsibble::fill_gaps() %>%
        select(all_of(col_names)) %>%
        mutate(across(input$select_reg_actB, list(zscore = ~as.numeric(scale(.)))))
      
      actB.arima$nrow_model_df <- nrow(model_df4)
      
  })
  
  observe({
    input$fit_arima2
    output$arima_order2 <- renderText({
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less).")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training.")
      )
      validate(
        need(input$fit_arima2 > 0,
             message = "Please click 'Fit ARIMA'")
      )
      
      dat <- stand.data()
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
      
      col_names <- c("datetime",input$select_tar_actB,input$select_reg_actB)
        
        model_df4 <- as_tsibble(wide_dat) %>%
          dplyr::slice_head(prop = input$prop) %>% # using a 70:30 split here
          tsibble::fill_gaps() %>%
          select(all_of(col_names)) %>%
          mutate(across(input$select_reg_actB, list(zscore = ~as.numeric(scale(.)))))
        
        reg_cols <- paste0(input$select_reg_actB,"_zscore")
        
        if(length(input$select_reg_actB) == 0){
          #my.arima <- model_df4 %>%
          #model(`ARIMA` = fable::ARIMA("chla"))
          order_txt <- "Please select at least one predictor to fit the ARIMA."
        } else if(length(input$select_reg_actB) == 1){
          my.formula <- formula(paste0(input$select_tar_actB," ~ ",reg_cols[1]))
          actB.arima$arima <- model_df4 %>%
            model(`ARIMA` = fable::ARIMA(formula = my.formula))
        } else if(length(input$select_reg_actB) == 2){ 
          my.formula <- formula(paste0(input$select_tar_actB," ~ ",reg_cols[1],"+",reg_cols[2]))
          actB.arima$arima <- model_df4 %>%
            model(`ARIMA` = fable::ARIMA(formula = my.formula))
        } else if(length(input$select_reg_actB) == 3){
          my.formula <- formula(paste0(input$select_tar_actB," ~ ",reg_cols[1],"+",reg_cols[2],"+",reg_cols[3]))
          actB.arima$arima <- model_df4 %>%
            model(`ARIMA` = fable::ARIMA(formula = my.formula))
        }
      
      if(length(input$select_reg_actB) >= 1 & length(input$select_reg_actB) <= 3){
        order <- strsplit(as.character(actB.arima$arima$ARIMA), split = " ")[[1]][3]
        predictors <- unique(dat$variable[which(dat$variable %in% input$select_reg_actB)])
        predictors_list = ""
        for (i in 1:length(predictors)){
          if(i == 1){
            predictors_list <- paste(predictors_list, predictors[i], sep = ": ")
          } else{
            predictors_list <- paste(predictors_list, predictors[i], sep = " and ")
          }
        }
        order_txt <- paste0("Fitted an ARIMA model to predict ",input$select_tar_actB," using predictors ",predictors_list," with order ",order,".")
      } else if(length(input$select) > 3){
        order_txt <- "Please select no more than three predictors to fit the ARIMA."
      }
      
      return(order_txt)
    })
  })
  
  # Reset ARIMA order text when change selected variables
  observe({
    input$select_reg_actB
    output$arima_order2 <- renderText({
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less).")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training.")
      )
      
      return("Please click 'Fit ARIMA'.")
      
    })
  })
  
  # ARIMA plot ----
  plot.arima2 <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima2
    
    output$arima_plot2 <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less).")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training.")
      )
      validate(
        need(input$fit_arima2 > 0,
             message = "Click 'Fit ARIMA'")
      )
      
      
      dat <- stand.data()
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
    
      plot_data <- as_tsibble(wide_dat) %>%
        dplyr::slice_head(prop = input$prop) %>% # using a 70:30 split here
        select(datetime, input$select_tar_actB) 
      
      colnames(plot_data) <- c("datetime","target")
      y_lab = input$select_tar_actB
      
      fitted_values <- fitted(actB.arima$arima)
      
      p <- ggplot()+
        xlab("datetime")+
        ylab(y_lab)+
        geom_point(data = plot_data, aes(x = datetime, y = target, color = "obs"))+
        geom_line(data = fitted_values, aes(x = datetime, y = .fitted, group = .model, color = .model))+
        labs(color = NULL, fill = NULL)+
        scale_color_manual(values = c("obs" = "#0d3658",.model = "#446c84"))+
        theme_classic()
      
      plot.arima2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  observe({
    input$select_reg_actB
    
    output$arima_plot2 <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less).")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training.")
      )
      validate(
        need(input$fit_arima2 > 0,
             message = "Click 'Fit ARIMA'")
      )
      
      p <- ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 6,
                 label = "Looks like you've chosen new regressors!\nPlease click 'Fit ARIMA' to regenerate this plot.") + 
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line = element_blank())
      
      plot.arima2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download scatterplot of arima
  output$save_arima_plot2 <- downloadHandler(
    filename = function() {
      paste("QXX-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.arima2$main, device = device)
    }
  )
  
  # Model coefficient table output
  coeff.table2 <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima2
    
    output$coeff_table2 <- renderDT({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less).")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training.")
      )
      validate(
        need(input$fit_arima2 > 0,
             message = "Click 'Fit ARIMA'")
      )
      
      t <- coefficients(actB.arima$arima %>% select(`ARIMA`))[,c(2:4)]
      t[,c(2:3)] <- round(t[,c(2:3)], digits = 3)

      coeff.table2$main <- t
      
      return(t)
      
    },colnames = c("","Model term","Estimate", "Standard error"))
    
  })
  
  observe({
    input$select_reg_actB
    
    output$coeff_table2 <- renderDT({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less).")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training.")
      )
      validate(
        need(input$fit_arima2 > 0,
             message = "Click 'Fit ARIMA'")
      )
      
      t <- data.frame(msg = c("Please click 'Fit ARIMA'"))
      
      coeff.table2$main <- t
      
      return(t)
      
    },colnames = c(""))
    
  })
  
  # Objective 8
  plot.train.test2 <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima2
    
    output$train_test_plot2 <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      
      
      dat <- stand.data() 
      
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
      
        train_data <- as_tsibble(wide_dat) %>%
          dplyr::slice_head(prop = input$prop) %>% # using a 70:30 split here
          select(datetime, input$select_tar_actB) %>%
          mutate(set = "training data")
        colnames(train_data) <- c("datetime","target","set")
        
        test_data <- as_tsibble(wide_dat) %>%
          dplyr::slice_tail(prop = 1-input$prop) %>% # using a 70:30 split here
          select(datetime, input$select_tar_actB) %>%
          mutate(set = "testing data")
        colnames(test_data) <- c("datetime","target","set")
        
        plot_data <- bind_rows(train_data, test_data)
        
        train_test_dates <- train_data %>%
          pull(datetime)
        train_test_line <- last(train_test_dates)
      
      fitted_values <- fitted(actB.arima$arima)
      
      p <- ggplot()+
        xlab("datetime")+
        ylab(input$select_tar_actB)+
        geom_point(data = plot_data, aes(x = datetime, y = target, color = set))+
        geom_line(data = fitted_values, aes(x = datetime, y = .fitted, group = .model, color = .model))+
        geom_vline(xintercept = train_test_line)+
        labs(color = NULL, fill = NULL)+
        scale_color_manual(values = c("training data" = "#cee3f1",.model = "#446c84","testing data" = "#0d3658"))+
        theme_classic()
      
      plot.train.test2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  observe({
    input$select_tar_actB
    input$select_reg_actB
    
    output$train_test_plot2 <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      
      p <- ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 6,
                 label = "Looks like you've chosen new regressors!\nPlease click 'Fit ARIMA' in Objective 7 to regenerate this plot.") + 
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line = element_blank())
      
      plot.train.test2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download train test plot
  output$save_train_test_plot2 <- downloadHandler(
    filename = function() {
      paste("QXX-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.train.test2$main, device = device)
    }
  )
  
  # predictions on testing data plot
  plot.test.pred2 <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima2
    
    output$test_pred_plot2 <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(input$generate_pred2 > 0,
             message = "Click 'Generate predictions'")
      )
      
      dat <- stand.data()
      
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
      
      train_data <- as_tsibble(wide_dat) %>%
        dplyr::slice_head(prop = input$prop) %>% # using a 70:30 split here
        select(datetime, input$select_tar_actB) %>%
        mutate(set = "training data")
      colnames(train_data) <- c("datetime","target","set")
      
      test_data <- as_tsibble(wide_dat) %>%
        dplyr::slice_tail(prop = 1-input$prop) %>% # using a 70:30 split here
        select(datetime, input$select_tar_actB) %>%
        mutate(set = "testing data")
      colnames(test_data) <- c("datetime","target","set")
      
      plot_data <- bind_rows(train_data, test_data)
      
      train_test_dates <- train_data %>%
        pull(datetime)
      train_test_line <- last(train_test_dates)
      
      fitted_values <- fitted(actB.arima$arima)
      
        col_names <- c("datetime",input$select_tar_actB,input$select_reg_actB)
        
        new_data <- as_tsibble(wide_dat) %>%
          filter(!datetime %in% train_data$datetime) %>% # using a 70:30 split here
          tsibble::fill_gaps() %>%
          select(all_of(col_names)) %>%
          mutate(across(input$select_reg_actB, list(zscore = ~as.numeric(scale(.)))))
      
      fitted_values <- fitted(actB.arima$arima)
      pred <- forecast(actB.arima$arima, new_data = new_data)
      
      
      p <- ggplot()+
        xlab("datetime")+
        ylab(input$select_tar_actB)+
        geom_point(data = plot_data, aes(x = datetime, y = target, color = set))+
        geom_line(data = pred, aes(x = datetime, y = .mean, group = .model, color = .model))+
        geom_vline(xintercept = train_test_line)+
        labs(color = NULL, fill = NULL)+
        scale_color_manual(values = c("training data" = "#cee3f1",.model = "#446c84","testing data" = "#0d3658"))+
        theme_classic()
      
      plot.test.pred2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  observe({
    input$select_tar_actB
    input$select_reg_actB
    
    output$test_pred_plot2 <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(input$generate_pred2 > 0,
             message = "Click 'Generate predictions'")
      )
      
      p <- ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 6,
                 label = "Looks like you've chosen new regressors!\nPlease click 'Fit ARIMA' in Objective 7 to regenerate this plot.") + 
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line = element_blank())
      
      plot.test.pred2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download scatterplot of arima
  output$save_test_pred_plot2 <- downloadHandler(
    filename = function() {
      paste("QXX-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.test.pred2$main, device = device)
    }
  )
  
  
  # Residuals plot
  plot.resid2 <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima2
    
    output$resid_plot2 <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(input$view_resid2 > 0,
             message = "Click 'View residuals'")
      )
      
      resid <- residuals(actB.arima$arima)
      sd.resid <- round(sd(resid$.resid, na.rm = TRUE),3)
      
      p <- ggplot(data = resid)+
        geom_histogram(aes(x = .resid), color = "#0d3658", fill = "#cee3f1")+
        xlab(input$select_tar_actB)+
        theme_bw()+
        ggtitle(paste0("Model residuals: Std. Dev. = ",sd.resid))
      
      plot.resid2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  observe({
    input$select_tar_actB
    input$select_reg_actB
    
    output$resid_plot2 <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(input$view_resid2 > 0,
             message = "Click 'View residuals'")
      )
      
      p <- ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 4,
                 label = "You've chosen new regressors!\nClick 'Fit ARIMA' in Obj. 7 \nto regenerate this plot.") + 
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line = element_blank())
      
      plot.resid2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download scatterplot of arima
  output$save_resid_plot2 <- downloadHandler(
    filename = function() {
      paste("QXX-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.resid2$main, device = device)
    }
  )
  
  # Predictions with uncertainty plot
  plot.uc2 <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima2
    
    output$uc_plot2 <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(input$add_uc2 > 0,
             message = "Click 'Add uncertainty'")
      )
      
      dat <- stand.data()
      
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
      
      train_data <- as_tsibble(wide_dat) %>%
        dplyr::slice_head(prop = input$prop) %>% # using a 70:30 split here
        select(datetime, input$select_tar_actB) %>%
        mutate(set = "training data")
      colnames(train_data) <- c("datetime","target","set")
      
      test_data <- as_tsibble(wide_dat) %>%
        dplyr::slice_tail(prop = 1-input$prop) %>% # using a 70:30 split here
        select(datetime, input$select_tar_actB) %>%
        mutate(set = "testing data")
      colnames(test_data) <- c("datetime","target","set")
      
      plot_data <- bind_rows(train_data, test_data)
      
      train_test_dates <- train_data %>%
        pull(datetime)
      train_test_line <- last(train_test_dates)
      
      fitted_values <- fitted(actB.arima$arima)
      
      col_names <- c("datetime",input$select_tar_actB,input$select_reg_actB)
      
      new_data <- as_tsibble(wide_dat) %>%
        filter(!datetime %in% train_data$datetime) %>% # using a 70:30 split here
        tsibble::fill_gaps() %>%
        select(all_of(col_names)) %>%
        mutate(across(input$select_reg_actB, list(zscore = ~as.numeric(scale(.)))))
      
      fitted_values <- fitted(actB.arima$arima)
      pred <- forecast(actB.arima$arima, new_data = new_data) %>%
        hilo()
      
      p <- ggplot()+
        xlab("datetime")+
        ylab(input$select_tar_actB)+
        geom_point(data = plot_data, aes(x = datetime, y = target, color = set))+
        geom_ribbon(data = pred, aes(x = datetime, ymin = `95%`$lower, ymax = `95%`$upper), color = "#DDE4E1", fill = "#DDE4E1",
                    alpha = 0.5)+
        geom_line(data = pred, aes(x = datetime, y = .mean, group = .model, color = .model))+
        geom_vline(xintercept = train_test_line)+
        labs(color = NULL, fill = NULL)+
        scale_color_manual(values = c("training data" = "#cee3f1",.model = "#446c84","testing data" = "#0d3658"))+
        theme_classic()
      
      plot.uc2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  observe({
    input$select_tar_actB
    input$select_reg_actB
    
    output$uc_plot2 <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(input$add_uc2 > 0,
             message = "Click 'Add uncertainty'")
      )
      
      p <- ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 4,
                 label = "Looks like you've chosen new regressors!\nPlease click 'Fit ARIMA' in Obj. 7 to regenerate this plot.") + 
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line = element_blank())
      
      plot.uc2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download scatterplot of arima
  output$save_uc_plot2 <- downloadHandler(
    filename = function() {
      paste("QXX-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.uc2$main, device = device)
    }
  )
  
  # Calculate RMSE
  rmse.text2 <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima2
    
    output$rmse_text2 <- renderText({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(input$assess_mod2 > 0,
             message = "Click 'Calculate RMSE and ignorance score'")
      )
      
      dat <- stand.data()
      
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
      
      train_data <- as_tsibble(wide_dat) %>%
        dplyr::slice_head(prop = input$prop) 
      
      col_names <- c("datetime",input$select_tar_actB,input$select_reg_actB)
      
      new_data <- as_tsibble(wide_dat) %>%
        filter(!datetime %in% train_data$datetime) %>% # using a 70:30 split here
        tsibble::fill_gaps() %>%
        select(all_of(col_names)) %>%
        mutate(across(input$select_reg_actB, list(zscore = ~as.numeric(scale(.)))))
      
      acc <- forecast(actB.arima$arima, new_data = new_data) %>%
        accuracy(data = new_data)
        
        rmse <- acc$RMSE
      
      rmse_out2 <- paste0("RMSE: ",round(mean(rmse, na.rm = TRUE), 2))
      
      rmse.text2$main <- rmse_out
      
      return(rmse_out2)
      
    })
    
  })
  
  observe({
    input$select_reg_actB
    
    output$rmse_text2 <- renderText({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(input$assess_mod2 > 0,
             message = "Click 'Calculate RMSE and ignorance score'")
      )
      
      rmse_out2 <- paste0("You've selected new regressors! Please click 'Fit ARIMA' in Obj. 4 to recalculate the ignorance score!")
      
      rmse.text2$main <- rmse_out2
      
      return(rmse_out2)
      
    })
    
  })
  
  
  # Calculate ignorance
  ign.text2 <- reactiveValues(main=NULL)
  
  observe({
    input$fit_arima2
    
    output$ign_text2 <- renderText({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(input$calc_ign2 > 0,
             message = "Click 'Calculate ignorance score'")
      )
      
      dat <- stand.data()
      
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
      
      train_data <- as_tsibble(wide_dat) %>%
        dplyr::slice_head(prop = input$prop) 

      col_names <- c("datetime",input$select_tar_actB,input$select_reg_actB)
      
      new_data <- as_tsibble(wide_dat) %>%
        filter(!datetime %in% train_data$datetime) %>% # using a 70:30 split here
        tsibble::fill_gaps() %>%
        select(all_of(col_names)) %>%
        mutate(across(input$select_reg_actB, list(zscore = ~as.numeric(scale(.)))))
      
        pred <- forecast(actB.arima$arima, new_data = new_data) %>%
          pull(input$select_tar_actB)
        
        dist_params <- distributional::parameters(pred)
        
        new_obs <- new_data %>%
          pull(input$select_tar_actB)
        
        ign <- scoringRules::logs_norm(new_obs, dist_params$mu, dist_params$sigma)
      
      ign_out <- paste0("Ignorance score: ",round(mean(ign, na.rm = TRUE), 2))
      
      ign.text2$main <- ign_out
      
      return(ign_out)
      
    })
    
  })
  
  observe({
    input$select_tar_actB
    input$select_reg_actB
    
    output$ign_text2 <- renderText({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(input$calc_ign2 > 0,
             message = "Click 'Calculate ignorance score'")
      )
      
      ign_out <- paste0("You've selected new regressors! Please click 'Fit ARIMA' in Obj. 7 to recalculate the ignorance score!")
      
      ign.text2$main <- ign_out
      
      return(ign_out)
      
    })
    
  })
  
  # Objective 9
  #** additional models slides ----
  output$addn_model_slides <- renderSlickR({
    slickR(model_slides) + settings(dots = TRUE)
  })
  
  # ARIMA plot ----
  plot.models <- reactiveValues(main=NULL)
  actC.models <- reactiveValues(nnetar = NULL,
                                rw = NULL,
                                doy = NULL)
  
  observe({
    input$fit_arima2
    
    output$plot_models <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less).")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Click 'Fit ARIMA'")
      )
      validate(
        need(input$fit_addn_mod > 0,
             message = "Click 'Fit additional models'")
      )
      
      
      dat <- stand.data()
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
      
      col_names <- c("datetime",input$select_tar_actB,input$select_reg_actB)
      
      model_df4 <- as_tsibble(wide_dat) %>%
        dplyr::slice_head(prop = input$prop) %>% # using a 70:30 split here
        tsibble::fill_gaps() %>%
        select(all_of(col_names)) %>%
        mutate(across(input$select_reg_actB, list(zscore = ~as.numeric(scale(.)))))
      
      # get persistence model
      rw_cols <- c("datetime",input$select_tar_actB)
      rw_data <- model_df4 %>%
        select(all_of(rw_cols))
      colnames(rw_data) <- c("datetime","target")
      actC.models$rw = rw_data %>%
        model(`persistence` = fable::RW(target))
      
      # get DOY model
      doy_cols <- c("doy",input$select_tar_actB)
      doy_data <- as.data.frame(model_df4) %>%
        mutate(doy = yday(datetime)) %>%
        select(any_of(doy_cols))
      colnames(doy_data) <- c("x","y")
      actC.models$doy <- mgcv::gam(formula = y ~ s(x, bs = "cs"), family = gaussian(),
                          data = doy_data, method = "REML")
      
      # get NNETAR model
      reg_cols <- paste0(input$select_reg_actB,"_zscore")
      
      if(length(input$select_reg_actB) == 0){
        #my.arima <- model_df4 %>%
        #model(`ARIMA` = fable::ARIMA("chla"))
        order_txt <- "Please select at least one predictor to fit the ARIMA."
      } else if(length(input$select_reg_actB) == 1){
        my.formula <- formula(paste0(input$select_tar_actB," ~ ",reg_cols[1]))
        actC.models$nnetar <- model_df4 %>%
          model(`NNETAR` = fable::NNETAR(formula = my.formula))
      } else if(length(input$select_reg_actB) == 2){ 
        my.formula <- formula(paste0(input$select_tar_actB," ~ ",reg_cols[1],"+",reg_cols[2]))
        actC.models$nnetar <- model_df4 %>%
          model(`NNETAR` = fable::NNETAR(formula = my.formula))
      } else if(length(input$select_reg_actB) == 3){
        my.formula <- formula(paste0(input$select_tar_actB," ~ ",reg_cols[1],"+",reg_cols[2],"+",reg_cols[3]))
        actC.models$nnetar <- model_df4 %>%
          model(`NNETAR` = fable::NNETAR(formula = my.formula))
      }
      
      plot_data <- as_tsibble(wide_dat) %>%
        dplyr::slice_head(prop = input$prop) %>% # using a 70:30 split here
        select(datetime, input$select_tar_actB) 
      
      colnames(plot_data) <- c("datetime","target")
      y_lab = input$select_tar_actB
      
      fitted_values_arima <- fitted(actB.arima$arima)
      fitted_values_nnetar <- fitted(actC.models$nnetar)
      fitted_values_rw <- fitted(actC.models$rw)
      fitted_values_doy <- data.frame(.model = "DOY",
                                      datetime = model_df4$datetime,
                                      .fitted = mgcv::predict.gam(actC.models$doy, doy_data))
      
      plot_fitted <- bind_rows(fitted_values_arima, fitted_values_nnetar) %>%
        bind_rows(., fitted_values_rw) %>%
        bind_rows(., fitted_values_doy)
      
      p <- ggplot()+
        xlab("datetime")+
        ylab(y_lab)+
        geom_point(data = plot_data, aes(x = datetime, y = target, color = "obs"))+
        geom_line(data = plot_fitted, aes(x = datetime, y = .fitted, group = .model, color = .model))+
        labs(color = NULL, fill = NULL)+
        scale_color_manual(values = c("obs" = "black","ARIMA" = "#E69F00","NNETAR" = "#56B4E9",
                                      "persistence" = "#009E73","DOY" = "#CC79A7"))+
        theme_classic()
      
      plot.models$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  observe({
    input$select_reg_actB
    
    output$plot_models <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less).")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training.")
      )
      validate(
        need(input$fit_arima2 > 0,
             message = "Click 'Fit ARIMA'")
      )
      validate(
        need(input$fit_addn_mod > 0,
             message = "Click 'Fit additional models'")
      )
      
      p <- ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 6,
                 label = "Looks like you've chosen new regressors!\nPlease click 'Fit ARIMA' in Obj. 7 to regenerate this plot.") + 
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line = element_blank())
      
      plot.arima2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download additional model fits
  output$save_plot_models <- downloadHandler(
    filename = function() {
      paste("QXX-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.models$main, device = device)
    }
  )
  
  observe({
    input$fit_arima2
    output$nnetar_order <- renderText({
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less).")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training.")
      )
      validate(
        need(input$fit_arima2 > 0,
             message = "Click 'Fit ARIMA'")
      )
      validate(
        need(!is.null(actC.models$nnetar),
             message = "Click 'Fit additional models'")
      )
      
      
      order <- as.character(actC.models$nnetar$NNETAR)
      order_txt <- paste0("Fitted a ",order, " model.")
      
      return(order_txt)
    })
  })
  
  # Reset ARIMA order text when change selected variables
  observe({
    input$select_reg_actB
    output$nnetar_order <- renderText({
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less).")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training.")
      )
      validate(
        need(input$fit_arima2 > 0,
             message = "Click 'Fit ARIMA'")
      )
      validate(
        need(!is.null(actC.models$nnetar),
             message = "Click 'Fit additional models'")
      )
      
      return("Please click 'Fit additional models'.")
      
    })
  })
  
  # Objective 10
  
  # Predictions with uncertainty plot
  plot.pred.all <- reactiveValues(main=NULL)
  dist_params <- reactiveValues(arima=NULL,
                             nnetar=NULL,
                             rw=NULL,
                             doy=NULL)
  
  observe({
    input$fit_arima2
    
    output$pred_all_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(!is.null(actC.models$nnetar),
             message = "Please fit additional models in Objective 8.")
      )
      validate(
        need(input$plot_pred_models > 0,
             message = "Click 'Generate predictions with uncertainty'")
      )
      
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Generating predictions with uncertainty",
                   detail = "Generating predictions for the NNETAR model may take several minutes. This window will disappear
                     when it is done.", value = 0.2)
      
      
      dat <- stand.data()
      
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
      
      train_data <- as_tsibble(wide_dat) %>%
        dplyr::slice_head(prop = input$prop) %>% # using a 70:30 split here
        select(datetime, input$select_tar_actB) %>%
        mutate(set = "training data") 
      colnames(train_data) <- c("datetime","target","set")
      
      plot_train_data <- train_data %>%
        dplyr::slice_tail(n=10)
      
      test_data <- as_tsibble(wide_dat) %>%
        dplyr::slice_tail(prop = 1-input$prop) %>% # using a 70:30 split here
        select(datetime, input$select_tar_actB) %>%
        mutate(set = "testing data")
      colnames(test_data) <- c("datetime","target","set")
      
      plot_data <- bind_rows(plot_train_data, test_data)
      
      train_test_dates <- train_data %>%
        pull(datetime)
      train_test_line <- last(train_test_dates)
      
      col_names <- c("datetime",input$select_tar_actB,input$select_reg_actB)
      
      new_data <- as_tsibble(wide_dat) %>%
        filter(!datetime %in% train_data$datetime) %>% # using a 70:30 split here
        tsibble::fill_gaps() %>%
        tidyr::fill(., .direction = "down") %>%
        select(all_of(col_names)) %>%
        mutate(across(input$select_reg_actB, list(zscore = ~as.numeric(scale(.)))))
      
      # predictions and confidence intervals for fable models
      pred_arima0 <- forecast(actB.arima$arima, new_data = new_data) %>%
        hilo() 
      pred_arima <- pred_arima0 %>%
        mutate(lower = `95%`$lower,
               upper = `95%`$upper) %>%
        select(.model, datetime, .mean, lower, upper)
      pred_a <- pred_arima0 %>% pull(input$select_tar_actB)
      dist_params$arima <- distributional::parameters(pred_a)
      
      rw_cols <- c("datetime",input$select_tar_actB)
      new_rw_data <- new_data %>%
        select(all_of(rw_cols))
      colnames(new_rw_data) <- c("datetime","target")
      pred_rw0 <- forecast(actC.models$rw, new_data = new_rw_data) %>%
        hilo() 
      pred_rw <- pred_rw0 %>%
        mutate(lower = `95%`$lower,
               upper = `95%`$upper) %>%
        select(.model, datetime, .mean, lower, upper)
      pred_r <- pred_rw0 %>% pull(target)
      dist_params$rw <- distributional::parameters(pred_r)
      
      progress$set(value = 0.5)
      pred_nnetar0 <- forecast(actC.models$nnetar, new_data = new_data, times = 500) %>%
        hilo() 
      pred_nnetar <- pred_nnetar0 %>%
        mutate(lower = `95%`$lower,
               upper = `95%`$upper) %>%
        select(.model, datetime, .mean, lower, upper)
      pred_n <- pred_nnetar0 %>% pull(input$select_tar_actB)
      dist_params$nnetar <- data.frame(mu = mean(pred_n, na.rm = TRUE),
                                    sigma = sqrt(distributional::variance(pred_n, na.rm = TRUE)))
      
      # and for gam
      doy_cols <- c("doy",input$select_tar_actB)
      new_doy_data <- as.data.frame(new_data) %>%
        mutate(doy = yday(datetime)) %>%
        select(any_of(doy_cols))
      colnames(new_doy_data) <- c("x","y")
      pred_gam <- mgcv::predict.gam(actC.models$doy, new_doy_data, se.fit = TRUE)
      pred_doy <- data.frame(.model = "DOY",
                             datetime = new_data$datetime,
                             .mean = pred_gam[[1]],
                             upper = pred_gam[[1]] + 2*pred_gam[[2]],
                             lower = pred_gam[[1]] - 2*pred_gam[[2]])
      dist_params$doy <- data.frame(mu = pred_gam[[1]],
                                    sigma = pred_gam[[2]])
      
      pred_all <- bind_rows(pred_arima, pred_nnetar) %>%
        bind_rows(., pred_rw) %>%
        bind_rows(., pred_doy)
      
      p <- ggplot()+
        xlab("datetime")+
        ylab(input$select_tar_actB)+
        geom_point(data = plot_data, aes(x = datetime, y = target, color = set))+
        geom_ribbon(data = pred_all, aes(x = datetime, ymin = lower, ymax = upper, color = .model, fill = .model),
                    alpha = 0.5)+
        geom_line(data = pred_all, aes(x = datetime, y = .mean, group = .model, color = .model))+
        geom_vline(xintercept = train_test_line)+
        labs(color = NULL, fill = NULL)+
        scale_color_manual(values = c("training data" = "gray","testing data" = "black","ARIMA" = "#E69F00","NNETAR" = "#56B4E9",
                                      "persistence" = "#009E73","DOY" = "#CC79A7"))+
        scale_fill_manual(values = c("training data" = "gray","testing data" = "black","ARIMA" = "#E69F00","NNETAR" = "#56B4E9",
                                      "persistence" = "#009E73","DOY" = "#CC79A7"), guide = "none")+
        theme_classic()
      
      plot.pred.all$main <- p
      progress$set(value = 1)
      
      p1 <- ggplotly(p, dynamicTicks = TRUE) 
      p2 <- style(p1, showlegend = FALSE, traces = 3:6)
      
      for (i in 1:length(p2$x$data)){
        if (!is.null(p2$x$data[[i]]$name)){
          p2$x$data[[i]]$name =  gsub("\\(","",str_split(p2$x$data[[i]]$name,",")[[1]][1])
        }
      }
      
      return(p2)
      
    })
    
  })
  
  observe({
    input$select_tar_actB
    input$select_reg_actB
    
    output$pred_all_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(!is.null(actC.models$nnetar),
             message = "Please fit additional models in Objective 8.")
      )
      validate(
        need(input$plot_pred_models > 0,
             message = "Click 'Generate predictions with uncertainty'")
      )
      
      p <- ggplot() +
        annotate("text", x = 10,  y = 10,
                 size = 4,
                 label = "Looks like you've chosen new regressors!\nPlease click 'Fit ARIMA' in Obj. 7 and 'Fit additional models' in Obj. 8 to regenerate this plot.") + 
        theme_void()+
        theme(panel.grid = element_blank(),
              axis.line = element_blank())
      
      plot.pred.all$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download predictions of all models
  output$save_pred_all_plot <- downloadHandler(
    filename = function() {
      paste("QXX-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.pred.all$main, device = device)
    }
  )
  
  # Calculate ignorance
  ign.values <- reactiveValues(arima=NULL,
                               nnetar=NULL,
                               rw=NULL,
                               doy=NULL)
  
  observe({
    input$fit_arima2
    
    output$ign_table <- renderDT({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(!is.null(actC.models$nnetar),
             message = "Please fit additional models in Objective 8.")
      )
      validate(
        need(!is.null(dist_params$nnetar),
             message = "Please generate predictions with uncertainty above.")
      )
      validate(
        need(input$calc_ign3 > 0,
             message = "Click 'Calculate ignorance scores'")
      )
      
      dat <- stand.data()
      
      wide_dat <- dat %>%
        pivot_wider(names_from = "variable", values_from = "observation")
      
      train_data <- as_tsibble(wide_dat) %>%
        dplyr::slice_head(prop = input$prop) 
      
      col_names <- c("datetime",input$select_tar_actB,input$select_reg_actB)
      
      new_data <- as_tsibble(wide_dat) %>%
        filter(!datetime %in% train_data$datetime) %>% # using a 70:30 split here
        tsibble::fill_gaps() %>%
        select(all_of(col_names)) %>%
        mutate(across(input$select_reg_actB, list(zscore = ~as.numeric(scale(.)))))
      
      new_obs <- new_data %>%
        pull(input$select_tar_actB)
      
      ign_arima <- scoringRules::logs_norm(new_obs, dist_params$arima$mu, dist_params$arima$sigma)
      ign.values$arima <- round(mean(ign_arima, na.rm = TRUE),2)
      ign_nnetar <- scoringRules::logs_norm(new_obs, dist_params$nnetar$mu, dist_params$nnetar$sigma)
      ign.values$nnetar <- round(mean(ign_nnetar, na.rm = TRUE),2)
      ign_rw <- scoringRules::logs_norm(new_obs, dist_params$rw$mu, dist_params$rw$sigma)
      ign.values$rw <- round(mean(ign_rw, na.rm = TRUE),2)
      ign_doy <- scoringRules::logs_norm(new_obs, dist_params$doy$mu, dist_params$doy$sigma)
      ign.values$doy <- round(mean(ign_doy, na.rm = TRUE),2)
      
      ign.table <- data.frame(Model = c("ARIMA","NNETAR","persistence","DOY"),
                              Ignorance = c(ign.values$arima, ign.values$nnetar, ign.values$rw, ign.values$doy))
      
      return(ign.table)
      
    })
    
  })
  
  observe({
    input$select_tar_actB
    input$select_reg_actB
    
    output$ign_table <- renderText({ 
      
      validate(
        need(!is.null(input$upload_data),
             message = "Please upload your data in Objective 6.")
      )
      validate(
        need(valid$main == TRUE,
             message = "Please correct your data format in Objective 6.")
      )
      validate(
        need(!is.null(input$select_tar_actB),
             message = "Please select a target variable from the dropdown menu in Objective 7.")
      )
      validate(
        need(!is.null(input$select_reg_actB),
             message = "Please select at least 1 predictor from the dropdown menu in Objective 7.")
      )
      validate(
        need(length(input$select_reg_actB) <= 3,
             message = "Please only select up to 3 regressors to fit your model in Objective 7.")
      )
      validate(
        need(plot.stand2$stand.tracker == 1,
             message = "Please standardize your exogenous regressors in Objective 7.")
      )
      validate(
        need(input$prop <= 0.9,
             message = "Please reserve at least 10% of your data for testing (select a proportion of training data = 0.9 or less) in Objective 7.")
      )
      validate(
        need(actB.arima$nrow_model_df >= 60,
             message = "You are using fewer than 60 data points for model training. Please select a larger proportion of your data for training in Objective 7.")
      )
      validate(
        need(!is.null(actB.arima$arima),
             message = "Please fit an ARIMA model in Objective 7.")
      )
      validate(
        need(!is.null(actC.models$nnetar),
             message = "Please fit additional models in Objective 8.")
      )
      validate(
        need(!is.null(dist_params$nnetar),
             message = "Please generate predictions with uncertainty above.")
      )
      validate(
        need(input$calc_ign3 > 0,
             message = "Click 'Calculate ignorance scores'")
      )
      
      ign.table <- data.frame(Message = c("You've changed regressors! Please re-fit your ARIMA in Obj. 7."))
      
      return(ign.table)
      
    })
    
  })
  
  # Navigating Tabs ----
  #* Main Tab ====
  rv1 <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$maintab, {
    curr_tab1 <- input$maintab
    rv1$prev <- readr::parse_number(curr_tab1) - 1
    rv1$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  observe({
    toggleState(id = "prevBtn1", condition = rv1$prev > 0)
    if(rv1$nxt > 6 & rv3a$nxt > 10) {
      shinyjs::disable("nextBtn1")
    } else {
      shinyjs::enable("nextBtn1")
    }
    hide(selector = ".page")
  })


  # Next button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx + 1]
    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if(curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries3
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]    } 
    if(curr_tab1 == "mtab6" & rv3a$nxt > 10) {
      updateActionButton(session, inputId = "nextBtn1", label = paste("End of module"))
    } else {
      # shinyjs::show(id = "nextBtn1")
      updateActionButton(session, inputId = "nextBtn1", label = paste(new_nam, ">"))
    }
  })

  # Previous button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx - 1]

    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj1") idx2 <- idx2 - 1 # Move off Activty A label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj6") idx2 <- idx2 - 1 # Move off Activty B label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if (curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries3
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj9") idx2 <- idx2 - 1 # Move off Activty C label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if(curr_tab1 == "mtab1") {
      updateActionButton(session, inputId = "prevBtn1", label = paste("Module begins"))
    } else {
      # shinyjs::show(id = "prevBtn1")
      updateActionButton(session, inputId = "prevBtn1", label = paste("<", new_nam))
    }
  })


  # Advancing Tabs
  save_prompt <- reactiveValues(times = 0)
  observeEvent(input$nextBtn1, {
    
    if(input$maintab == "mtab4" & input$tabseries1 == "obj5" & save_prompt$times == 0) {
      showModal(
        modalDialog(
          title = "Save Progress",
          "Don't forget to save your progress as you go just in case you lose your internet connection. Click 'Bookmark my progress' at the top of the page and copy-paste the link into your report.")
      )
      save_prompt$times <- save_prompt$times+1
    } else if(input$maintab == "mtab5" & input$tabseries2 == "obj8" & save_prompt$times < 2) {
      showModal(
        modalDialog(
          title = "Save Progress",
          "Don't forget to save your progress as you go just in case you lose your internet connection. Click 'Bookmark my progress' at the top of the page and copy-paste the link into your report.")
      )
      save_prompt$times <- save_prompt$times+1
    } else {

    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    if (curr_tab1 == "mtab4" & rv1a$nxt < 6) {
      curr_obj <- input$tabseries1

      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("obj", rv1a$nxt))

    } else if (curr_tab1 == "mtab5" & rv2a$nxt < 9) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("obj", rv2a$nxt))
    } else if (curr_tab1 == "mtab6" & rv3a$nxt < 12) {
      curr_obj <- input$tabseries3
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("obj", rv3a$nxt))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "obj1")
      updateTabsetPanel(session, "tabseries2",
                        selected = "obj6")
      updateTabsetPanel(session, "tabseries3",
                        selected = "obj9")
      updateTabsetPanel(session, "maintab",
                        selected = paste0("mtab", rv1$nxt))
    }
    shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
    }
  })

  # Moving back through tabs
  observeEvent(input$prevBtn1, {
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    if (curr_tab1 == "mtab4" & rv1a$prev > 0) {
      curr_obj <- input$tabseries1

      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("obj", rv1a$prev))

    } else if (curr_tab1 == "mtab5" & rv2a$prev > 5) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("obj", rv2a$prev))
    } else if (curr_tab1 == "mtab6" & rv3a$prev > 8) {
      curr_obj <- input$tabseries3
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("obj", rv3a$prev))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "obj5")
      updateTabsetPanel(session, "tabseries2",
                        selected = "obj8")
      updateTabsetPanel(session, "tabseries3",
                        selected = "obj10")
      updateTabsetPanel(session, "maintab",
                        selected = paste0("mtab", rv1$prev))
    }
    shinyjs::runjs("window.scrollTo(0, 0)")

  })

  #* Tab 1a ----
  rv1a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries1, {
    curr_tab1 <- input$tabseries1
    rv1a$prev <- readr::parse_number(curr_tab1) - 1
    rv1a$nxt <- readr::parse_number(curr_tab1) + 1
  })

  #* Tab 2a ----
  rv2a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries2, {
    curr_tab1 <- input$tabseries2
    rv2a$prev <- readr::parse_number(curr_tab1) - 1
    rv2a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  #* Tab 3a ----
  rv3a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries3, {
    curr_tab1 <- input$tabseries3
    rv3a$prev <- readr::parse_number(curr_tab1) - 1
    rv3a$nxt <- readr::parse_number(curr_tab1) + 1
  })

  # Return to Introduction tab
  observeEvent(input$return_intro, {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab3")
    shinyjs::runjs("window.scrollTo(0, 600)") # scroll to top of page
  })
  observeEvent(input$return_intro2, {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab3")
    shinyjs::runjs("window.scrollTo(0, 600)") # scroll to top of page
  })

  # Embedded Action links
  observeEvent(input$act_A_obj_5, {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab4")
    updateTabsetPanel(session, "tabseries1",
                      selected = "obj5")
    shinyjs::runjs("window.scrollTo(0, 0)")
  })

  observeEvent(input$obj_2, {
    updateTabsetPanel(session, "tabseries1",
                      selected = "obj2")
    shinyjs::runjs("window.scrollTo(0, 620)")
  })
  
  #Bookmarking
  bookmarkingWhitelist <- c("phy_ic","row_num","run_fc3","load_fc3","assess_fc4","update_fc2",
                            "assess_fc3","run_fc2","load_fc2","conv_fc","add_lm3","run_qaqc2","add_lm2",
                            "run_qaqc1","load_fc","submit_ques","run_mod_ann","run_mod_parm",
                            "run_mod_ic","tabseries1","maintab","nut_uptake2","mort_rate2","phy_init2",
                            "nut_uptake","mort_rate","phy_init","parm_mort_rate","members2",
                            "add_newobs","add_obs","add_obs_parm","add_obs_ic","phy_init4","table01_rows_selected")

  observeEvent(input$bookmarkBtn, {
    session$doBookmark()
  })

  ExcludedIDs <- reactiveVal(value = NULL)

  observe({
    toExclude <- setdiff(names(input), bookmarkingWhitelist)
    setBookmarkExclude(toExclude)
    ExcludedIDs(toExclude)
  })

  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$sel_row <- input$table01_rows_selected
  })

  # Read values from state$values when we restore
  onRestore(function(state) {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab4")
    updateTabsetPanel(session, "tabseries1",
                      selected = "obj1")
  })

  onRestored(function(state) {
    updateSelectizeInput(session, "row_num", selected = state$values$sel_row)
  })


  
  # Remove tool tip from forward and back buttons
  observe({
    if(input$nextBtn1 > 2) {
      removeTooltip(session, "nextBtn1")
    }
    if(input$prevBtn1 > 2) {
      removeTooltip(session, "prevBtn1")
    }
  })
}

# end

