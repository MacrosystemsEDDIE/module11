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
  
  # Download scatterplot of pacf
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
    
    multi.select$lst <- select_list
    
  })
  
  # Update the selectInput with the named list
  observe({
    updateSelectInput(session, "select", choices = multi.select$lst)
  })
  
  # test of multi select dropdown
  output$value <- renderText({input$select})
  
  # fit ARIMA
  observe({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(site_data()$data),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$fit_arima > 0,
           message = "Click 'Fit ARIMA'")
    )
    
    row_selected = sites_df[input$table01_rows_selected, ]
    site_id <- row_selected$SiteID
    
    if(site_id == "cann"){
      model_df4 <- as_tsibble(cann_model_data) %>%
        slice_head(prop = .7) %>% # using a 70:30 split here
        fill_gaps() %>%
        select(datetime, chla, input$select)
      
      my.arima <- model_df4 %>%
        model(`ARIMA` = fable::ARIMA(formula = chla ~ .)) # get errors if include all variables due to small data size
    }
    
    
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
    if(rv1$nxt > 6 & rv3a$nxt > 12) {
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
    if(curr_tab1 == "mtab6" & rv3a$nxt > 12) {
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
      if(curr_obj == "obj11") idx2 <- idx2 - 1 # Move off Activty C label
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
    } else if(input$maintab == "mtab5" & input$tabseries2 == "obj10" & save_prompt$times < 2) {
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

    } else if (curr_tab1 == "mtab5" & rv2a$nxt < 11) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("obj", rv2a$nxt))
    } else if (curr_tab1 == "mtab6" & rv3a$nxt < 13) {
      curr_obj <- input$tabseries3
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("obj", rv3a$nxt))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "obj1")
      updateTabsetPanel(session, "tabseries2",
                        selected = "obj6")
      updateTabsetPanel(session, "tabseries3",
                        selected = "obj11")
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
    } else if (curr_tab1 == "mtab6" & rv3a$prev > 10) {
      curr_obj <- input$tabseries3
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("obj", rv3a$prev))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "obj5")
      updateTabsetPanel(session, "tabseries2",
                        selected = "obj10")
      updateTabsetPanel(session, "tabseries3",
                        selected = "obj12")
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

