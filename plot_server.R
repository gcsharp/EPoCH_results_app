source("plot.R")

## When the user clicks "visualise results"...

output$exposureManhattanPlot_p <- renderPlotly({
    if (input$plot_data_manhattans == 0)
      return()
    isolate({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice_manhattans]
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]

    exp_df <- create_exposure_dfs(tolower(input$exposure_choice_manhattans),dat)
    filtered_df <- create_outcome_dfs(tolower(input$outcome_choice_manhattans),exp_df)
    if (input$exposure_choice_manhattans == "All") {
      create_manhattan_plot(filtered_df, input$dimension[2]-110,
                            "exposure_class", "Exposure class","p")
    } else {
      create_manhattan_plot(filtered_df, input$dimension[2]-110,
                            "exposure_subclass_time_dose", "Exposure type","p")
    }
    })
    })
  

  output$outcomeManhattanPlot_p <- renderPlotly({
    if (input$plot_data_manhattans == 0)
      return()
    isolate({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice_manhattans]
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]

    outc_df <- create_outcome_dfs(tolower(input$outcome_choice_manhattans),dat)
    filtered_df <- create_exposure_dfs(tolower(input$exposure_choice_manhattans),outc_df)
    if (input$outcome_choice_manhattans == "All") {
      create_manhattan_plot(filtered_df, input$dimension[2]-110,
                            "outcome_class", "Outcome class","p")
    } else {
      create_manhattan_plot(filtered_df, input$dimension[2]-110,
                            "outcome_subclass2_time", "Outcome type","p")
    }
    })
  })
  
  output$exposureManhattanPlot_c <- renderPlotly({
    if (input$plot_data_manhattans == 0)
      return()
    isolate({
      model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice_manhattans]
      dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]
      
      exp_df <- create_exposure_dfs(tolower(input$exposure_choice_manhattans),dat)
      filtered_df <- create_outcome_dfs(tolower(input$outcome_choice_manhattans),exp_df)
      if (input$exposure_choice_manhattans == "All") {
        create_manhattan_plot(filtered_df, input$dimension[2]-110,
                              "exposure_class", "Exposure class","c")
      } else {
        create_manhattan_plot(filtered_df, input$dimension[2]-110,
                              "exposure_subclass_time_dose", "Exposure type","c")
      }
    })
  })
  
  
  output$outcomeManhattanPlot_c <- renderPlotly({
    if (input$plot_data_manhattans == 0)
      return()
    isolate({
      model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice_manhattans]
      dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]
      
      outc_df <- create_outcome_dfs(tolower(input$outcome_choice_manhattans),dat)
      filtered_df <- create_exposure_dfs(tolower(input$exposure_choice_manhattans),outc_df)
      if (input$outcome_choice_manhattans == "All") {
        create_manhattan_plot(filtered_df, input$dimension[2]-110,
                              "outcome_class", "Outcome class","c")
      } else {
        create_manhattan_plot(filtered_df, input$dimension[2]-110,
                              "outcome_subclass2_time", "Outcome type","c")
      }
    })
  })

  output$exposureVolcanoPlot_ranked <- renderPlotly({
    if (input$plot_data_volcanoes == 0)
      return()
    isolate({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice_volcanoes]
    dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]
    exp_df <- create_exposure_dfs(tolower(input$exposure_choice_volcanoes),dat)
    filtered_df <- create_outcome_dfs(tolower(input$outcome_choice_volcanoes),exp_df)
    p_m <- create_volcano_plot(filter(filtered_df, person_exposed=="mother"),ranked=T)
    p_f <- create_volcano_plot(filter(filtered_df, person_exposed=="partner"),ranked=T)
    subplot(p_m, p_f, shareY = TRUE, titleX = TRUE)%>%
      layout(xaxis = list(title = "Standardised effect estimate",
                         range = list(-0.75, 0.75)),
             yaxis = list(title = "Rank of -log10(P)"))
    })
})
  
  output$exposureVolcanoPlot_raw <- renderPlotly({
    if (input$plot_data_volcanoes == 0)
      return()
    isolate({
      model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice_volcanoes]
      dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]
      exp_df <- create_exposure_dfs(tolower(input$exposure_choice_volcanoes),dat)
      filtered_df <- create_outcome_dfs(tolower(input$outcome_choice_volcanoes),exp_df)
      p_m <- create_volcano_plot(filter(filtered_df, person_exposed=="mother"),ranked=F)
      p_f <- create_volcano_plot(filter(filtered_df, person_exposed=="partner"),ranked=F)
      subplot(p_m, p_f, shareY = TRUE, titleX = TRUE)%>%
        layout(xaxis = list(title = "Standardised effect estimate",
                            range = list(-0.75, 0.75)),
               yaxis = list(title = "-log10(P)"))
    })
  })

  output$exposureCoeffPlot <- renderPlotly({
    if (input$plot_data_coeff == 0)
      return()
    isolate({
    dat <- global_data$data$all_res
    plots <- list()
    y_data <- c()
    plot_dfs <- list()
    
    for (l in 1:nrow(global_data$coeff_linkers)){
      explink <- as.vector(global_data$coeff_linkers[l,1:5])
      model <- as.vector(global_data$coeff_linkers[l,6])
      model <- global_data$df_models$shortname[global_data$df_models$name == model]
      coeff_filtered <- dat[which(dat$exposure_class==tolower(explink[1])&
                                    dat$person_exposed==tolower(explink[2])&
                                    tolower(dat$exposure_subclass)==tolower(explink[3])&
                                    dat$exposure_time==tolower(explink[4])&
                                    dat$exposure_dose_type==tolower(explink[5])&
                                    dat$model==model),]
      plot_df <- create_outcome_dfs(tolower(input$outcome_choice_coeff),coeff_filtered)
      plot_df <- plot_df[plot_df$outcome_type==tolower(input$outcome_type_coeff),]
      plot_dfs[[l]] <- plot_df
    }
    
   lapply(plot_dfs,print)
    
    maxrows <- max(unlist(lapply(plot_dfs,nrow)),na.rm = T)
    
    print("maxrows:")
    print(maxrows)
    
    for (l in 1:nrow(global_data$coeff_linkers)){
      explink <- as.vector(global_data$coeff_linkers[l,1:5])
      model <- as.vector(global_data$coeff_linkers[l,6])
      model <- global_data$df_models$shortname[global_data$df_models$name == model]
      plot_title <-  paste0(c(explink,model),collapse = "\n")
      y_data <- sort(unique(c(y_data, plot_dfs[[l]]$outcome_linker)))
      print(y_data)
      plots[[l]] <- create_coeff_plot(plot_dfs[[l]], y_data, plot_title,maxheight=maxrows)
    }

    if (length(plots) == 1){
      fig <- subplot(plots[[1]], shareX = TRUE, shareY = TRUE, titleX = TRUE)
    } else if (length(plots) == 2) {
      fig <- subplot(plots[[1]], plots[[2]], shareX = TRUE, shareY = TRUE, titleX = TRUE)
    } else if (length(plots) == 3) {
      fig <- subplot(plots[[1]], plots[[2]], plots[[3]], shareX = TRUE, shareY = TRUE, titleX = TRUE)
    } else if (length(plots) == 4) {
      fig <- subplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], shareX = TRUE, shareY = TRUE, titleX = TRUE)
    }

    fig <- fig %>% layout(yaxis = list(title = NULL,
                                       showline = FALSE,
  #                                     ticktext = str_to_sentence(y_data),
 #                                      tickvals = seq.int(1,length(y_data)),
                                       tickmode = "array"))
    })
})
  
  output$forestPlot <- renderPlotly({
    if (input$plot_data_forest == 0)
      return()
    isolate({
      model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice_forest]
      dat <- global_data$data$all_res[which(global_data$data$all_res$model==model),]
      dat <- dat[which(tolower(dat$exposure_linker)==tolower(input$forest_explink) &
                         tolower(dat$outcome_linker)==tolower(input$forest_outlink)),]
      print(dim(dat))
      print(head(dat))
      forest_df <- create_forest_dfs(dat, input$forest_explink, input$forest_outlink)
      print(forest_df)
      create_forest_plot(forest_df)
    })
  })

  
  ## triangulation
  
  
  output$triangSummary <- renderPlot({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      triang_sum <- global_data$triangulation_summaries$triangulation_pregnancy
      print(dim(triang_sum))
      triang_sum <- droplevels(triang_sum[(tolower(triang_sum$exposure)==tolower(input$exposure_class_tri))&
                                            (tolower(triang_sum$outcome_subclass2)==tolower(input$outcome_subclass2_tri))&
                                            (tolower(triang_sum$parent)==tolower(input$exposure_person_tri))&
                                            grepl(tolower(triang_sum$outcome_subclass),pattern=tolower(input$outcome_time_tri)),])
          print(dim(triang_sum))
      print(head(triang_sum))
      create_triangsum_plot(triang_sum)
    })
  })
  
  output$triangMVRplot <- renderPlotly({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      df <- global_data$data$all_res
      expclass <- tolower(input$exposure_class_tri)
      parent <- tolower(input$exposure_person_tri)
      outc <- tolower(input$outcome_subclass2_tri)
      outtime <-tolower(input$outcome_time_tri)
      P <-try(create_triangmvr_plot(df,expclass,parent,outc,outtime,dose=F,grs=F,time=F,sep=F,oe=F))
      if("try-error" %in% class(P)){
        P <-create_null_plotly()
      }
      P
    })
  })
  
  output$triangNCplot <- renderPlotly({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      df <- global_data$data$all_res
      expclass <- tolower(input$exposure_class_tri)
      parent <- tolower(input$exposure_person_tri)
      outc <- tolower(input$outcome_subclass2_tri)
      outtime <-tolower(input$outcome_time_tri)
      P<-create_triangNC_plot(df,expclass,parent,outc,outtime,dose=F,grs=F,time=F,sep=F,oe=F)
      if("try-error" %in% class(P)){
        P <-create_null_plotly()
      }
      P
    })
  })
    
  
  output$triangDOSEplot <- renderPlotly({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      df <- global_data$data$all_res
      expclass <- tolower(input$exposure_class_tri)
      parent <- tolower(input$exposure_person_tri)
      outc <- tolower(input$outcome_subclass2_tri)
      outtime <-tolower(input$outcome_time_tri)
      P <- try(create_triangDOSE_plot(df,expclass,parent,outc,outtime,dose=T,grs=F,time=F,sep=F,oe=F))
      if("try-error" %in% class(P)){
        P <-create_null_plotly()
      }
      P
    })
  })
    
  
  output$triangGRSplot <- renderPlotly({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      df <- global_data$data$all_res
      expclass <- tolower(input$exposure_class_tri)
      parent <- tolower(input$exposure_person_tri)
      outc <- tolower(input$outcome_subclass2_tri)
      outtime <-tolower(input$outcome_time_tri)
      P <- try(create_triangGRS_plot(df,expclass,parent,outc,outtime,dose=F,grs=T,time=F,sep=F,oe=F))
      if("try-error" %in% class(P)){
        P <-create_null_plotly()
      }
      P
    })
  })
  
  output$triangTIMEplot <- renderPlotly({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      df <- global_data$data$all_res
      expclass <- tolower(input$exposure_class_tri)
      parent <- tolower(input$exposure_person_tri)
      outc <- tolower(input$outcome_subclass2_tri)
      outtime <-tolower(input$outcome_time_tri)
      P <-try(create_triangTIME_plot(df,expclass,parent,outc,outtime,dose=F,grs=F,time=T,sep=F,oe=F))
      if("try-error" %in% class(P)){
        P <-create_null_plotly()
      }
      P
    })
  })

  
  output$triangSEPplot <- renderPlotly({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      df <- global_data$data$all_res
      expclass <- tolower(input$exposure_class_tri)
      parent <- tolower(input$exposure_person_tri)
      outc <- tolower(input$outcome_subclass2_tri)
      outtime <-tolower(input$outcome_time_tri)
      P <-try(create_triangSEP_plot(df,expclass,parent,outc,outtime,dose=F,grs=T,time=F,sep=T,oe=F))
      if("try-error" %in% class(P)){
        P <-create_null_plotly()
      }
      P
    })
  })
  
  output$triangOEplot <- renderPlotly({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      df <- global_data$data$all_res
      expclass <- tolower(input$exposure_class_tri)
      parent <- tolower(input$exposure_person_tri)
      outc <- tolower(input$outcome_subclass2_tri)
      outtime <-tolower(input$outcome_time_tri)
      P <-try(create_triangOE_plot(df,expclass,parent,outc,outtime,dose=F,grs=F,time=F,sep=F,oe=T))
      if("try-error" %in% class(P)){
        P <-create_null_plotly()
      }
        P
    })
  })
  
  
  output$tri_report_summary <- renderUI({
    if (input$gen_tri_report == 0)
      return()
    isolate({
    withMathJax(HTML(readLines(rmarkdown::render(input = "gen_tri_report_rmd_summary.Rmd",
                                                 output_format = rmarkdown::html_fragment(),
                                                 quiet = TRUE
    ))))
  })
  })
  
  output$tri_report_mvr <- renderUI({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      withMathJax(HTML(readLines(rmarkdown::render(input = "gen_tri_report_rmd_mvr.Rmd",
                                                   output_format = rmarkdown::html_fragment(),
                                                   quiet = TRUE
      ))))
    })
  })
  
  output$tri_report_dose <- renderUI({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      withMathJax(HTML(readLines(rmarkdown::render(input = "gen_tri_report_rmd_d.Rmd",
                                                   output_format = rmarkdown::html_fragment(),
                                                   quiet = TRUE
      ))))
    })
  })
  
  output$tri_report_negcon <- renderUI({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      withMathJax(HTML(readLines(rmarkdown::render(input = "gen_tri_report_rmd_n.Rmd",
                                                   output_format = rmarkdown::html_fragment(),
                                                   quiet = TRUE
      ))))
    })
  })
  
  output$tri_report_timing <- renderUI({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      withMathJax(HTML(readLines(rmarkdown::render(input = "gen_tri_report_rmd_p.Rmd",
                                                   output_format = rmarkdown::html_fragment(),
                                                   quiet = TRUE
      ))))
    })
  })
  
  output$tri_report_grs <- renderUI({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      withMathJax(HTML(readLines(rmarkdown::render(input = "gen_tri_report_rmd_g.Rmd",
                                                   output_format = rmarkdown::html_fragment(),
                                                   quiet = TRUE
      ))))
    })
  })
  
  output$tri_report_other_exp <- renderUI({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      withMathJax(HTML(readLines(rmarkdown::render(input = "gen_tri_report_rmd_oe.Rmd",
                                                   output_format = rmarkdown::html_fragment(),
                                                   quiet = TRUE
      ))))
    })
  })
  
  output$tri_report_sep <- renderUI({
    if (input$gen_tri_report == 0)
      return()
    isolate({
      withMathJax(HTML(readLines(rmarkdown::render(input = "gen_tri_report_rmd_sep.Rmd",
                                                   output_format = rmarkdown::html_fragment(),
                                                   quiet = TRUE
      ))))
    })
  })
  
  ## information
  
  output$information_tab <- renderUI({
      withMathJax(HTML(readLines(rmarkdown::render(input = "information_tab.Rmd",
                                                   output_format = rmarkdown::html_fragment(),
                                                   quiet = TRUE
      ))))
    })
  
  ## models information
  
  output$models_information_tab <- renderUI({
    withMathJax(HTML(readLines(rmarkdown::render(input = "models_information_tab.Rmd",
                                                 output_format = rmarkdown::html_fragment(),
                                                 quiet = TRUE
    ))))
  })
