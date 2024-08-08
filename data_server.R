source("data.R")



# when the user clicks the button to load the data...
loaded_data <- observeEvent(input$load_results,{

  # the data will load from dropbox (with a message explaining what's happening)
  showModal(modalDialog(HTML("
  <h1>Loading EPoCH Explorer</h1>
  <br>
  <p>This will take a few seconds...</p>"),size = "m", fade = T,footer = NULL))
  loaded_data <- import_data_local(global_data)
  
  triangulation_summaries <-import_triangulation_summaries()
  names(triangulation_summaries) <- sort(c("triangulation_pregnancy","triangulation_prenatal"))
 
  cohort_summaries <-import_table_ones()
  names(cohort_summaries) <- sort(c("alspac","bib","mcs","moba"))
  
  #Generate a dataframe matching the model names to their IDs (essentially "Model 1a" to "model1a" etc)
  model_n = 4 
  model_l = 2
  model_s = c("","_FEMALE","_MALE")
  X <- data.frame(shortname=paste0("model",as.vector(outer(as.vector(outer(1:model_n, letters[1:model_l], paste0)),model_s,paste0)))
  )
  X$name <-NA
  X$name[X$shortname=="model1a"]<-"Minimal [all]"
  X$name[X$shortname=="model2a"]<-"Standard [all]"
  X$name[X$shortname=="model3a"]<-"Standard + previous timepoint(s) [all]"
  X$name[X$shortname=="model4a"]<-"Standard + mediator(s) [all]"
  X$name[X$shortname=="model1b"]<-"Minimal + co-parent [all]"
  X$name[X$shortname=="model2b"]<-"Standard + co-parent [all]"
  X$name[X$shortname=="model3b"]<-"Standard + previous timepoint(s) + co-parent [all]"
  X$name[X$shortname=="model4b"]<-"Standard + mediator(s) + co-parent [all]"
  X$name[X$shortname=="model1a_FEMALE"]<-"Minimal [females]"
  X$name[X$shortname=="model2a_FEMALE"]<-"Standard [females]"
  X$name[X$shortname=="model3a_FEMALE"]<-"Standard + previous timepoint(s) [females]"
  X$name[X$shortname=="model4a_FEMALE"]<-"Standard + mediator(s) [females]"
  X$name[X$shortname=="model1b_FEMALE"]<-"Minimal + co-parent [females]"
  X$name[X$shortname=="model2b_FEMALE"]<-"Standard + co-parent [females]"
  X$name[X$shortname=="model3b_FEMALE"]<-"Standard + previous timepoint(s) + co-parent [females]"
  X$name[X$shortname=="model4b_FEMALE"]<-"Standard + mediator(s) + co-parent [females]"
  X$name[X$shortname=="model1a_MALE"]<-"Minimal [males]"
  X$name[X$shortname=="model2a_MALE"]<-"Standard [males]"
  X$name[X$shortname=="model3a_MALE"]<-"Standard + previous timepoint(s) [males]"
  X$name[X$shortname=="model4a_MALE"]<-"Standard + mediator(s) [males]"
  X$name[X$shortname=="model1b_MALE"]<-"Minimal + co-parent [males]"
  X$name[X$shortname=="model2b_MALE"]<-"Standard + co-parent [males]"
  X$name[X$shortname=="model3b_MALE"]<-"Standard + previous timepoint(s) + co-parent [males]"
  X$name[X$shortname=="model4b_MALE"]<-"Standard + mediator(s) + co-parent [males]"
  
  

  updateSelectizeInput(inputId = "exposure_choice_manhattans", selected = 'Exposure class',
                       choices = str_to_sentence(append(global_data$exp_classes, 'All', after=0)))

  updateSelectizeInput(inputId = "exposure_choice_volcanoes", selected = 'Exposure class',
                       choices = str_to_sentence(append(global_data$exp_classes, 'All', after=0)))
  
  updateSelectizeInput(inputId = "exposure_choice_coeff",
                       choices = str_to_sentence(global_data$exp_classes))
  
  updateSelectizeInput(inputId = "exposure_class_tri",
                       choices = str_to_sentence(c("smoking","alcohol consumption","caffeine consumption")))
  
  updateSelectizeInput(inputId = "exposure_choice_forest",
                       choices = str_to_sentence(global_data$exp_classes))
  

  # And the rest of the input options will be updated based on the data
  updateRadioButtons(session, "exp_class",
                     choices = loaded_data$exp_classes)
  updateRadioButtons(session, "out_class",
                     choices = loaded_data$out_classes)
  updateRadioButtons(session, "n_comparisons",
                     choices = 1:4)
  
  # the main app will appear
  shinyjs::show("epoch")
  shinyjs::hide("loading_page")

  # The message about loading the data will be removed
  removeModal()

  # And loaded_data will be returned for use elsewhere in the app
  global_data$data_is_loaded = TRUE
  global_data$data <- loaded_data
  global_data$triangulation_summaries <- triangulation_summaries
  global_data$df_models <- X
  global_data$cohort_summaries <-cohort_summaries
})

# Manhattan plot data organisation

observeEvent(input$exposure_choice_manhattans,{
  manhattan_dat <- global_data$data$all_res
  all_classes <- unique(manhattan_dat$outcome_class)
  if(input$exposure_choice_manhattans%in%manhattan_dat$exposure_class){
    all_classes <- unique(manhattan_dat$outcome_class[manhattan_dat$exposure_class==input$exposure_choice_manhattans])
  }
  updateSelectizeInput(session, inputId = "outcome_choice_manhattans",
                       choices = c("All",all_classes),
                       selected = '----------')
})

observeEvent(input$outcome_choice_manhattans,{
  manhattan_dat <- global_data$data$all_res
  all_models <- unique(global_data$df_models$name) #if both exp and out = All

  if(input$exposure_choice_manhattans=="All"&tolower(input$outcome_choice_manhattans)%in%manhattan_dat$outcome_class){ #if exp=All and outcome=specific
    all_models <- unique(
      global_data$df_models$name[global_data$df_models$shortname %in%
                                   manhattan_dat$model[manhattan_dat$outcome_class==tolower(input$outcome_choice_manhattans)]])
  }
  if(input$outcome_choice_manhattans=="All"&tolower(input$exposure_choice_manhattans)%in%manhattan_dat$exposure_class){ #if exp=specific and outcome=All
    all_models <- unique(
      global_data$df_models$name[global_data$df_models$shortname %in%
                                   manhattan_dat$model[manhattan_dat$exposure_class==tolower(input$exposure_choice_manhattans)]])
  }
  if(tolower(input$exposure_choice_manhattans)%in%manhattan_dat$exposure_class&tolower(input$outcome_choice_manhattans)%in%manhattan_dat$outcome_class){ #if both specific
    all_models <- unique(
      global_data$df_models$name[global_data$df_models$shortname %in%
                                   manhattan_dat$model[manhattan_dat$exposure_class==tolower(input$exposure_choice_manhattans)&
                                                         manhattan_dat$outcome_class==tolower(input$outcome_choice_manhattans)]])
  }
  updateSelectizeInput(session, inputId = "model_choice_manhattans",
                       choices = all_models,
                       selected = "Model 1a (all)")

})

# Volcano plot data organisation

observeEvent(input$exposure_choice_volcanoes,{
  manhattan_dat <- global_data$data$all_res
  all_classes <- unique(manhattan_dat$outcome_class)
  if(input$exposure_choice_volcanoes%in%manhattan_dat$exposure_class){
    all_classes <- unique(manhattan_dat$outcome_class[manhattan_dat$exposure_class==input$exposure_choice_volcanoes])
  }
  updateSelectizeInput(session, inputId = "outcome_choice_volcanoes",
                       choices = c("All",all_classes),
                       selected = '----------')
})

observeEvent(input$outcome_choice_volcanoes,{
  manhattan_dat <- global_data$data$all_res
  all_models <- unique(global_data$df_models$name) #if both exp and out = All
  
  if(input$exposure_choice_volcanoes=="All"&tolower(input$outcome_choice_volcanoes)%in%manhattan_dat$outcome_class){ #if exp=All and outcome=specific
    all_models <- unique(
      global_data$df_models$name[global_data$df_models$shortname %in%
                                   manhattan_dat$model[manhattan_dat$outcome_class==tolower(input$outcome_choice_volcanoes)]])
  }
  if(input$outcome_choice_volcanoes=="All"&tolower(input$exposure_choice_volcanoes)%in%manhattan_dat$exposure_class){ #if exp=specific and outcome=All
    all_models <- unique(
      global_data$df_models$name[global_data$df_models$shortname %in%
                                   manhattan_dat$model[manhattan_dat$exposure_class==tolower(input$exposure_choice_volcanoes)]])
  }
  if(tolower(input$exposure_choice_volcanoes)%in%manhattan_dat$exposure_class&tolower(input$outcome_choice_volcanoes)%in%manhattan_dat$outcome_class){ #if both specific
    all_models <- unique(
      global_data$df_models$name[global_data$df_models$shortname %in%
                                   manhattan_dat$model[manhattan_dat$exposure_class==tolower(input$exposure_choice_volcanoes)&
                                                         manhattan_dat$outcome_class==tolower(input$outcome_choice_volcanoes)]])
  }
  updateSelectizeInput(session, inputId = "model_choice_volcanoes",
                       choices = all_models,
                       selected = "Model 1a (all)")
  
})

# Coefficient plot data organisation


observeEvent(input$exposure_choice_coeff,{
  coeff_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "coeff_person",
                       choices = list("Mother", "Partner"),
                       selected = "Select person exposed")
})

observeEvent(input$coeff_person,{
  coeff_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "coeff_subclass",
                       choices = unique(str_to_sentence(
    coeff_dat$exposure_subclass[coeff_dat$exposure_class==tolower(input$exposure_choice_coeff)&
                                coeff_dat$person_exposed==tolower(input$coeff_person)]
                                       )),
                       selected = '----------')
})

observeEvent(input$coeff_subclass,{
  coeff_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "coeff_exptime",
                       choices = unique(str_to_sentence(
    coeff_dat$exposure_time[coeff_dat$exposure_class==tolower(input$exposure_choice_coeff)&
                            coeff_dat$person_exposed==tolower(input$coeff_person)&
                            tolower(coeff_dat$exposure_subclass)==tolower(input$coeff_subclass)])),
                       selected = '----------')
})


observeEvent(input$coeff_exptime,{
  coeff_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "coeff_exptypedose",
                       choices = unique(str_to_sentence(
                         coeff_dat$exposure_dose_type[coeff_dat$exposure_class==tolower(input$exposure_choice_coeff)&
                                                   coeff_dat$person_exposed==tolower(input$coeff_person)&
                                                   tolower(coeff_dat$exposure_subclass)==tolower(input$coeff_subclass)&
                                                     tolower(coeff_dat$exposure_time)==tolower(input$coeff_exptime)])),
                       selected = '----------')
})

observeEvent(input$coeff_exptypedose,{
  coeff_dat <- global_data$data$all_res
  print(tolower(input$exposure_choice_coeff))
  print(tolower(input$coeff_person))
  print(tolower(input$coeff_subclass))
  print(tolower(input$coeff_time))
  print(tolower(input$coeff_exptypedose))
  updateSelectizeInput(session, inputId = "model_choice_coeff",
                       choices = unique(
                         global_data$df_models$name[global_data$df_models$shortname %in%
                         coeff_dat$model[coeff_dat$exposure_class==tolower(input$exposure_choice_coeff)&
                                                   coeff_dat$person_exposed==tolower(input$coeff_person)&
                                                   tolower(coeff_dat$exposure_subclass)==tolower(input$coeff_subclass)&
                                           tolower(coeff_dat$exposure_time)==tolower(input$coeff_exptime)&
                                           coeff_dat$exposure_dose_type==tolower(input$coeff_exptypedose)
                                         ]]),
                       selected = '----------')
})


observeEvent(input$model_choice_coeff,{
  coeff_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "coeff_explink",
                       choices = unique(
    paste(coeff_dat$exposure_linker[coeff_dat$exposure_class==tolower(input$exposure_choice_coeff)&
                              coeff_dat$person_exposed==tolower(input$coeff_person)&
                              tolower(coeff_dat$exposure_subclass)==tolower(input$coeff_subclass)&
                              tolower(coeff_dat$exposure_time)==tolower(input$coeff_exptime)&
                                coeff_dat$exposure_dose_type==tolower(input$coeff_exptypedose)],
          input$model_choice_coeff
                                       )),
                       selected = unique(
                         paste(coeff_dat$exposure_linker[coeff_dat$exposure_class==tolower(input$exposure_choice_coeff)&
                                                           coeff_dat$person_exposed==tolower(input$coeff_person)&
                                                           tolower(coeff_dat$exposure_subclass)==tolower(input$coeff_subclass)&
                                                           tolower(coeff_dat$exposure_time)==tolower(input$coeff_exptime)&
                                                           coeff_dat$exposure_dose_type==tolower(input$coeff_exptypedose)],
                               input$model_choice_coeff
                         ))[1]
                              )
})

observeEvent(input$add_comp,{
  coeff_dat <- global_data$data$all_res
  new_linker <- coeff_dat[which(coeff_dat$exposure_class==tolower(input$exposure_choice_coeff)&
                                                  coeff_dat$person_exposed==tolower(input$coeff_person)&
                                                  tolower(coeff_dat$exposure_subclass)==tolower(input$coeff_subclass)&
                                                  tolower(coeff_dat$exposure_time)==tolower(input$coeff_exptime)&
                                                  coeff_dat$exposure_dose_type==tolower(input$coeff_exptypedose)),
                                c("exposure_class","person_exposed","exposure_subclass","exposure_time","exposure_dose_type")]
  new_linker <- new_linker[1,]
  new_linker$model <- input$model_choice_coeff
  print("NEW LINKER")
  print(new_linker)
  print("START COEFF_LINKERS")
  print(global_data$coeff_linkers)
  print("ISNULL, NROW coeff linkers")
  print(is.null(global_data$coeff_linkers))
  print(nrow(global_data$coeff_linkers))
  
  if(is.null((global_data$coeff_linkers))){
  global_data$coeff_linkers <-new_linker
  } else {
  
  ldf_len = nrow(global_data$coeff_linkers)
  print("LENGTH")
  print(ldf_len)

if (ldf_len == 4){
    showModal(modalDialog("Maximum of 4 comparisons can be made"))
}else{
    global_data$coeff_linkers[ldf_len + 1,] <- new_linker
    }
}
  print("END COEFF_LINKERS")
  print (global_data$coeff_linkers)
})

observeEvent(input$clear_comps,{
    global_data$coeff_linkers <- NULL
})


output$showActiveLinkers <- renderTable({
  global_data$coeff_linkers
})

observeEvent(input$add_comp,{
  coeff_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "outcome_choice_coeff",
                       choices = unique(str_to_sentence(
                         coeff_dat$outcome_class[coeff_dat$exposure_class==tolower(input$exposure_choice_coeff)&
                                           coeff_dat$person_exposed==tolower(input$coeff_person)&
                                           tolower(coeff_dat$exposure_subclass)==tolower(input$coeff_subclass)&
                                             tolower(coeff_dat$exposure_time)==tolower(input$coeff_exptime)&
                                             tolower(coeff_dat$exposure_dose_type)==tolower(input$coeff_exptypedose)&
                                             coeff_dat$model==global_data$df_models$shortname[global_data$df_models$name == input$model_choice_coeff]
                                           ])),
                       selected = '----------')
})

  observeEvent(input$outcome_choice_coeff,{
    coeff_dat <- global_data$data$all_res
    updateSelectizeInput(session, inputId = "outcome_type_coeff",
                         choices = unique(str_to_sentence(
                           coeff_dat$outcome_type[coeff_dat$exposure_class==tolower(input$exposure_choice_coeff)&
                                                    tolower(coeff_dat$person_exposed)==tolower(input$coeff_person)&
                                                    tolower(coeff_dat$exposure_subclass)==tolower(input$coeff_subclass)&
                                                    tolower(coeff_dat$exposure_time)==tolower(input$coeff_exptime)&
                                                    coeff_dat$exposure_dose_type==tolower(input$coeff_exptypedose)&
                                                     coeff_dat$model==global_data$df_models$shortname[global_data$df_models$name == input$model_choice_coeff]&
                                                    coeff_dat$outcome_class==tolower(input$outcome_choice_coeff)
                           ])),
                         selected = '----------')
    
})

  
# Triangulation organisation
  
  
  observeEvent(input$exposure_class_tri,{
    tri_dat <- global_data$data$all_res
    updateSelectizeInput(session, inputId = "exposure_person_tri",
                         choices = unique(str_to_sentence(
                           tri_dat$person_exposed[tri_dat$exposure_time %in% c("ever in pregnancy", "first trimester","second trimester", "third trimester") &
                                                    tri_dat$exposure_class==tolower(input$exposure_class_tri)]
                         )),
                         selected = "Select person exposed")
  })
  
  observeEvent(input$exposure_person_tri,{
    tri_dat <- global_data$data$all_res
    updateSelectizeInput(session, inputId = "outcome_class_tri",
                         choices = unique(str_to_sentence(
                           tri_dat$outcome_class[tri_dat$exposure_time %in% c("ever in pregnancy", "first trimester","second trimester", "third trimester")&
                                                   tri_dat$exposure_class==tolower(input$exposure_class_tri)&
                                                   tri_dat$person_exposed==tolower(input$exposure_person_tri)]
                         )),
                         selected = '----------')
  })
  
  observeEvent(input$outcome_class_tri,{
    tri_dat <- global_data$data$all_res
    updateSelectizeInput(session, inputId = "outcome_subclass1_tri",
                         choices = unique(str_to_sentence(
                           tri_dat$outcome_subclass1[tri_dat$exposure_time %in% c("ever in pregnancy", "first trimester","second trimester", "third trimester")&
                                                       tri_dat$exposure_class==tolower(input$exposure_class_tri)&
                                                       tri_dat$person_exposed==tolower(input$exposure_person_tri)&
                                                       tri_dat$outcome_class==tolower(input$outcome_class_tri)]
                         )),
                         selected = '----------')
  })
  
  observeEvent(input$outcome_subclass1_tri,{
    tri_dat <- global_data$data$all_res
    updateSelectizeInput(session, inputId = "outcome_subclass2_tri",
                         choices = unique(str_to_sentence(
                           tri_dat$outcome_subclass2[tri_dat$exposure_time %in% c("ever in pregnancy", "first trimester","second trimester", "third trimester")&
                                                       tri_dat$exposure_class==tolower(input$exposure_class_tri)&
                                                       tri_dat$person_exposed==tolower(input$exposure_person_tri)&
                                                       tri_dat$outcome_class==tolower(input$outcome_class_tri)&
                                                       tolower(tri_dat$outcome_subclass1)==tolower(input$outcome_subclass1_tri)]
                         )),
                         selected = '----------')
  })
  

# Forest plot data organisation
  
observeEvent(input$exposure_choice_forest,{
  forest_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "forest_person",
                       choices = unique(str_to_sentence(
                         forest_dat$person_exposed[forest_dat$exposure_class==tolower(input$exposure_choice_forest)]
                         )),
                       selected = "----------")
})

observeEvent(input$forest_person,{
  forest_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "forest_subclass",
                       choices = unique(str_to_sentence(
    forest_dat$exposure_subclass[forest_dat$exposure_class==tolower(input$exposure_choice_forest)&
                                forest_dat$person_exposed==tolower(input$forest_person)]
                                       )),
                       selected = '----------')
})

observeEvent(input$forest_subclass,{
  forest_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "forest_exptime",
                       choices = unique(str_to_sentence(
    forest_dat$exposure_time[forest_dat$exposure_class==tolower(input$exposure_choice_forest)&
                            forest_dat$person_exposed==tolower(input$forest_person)&
                              tolower(forest_dat$exposure_subclass)==tolower(input$forest_subclass)])),
                       selected = '----------')
})

observeEvent(input$forest_exptime,{
  forest_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "forest_explink",
                       choices = unique(str_to_sentence(
                         forest_dat$exposure_linker[forest_dat$exposure_class==tolower(input$exposure_choice_forest)&
                                                      forest_dat$person_exposed==tolower(input$forest_person)&
                                                      tolower(forest_dat$exposure_subclass)==tolower(input$forest_subclass)&
                                                                tolower(forest_dat$exposure_time)==tolower(input$forest_exptime)]
                       )),
                       selected = "Please select an option"
  )
})

observeEvent(input$forest_explink,{
  forest_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "outcome_choice_forest",
                       choices = unique(str_to_sentence(
    forest_dat$outcome_class[tolower(forest_dat$exposure_linker)==tolower(input$forest_explink)])),
                       selected = '----------')
})

observeEvent(input$outcome_choice_forest,{
  forest_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "forest_outcometype",
                       choices = unique(str_to_sentence(
                         forest_dat$outcome_subclass1[tolower(forest_dat$exposure_linker)==tolower(input$forest_explink)&
                                                        forest_dat$outcome_class==tolower(input$outcome_choice_forest)])),
                       selected = '----------')
})


observeEvent(input$forest_outcometype,{
  forest_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "forest_outlink",
                       choices = unique(str_to_sentence(
    forest_dat$outcome_linker[tolower(forest_dat$exposure_linker)==tolower(input$forest_explink)&
                                 forest_dat$outcome_class==tolower(input$outcome_choice_forest)&
                                 forest_dat$outcome_subclass1==tolower(input$forest_outcometype)]
                                       )),
                       selected = "Please select an option"
                              )
})

observeEvent(input$forest_outlink,{
  forest_dat <- global_data$data$all_res
  updateSelectizeInput(session, inputId = "model_choice_forest",
                       choices = unique(
                           global_data$df_models$name[tolower(global_data$df_models$shortname) %in%
                         tolower(forest_dat$model[tolower(forest_dat$exposure_linker)==tolower(input$forest_explink)&
                                                     forest_dat$outcome_linker==tolower(input$forest_outlink) ])
                           ]
                       ),
                       selected = unique(
                         global_data$df_models$name[tolower(global_data$df_models$shortname) %in%
                                                      tolower(forest_dat$model[tolower(forest_dat$exposure_linker)==tolower(input$forest_explink)&
                                                                                 forest_dat$outcome_linker==tolower(input$forest_outlink) ])
                         ]
                       )[1]
  )
})

# Download data

observeEvent(input$checklist, {
  selected_items <- input$checklist
  
  if (is.null(selected_items) || length(selected_items) < 3) {
    output$message <- renderText({
      "You must agree to all conditions to download the data"
    })
    hide("download_results")
  } else {
    output$message <- renderText({
      "Thank you for agreeing to these conditions"
    })
    show("download_results")
  }
})

output$download_results <-downloadHandler(
  filename = function() {
    paste("epochresults-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    shiny::withProgress(
      message = "Preparing to download...",
      value = 0,
      {
        shiny::incProgress(1/10)
        Sys.sleep(1)
        shiny::incProgress(5/10)
        # write.csv(global_data$data$all_res[,c("model","exposure_class","exposure_subclass","exposure_time","exposure_type","exposure_source","person_exposed","exposure_dose","outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type","cohorts","total_n","total_n_exposure","total_n_outcome","est","se","p","i2","hetp")], file,row.names=F) COMMENTED FOR NOW UNTIL FINAL VERSION
      }
    )
  }
)

output$inWords <- renderUI({
  if (input$plot_data_forest == 0)
    return()
  isolate({
    model <- global_data$df_models$shortname[global_data$df_models$name == input$model_choice_forest]
    resinWords <- interpret(res=global_data$data$all_res,
                            exp1=tolower(input$forest_explink),
                            out1=tolower(input$forest_outlink),
                            model=model)
HTML(resinWords)
  })
})

# Table ones
output$alspactable <- DT::renderDT(global_data$cohort_summaries$alspac,
                                   filter="top",rownames=F)
output$bibtable <- DT::renderDT(global_data$cohort_summaries$bib,
                                   filter="top",rownames=F)
output$mobatable <- DT::renderDT(global_data$cohort_summaries$moba,
                                   filter="top",rownames=F)
output$mcstable <- DT::renderDT(global_data$cohort_summaries$mcs,
                                   filter="top",rownames=F)



