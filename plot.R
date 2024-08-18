library(shiny)
library(tidyverse)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(shinyTree)
library(RColorBrewer)
library(stringr)
library(scales)

graph_colours = "Dark2"

# Introduce a function to allow horizontal line plotting in plot_ly
hline <- function(y = 0, colour = "#898989") {
  list(type = "line", x0 = 0, x1 = 1,
       xref = "paper", y0 = y, y1 = y,
       line = list(color = colour, dash="dash")
  )
}

vline <- function(x = 0, colour = "#898989") {
  list(type = "line", x0 = x, x1 = x,
       yref = "paper", y0 = 0, y1 = 1,
       line = list(color = colour, dash="dash")
  )
}

plot_df_manhattan_p <- function(fig, df, x_data, label) {
  fig <- fig %>%
    add_markers(name = label, x = jitter(as.numeric(as.factor(df[[x_data]])), amount=0.3), y =-log10(df$p),
                color = as.character(df[[x_data]]),
                marker = list(size = 6), alpha=0.5,
                hoverinfo = "text",
                text = paste0("<br><b>Exposure class:</b> ",df$exposure_class,
                               "<br><b>Exposure type:</b> ",df$exposure_subclass_time_dose,
                               "<br><b>Outcome class:</b> ",df$outcome_class,
                               "<br><b>Outcome type:</b> ",df$outcome_subclass2_time,
                               "<br><b>Cohorts:</b> ",df$cohorts,
                               "<br><b>Parent Exposed:</b> ",df$person_exposed,
                               "<br><b>Total N:</b> ",df$total_n,
                              "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                              "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                              "<br><b>Cohens D:</b> ",signif(df$est_SDM,3),
                                "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                showlegend = FALSE)
}

plot_df_manhattan_c <- function(fig, df, x_data, label) {
  fig <- fig %>%
    add_markers(name = label, x = jitter(as.numeric(as.factor(df[[x_data]])), amount=0.3), y =df$est_SDM,
                color = as.character(df[[x_data]]),
                marker = list(size = 6), alpha=0.5,
                hoverinfo = "text",
                text = paste0("<br><b>Exposure class:</b> ",df$exposure_class,
                              "<br><b>Exposure type:</b> ",df$exposure_subclass_time_dose,
                              "<br><b>Outcome class:</b> ",df$outcome_class,
                              "<br><b>Outcome type:</b> ",df$outcome_subclass2_time,
                              "<br><b>Cohorts:</b> ",df$cohorts,
                              "<br><b>Parent Exposed:</b> ",df$person_exposed,
                              "<br><b>Total N:</b> ",df$total_n,
                              "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                              "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                              "<br><b>Cohens D:</b> ",signif(df$est_SDM,3),
                              "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                showlegend = FALSE)
}

create_manhattan_plot <- function(df, height, x_data, x_label, y_data) {
  adj_pthreshold <- 0.05/nrow(df)
  df_mother <- df[df$person_exposed=="mother",]
  df_partner <- df[df$person_exposed=="partner",]
  lmap_mother <- length(unique(df_mother[[x_data]]))
  lmap_partner <- length(unique(df_partner[[x_data]]))
  fig <- plot_ly(height = height, colors = graph_colours)
  if(y_data=="p"){
  fig <- plot_df_manhattan_p(fig, df_mother, x_data, label="Mother")
  fig <- plot_df_manhattan_p(fig, df_partner, x_data, label="Partner")
  fig <- fig %>% layout(shapes = list(hline(-log10(adj_pthreshold))))
  }
  if(y_data=="c"){
    fig <- plot_df_manhattan_c(fig, df_mother, x_data, label="Mother")
    fig <- plot_df_manhattan_c(fig, df_partner, x_data, label="Partner")
  }  
  
  fig <- fig %>% layout(xaxis = list(ticktext = str_to_sentence(unique(df[[x_data]])),
                                     tickvals = unique(as.numeric(as.factor(df[[x_data]]))),
                                     tickmode = "array"),
                        updatemenus = list(
                                    list(
                                      active = -1,
                                      type = 'buttons',
                                      buttons = list(
                                        list(label = "Mother",
                                             method = "update",
                                             args = list(list(visible = c(rep(TRUE,lmap_mother), rep(FALSE,lmap_partner))))),
                                        list(label = "Partner",
                                             method = "update",
                                             args = list(list(visible = c(rep(FALSE,lmap_mother), rep(TRUE,lmap_partner))))),
                                        list(label = "Both",
                                             method = "update",
                                             args = list(list(visible = c(TRUE))))
                                      )
                                    )
                                  )

                        ) %>%
    config(toImageButtonOptions = list(format = "png", scale = 5))
}

create_volcano_plot <- function(df,ranked) {
  adj_pthreshold <- 0.05/nrow(df)
  pthreshold_rank <- df$p_rank[which.min(abs(df$p-0.05))]-1
  adj_pthreshold_rank <- df$p_rank[which.min(abs(df$p-adj_pthreshold))]-1
  ttext <- str_to_sentence(unique(df$person_exposed))

  if(ranked==F){
    df %>%
      plot_ly(height = 540, colors=graph_colours) %>%
      add_markers(x = ~est_SDM,y = ~-log10(p), color = ~exposure_subclass,
                  marker = list(size = 6), alpha=0.5,
                  hoverinfo = "text",
                  text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                                 "<br><b>Exposure type:</b> ",exposure_subclass_time_dose,
                                 "<br><b>Outcome class:</b> ",outcome_class,
                                 "<br><b>Outcome type:</b> ",outcome_subclass_time,
                                 "<br><b>Cohorts:</b> ",cohorts,
                                 "<br><b>Total N:</b> ",total_n,
                                 "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                                 "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                                 "<br><b>Cohens D:</b> ",signif(df$est_SDM,3),
                                 "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                  showlegend = FALSE) %>%
      add_annotations(text = ttext,
                      x = 0.5,
                      y = 1,
                      yref = "paper",
                      xref = "paper",
                      xanchor = "left",
                      yanchor = "top",
                      showarrow = FALSE) %>%
      layout(xaxis = list(title = "Standardised effect estimate",
                          range = list(-0.75, 0.75)),
             yaxis = list(title = "-log10(P)",
                          rangemode = "tozero")) %>%
      config(toImageButtonOptions = list(format = "png", scale = 5))
  }else{
  
  if(ranked==T){
    df %>%
      plot_ly(height = 540, colors=graph_colours) %>%
      add_markers(x = ~est_SDM,y = ~p_rank, color = ~exposure_subclass,
                  marker = list(size = 6), alpha=0.5,
                  hoverinfo = "text",
                  text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                                 "<br><b>Exposure type:</b> ",exposure_subclass_time_dose,
                                 "<br><b>Outcome class:</b> ",outcome_class,
                                 "<br><b>Outcome type:</b> ",outcome_subclass_time,
                                 "<br><b>Cohorts:</b> ",cohorts,
                                 "<br><b>Total N:</b> ",total_n,
                                 "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                                 "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                                 "<br><b>Cohens D:</b> ",signif(df$est_SDM,3),
                                 "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                  showlegend = FALSE) %>%
      add_annotations(text = ttext,
                      x = 0.5,
                      y = 1,
                      yref = "paper",
                      xref = "paper",
                      xanchor = "left",
                      yanchor = "top",
                      showarrow = FALSE) %>%
      layout(xaxis = list(title = "Standardised effect estimate",
                          range = list(-0.75, 0.75)),
             yaxis = list(title = "Rank of -log10(P)",
                          rangemode = "tozero")) %>%
      config(toImageButtonOptions = list(format = "png", scale = 5))
  }
}}


create_coeff_plot <- function(df, ydat, title,maxheight) {
  df <- df[df$outcome_linker %in% ydat,]
  if("binary" %in% df$outcome_type){
    xtitle <- "Odds Ratio"
    x_origin = 1
    df %>%
      plot_ly(height = max(30*maxheight+50, 400)) %>%
      add_trace(x = ~or,y = ~outcome_subclass2_time, color="#7570b3", 
                type = "scatter",
                mode = 'markers',
                hoverinfo = "text",
                text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                               "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                               "<br><b>Cohorts:</b> ",cohorts,
                               "<br><b>Total N:</b> ",total_n,
                               "<br><b>Odds ratio:</b> ",exp(est),
                               "<br><b>Upper 95%CI:</b> ",or_uci,
                               "<br><b>Lower 95% CI:</b> ",or_lci,
                               "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                showlegend = FALSE) %>%
      add_segments(x=~or_lci,xend=~or_uci,y=~outcome_subclass2_time,yend =~outcome_subclass2_time,color="#7570b3",
                   hoverinfo = "text",
                   text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                                  "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                                  "<br><b>Cohorts:</b> ",cohorts,
                                  "<br><b>Total N:</b> ",total_n,
                                  "<br><b>Odds ratio:</b> ",exp(est),
                                  "<br><b>Upper 95%CI:</b> ",or_uci,
                                  "<br><b>Lower 95% CI:</b> ",or_lci,
                                  "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                   showlegend = FALSE)  %>%
      add_annotations(text = str_to_sentence(title), font = list(size=10), bgcolor="white",
                      x = x_origin, y = maxheight+4,
                      yref = "y", xref = "x",
                      xanchor = "middle", yanchor = "top",
                      showarrow = FALSE) %>%
      
      layout(shapes = list(vline(x_origin)),
             xaxis = list(title = xtitle,zeroline=F,
                          range=c(min(df$or_lci)-0.001,max(df$or_uci)+0.001)),
             yaxis = list(title="")) %>%
      config(toImageButtonOptions = list(format = "png", scale = 5))
    
  } else {
    xtitle <- "Std Dev. Difference"
    x_origin = 0
    
    df %>%
      plot_ly(height = max(20*maxheight+50, 400)) %>%
      add_trace(x = ~est,y = ~outcome_subclass2_time,color="#7570b3",
                type = "scatter",
                mode = 'markers',
                hoverinfo = "text",
                text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                               "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                               "<br><b>Cohorts:</b> ",cohorts,
                               "<br><b>Total N:</b> ",total_n,
                               "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                               "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                               "<br><b>Cohens D:</b> ",signif(df$est_SDM,3),
                               "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                               "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                               "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                showlegend = FALSE) %>%
      add_segments(x=~lci,xend=~uci,y=~outcome_subclass2_time,yend =~outcome_subclass2_time,color="#7570b3",
                   hoverinfo = "text",
                   text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                                  "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                                  "<br><b>Cohorts:</b> ",cohorts,
                                  "<br><b>Total N:</b> ",total_n,
                                  "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                                  "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                                  "<br><b>Cohens D:</b> ",signif(df$est_SDM,3),
                                  "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                                  "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                                  "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                   showlegend = FALSE) %>%
      add_annotations(text = str_to_sentence(title), font = list(size=10), bgcolor="white",
                      x = x_origin, y = maxheight+4,
                      yref = "y", xref = "x",
                      xanchor = "middle", yanchor = "top",
                      showarrow = FALSE) %>%
      
      layout(shapes = list(vline(x_origin)),
             xaxis = list(title = xtitle,zeroline=FALSE,
                          range=c(min(df$lci)-0.001,max(df$uci)+0.001)),
             yaxis = list(title = "")) %>%
      config(toImageButtonOptions = list(format = "png", scale = 5))
  }
}
  
  
create_triangmvr_plot <-function(df,expclass,parent,outc,outtime,dose=F,grs=F,time=F,sep=F,oe=F){
     res <- create_triang_DFs(df,expclass,parent,outc,outtime,dose=F,grs=F,time=F,sep=F,oe=F)
     df <- droplevels(res$df)
    df$y <- paste0(df$model,": ",df$modelnumber,", ",df$mutualadj)
    df$modelnumber <- as.character(df$modelnumber)
    H <- nrow(df)*30
    nullvalue <- res$nullvalue
    xtitle <- res$xtitle
    
    df %>%
      plot_ly(x = ~est,y = ~y, color=~modelnumber, type = "scatter",mode="markers",hoverinfo="text",height=H,
              text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                             "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                             "<br><b>Cohorts:</b> ",cohorts,
                             "<br><b>Total N:</b> ",total_n,
                             "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                             "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                             "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                             "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                             "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),showlegend = FALSE) %>%
      add_segments(x=~lci,xend=~uci,y=~y,yend =~y,color=~modelnumber,
                   hoverinfo = "text",
                   text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                                  "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                                  "<br><b>Cohorts:</b> ",cohorts,
                                  "<br><b>Total N:</b> ",total_n,
                                  "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                                  "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                                  "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                                  "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                                  "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                   showlegend = FALSE)  %>%
      layout(shapes = list(vline(nullvalue)),
             xaxis = list(title = xtitle,zeroline=F,
                          range=range(c(min(df$lci)-0.001,max(df$uci)+0.001,nullvalue))),
             yaxis = list(title="")) %>%
      config(toImageButtonOptions = list(format = "png", scale = 5))
  }
  
create_triangNC_plot <-function(df,expclass,parent,outc,outtime,dose=F,grs=F,time=F,sep=F,oe=F){
  
  res <- create_triang_DFs(df,expclass,parent,outc,outtime,dose=F,grs=F,time=F,sep=F,oe=F)
  
  bdf <- droplevels(res$bdf[res$bdf$model%in%c("model2a","model2b"),])
  bdf <-bdf[order(bdf$person_exposed,bdf$model),]
  bdf$y <- paste0(bdf$model," (",tolower(bdf$modelnumber),"): ",bdf$mutual_parent)
  bdf$y <-factor(bdf$y,ordered=T,levels=unique(bdf$y))
  H <- nrow(bdf)*60
  nullvalue <- res$nullvalue
  xtitle <- res$xtitle
  
  pal <- c(rep(brewer.pal(4,"Set2")[1],2),rep(brewer.pal(3,"Set2")[3],2))
  pal <- setNames(pal, c("mother (no mutual adj)", "mother (mutual adj)", "partner (no mutual adj)","partner (mutual adj)"))

  
    bdf %>%
      plot_ly(x = ~est,y = ~y, color=~mutual_parent, type = "scatter",mode="markers",hoverinfo="text",height=H,
              colors=pal,
              text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                             "<br><b>Model:</b> ",modelnumber,
                             "<br><b>Mutual adjustment:</b> ",mutual_parent,
                             "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                             "<br><b>Cohorts:</b> ",cohorts,
                             "<br><b>Total N:</b> ",total_n,
                             "<br><b>Estimate:</b> ",ifelse(bdf$outcome_type=="continuous",signif(bdf$est,3),"N/A"),
                             "<br><b>Odds Ratio:</b> ",ifelse(bdf$outcome_type=="binary",signif(exp(bdf$est),3),"N/A"),
                             
                             "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                             "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                             "<br><b>P value:</b> ",ifelse(bdf$p<0.0009,scientific(bdf$p,2),signif(bdf$p,2))),showlegend = FALSE) %>%
      add_segments(x=~lci,xend=~uci,y=~y,yend =~y,color=~mutual_parent,colors=pal,
                   hoverinfo = "text",
                   text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                                  "<br><b>Model:</b> ",modelnumber,
                                  "<br><b>Mutual adjustment:</b> ",mutual_parent,
                                  "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                                  "<br><b>Cohorts:</b> ",cohorts,
                                  "<br><b>Total N:</b> ",total_n,
                                  "<br><b>Estimate:</b> ",ifelse(bdf$outcome_type=="continuous",signif(bdf$est,3),"N/A"),
                                  "<br><b>Odds Ratio:</b> ",ifelse(bdf$outcome_type=="binary",signif(exp(bdf$est),3),"N/A"),

                                  "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                                  "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                                  "<br><b>P value:</b> ",ifelse(bdf$p<0.0009,scientific(bdf$p,2),signif(bdf$p,2))),
                   showlegend = FALSE)  %>%
      layout(shapes = list(vline(nullvalue)),
             xaxis = list(title = xtitle,zeroline=F,
                          range=range(c(min(bdf$lci)-0.001,max(bdf$uci)+0.001,nullvalue))),
             yaxis = list(title="")) %>%
      config(toImageButtonOptions = list(format = "png", scale = 5))
    


}

create_triangDOSE_plot <-function(df,expclass,parent,outc,outtime,dose=T,grs=F,time=F,sep=F,oe=F){
  res <- create_triang_DFs(df,expclass,parent,outc,outtime,dose=T,grs=F,time=F,sep=F,oe=F)
  df <- droplevels(res$df)
  df <- droplevels(df[df$model%in%(c("model2b")),]) 
  df <- df[order(df$modelname,df$exposure_time,df$exposure_dose),]
  df$y <- paste0(df$model," (",tolower(df$modelnumber),", ",df$mutualadj,"): ",df$exposure_dose, " amount - ", df$exposure_time)
  df$y <-factor(df$y,ordered=T,levels=unique(df$y))
  df$exposure_dose <- as.character(df$exposure_dose)
  H <- nrow(df)*30
  nullvalue <- res$nullvalue
  xtitle <- res$xtitle
  
  pal <- brewer.pal(4,"BuPu")[2:4]
  pal <- setNames(pal, c("light","moderate","heavy"))
  

    
  df %>%
    plot_ly(x = ~est,y = ~y, color=~exposure_dose, type = "scatter",mode="markers",hoverinfo="text",height=H,colors=pal,
            text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                           "<br><b>Exposure dose:</b> ",exposure_dose,
                           "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                           "<br><b>Cohorts:</b> ",cohorts,
                           "<br><b>Total N:</b> ",total_n,
                           "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                           "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                           
                           "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                           "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                           "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),showlegend = FALSE) %>%
    add_segments(x=~lci,xend=~uci,y=~y,yend =~y,color=~exposure_dose,colors=pal,
                 hoverinfo = "text",
                 text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                                "<br><b>Exposure dose:</b> ",exposure_dose,
                                "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                                "<br><b>Cohorts:</b> ",cohorts,
                                "<br><b>Total N:</b> ",total_n,
                                "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                                "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                                
                                "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                                "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                                "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                 showlegend = FALSE)  %>%
    layout(shapes = list(vline(nullvalue)),
           xaxis = list(title = xtitle,zeroline=F,
                        range=range(c(min(df$lci)-0.001,max(df$uci)+0.001,nullvalue))),
           yaxis = list(title="")) %>%
    config(toImageButtonOptions = list(format = "png", scale = 5))
    

}

create_triangTIME_plot <-function(df,expclass,parent,outc,outtime,dose=F,grs=F,time=T,sep=F,oe=F){
  res <- create_triang_DFs(df,expclass,parent,outc,outtime,dose=F,grs=F,time=T,sep=F,oe=F)
  df <- droplevels(res$df)
  df <- df[order(df$exposure_time,df$modelname),]
  df$y <- paste0(df$model," (",tolower(df$modelnumber),"): ",df$exposure_time)
  df$y <-factor(df$y,ordered=T,levels=unique(df$y))
  df$exposure_time2 <- as.character(df$exposure_time %in%c("preconception","first two postnatal years"))
  H <- nrow(df)*30
  nullvalue <- res$nullvalue
  xtitle <- res$xtitle
  

    df %>%
      plot_ly(x = ~est,y = ~y, color=~exposure_time2, type = "scatter",mode="markers",hoverinfo="text",height=H,
              text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                             "<br><b>Exposure time:</b> ",exposure_time,
                             "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                             "<br><b>Cohorts:</b> ",cohorts,
                             "<br><b>Total N:</b> ",total_n,
                             "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                             "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                             
                             "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                             "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                             "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),showlegend = FALSE) %>%
      add_segments(x=~lci,xend=~uci,y=~y,yend =~y,color=~exposure_time2,
                   hoverinfo = "text",
                   text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                                  "<br><b>Exposure time:</b> ",exposure_time,
                                  "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                                  "<br><b>Cohorts:</b> ",cohorts,
                                  "<br><b>Total N:</b> ",total_n,
                                  "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                                  "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                                  
                                  "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                                  "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                                  "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                   showlegend = FALSE)  %>%
      layout(shapes = list(vline(nullvalue)),
             xaxis = list(title = xtitle,zeroline=F,
                          range=range(c(min(df$lci)-0.001,max(df$uci)+0.001,nullvalue))),
             yaxis = list(title="")) %>%
      config(toImageButtonOptions = list(format = "png", scale = 5))
    

}

create_triangSEP_plot <-function(df,expclass,parent,outc,outtime,dose=F,grs=F,time=F,sep=T,oe=F){
  res <- create_triang_DFs(df,expclass,parent,outc,outtime,dose=F,grs=F,time=F,sep=T,oe=F)
  df <- droplevels(res$sep_df)
  df <- df[order(df$exposure_class,df$exposure_time_sep),]
  df$y <- paste0(df$model," (",tolower(df$modelnumber),"): ",df$exposure_time_sep)
  df$y <-factor(df$y,ordered=T,levels=unique(df$y))
  df$exposure_class <- as.character(df$exposure_class)
  H <- nrow(df)*30
  nullvalue <- res$nullvalue
  xtitle <- res$xtitle
  
    
    df %>%
      plot_ly(x = ~est,y = ~y, color=~exposure_class, type = "scatter",mode="markers",hoverinfo="text",height=H,
              text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                             "<br><b>Exposure subclass:</b> ",exposure_subclass,
                             "<br><b>Exposure time:</b> ",exposure_time,
                             "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                             "<br><b>Cohorts:</b> ",cohorts,
                             "<br><b>Total N:</b> ",total_n,
                             "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                             "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                             
                             "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                             "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                             "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),showlegend = FALSE) %>%
      add_segments(x=~lci,xend=~uci,y=~y,yend =~y,color=~exposure_class,
                   hoverinfo = "text",
                   text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                                  "<br><b>Exposure subclass:</b> ",exposure_subclass,
                                  "<br><b>Exposure time:</b> ",exposure_time,
                                  "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                                  "<br><b>Cohorts:</b> ",cohorts,
                                  "<br><b>Total N:</b> ",total_n,
                                  "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                                  "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                                  
                                  "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                                  "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                                  "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                   showlegend = FALSE)  %>%
      layout(shapes = list(vline(nullvalue)),
             xaxis = list(title = xtitle,zeroline=F,
                          range=range(c(min(df$lci)-0.001,max(df$uci)+0.001,nullvalue))),
             yaxis = list(title="")) %>%
      config(toImageButtonOptions = list(format = "png", scale = 5))

}

create_triangOE_plot <-function(df,expclass,parent,outc,outtime,dose=F,grs=F,time=F,sep=F,oe=T){
  res <- create_triang_DFs(df,expclass,parent,outc,outtime,dose=F,grs=F,time=F,sep=F,oe=T)
  df <- droplevels(res$sc_df)
  df <- df[order(df$exposure_subclass,df$exposure_time,df$exposure_dose2),]
  df$y <- paste0(df$model," (",tolower(df$modelnumber),"): ",df$exposure_subclass,", ",df$exposure_time)
  df$y <-factor(df$y,ordered=T,levels=unique(df$y))
  df$exposure_subclass <- as.character(df$exposure_subclass)
  H <- nrow(df)*30
  nullvalue <- res$nullvalue
  xtitle <- res$xtitle
  if(any(c("passive smoke exposure",'binge drinking',"coffee","tea","cola")%in%df$exposure_subclass)){
    df <- df }else{
      df <-0
}

    df %>%
      plot_ly(x = ~est,y = ~y, color=~exposure_subclass, type = "scatter",mode="markers",hoverinfo="text",height=H,
              text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                             "<br><b>Exposure subclass:</b> ",exposure_subclass,
                             "<br><b>Exposure time:</b> ",exposure_time,
                             "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                             "<br><b>Cohorts:</b> ",cohorts,
                             "<br><b>Total N:</b> ",total_n,
                             "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                             "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                             
                             "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                             "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                             "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),showlegend = FALSE) %>%
      add_segments(x=~lci,xend=~uci,y=~y,yend =~y,color=~exposure_subclass,
                   hoverinfo = "text",
                   text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                                  "<br><b>Exposure subclass:</b> ",exposure_subclass,
                                  "<br><b>Exposure time:</b> ",exposure_time,
                                  "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                                  "<br><b>Cohorts:</b> ",cohorts,
                                  "<br><b>Total N:</b> ",total_n,
                                  "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                                  "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                                  
                                  "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                                  "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                                  "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                   showlegend = FALSE)  %>%
      layout(shapes = list(vline(nullvalue)),
             xaxis = list(title = xtitle,zeroline=F,
                          range=range(c(min(df$lci)-0.001,max(df$uci)+0.001,nullvalue))),
             yaxis = list(title="")) %>%
      config(toImageButtonOptions = list(format = "png", scale = 5))

}

create_triangGRS_plot <-function(df,expclass,parent,outc,outtime,dose=F,grs=T,time=F,sep=F,oe=F){
  res <- create_triang_DFs(df,expclass,parent,outc,outtime,dose=F,grs=T,time=F,sep=F,oe=F)
  df <- droplevels(res$df)
  df <- df[order(df$exposure_time,df$outcome_subclass2_time,df$mutual_model_grs),]
  df$y <- paste0(df$model," (",tolower(df$mutual_model_grs),"): ",df$exposure_time," grs")
  df$y <-factor(df$y,ordered=T,levels=unique(df$y))
  df$modelnumber <- as.character(df$modelnumber)
  H <- nrow(df)*30
  nullvalue <- res$nullvalue
  xtitle <- res$xtitle
    
    df %>%
      plot_ly(x = ~est,y = ~y, color=~modelnumber, type = "scatter",mode="markers",hoverinfo="text",height=H,
              text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                             "<br><b>GRS for:</b> ",exposure_time,
                             "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                             "<br><b>Cohorts:</b> ",cohorts,
                             "<br><b>Total N:</b> ",total_n,
                             "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                             "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                             
                             "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                             "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                             "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),showlegend = FALSE) %>%
      add_segments(x=~lci,xend=~uci,y=~y,yend =~y,color=~modelnumber,
                   hoverinfo = "text",
                   text = ~paste0("<br><b>Exposure class:</b> ",exposure_class,
                                  "<br><b>GRS for:</b> ",exposure_time,
                                  "<br><b>Outcome subclass:</b> ",outcome_subclass2_time,
                                  "<br><b>Cohorts:</b> ",cohorts,
                                  "<br><b>Total N:</b> ",total_n,
                                  "<br><b>Estimate:</b> ",ifelse(df$outcome_type=="continuous",signif(df$est,3),"N/A"),
                                  "<br><b>Odds Ratio:</b> ",ifelse(df$outcome_type=="binary",signif(exp(df$est),3),"N/A"),
                                  
                                  "<br><b>Upper 95%CI:</b> ",signif(uci,3),
                                  "<br><b>Lower 95%CI:</b> ",signif(lci,3),
                                  "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                   showlegend = FALSE)  %>%
      layout(shapes = list(vline(nullvalue)),
             xaxis = list(title = xtitle,zeroline=F,
                          range=range(c(min(df$lci)-0.001,max(df$uci)+0.001,nullvalue))),
             yaxis = list(title="")) %>%
      config(toImageButtonOptions = list(format = "png", scale = 5))

}

create_forest_plot <- function(df) {
  y_range = list(-1, length(df$cohort))
  if (all(df$binary)) {
    xtitle <- "Odds Ratio"
    x_origin = 1
    df<-df[,c("cohort","or","point_size","point_symbol","n","p","or_uci","or_lci","binary")]
    colnames(df) <- c("cohort","est","point_size","point_symbol","n","p","uci","lci","binary")
    x_range <- c(min(df$lci)-0.001,max(df$uci)+0.001)
  } else {
    xtitle <- "Std Dev. Difference"
    x_origin = 0
    x_range <- c(min(df$lci)-0.001,max(df$uci)+0.001)
  }
    df %>%
      plot_ly(width=650) %>%
      add_trace(x = ~est,y = ~cohort, color = ~cohort=="meta", 
                fill = ~cohort=="meta", size = ~point_size, symbol= ~point_symbol,
                type = "scatter", mode="markers", symbols=c("diamond","square"), hoverinfo = "text", opacity=1,
                text = paste0("<br><b>Sample size:</b> ",df$n,
                              "<br><b>Estimate:</b> ",ifelse(df$binary==F,signif(df$est,3),"N/A"),
                              "<br><b>Odds Ratio:</b> ",ifelse(df$binary==T,signif(df$est,3),"N/A"),
                              "<br><b>Upper 95% CI:</b> ",signif(df$uci,3),
                              "<br><b>Lower 95% CI:</b> ",signif(df$lci,3),
                              "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                showlegend = FALSE) %>%
      add_segments(x=~lci,xend=~uci,y=~cohort,yend =~cohort, color_discrete_sequence="goldenrod",
                   type = "scatter", mode="lines", hoverinfo = "text",
                   text = paste0("<br><b>Sample size:</b> ",df$n,
                                 "<br><b>Estimate:</b> ",ifelse(df$binary==F,signif(df$est,3),"N/A"),
                                 "<br><b>Odds Ratio:</b> ",ifelse(df$binary==T,signif(df$est,3),"N/A"),
                                 "<br><b>Upper 95% CI:</b> ",signif(df$uci,3),
                                 "<br><b>Lower 95% CI:</b> ",signif(df$lci,3),
                                 "<br><b>P value:</b> ",ifelse(df$p<0.0009,scientific(df$p,2),signif(df$p,2))),
                   showlegend = FALSE) %>%
      layout(shapes = list(vline(x_origin)),
             xaxis = list(title = xtitle,zeroline=FALSE),
             yaxis = list(title = " ", range = y_range,
                          tickfont = list(size = 16))) %>%
      config(toImageButtonOptions = list(format = "png", scale = 5))
}

create_triangsum_plot <- function(df) {
  cols <- c("negative\nassociation" = "#fc8d59", "positive\nassociation" = "#91bfdb","0 lines\nof evidence" = "grey90", "1 line\nof evidence" = "#edf8e9", "2 lines\nof evidence" = "#bae4b3", "3 lines\nof evidence"="#74c476", "4 lines\nof evidence"="#31a354", "5 lines\nof evidence"="#006d2c")
  ggplot(df,aes(x=variable2,y=outcome_subclass))+
    geom_tile(aes(fill=factor(value)),colour="white")+
    scale_fill_manual(values = cols,na.translate=F)+
    facet_nested(.~T,scales = "free",space="free")+
    theme_minimal()+
    guides(colour="none",fill = guide_legend(byrow = TRUE))+
    theme(axis.text=element_text(size=12),axis.title=element_blank(),
          panel.spacing.x = unit(c(0.1), "lines"),
          panel.grid.major.x=element_blank(),
          panel.grid.major.y=element_line(linewidth=0.2),
          strip.text=element_blank(),
          legend.title=element_blank(),legend.text=element_text(size=12),
          legend.key.spacing.y = unit(1, "lines"),
          legend.margin=margin(0,0,0,0),
          legend.box.spacing = unit(0, "pt"),
          legend.position = "bottom")
}


create_null_plotly <-function(){
  a <- list(
    text = "Insufficient data to create plot",
    showarrow=F,
    font = list(size = 20))
  P <-plotly_empty(type = 'scatter',
                   mode = 'text')
  P %>% layout(annotations = a) %>%
    config(toImageButtonOptions = list(format = "png", scale = 5))
}




