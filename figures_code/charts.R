#### Setup ####

rm(list=ls())
library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(ggsci)
library(ggsignif)


library(scales)

#### GGPLOT Theme  #####

smaller_axis_textsize_001 = 9
larger_axis_textsize_001 = 18



#### Model Comparison ####

csv_names = c(
              "performance_metrics_kcal_3month_averagedb.csv",
              "performance_metrics_kcal_1month_averagedb.csv",
              "performance_metrics_kcal_singlebaskets.csv"
              )

ml_plt = function(csv_name, legend_switch){
  
  df = read.csv(csv_name) %>%
    filter(metric == "macro avg") %>%
    mutate(data_approach = gsub("macro", "Macronutrients", data_approach)) %>%
    mutate(data_approach = gsub("fg_maj", "Major food groups", data_approach)) %>%
    mutate(data_approach = gsub("fg_min", "Minor food groups", data_approach))
  
  df$model = as.factor(df$model)
  df$data_approach = as.factor(df$data_approach)
  
  
  df$model <- gsub('svm', 'Support Vector Machines', df$model)
  df$model <- gsub('xgb', 'XGBoost', df$model)
  df$model <- gsub('rf', 'Random Forest', df$model)
  df$model <- gsub('nb', 'Naive Bayes', df$model)
  
  chart = ggplot(df, mapping=aes(fill=data_approach, y=f1.score, x=(model), group=(data_approach))) + 
    
    geom_hline(yintercept=1, size=0.2, color = "grey") +
    geom_hline(yintercept=0.75, size=0.2, color = "grey") +
    geom_hline(yintercept=0.5, size=0.2, color = "grey") +
    geom_hline(yintercept=0.25, size=0.2, color = "grey") +
    
    geom_bar(position=position_dodge(0.8), stat="identity", width=0.7) +
    ylim(0, 1) +
    
    #adding data labels
    geom_text(aes(label=round(f1.score, digits=2)), position=position_dodge(width=0.8), vjust=-0.5, 
              size=3) + 
    
    theme_cowplot(12) +
    scale_fill_manual(values = c("#003f5c", "#bc5090", "#2a9d8f"),
      guide = guide_legend(override.aes = list(alpha = ifelse(legend_switch, 1, 0)))
    ) +
    labs(
      title = " ",
      fill="Data type",
      #subtitle = "Subtitle",
      #caption = "Caption"
    ) +      
    xlab("Algorithms") + 
    ylab("Macro-averaged F1-score") +
    
    scale_y_continuous(expand = c(0, 0)) + # making sure bars start at 0
    
    theme(
      #removing legend for top and bottom chart
        legend.text = element_text(color = ifelse(legend_switch, "black", "white")),
        legend.title = element_text(color = ifelse(legend_switch, "black", "white")),
        #legend.key = element_rect(alpha = ifelse(legend_switch, 1, 0)    ),
    
      axis.title.x = element_text(size=smaller_axis_textsize_001),
      axis.text.x = element_text(size=smaller_axis_textsize_001),
      
      axis.title.y = element_text(size=smaller_axis_textsize_001),
      axis.text.y = element_text(size=smaller_axis_textsize_001-1),
      
      plot.title = element_text(size=10)
    ) 
  
  return(chart)
}

ml1 = ml_plt(csv_names[1], legend_switch = FALSE)
ml2 = ml_plt(csv_names[2], legend_switch = TRUE)
ml3 = ml_plt(csv_names[3], legend_switch = FALSE)

plot_grid(ml1, ml2, ml3,
          labels = c("Quarterly baskets", "Monthly baskets", "Single baskets"),
          nrow = 3,
          ncol = 1,
          label_size = 12) 

ggsave("model_performance_comparison.pdf", width = 8, height =7)
