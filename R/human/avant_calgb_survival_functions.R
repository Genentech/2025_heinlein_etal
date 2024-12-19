

analyze_survival_highLow_year_2 = function(set_data, 
                                           event = "OS", 
                                           feature = "coreHR")
  
{
  
  
  #event = ev
  
  #feature = "cb"
  # set_data = set_data[which(set_data$feature %in% c("high_low","low_high")),]
  # feature = "cb"
  # event = ev
  # 
  colnames(set_data) = c("time","event","feature","feature_group")
  
  
  set_data$time = set_data$time/365
  
  num_event_nz = which(set_data$event == 1)
  num_nevent_nz = which(set_data$event == 0)
  
  
  if(length(num_event_nz)>5 & length(num_nevent_nz)>5)
  {
    
    cpres = coxph(Surv(time, event) ~ feature , data = set_data)
    s = summary(cpres)
    
    rec_df = data.frame(feat = feature,
                        length_nzero_event = length(num_event_nz),
                        length_nzero_nevent = length(num_nevent_nz),
                        coef = round((s$coefficients[1,1]),3),
                        p =  round(s$coefficients[1,5],3),
                        wald_p = round(s$waldtest[3],3),
                        hazard_ratio = exp(s$coefficients[1,1]),
                        stringsAsFactors = F)
    rownames(rec_df) = NULL
    
    
    
    
    model_fit <- survfit(Surv(time, event) ~ feature_group , data = set_data)
    
    cpres = coxph(Surv(time, event) ~ feature_group , data = set_data)
    s = summary(cpres)
    
    # 
    p = autoplot(model_fit) +
      labs(x = paste0("\n", event," Time (Years) "), y = paste0(event, " Probabilities \n"),
           title = paste0(feature, 
                          " coef: ", round((s$coefficients[1,1]),3),
                          " p: ",round(s$sctest[3],3),
                          " HR: ", round(exp(s$coefficients[1,1]),3))) +
      theme_bw()+
      theme(plot.title = element_text(size = 9))+
      scale_color_manual(values = c(
                                    "low_high" = "purple", "high_low" = "darkgreen"))+
      scale_fill_manual(values = c(
                                   "low_high" = "purple", "high_low" = "darkgreen"))
    
    rl = list(rec_df, p)
    
    return(rl)
    
  }
  
}








analyze_survival_highLow_monthTyear_2 = function(set_data, 
                                                 event = "OS", 
                                                 feature = "coreHR")
  
{
  # 
  
  #set_data = set_data[which(set_data$feature %in% c("high_low","low_high")),]
  
  colnames(set_data) = c("time","event","feature","feature_group")
  
  
  set_data$time = set_data$time/12
  
  num_event_nz = which(set_data$event == 1)
  num_nevent_nz = which(set_data$event == 0)
  
  
  if(length(num_event_nz)>5 & length(num_nevent_nz)>5)
  {
    
    cpres = coxph(Surv(time, event) ~ feature , data = set_data)
    s = summary(cpres)
    
    rec_df = data.frame(feat = feature,
                        length_nzero_event = length(num_event_nz),
                        length_nzero_nevent = length(num_nevent_nz),
                        coef = round((s$coefficients[1,1]),3),
                        p =  round(s$coefficients[1,5],3),
                        wald_p = round(s$waldtest[3],3),
                        hazard_ratio = exp(s$coefficients[1,1]),
                        stringsAsFactors = F)
    rownames(rec_df) = NULL
    
    
    
    
    model_fit <- survfit(Surv(time, event) ~ feature_group , data = set_data)
    
    cpres = coxph(Surv(time, event) ~ feature_group , data = set_data)
    s = summary(cpres)
    
    # 
    p = autoplot(model_fit) +
      labs(x = paste0("\n", event," Time (Years) "), y = paste0(event, " Probabilities \n"),
           title = paste0(feature, 
                          " coef: ", round((s$coefficients[1,1]),3),
                          " p: ",round(s$sctest[3],3),
                          " HR: ", round(exp(s$coefficients[1,1]),3))) +
      theme_bw()+
      theme(plot.title = element_text(size = 9))+
      scale_color_manual(values = c(
                                    "low_high" = "purple", "high_low" = "darkgreen"))+
      scale_fill_manual(values = c(
                                   "low_high" = "purple", "high_low" = "darkgreen"))
    
    
    rl = list(rec_df, p)
    
    return(rl)
    
  }
  
}


