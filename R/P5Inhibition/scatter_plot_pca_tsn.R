
plot_pca = function(ori_data, 
                    tag,
                    type_annot,
                    simp_name,
                    col_code)
{
  
  #   
  # ori_data = gb_matrix
  # tag = er_tag
  # type_annot = er_annot
  # col_code = er_col
  #simp_name = gb_col$simp_name
  
  df = ori_data%>%
    na.omit()
  df_mat = as.matrix(df)
  
  
  
  pc = prcomp(t(df_mat))
  pc_v = pc$sdev/sum(pc$sdev)*100
  pc_v = round(pc_v, 2)
  
  pc_df = data.frame(sample = colnames(df), 
                     simp_name,
                     type = type_annot,
                     PC1 = pc$x[,1], PC2 = pc$x[,2], stringsAsFactors = F)
  
  
  mark_t = as.data.frame(table(pc_df$type))%>%
    dplyr::left_join(col_code, by = c("Var1" = "type"))%>%
    dplyr::arrange(Var1)
  
  
  wid = max(pc_df$PC1) - min(pc_df$PC1)
  hei = max(pc_df$PC2) - min(pc_df$PC2)
  
  
  p = ggplot(pc_df, aes(PC1,PC2))+
    geom_point(aes(color = type),
               size = 2) +
    scale_colour_manual(values=mark_t$color, name = "", labels = mark_t$Var1)+
    geom_text_repel(data = pc_df, aes(PC1, PC2, label = simp_name),
                    size = 2,
                    segment.size = 0.3,
                    segment.alpha = 0.3,
                    force = 4,
                    max.overlaps = Inf,
                    box.padding = unit(0.35, "lines"),
                    point.padding = unit(0.3, "lines"),
                    seed = 123)+
    theme_bw()+
    theme(text = element_text(size=7)) +
    coord_fixed(ratio = wid/hei)+
    ggtitle(tag)+
    labs(x= paste0("PC1 ", pc_v[1], "%"),
         y = paste0("PC2 ", pc_v[2], "%"))
  
  return(p)
  
}





plot_pca_13 = function(ori_data, 
                    tag,
                    type_annot,
                    simp_name,
                    col_code)
{
  
  #   
  # ori_data = gb_matrix
  # tag = er_tag
  # type_annot = er_annot
  # col_code = er_col
  #simp_name = gb_col$simp_name
  
  df = ori_data%>%
    na.omit()
  df_mat = as.matrix(df)
  
  
  
  pc = prcomp(t(df_mat))
  pc_v = pc$sdev/sum(pc$sdev)*100
  pc_v = round(pc_v, 2)
  
  pc_df = data.frame(sample = colnames(df), 
                     simp_name,
                     type = type_annot,
                     PC1 = pc$x[,1], PC3 = pc$x[,3], stringsAsFactors = F)
  
  
  mark_t = as.data.frame(table(pc_df$type))%>%
    dplyr::left_join(col_code, by = c("Var1" = "type"))%>%
    dplyr::arrange(Var1)
  
  
  wid = max(pc_df$PC1) - min(pc_df$PC1)
  hei = max(pc_df$PC3) - min(pc_df$PC3)
  
  
  p = ggplot(pc_df, aes(PC1,PC3))+
    geom_point(aes(color = type),
               size = 2) +
    scale_colour_manual(values=mark_t$color, name = "", labels = mark_t$Var1)+
    geom_text_repel(data = pc_df, aes(PC1, PC3, label = simp_name),
                    size = 2,
                    segment.size = 0.3,
                    segment.alpha = 0.3,
                    force = 4,
                    max.overlaps = Inf,
                    box.padding = unit(0.35, "lines"),
                    point.padding = unit(0.3, "lines"),
                    seed = 123)+
    theme_bw()+
    theme(text = element_text(size=7)) +
    coord_fixed(ratio = wid/hei)+
    ggtitle(tag)+
    labs(x= paste0("PC1 ", pc_v[1], "%"),
         y = paste0("PC3 ", pc_v[3], "%"))
  
  return(p)
  
}




library(Rtsne)
# 
# ori_data = gb_matrix
# tag = "ER_category"
# type_annot = gb_col$Tumor_ER_category
# col_code = er_col
# simp_name = gb_col$simp_name


plot_tsne = function(ori_data, 
                     tag,
                     type_annot,
                     simp_name,
                     col_code)
{
  
  
  df = ori_data%>%
    na.omit()
  df_mat = as.matrix(df)
  
  set.seed(123)
  tsne_out = Rtsne(t(df_mat), pca = FALSE, perplexity = 5, theta = 0)
  
  
  pc_df = data.frame(sample = colnames(df), 
                     simp_name,
                     type = type_annot,
                     tsne1 = tsne_out$Y[,1], tsne2 = tsne_out$Y[,2], stringsAsFactors = F)
  
  
  mark_t = as.data.frame(table(pc_df$type))%>%
    dplyr::left_join(col_code, by = c("Var1" = "type"))%>%
    dplyr::arrange(Var1)
  
  
  wid = max(pc_df$tsne1) - min(pc_df$tsne1)
  hei = max(pc_df$tsne2) - min(pc_df$tsne2)
  
  
  p = ggplot(pc_df, aes(tsne1, tsne2))+
    geom_point(aes(color = type),
               size = 2) +
    scale_colour_manual(values=mark_t$color, name = "", labels = mark_t$Var1)+
    geom_text_repel(data = pc_df, aes(tsne1, tsne2, label = simp_name),
                    size = 2,
                    segment.size = 0.3,
                    segment.alpha = 0.3,
                    force = 4,
                    max.overlaps = Inf,
                    box.padding = unit(0.35, "lines"),
                    point.padding = unit(0.3, "lines"),
                    seed = 123)+
    theme_bw()+
    theme(text = element_text(size=7)) +
    coord_fixed(ratio = wid/hei)+
    ggtitle(tag)+
    labs(x= "tsne 1",
         y ="tsne 2")
  
  return(p)
  
  
}


plot_pca_color_shape = 
function(ori_data, 
         tag,
         type_annot,
         shape_annot, 
         simp_name,
         col_code)
{
  
  #   
  # ori_data = gb_matrix
  # tag = er_tag
  # type_annot = er_annot
  # col_code = er_col
  #simp_name = gb_col$simp_name
  
  df = ori_data%>%
    na.omit()
  df_mat = as.matrix(df)
  
  
  
  pc = prcomp(t(df_mat))
  pc_v = pc$sdev/sum(pc$sdev)*100
  pc_v = round(pc_v, 2)
  
  pc_df = data.frame(sample = colnames(df), 
                     simp_name,
                     type = type_annot,
                     shape = shape_annot, 
                     PC1 = pc$x[,1], PC2 = pc$x[,2], stringsAsFactors = F)
  
  
  mark_t = as.data.frame(table(pc_df$type))%>%
    dplyr::left_join(col_code, by = c("Var1" = "type"))%>%
    dplyr::arrange(Var1)
  
  
  
  wid = max(pc_df$PC1) - min(pc_df$PC1)
  hei = max(pc_df$PC2) - min(pc_df$PC2)
  
  
  p = ggplot(pc_df, aes(PC1,PC2))+
    geom_point(aes(color = type, shape = shape),
               size = 2) +
    scale_colour_manual(values=mark_t$color, name = "", labels = mark_t$Var1)+
    geom_text_repel(data = pc_df, aes(PC1, PC2, label = simp_name),
                    size = 2,
                    segment.size = 0.3,
                    segment.alpha = 0.3,
                    force = 4,
                    max.overlaps = Inf,
                    box.padding = unit(0.35, "lines"),
                    point.padding = unit(0.3, "lines"),
                    seed = 123)+
    theme_bw()+
    theme(text = element_text(size=7)) +
    coord_fixed(ratio = wid/hei)+
    ggtitle(tag)+
    labs(x= paste0("PC1 ", pc_v[1], "%"),
         y = paste0("PC2 ", pc_v[2], "%"))
  
  return(p)
  
}

