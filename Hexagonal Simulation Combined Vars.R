setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source('Lib_Data_Source.R')



plot_sg_jointvar = function(shp_file = PA_14_shp , var1_matrix, var2_matrix=NULL , 
                            areas_to_plot , area_names_var , learn = 0.5 ,
                            grid_seed = 14 , grid = 'hexagonal' , category_names, 
                            variable_names=NULL, plot_main, save_plot = FALSE, 
                            clust_method, clust_num,  which_cols, include_cluster = TRUE  ,...){
  
  if (missing(shp_file)) {
    warning('No Shapefile Given!')
    stop()
  }
  
  if(missing(var1_matrix)){
    warning('Variable Matrix Not Found!')
    stop()
  }
  
  if(missing(areas_to_plot)){
    warning('Areas to Plot Not Found!')
    stop()
  }

  if(missing(area_names_var)){
    warning('Area Name Variable Not Found!')
    stop()
  }
  
  
    ## creating the hexagonal map dataframes
  
  shapes = calculate_grid(shp_file, learning_rate = learn, seed = grid_seed, grid_type = grid)
  shape_assign = assign_polygons(shp_file, shapes)
  shape_assign = as(shape_assign , 'Spatial')
  col_num = which(names(shape_assign@data) == area_names_var)
  
  #find plotting order of polygons to match
  fullnames = PA14_names
  plot_order_names = shape_assign@data[,col_num]
  
  shape_assign$abbr = abbreviate(plot_order_names, 1,5)
  x_mid = shape_assign$V1
  y_mid = shape_assign$V2
  shapefile = suppressWarnings(tidy(shape_assign))
  
  ##indexes to annotate PA
  index_inside = which(plot_order_names %in% areas_to_plot)
  index_not_inside = setdiff(1:55, index_inside)
  
  #subset & reordering of first variable
  qvals = var1_matrix[plot_order_names,]
  numarrows = ncol(var1_matrix)
  
  if (!missing(var2_matrix)) {
    #subset & reordering of 2nd variable 
    qvals2 = var2_matrix[plot_order_names,]
    
    #arrowthickness
    up_thick = as.numeric(cut(qvals2[index_inside,1],4))
    right_thick = as.numeric(cut(qvals2[index_inside,2],4))
    down_thick = as.numeric(cut(qvals2[index_inside,3],4))
    left_thick = as.numeric(cut(qvals2[index_inside,4],4))
    
    if (missing(variable_names)) {
      variable_names=c()
      vars = c('Length' , 'Thickness')
      for(variable in 1:2){
        print_msg = paste0('Input Variable ',  vars[variable], ':')
        variable_name_raw = dlg_input(message = print_msg, 
                                      default = "" , gui = .GUI)
  
        if (length(variable_name_raw[['res']]) ==0L) {
          warning('Variable Names not Specified!')
          stop()
        }
        variable_names[variable] = variable_name_raw[['res']]
      }
    }
    
    #extra legend
    
    GG_extra_legend = list(

      annotate("rect", xmin = 37000, xmax = 45000, ymin = 23000, ymax = 28500,
               alpha = 1, linetype = 1, colour = 'black',fill = 'white'),
      
      annotate("text", x = 41000, y = 26460, 
               label = paste('Length = ', as.character(variable_names[1])), 
               alpha = 1, colour = 'black', size = 3, fontface = 'bold' )  ,
      
      annotate("text", x = 41000, y = 24830, 
                 label = paste('Thickness = ', as.character(variable_names[2])  ), 
               alpha = 1, colour = 'black', size = 3, fontface = 'bold' )  
    )
  }
  else{
    #arrowthickness
    up_thick = 0.8
    right_thick = 0.8
    down_thick = 0.8
    left_thick = 0.8
    
  }
  
  newcol = c('orange3', 'grey20' , 'magenta' , 'purple' , 'brown' , 'sienna3' , 'darkslateblue' , 'aquamarine' , 'black')
  
  cluster_fn = function(shp_file , info_matrix ,clust_method=c('Kmeans', 'Kmedian','Hclust') , 
                        clust_num , seed = 1 , columns, area_names, 
                        colour_types = newcol, 
                        areas_to_plot, plot_map = TRUE, fullnames){
    
    if (missing(clust_method) | !clust_method%in% c('Kmeans', 'Kmedian','Hclust') )  {
      warning('Choose Clust method: Kmeans, Kmedian or hclust')
      stop()
    }
    
    if (missing(plot_map)) {
      plot_map_raw = tk_messageBox(caption = "Plotting", 
                              message = 'Do you want to Plot Map of Singapore?', 
                              icon = "info", type = "yesnocancel")
      plot_map =ifelse(plot_map_raw == 'yes',TRUE, FALSE)
    }
    
    
    land_plot = function(shp_file, area_names,colour_types, clust_num, 
                         area_cluster_list, areas_to_plot, plot_map){
      #cluster names for legend plot
      cluster_names = function(clust_num){
        vec = vector()
        for (i in 1:clust_num) {
          vec=c(vec , paste('Cluster',i))
        }
        return(vec)
      }
      
      cluster_numbers_legend = cluster_names(clust_num)
      par(mar=c(0,0,0,0))
      plot.new()
      
      column = which(names(shp_file) == area_names)
      not_in =   which(!fullnames %in% areas_to_plot)
      
      plot_col = vector(mode="character", length=length(fullnames))
      
      
      for (cluster_number in 1:length(area_cluster_list)) {
        regions = rownames(area_cluster_list[[cluster_number]])
        index =  which(fullnames%in%regions )
        plot_col[index] = colour_types[cluster_number]
      }
      
      plot_col[not_in] = 'grey40'   
      if (plot_map) {
        plot(shp_file[,column],  col = plot_col, main = paste(clust_method ,'Clustering of Planning Areas') )
        legend('bottomright' ,
               legend=cluster_numbers_legend,
               fill=colour_types, box.lty = 0, box.lwd = 0.5, y.intersp = 0.6, cex = 1)
      }
      return(plot_col)
    }
    
    
    if (clust_method == 'Kmeans') {
      kmeans_model = kmeans(info_matrix[,columns], centers = clust_num)
      kmeans_cluster = kmeans_model$cluster
      area_cluster = data.frame(row.names = rownames(info_matrix) ,
                                cluster= kmeans_cluster)
      attach(area_cluster) 
      area_cluster_list = split(area_cluster, cluster)
      kmeans_plot = land_plot(shp_file = shp_file, area_names = area_names , colour_types = newcol , 
                  clust_num = clust_num, area_cluster_list = area_cluster_list, 
                  areas_to_plot =  areas_to_plot, plot_map = plot_map ) 
      detach(area_cluster)
      plot_colours = data.frame(kmeans_plot)
      rownames(plot_colours) = fullnames
      return(list(area_cluster_list, plot_colours = plot_colours))
    }
    
    if (clust_method == 'Kmedian') {
      kmedian_model = kGmedian(info_matrix[,columns], ncenters = clust_num, ...)
      kmedian_cluster = kmedian_model$cluster
      area_cluster = data.frame(row.names = rownames(info_matrix) ,
                                kmedian_cluster= kmedian_cluster)
      attach(area_cluster) 
      area_cluster_list = split(area_cluster, kmedian_cluster)
      kmedian_plot = land_plot(shp_file = shp_file, area_names = area_names , colour_types = newcol , 
                  clust_num = clust_num, area_cluster_list = area_cluster_list, 
                  areas_to_plot = areas_to_plot , plot_map = plot_map) 
      plot_colours = data.frame(kmedian_plot)
      rownames(plot_colours) = fullnames
      detach(area_cluster)
      return(list(area_cluster_list, plot_colours = plot_colours))
    }
    
    if(clust_method == 'Hclust'){
      print_msg = paste('Input Linkage(average/single/complete/median/centroid): ')
      clust_method_raw = dlg_input(message = print_msg, 
                                   default = "" , gui = .GUI)
      link = clust_method_raw[['res']]
      new_info_mat = subset(info_matrix , rownames(info_matrix) %in% areas_to_plot)
      hclust_model = hclust(dist(new_info_mat), method = link)
      ans_raw = tk_messageBox(caption = "Plotting", 
                              message = 'Do you want to Plot Hierarchical Clustering Model?', 
                              icon = "info", type = "yesnocancel")
      ans_logic =ifelse(ans_raw == 'yes',TRUE, FALSE)
      if (ans_logic) {
        plot.new()
        par(OP)
        plot(hclust_model, main = paste('Dendrogram with ', link , ' linkage'), new = FALSE)
      }
      hclust_cluster = cutree(hclust_model, k=clust_num)
      
      area_cluster = data.frame(row.names = areas_to_plot ,
                                hclust= hclust_cluster)
      attach(area_cluster)
      
      hclust_list = split(area_cluster, hclust)
      
     
      hclust_plot = land_plot(shp_file = shp_file , area_names = area_names , colour_types = newcol , 
                  clust_num = clust_num, area_cluster_list = hclust_list, 
                  areas_to_plot = areas_to_plot, plot_map = plot_map)
      plot_colours = data.frame(hclust_plot)
      rownames(plot_colours) = fullnames
      detach(area_cluster)
      
      return(list(hclust_list, plot_colours = plot_colours))

    }
  }
  
  if(missing(var2_matrix)){
    cluster_res = cluster_fn(shp_file = shp_file,clust_method = clust_method, columns = which_cols,
                             info_matrix = var1_matrix, area_names = area_names_var,
                             clust_num = clust_num, areas_to_plot = areas_to_plot, fullnames = fullnames)
    plot_which = cluster_res[[2]]
    }
  
  else{
    
    cluster_res = cluster_fn(shp_file = shp_file,clust_method = clust_method, columns = which_cols,
                             info_matrix = var1_matrix, area_names = area_names_var,
                             clust_num = clust_num, areas_to_plot = areas_to_plot, fullnames = fullnames)
    
    cluster_res2 = cluster_fn(shp_file = shp_file,clust_method = clust_method, columns = which_cols,
                             info_matrix = var2_matrix, area_names = area_names_var,
                             clust_num = clust_num, areas_to_plot = areas_to_plot, fullnames = fullnames)
    
    print_msg = paste('Use which cluster (1/2)? ')
    plot_which_raw = dlg_input(message = print_msg, 
                                 default = "" , gui = .GUI)
    plot_which = as.integer(plot_which_raw[['res']])
    if (plot_which == 1) {
      plot_which = cluster_res[[2]]
    }
    else{
      plot_which = cluster_res2[[2]]
    }
}

  
  
  #DEF ARROW LENGTH
  arrow_len = seq(350,1340,length.out =  4)
  
  # Up arrows
  
  q_up = as.numeric(cut(qvals[index_inside,1],4))
  x_arrow_up = x_mid[index_inside] 
  y_arrow_start_up = y_mid[index_inside] + 300
  y_arrow_end_up = y_arrow_start_up + arrow_len[q_up]
  
  
  # Right arrows
  q_right = as.numeric(cut(qvals[index_inside,2],4))
  
  x_arrow_start_right = x_mid[index_inside] + 450
  x_arrow_end_right = x_arrow_start_right + arrow_len[q_right]
  y_arrow_right = y_mid[index_inside] 
  
  
  # Down arrows
  q_down = as.numeric(cut(qvals[index_inside,3],4))
  
  x_arrow_down = x_mid[index_inside] 
  y_arrow_start_down = y_mid[index_inside] - 300
  y_arrow_end_down = y_arrow_start_down - arrow_len[q_down]
  
  
  # Left arrows
  q_left = as.numeric(cut(qvals[index_inside,4],4))
  
  x_arrow_start_left = x_mid[index_inside] - 450
  x_arrow_end_left = x_arrow_start_left - arrow_len[q_left]
  y_arrow_left = y_mid[index_inside] 

  if (missing(category_names)) {
    category_names = c()
    for(cat_number in 1:numarrows){
      print_msg = paste('Input Legend: Category '  , cat_number)
      cat_name_raw = dlg_input(message = print_msg, 
                                  default = "" , gui = .GUI)
      if (length(cat_name_raw[['res']]) ==0L) {
        warning('Category Names not Specified!')
        stop()
      }
      category_names[cat_number] = cat_name_raw[['res']]
    }
  }
  
  
  
  
  # THEME FOR GGPLOT
  
  themeatic =  function(){theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 20),
                                  axis.line = element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks = element_blank(),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.title.x=element_blank(),
                                  axis.title.y=element_blank(),
                                  panel.border = element_blank(),
                                  panel.background = element_blank())}
  
  
  # Arrows for GGPLOT
  
  
  GG_arrows = function(){list(
    geom_segment(aes(x = x_arrow_up, y = y_arrow_start_up,
                     xend = x_arrow_up, yend = y_arrow_end_up),
                 color = 'black', arrow = arrow(length = unit(0.2, "cm")), 
                 alpha = 1, size = up_thick) ,
    
    geom_segment(aes(x = x_arrow_start_right, 
                     y = y_arrow_right, xend = x_arrow_end_right, yend = y_arrow_right),
                 color = 'blue', arrow = arrow(length = unit(0.2, "cm")),
                 alpha = 1, size = right_thick) ,
    
    geom_segment(aes(x = x_arrow_down, y = y_arrow_start_down,
                     xend = x_arrow_down, yend = y_arrow_end_down),
                 color = 'red', arrow = arrow(length = unit(0.2, "cm")),
                 alpha = 1, size = down_thick) ,
    
    geom_segment(aes(x = x_arrow_start_left, y = y_arrow_left,
                     xend = x_arrow_end_left, yend = y_arrow_left),
                 color = 'green4',arrow = arrow(length = unit(0.2, "cm")),
                 alpha = 1, size = left_thick)
  )}
  
  ##GGPLOT Legend
  
  GG_legend = function(){list(
    annotate("rect", xmin = 45000, xmax = 55000, ymin = 23000, ymax = 28500,
             alpha = 1, linetype = 1, colour = 'black',fill = 'white') ,

    annotate('segment' , x = 45500 , y= 27400 , xend = 49000 , yend = 27400,
             colour = 'black' , arrow = arrow(length = unit(0.3, "cm")), size = 1) ,
    
    annotate('segment' , x = 45500 , y= 26300 , xend = 49000 , yend = 26300,
             colour = 'blue' , arrow = arrow(length = unit(0.3, "cm")), size = 1) ,
    
    annotate('segment' , x = 45500 , y= 25200 , xend = 49000 , yend = 25200,
             colour = 'red' , arrow = arrow(length = unit(0.3, "cm")), size = 1) ,
    
    annotate('segment' , x = 45500 , y= 24100 , xend = 49000 , yend = 24100,
             colour = 'green4' , arrow = arrow(length = unit(0.3, "cm")), size = 1) ,
    
    annotate("text", x = 52000, y = 27500, label = category_names[1] , 
             alpha = 1, colour = 'black', size = 3.3, fontface = 'bold' ) ,
    
    annotate("text", x = 52000, y = 26400, label = category_names[2] , 
             alpha = 1, colour = 'blue', size = 3.3, fontface = 'bold' )  ,
    
    annotate("text", x = 52000, y = 25300, label = category_names[3] , 
             alpha = 1, colour = 'red', size = 3.3, fontface = 'bold' )  ,
    
    annotate("text", x = 52000, y = 24200, label = category_names[4] , 
             alpha = 1, colour = 'green4', size = 3.3, fontface = 'bold' )
  )}
  
  GG_legend_title = ifelse(missing(var2_matrix),
              list(annotate("text", x = 50000, y = 29200, label = 'Legend' , 
                    alpha = 1, colour = 'black', size = 4.5, fontface = 'bold' )),
              list(annotate("text", x = 46000, y = 29200, label = 'Legend' , 
                            alpha = 1, colour = 'black', size = 4.5, fontface = 'bold' )))    
  
  if (!include_cluster) {
    GG_labels = list(annotate('text', x = x_mid, y = y_mid,  label= shape_assign$abbr , size = 2.7, 
                              colour = 'black' , fontface = 'bold')  )
  }
  
  else{
    GG_labels = list(annotate('text', x = x_mid, y = y_mid,  label= shape_assign$abbr , size = 2.7, 
                              colour = plot_which[plot_order_names,] , fontface = 'bold'))
  }

  
  map <- ggplot() + themeatic() +
    ggtitle(plot_main) +
    
    geom_polygon(data = shapefile, 
                 aes(x = long , y = lat , group = group),
                 color = 'black', size = 0.5, fill = 'white')  +
    GG_arrows() + GG_legend() + GG_legend_title + GG_labels
    
  
  if(!missing(var2_matrix)){
    map <- map + GG_extra_legend
    }
  
  if (save_plot) {
    save_name = paste(plot_main , '.tiff' , sep = '')
    #saving plots in high res
    tiff(save_name, units="in", width=15, height=7.5, res=1000,...)
    plot(map)
    dev.off()
    cat('Map Saved' , '\n')
  }
  plot(map)
  return(cluster_res)
}

#areas_to_plot = rownames(ethnic_prop[-1,])
#category_names = c('Chinese Pop'  , 'Malay Pop' , 'Indian Pop' , 'Others Pop')

plot_sg_jointvar(shp_file = PA_14_shp , var1_matrix = ethnic_0, area_names_var = 'PLN_AREA_N' , var2_matrix = gender_male_0,
                 
                 areas_to_plot = areas_to_plot, clust_method = 'Kmeans', clust_num = 5, which_cols = 1:3,
                 
                 category_names = category_names , save_plot = TRUE, include_cluster = TRUE,
                 plot_main = 'Ethnic Distribution of Singapore',
                 grid = 'hexagonal')

colour_palette


