# current_working_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(current_working_dir) 
source('Lib_Data_Source.R')

# #finding the best seed
# 
# par(mfrow=c(3,3), mar = c(0,0,2,0))
# 
# set.seed(1)
# for (i in 1:110){
#   new_cells <-  calculate_grid(PA_14_shp, learning_rate =  0.3 , seed = i, grid_type = 'hexagonal')
#   plot(new_cells[[2]], main = paste("Seed",i, sep=" "))
# }

#72,30, ok
## creating the hexagonal map dataframes

shapes = calculate_grid(PA_14_shp, learning_rate = 0.5, seed = 14, grid_type = 'hexagonal')
hex_PA14 = assign_polygons(PA_14_shp, shapes)


hex_PA14 = as(hex_PA14 , 'Spatial')
hex_PA14$abbr = abbreviate(hex_PA14$PLN_AREA_N, 1,5)
x_mid = hex_PA14$V1
y_mid = hex_PA14$V2

shapefile = suppressWarnings( tidy(hex_PA14))

##indexes to annotate PA
index_inside = which(hex_PA14$PLN_AREA_N %in% rownames(ethnic_prop[-1,]))

index_not_inside = setdiff(1:55, index_inside)

###chinese colours

qc = quantile(Chinese[-1],probs = seq(0,1,0.25))

qvals = ethnic_0[hex_PA14$PLN_AREA_N,]

c_col_vec = ifelse(qvals[,1]==0 , 'white',
                   ifelse(qvals<= qc["25%"], c_col[1],
                          ifelse(qvals <= qc["50%"], c_col[2],
                                 ifelse(qvals <= qc["75%"] , c_col[3], c_col[4]))))
c_col_vec = rep(c_col_vec, rep(7, length(c_col_vec)))

#DEF ARROW LENGTH
lm=600*sqrt(2)
li = 450*sqrt(2)
lo = 300*sqrt(2)


##Malay arrows
qm = as.numeric(cut(qvals[,2],8))

m_grouping = seq(1,8)

x_arrow_start_group = c(0,300,sqrt(2*300^2),300,0,-300,-sqrt(2*300^2),-300)
y_arrow_start_group = c(sqrt(2*300^2),300,0,-300,-sqrt(2*300^2),-300,0,300)                                                      
x_arrow_start_m = (x_mid[index_inside] + x_arrow_start_group[qm][index_inside])
y_arrow_start_m = (y_mid[index_inside]+y_arrow_start_group[qm][index_inside])
x_arrow_end_group = c(0,lm,sqrt(2*lm^2),lm,0,-lm,-sqrt(2*lm^2),-lm)
y_arrow_end_group = c(sqrt(2*lm^2),lm,0,-lm,-sqrt(2*lm^2),-lm,0,lm)   
x_arrow_end_m = ( x_arrow_start_m + x_arrow_end_group[qm][index_inside])
y_arrow_end_m = ( y_arrow_start_m  +y_arrow_end_group[qm][index_inside])



##Indian arrows
qi = as.numeric(cut(qvals[,3],8))

i_grouping = seq(1,8)

                                                    
x_arrow_start_i = (x_mid[index_inside] + x_arrow_start_group[qi][index_inside])
y_arrow_start_i = (y_mid[index_inside] + y_arrow_start_group[qi][index_inside])
x_arrow_end_group_i = c(0,li,sqrt(2*li^2),li,0,-li,-sqrt(2*li^2),-li)
y_arrow_end_group_i = c(sqrt(2*li^2),li,0,-li,-sqrt(2*li^2),-li,0,li)   
x_arrow_end_i = ( x_arrow_start_i + x_arrow_end_group_i[qi][index_inside])
y_arrow_end_i = ( y_arrow_start_i  +y_arrow_end_group_i[qi][index_inside])

##Other arrows
qo = as.numeric(cut(qvals[,4],8))

o_grouping = seq(1,8)


x_arrow_start_o = (x_mid[index_inside] + x_arrow_start_group[qo][index_inside])
y_arrow_start_o = (y_mid[index_inside] + y_arrow_start_group[qo][index_inside])
x_arrow_end_group_o = c(0,lo,sqrt(2*lo^2),lo,0,-lo,-sqrt(2*lo^2),-lo)
y_arrow_end_group_o = c(sqrt(2*lo^2),lo,0,-lo,-sqrt(2*lo^2),-lo,0,lo) 
x_arrow_end_o = ( x_arrow_start_o + x_arrow_end_group_o[qo][index_inside])
y_arrow_end_o = ( y_arrow_start_o  +y_arrow_end_group_o[qo][index_inside])




### THEME FOR GGPLOT

themeatic =  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 20),
                    axis.line = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank())

## HEX MAP PLOT
#saving plots in high res

tiff("test.tiff", units="in", width=30, height=15, res=1000)


map <- ggplot() + themeatic +
  ggtitle('Ethnic Distribution of Singapore') +
  geom_polygon(data = shapefile, 
            aes(x = long , y = lat , group = group),
            color = 'black', size = 0.5, fill = c_col_vec)  +
  annotate("text", x = x_mid, y = y_mid,  label= hex_PA14$abbr, size =2) +
  geom_segment(aes(x = x_arrow_start_m, 
                   y = y_arrow_start_m
                   , xend = x_arrow_end_m, yend = y_arrow_end_m),
               color = 'black',arrow = arrow(length = unit(0.15, "cm"))
               , alpha = 1, size =0.9) +
  geom_segment(aes(x = x_arrow_start_i, 
                   y = y_arrow_start_i
                   , xend = x_arrow_end_i, yend = y_arrow_end_i),
               color = 'red',arrow = arrow(length = unit(0.15, "cm"))
               , alpha = 1, size =0.9) +
  geom_segment(aes(x = x_arrow_start_o, 
                   y = y_arrow_start_o
                   , xend = x_arrow_end_o, yend = y_arrow_end_o),
               color = 'green4',arrow = arrow(length = unit(0.15, "cm"))
               , alpha = 1, size =0.9)
plot(map)

#saving plot
dev.off()





