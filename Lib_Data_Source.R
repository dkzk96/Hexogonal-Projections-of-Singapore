havingIP <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  if (any(grep(validIP, ipmessage))) {
    cat('Connected to internet!' , '\n')
  }
  else{warning('Not connected to internet!')}
}

havingIP()

# installing useful libraries
# readxl for xlsx items
# devtools, dplyr, XML for downloading/processing data
# sf , geojsonio, geogrid for reading geo items
# the rest for plotting

list.of.packages <- c('downloader','readxl', 'ggplot2', 'plyr','grid','broom',
                      'colorspace','RColorBrewer', 'sf', 'geojsonio', 'devtools',
                      'geogrid', 'dplyr', 'viridis' ,'XML')

package_install = function(pack.list){
  cat('\n' , 'Checking for packages to be installed' , '\n')
  new.packages <- pack.list[!(pack.list %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    cat(new.packages, ' to be installed!' , '\n')
    install.packages(new.packages)  
    cat(new.packages, ' installed!' , '\n')
  }
  invisible(lapply(list.of.packages, library, character.only = TRUE, quietly = TRUE, 
                   verbose = FALSE, warn.conflicts = FALSE))
  cat('Packages installed and in use!' , '\n' , '\n')
}


package_install(list.of.packages)

#current working dir
current_wd <- dirname(rstudioapi::getSourceEditorContext()$path) ; setwd(current_wd)


#download files if it doesnt exist in working dir
download_shapefiles = function(){
  cat('Checking if required files are available...',  '\n')
  dest_filename_2014shp = paste(current_wd, 'SHP_14.zip' , sep = '/')
  dest_filename_2019geojson = paste(current_wd, 'GJSON_19.zip' , sep = '/')
  dest_filename_pc_data = paste(current_wd, 'Singapore_ethnic_composition.csv', sep = '/')
  dest_filename_count_data = paste(current_wd , 'Singapore_ethnic_composition_counts.xlsx', sep = '/')
  dest_filenames = c(dest_filename_2014shp, dest_filename_2019geojson, 
                     dest_filename_pc_data, dest_filename_count_data)
  files = c("MP14_PLNG_AREA_WEB_PL.shp",
            'master-plan-2019-planning-area-boundary-no-sea-geojson.geojson',
            'Singapore_ethnic_composition.csv',
            'Singapore_ethnic_composition_counts.xlsx')
  file_calls = c('SHP_14 not found!' , 'GJSON_19 not found!', 
                 'SG_ethnic_composition not found!', 'SG_ethnic_composition_counts not found!')
  checker = file.exists(files) ; index = 1:4
  shp_14_web = 'https://data.gov.sg/dataset/4d9e3b2f-3f4b-488e-bb3a-3638ca656247/download'
  gjson_19_web = 'https://data.gov.sg/dataset/40267ab6-7c08-45c4-b777-a3b10e68f1c8/download'
  sg_pc_web = 'https://drive.google.com/uc?export=download&id=1KCfh3GhcLsITlxr9Y7qNCBLOJ1OSdV7o'
  sg_count_web = 'https://drive.google.com/uc?export=download&id=1ry-neiC0TlR1gGJoNqnpTIbNKM0LHaoK'
  webs = c(shp_14_web, gjson_19_web , sg_pc_web , sg_count_web)
  if(sum(checker) == 4){
    cat('Files already in current working directory!', '\n')
  }
  else{
    invisible(lapply(FUN = cat, X = file_calls[which(!checker)]  , '\n'  ))
    for(i in index){
      if(!checker[i]){
        download(webs[i], destfile = dest_filenames[i], 
                 mode = 'wb' , quiet = TRUE)
      }
      if(i==1 & !checker[i]){
        zip::unzip(dest_filenames[i], exdir = current_wd)
        zip::unzip("master-plan-2014-planning-area-boundary-web-shp.zip", exdir = current_wd)
        cat('Download and unzip of 2014 SHP data done!', '\n')
      }
      else if(i==2 & !checker[i]){
        zip::unzip(dest_filenames[i], exdir = current_wd)
        cat('Download and unzip of 2019 GJSON data done!', '\n')
      }
      else if(i==3 & !checker[i]){
        cat('Download of SG ethnic composition data done!', '\n')
      }
      else if(i ==4 & !checker[i]){
        cat('Download of SG ethnic composition counts data done!', '\n')
      }
    }
    cat('All files downloaded!', '\n' , '\n')
  }
}

download_shapefiles()


#Saving original graphical parameters
OP = par(no.readonly = T) ; suppressWarnings(par(OP))
#define colour (16 colours max)
colour_palette = c('red', 'blue', 'green', 'yellow', 'sienna3', 'rosybrown','paleturquoise'
                   , 'thistle', 'darkorange','darkslateblue', 'lightsalmon', 'aquamarine', 'grey51',
                   'lemonchiffon','grey31','plum')

c_col = c('gray20', 'gray40', 'gray60', 'gray80')


#shape codes for planning area 2014 SHP AND GEOJSON files

planning_area_filename <- "MP14_PLNG_AREA_WEB_PL.shp"
planning_area_file <- paste(current_wd, planning_area_filename, sep="/")
PA_14_shp = st_read(planning_area_file, quiet = TRUE)


#Shape Code for Planning Area 2019 GEOJSON FILES

PA_19 = st_read('master-plan-2019-planning-area-boundary-no-sea-geojson.geojson', quiet = T)
PA_19 = st_zm(PA_19, drop = T)
cat('Dropping unused Z coordinates into 2D file' , '\n')
geojson_write(PA_19, file = '2D_new19PA.geojson', overwrite = T)

new = geojson_read('2D_new19PA.geojson', what = 'sp')
char = as.character(new$Description)
char_list = readHTMLTable(char)
names(char_list) = 1:55

new_dat = as.vector(char_list[[1]][,2])
for(i in 2:55){
  new_dat = cbind(new_dat,as.vector(char_list[[i]][,2]))
}
new_dat =as.data.frame( t(new_dat))      
dimnames(new_dat) = list(1:55, as.vector(char_list[[1]][,1]))


new_dat = cbind.data.frame(OBJECTID = 1:55 , new_dat)

new@data = new_dat
PA_19 = new


rm(list = c('new', 'new_dat','char','i', 'planning_area_filename','package_install',
            'list.of.packages', 'planning_area_file','char_list' ,
            'download_shapefiles' , 'havingIP'))


##Ethnic Data Processing

#Base Ethnic Proportions

ethnic = read.csv( file = 'Singapore_ethnic_composition.csv' , skip = 1 , header = T, stringsAsFactors = F)
colnames(ethnic) = c( 'Planning Area' , 'Total Number' , 'Chinese' , 'Malay' , 'Indian' , 'Others')
ethnic[,1] = gsub('- Total ' , '' , ethnic[,1] , fixed = T)
ethnic[,1] = gsub( "     ", "", ethnic[,1], fixed = TRUE)
ethnic[,1] = toupper(ethnic[,1])
rownames(ethnic) = ethnic[,1]
ethnic = ethnic[,-1]

ethnic_0 = ethnic
ethnic_0[is.na(ethnic_0)] =0 ; ethnic_0=ethnic_0[-1,-1]

ethnic_prop = na.omit(ethnic)
total_number = ethnic_prop[,1]
ethnic_prop = ethnic_prop[,-1]
attach(ethnic_prop)

#Ethnic Counts
ethnic_counts = read_xlsx('Singapore_ethnic_composition_counts.xlsx')
ethnic_counts = as.data.frame( na.omit(ethnic_counts))
rownames(ethnic_counts) = ethnic_counts[,1]
ethnic_counts = ethnic_counts[,-1]
attach(ethnic_counts)
rm(list = c('ethnic', 'total_number'))


cat('\n','Files ready for processing!' , '\n')
