# ST3288-UROPS-SG-EthnicProjections
Ethnic Projections on the Singapore Map(by Planning Area) - in fulfilment of NUS ST3288 UROPS

Project is done in R, RStudio GUI is used for this purpose.

Project Files:
Lib_Data_Source.R
Hexagonal Simulation.R

Required Files are obtained from the Singapore Department of Statistics(DOS) Table Builder:
    https://www.tablebuilder.singstat.gov.sg/publicfacing/mainMenu.action


Explanation of Project Files:

Lib_Data_Source.R:

- Checks if Internet Connection is established

- Installs required files which can be found from google drive: 
          - Original Data from DOS(.xlsx):
                  -https://drive.google.com/open?id=1T6kIgaVS22Wcvreqn_uOboKyYH_Wuig3
          - Singapore Ethnic Composition Counts by Planning Area(.xlsx):
                  -https://drive.google.com/open?id=1ry-neiC0TlR1gGJoNqnpTIbNKM0LHaoK
          - Singapore Ethnic Composition(percentage) by Planning Area(.csv):
                  -https://drive.google.com/open?id=1KCfh3GhcLsITlxr9Y7qNCBLOJ1OSdV7o
          
- Installs required libraries in R:
      - For downloading/reading/transforming data:
            -  'downloader', 'readxl', 'plyr', 'XML', 'dplyr'
      - For doing GIS functions:
            - 'sf', 'geojsonio', 'geogrid',
      - For Plotting/Colours:
            - 'ggplot2', 'grid', 'broom', 'colorspace', 'RColorBrewer', 'devtools', 'viridis' 

- Reads, and Processes the Planning Area/CSV/XLSX files for further exploration


Hexagonal Simulation.R (in progress):

- Simulates grid of hexagons via Hungarian Matching Algorithm(package 'Geogrid')
- Plots the hexagons according to ethic proportions
