# Install required libraries
install.packages(c("shiny","shinycssloaders",
                   "shinyjs","shinydashboard",
                   "shinyalert","leaflet",
                   "htmltools","xml2",
                   "zoo","tsibble",
                   "fable","sf","ggplot2",
                   "plotly","ncdf4","reshape",
                   "sortable","tinytex",
                   "rvest","DT","rintrojs",
                   "hover","stringr","tidyr",
                   "RColorBrewer","ggpubr",
                   "readr","shinyBS","httr",
                   "tidyverse","feasts","urca",
                   "scoringRules","distributional"))
remotes::install_github('yonicd/slickR')
