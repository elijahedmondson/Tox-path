devtools::install_github("USEPA/CompTox-ToxCast-tcpl")
library(tcpl)
#https://cran.r-project.org/web/packages/tcpl/vignettes/Introduction_Appendices.html

pkg_dir <- system.file(package = "tcpl")

tcplConf(drvr = "MySQL", 
         user = "username", 
         pass = "password", 
         host = "localhost",
         db   = "invitrodb")

