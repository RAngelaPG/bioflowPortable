.libPaths("./R-Portable/R-4.4.3/library")

#Sys.setenv(RSTUDIO_PANDOC="C:/Users/RAPACHECO/OneDrive - CIMMYT/Documents/GitHub/bioflowPortable/pandocExe/")
Sys.setenv(RSTUDIO_PANDOC=paste0(unlist(strsplit(.libPaths(),"/R-Portable/R-4.4.3/library")),"/pandocExe/"))
rmarkdown::find_pandoc(cache=F,dir=Sys.getenv("RSTUDIO_PANDOC"))
message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))

#library(devtools)
#update_github <- function() {
 # pkgs = c("cgiarBase","cgiarPipeline","cgiarOcs","bioflow")
 # print(pkgs)
 # desc <- lapply(pkgs, packageDescription, lib.loc = NULL)
 # for (d in desc) {
 #   message("working on ", d$Package)
 #   if (!is.null(d$GithubSHA1)) {
 #     message("Github found")
 #     checkInt=curl::has_internet()
 #	   if(checkInt==TRUE){install_github(repo = paste0(d$GithubUsername, "/", d$GithubRepo))}
 #     install_github(repo = paste0(d$GithubUsername, "/", d$GithubRepo))
 #   }
 # }
#}
#update_github()
options(shiny.maxRequestSize=30*1024^2)
bioflow::run_app()
