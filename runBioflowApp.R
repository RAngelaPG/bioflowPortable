.libPaths("./R-Portable/R-4.4.3/library")

Sys.setenv(RSTUDIO_PANDOC=paste0(unlist(strsplit(.libPaths(),"/R-Portable/R-4.4.3/library")),"/pandocExe/"))
rmarkdown::find_pandoc(cache=F,dir=Sys.getenv("RSTUDIO_PANDOC"))
message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))

options(shiny.maxRequestSize=30*1024^2)
bioflow::run_app()
