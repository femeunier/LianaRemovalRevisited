write.script <- function(file.name,
                         dir.name,
                         site.name,
                         settings.location){

  file <- file.path(dir.name,file.name)

  writeLines("rm(list = ls())",con = file)
  write("",file=file,append=TRUE)
  write("LianaRemovalRevisited::load.everything()",file=file,append=TRUE)
  write("",file=file,append=TRUE)

  write(paste0("settings <- readRDS(\"",
               settings.location,
               "\")"),file=file,append=TRUE)

  write(paste0("dir.name <- \"",dir.name,"\""),
        file=file,append=TRUE)

  write(paste0("site.name <- \"",site.name,"\""),
        file=file,append=TRUE)


  write("",file=file,append=TRUE)

  write("Opt.Bayes.Model(dir.name,settings,site.name)",file=file,append=TRUE)




}
