write.script.sp <- function(file.name,
                         dir.name,
                         sp.name,
                         settings.location,
                         strong = FALSE,
                         site.re = FALSE,
                         threads = FALSE){

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

  write(paste0("sp.name <- \"",sp.name,"\""),
        file=file,append=TRUE)


  write("",file=file,append=TRUE)

  if (!threads){
    if (strong & site.re){
      write("Opt.Bayes.Model.sp(dir.name,settings,sp.name,TRUE,TRUE)",file=file,append=TRUE)
    } else if (strong & !site.re){
      write("Opt.Bayes.Model.sp(dir.name,settings,sp.name,TRUE,FALSE)",file=file,append=TRUE)
    } else if (!strong & site.re) {
      write("Opt.Bayes.Model.sp(dir.name,settings,sp.name,FALSE,TRUE)",file=file,append=TRUE)
    } else {
      write("Opt.Bayes.Model.sp(dir.name,settings,sp.name)",file=file,append=TRUE)
    }
  } else {
    if (strong & site.re){
      write("Opt.Bayes.Model.sp(dir.name,settings,sp.name,TRUE,TRUE,TRUE)",file=file,append=TRUE)
    } else if (strong & !site.re){
      write("Opt.Bayes.Model.sp(dir.name,settings,sp.name,TRUE,FALSE,TRUE)",file=file,append=TRUE)
    } else if (!strong & site.re) {
      write("Opt.Bayes.Model.sp(dir.name,settings,sp.name,FALSE,TRUE,TRUE)",file=file,append=TRUE)
    } else {
      write("Opt.Bayes.Model.sp(dir.name,settings,sp.name,threads = TRUE)",file=file,append=TRUE)
    }
  }
}
