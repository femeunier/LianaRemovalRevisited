write.script.crossed <- function(file.name,
                                dir.name,
                                site.name,
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

  write(paste0("site.name <- \"",site.name,"\""),
        file=file,append=TRUE)


  write("",file=file,append=TRUE)

  if (!threads){
    if (strong & site.re){
      write("Opt.Bayes.Model(dir.name,settings,site.name,TRUE,TRUE,crossed = TRUE)",file=file,append=TRUE)
    } else if (strong & !site.re){
      write("Opt.Bayes.Model(dir.name,settings,site.name,TRUE,FALSE,crossed = TRUE)",file=file,append=TRUE)
    } else if (!strong & site.re) {
      write("Opt.Bayes.Model(dir.name,settings,site.name,FALSE,TRUE,crossed = TRUE)",file=file,append=TRUE)
    } else {
      write("Opt.Bayes.Model(dir.name,settings,site.name,crossed = TRUE)",file=file,append=TRUE)
    }
  } else {
    if (strong & site.re){
      write("Opt.Bayes.Model(dir.name,settings,site.name,TRUE,TRUE,TRUE,crossed = TRUE)",file=file,append=TRUE)
    } else if (strong & !site.re){
      write("Opt.Bayes.Model(dir.name,settings,site.name,TRUE,FALSE,TRUE,crossed = TRUE)",file=file,append=TRUE)
    } else if (!strong & site.re) {
      write("Opt.Bayes.Model(dir.name,settings,site.name,FALSE,TRUE,TRUE,crossed = TRUE)",file=file,append=TRUE)
    } else {
      write("Opt.Bayes.Model(dir.name,settings,site.name,threads = TRUE,crossed = TRUE)",file=file,append=TRUE)
    }
  }
}
