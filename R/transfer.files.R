transfer.files <- function(files,
                           base = "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",
                           source = "outputs",
                           destination = "./outputs/",
                           show.progress = TRUE){

  for (ifile in seq(1,length(files))){

    file <- files[ifile]

    if (show.progress){
      print(paste("Transferring file:",
                  file,
                  paste0("(",ifile,"/",length(files),")")))
    }

    system2("rsync",
            paste(
              file.path(base,source,file),
              destination
            ))
  }
}


