# run make data files

make_data_gms <- function(gamsfile, force = F, data_directory = NULL){

  if(is.null(data_directory)) {
    data_directory = here::here('..','witch-data')
  }

  # generated gdx
  gengdx = here::here('input','build',paste0(stringr::str_sub(basename(gamsfile),6,-5),'.gdx'))

  #check if the generated gdx is older than the make_data gams file
  if(file.exists(gengdx)){
    if(!force & (file.mtime(gengdx)>file.mtime(gamsfile))){
      return()
    }
  }
  cat(blue$bold(paste("Compiling", basename(gamsfile), "\n")))
  lst = paste0(stringr::str_sub(gamsfile,1,-5),'.lst')

  if(system(paste0('gams "',gamsfile,'" output="',lst,'" cdir="',here('input'),'" --data="',data_directory,'"'))!=0){
    stop(paste('gams execution error with',gamsfile))
  }
  if(file.exists(lst)) file.remove(lst)

  return(gamsfile)

}

make_data_R <- function(Rfile, force = F, data_directory = NULL){

  if(is.null(data_directory)) {
    data_directory = here::here('..','witch-data')
  }

  # generated gdx
  gengdx = here::here('input','build',paste0(stringr::str_sub(basename(Rfile),6,-3),'.gdx'))

  #check if the generated gdx is older than the make_data gams file
  if(file.exists(gengdx)){
    if(!force & (file.mtime(gengdx)>file.mtime(Rfile))){
      return()
    }
  }

  cat(blue$bold(paste("Compiling", basename(Rfile), "\n")))
  res <- system(paste0('Rscript --vanilla "', Rfile, '" -d "', data_directory, '"'))
  stopifnot(res==0)

  return(Rfile)
}
