# run make data files

make_data_gms <- function(gamsfile, idir, witch_dir, force = FALSE){

  # generated gdx
  gengdx = file.path(witch_dir,'input','build',paste0(stringr::str_sub(basename(gamsfile),6,-5),'.gdx'))

  #check if the generated gdx is older than the make_data gams file
  if (file.exists(gengdx)) {
    if (!force & (file.mtime(gengdx) > file.mtime(gamsfile))) {
      return()
    }
  }
  cat(crayon::blue$bold(paste("Compiling", basename(gamsfile), "\n")))
  lst = paste0(stringr::str_sub(gamsfile,1,-5),'.lst')

  cmd <- paste0('gams "',gamsfile,'" output="',lst,'" cdir="',normalizePath(file.path(witch_dir,'input')),'" --data="',idir,'" --method=',getOption("witchtools.method"))

  if (system(cmd) != 0) {
    stop(paste('gams execution error with',gamsfile))
  }
  if (file.exists(lst)) file.remove(lst)

  return(gamsfile)

}

make_data_R <- function(Rfile, idir, witch_dir, force = FALSE){

  # generated gdx
  gengdx = file.path(witch_dir,'input','build',paste0(stringr::str_sub(basename(Rfile),6,-3),'.gdx'))

  #check if the generated gdx is older than the make_data gams file
  if (file.exists(gengdx)) {
    if (!force & (file.mtime(gengdx) > file.mtime(Rfile))) {
      return()
    }
  }

  cat(crayon::blue$bold(paste("Compiling", basename(Rfile), "\n")))
  res <- system(paste0('Rscript --vanilla "', Rfile, '" -i "', normalizePath(idir), '" -w "', normalizePath(witch_dir), '"'))
  stopifnot(res == 0)

  return(Rfile)
}
