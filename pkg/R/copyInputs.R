#' Copy input files from WorldPop server to your local working directory
#'
#' @param srcdir Directory of source files
#' @param outdir Directory to write output files to
#' @param overwrite Logical indicating to overwrite local files if they already exist
#' @param OS.type Type of operating system retreived from \code{.Platform$OS.type}
#'
#' @return Writes input files to disk
#'
#' @export

copyInputs <- function(srcdir, outdir, overwrite=F, OS.type='windows'){

  # format server name
  if(.Platform$OS.type=="unix"){
    srcdir <- file.path('/Volumes', srcdir)
  } else {
    srcdir <- file.path('//worldpop.files.soton.ac.uk', srcdir)
  }

  # create output directory
  dir.create(outdir, showWarnings=F)

  # list files
  lf <- list.files(srcdir, recursive=T, include.dirs=T)

  # copy files
  for(f in lf){
    print(f)
    file.copy(from=file.path(srcdir,f), to=file.path(outdir), overwrite=overwrite)
  }
}
