# function to check whether you have installed all packages used in this R Markdown file. 
# It also gives you the package version
# Read it the first time you use this Code in order to know whether you have all required packages installed.
# After installing all required packages, you do not need to run it again 

using <- function(...) {
  packages <- unlist(list(...)) #list of packages you want to check
  package_list <- .packages(all.available = T) #all the packages you have in your current library path
  for (vari in packages) {
    if (length(which(package_list %in% vari)) > 0) {
      print(paste(vari, "is installed"))
      print(paste("package Version", packageVersion(vari)))
    } else if (length(which(package_list %in% vari)) == 0) {
      print(paste(vari, "is not installed"))
      print(paste0("Please type: install.packages('",vari,"') for installing the package"))
    }
  }
}
