#---------------------------------------------
# Check and install packages
#---------------------------------------------

check_and_install_packages <- function(packages) {
  for (pkg in packages){
    if (!requireNamespace(pkg, quietly = TRUE)){
      install.packages(pkg)
    }
  }  
} 

# Usage:
#required_packages <- c("stringi")
#check_and_install_packages(required_packages)
#lapply(required_packages, library, character.only = TRUE)

#---------------------------------------------
# Useful functions to clean environment
#---------------------------------------------

remove_non_functions <- function(keep_objects) {
  
  all_objects <- ls(envir =.GlobalEnv)
  objects_to_remove <- Filter(function(x)!is.function(get(x, envir =.GlobalEnv)), all_objects) # Check in global environment
  objects_to_remove <- intersect(objects_to_remove, setdiff(all_objects, keep_objects))
  rm(list = objects_to_remove, envir =.GlobalEnv)
  
}

# Usage: 
#keep_objects <- c("dt","dataref_correcaomonetaria")
#remove_non_functions(keep_objects)