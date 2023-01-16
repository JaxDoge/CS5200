# author: Jax Tan
# 2023-01-13
# NEU 5200 Database System

library(pacman)
p_load(tidyverse)

rootDir <- "docDB"
sysOS <- Sys.info()[['sysname']]

#' main function

main <- function() {
  # test database configuration
  # configDB() # Input root argument is empty
  configDB(root = rootDir)
  # configDB(root = rootDir) # Database root folder is already exist.
  
  # test genObjPath, assuming input is nothing or valid argument
  # genObjPath() # Error in genObjPath() : Input arguments has empty string
  # genObjPath(123, "bbc") # Error in genObjPath(123, "bbc") : root must be a string
  # genObjPath("nothere", "BBC") # Error in genObjPath("nothere", "BBC") : Root folder is not exist
  genObjPath(rootDir, "BBC") # "docDB/BBC"
  
  # test getTags
  # getTags() # Error in getTags() : Input fileName is empty
  # getTags(223) # Error in getTags(223) : Input fileName is not a string
  getTags("CampusAtNight.jpg #Northeastern #ISEC") # [1] "#Northeastern" "#ISEC"
  getTags("CampusAtNight #Northeastern #ISEC.jpg") # [1] "#Northeastern" "#ISEC"
  getTags("CampusAtNight.jpg") # character(0)
  
  # test getFileName, assuming the input non-empty file name string contain right name formation.
  # getFileName() # Error in getFileName() : Input fileName is empty
  # getFileName(332) # Error in getFileName(332) : Input fileName is not a string
  getFileName("CampusAtNight.jpg #Northeastern #ISEC") # [1] "CampusAtNight.jpg"
  getFileName("CampusAtNight.jpg #Northeastern #ISEC") # [1] "CampusAtNight.jpg"
  
  # test storeObjs, generate some dummy files in 'data' folder
  # storeObjs() # Error in getTags(obj) : argument "folder" is missing, with no default
  # storeObjs(222, rootDir, T) # Error in storeObjs(222, rootDir, T) : Argument folder has to be a string.
  # storeObjs("data", "", T) # Error in storeObjs("data", "", T) : Argument root is empty.
  # storeObjs("data", "nothinghere", T) # root directory does not exist. Please initialize database before storing.
  storeObjs(folder = "data", root = rootDir, verbose = T) # Success!
  
  # test clearDB
  # clearDB() # root is an empty string.
  # clearDB(222) # root is an empty string.
  # clearDB("nowhere") # Clear operation is invalid. Root folder is not exist
  clearDB(rootDir) # Success!
  
}

#' Database configuration function
#'
#' In this assignment, create the necessary root folder under given path
#' @param root path name of root dir
#' @param second path of root. default is empty string
#' @return NA

configDB <- function(root = "", path = "") {
  if (nchar(root) <= 0) {
    message("Input root argument is empty")
    return(invisible(NULL))
  }
  
  # if path argument is empty, assign current working directory to it
  path <- if (nchar(path) == 0) getwd() else path
  rootPath <- file.path(path, root)
  if (dir.exists(rootPath)) {
    message("Database root folder is already exist.")
    # return()
  } else {
    dir.create(rootPath)
  }

  
}

#' Get the relative path of given tag of an object
#' 
#' This function should only be invoked internally
#' @param root The root dir of database
#' @param tag The input tag, e.g., #NEU
#' @return The path of input tag

genObjPath <- function(root = "", tag = "") {
  if(!is.character(root)) stop("root must be a string")
  if(!is.character(tag)) stop("tag must be a string")
  if(!dir.exists(root)) stop("Root folder is not exist")
  
  if (nchar(root) == 0 || nchar(tag) == 0) {
    stop("Input arguments has empty string")
  }
  
  pureTag <- str_replace_all(tag, "#", "")
  
  return(file.path(root, pureTag))
}

#' Extra the correct tag name from a file name
#' 
#' Function could extra tag from file names in both Win and Unix-like OSs
#' @param fileName Input file name
#' @return Tag in a string vector, e.g., c('#ABC', '#212'), or a empty vector of character - character(0)


getTags <- function(fileName = "") {
  
  # check bad cases
  if (!is.character(fileName)) stop("Input fileName is not a string")
  if (nchar(fileName) == 0) stop("Input fileName is empty")
  
  # Extract all hash tag from fileName with regular expression.
  res <- str_extract_all(fileName, "#[^\\s\\.]+")[[1]]
  return(res)
}


#' Extra the correct file name from the original file name
#' 
#' Function could extra file name from input file names in both Win and Unix-like OSs
#' @param fileName Input file name
#' @return file name in a string vector, e.g., c("abc.jpg")

getFileName <- function(fileName = "") {
  # Check all bad case
  if (!is.character(fileName)) stop("Input fileName is not a string")
  if (nchar(fileName) == 0) stop("Input fileName is empty")
  
  # remove all hash tag then remove all spaces before period or after file extension.
  res <- str_replace_all(fileName, "#[^\\s\\.]+", "") %>% 
    str_replace(., "\\s+\\.", ".") %>% 
    str_replace(., "\\s$", "")
  return(res)
}


#' Store objects into database
#' 
#' Assuming the root DB folder is created, then allocated copies of each object 
#' in given folder to assigned tag folders. If the tag folder is not exist, create
#' a new one
#' @param folder The folder holds all original objects. Should be a path.
#' @param root The database root folder. Should be docDB in this task
#' @param verbose If true, print a message of current storing operation
#' @return NA

storeObjs <- function(folder, root, verbose = TRUE) {
  # Check if all parameters have correct type
  if (!is.character(folder)) stop("Argument folder has to be a string.")
  if (!is.character(root)) stop("Argument root has to be a string.")
  if (!is.logical(verbose)) stop("Argument verbose has to be a boolean variable.")
  
  # Check if folder or root is empty
  if (nchar(folder) == 0) stop("Argument folder is empty.")
  if (nchar(root) == 0) stop("Argument root is empty.")
  
  # Check if root or folder exists under current working directory.
  if (!dir.exists(folder)) {
    message("folder directory does not exist.")
    return(invisible(NULL))
  }
  if (!dir.exists(root)) {
    message("root directory does not exist. Please initialize database before storing.")
    return(invisible(NULL))
  }
  
  allFiles <- list.files(path = folder, include.dirs = FALSE, full.names = FALSE)
  for (oneFile in allFiles) {
    storeObj(obj = oneFile, folder = folder, root = root, verbose = verbose)
  }
  
  cat("Storing process complete!\n")
}

#' Store one object into database
#' 
#' It should only be called within storeObjs function
#' Extra tags and the canonical file name from the input file name
#' make a copy of file to each tag folder.
#' @param obj The file name of an object ready for being stored.
#' @param folder The folder holds all original objects. Should be a path.
#' @param root The database root folder. Should be 'docDB' in this task
#' @param verbose If true, print a message of current storing operation
#' @return NA

storeObj <- function(obj, folder, root, verbose) {
  # Get tags of obj
  tags <- getTags(obj)
  # Get canonical file name
  fileName <- getFileName(obj)
  
  # If a tag folder is not exist, create a new one
  # store a copy of object into that tag folder.
  for (tag in tags) {
    tmpTagPath <- genObjPath(root, tag)
    if (!dir.exists(tmpTagPath)) {
      dir.create(tmpTagPath)
    }
    
    # store a copy of object in to current tag folder.
    file.copy(file.path(folder, obj), file.path(tmpTagPath, fileName))
  }
  
  # if verbose is true, print some message
  if (verbose & length(tags) > 0) {
    tmpFormat <- sprintf("Copying %s to %s", fileName, paste(str_replace_all(tags, "#", ""), collapse=", "))
    print(tmpFormat)
  }
  
}

#' Re-install the database
#' 
#' removes all folders and files in the folder specified by root but not the folder for root itself.
#' @param root The database root folder. Should be 'docDB' in this task
#' @return NA

clearDB <- function(root = "") {
  if (!is.character(root)) {
    message("root has to be a string.")
    return(invisible(NULL))
  }
  if (nchar(root) == 0) {
    message("root is an empty string.")
    return(invisible(NULL))
  }
  if (!dir.exists(root)) {
    message("Clear operation is invalid. Root folder is not exist")
    return(invisible(NULL))
  }
  res <- unlink(file.path(root,"*"), recursive = TRUE)
  if (res == 0) cat("Database reset successfully!") else cat("Cannot reinitialize database.")
}

##################

main()
quit()