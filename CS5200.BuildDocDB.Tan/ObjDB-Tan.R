# author: Jax Tan
# 2023-01-13
# NEU 5200 Database System

library(pacman)
p_load(tidyverse)
# p_load(r2r)

rootDir <- "docDB"
sysOS <- Sys.info()[['sysname']]

#' main function

main <- function() {
  # test database configuration
  configDB()
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
    return()
  }
  
  # if path argument is empty, assign current working directory to it
  path <- if (nchar(path) == 0) getwd() else path
  rootPath <- file.path(path, root)
  if (dir.exists(rootPath)) {
    message("Database root folder is already exist.")
    return()
  }
  dir.create(rootPath)
  
}

#' Get the relative path of given tag of an object
#' 
#' @param root The root dir of database
#' @param tag The input tag, e.g., #NEU
#' @return The path of input tag

genObjPath <- function(root, tag) {
  if (nchar(root) == 0 || nchar(tag) == 0) {
    stop("Input arguments has empty string")
  }
  
  pureTag <- substring(tag, 2)
  
  return(file.path(root, pureTag))
}

#' Extra the correct tag name from a file name
#' 
#' Function could extra tag from file names in both Win and Unix-like OSs
#' @param fileName Input file name
#' @return Tag in a string vector, e.g., c('#ABC', '#212')

getTags <- function(fileName) {
  res <- str_extract_all(fileName, "#[^\\s\\.]+")[[1]]
  return(res)
}

#' Extra the correct file name from the original file name
#' 
#' Function could extra file name from input file names in both Win and Unix-like OSs
#' @param fileName Input file name
#' @return file name in a string vector, e.g., c("abc.jpg")

getFileName <- function(fileName) {
  res <- str_replace_all(fileName, "#[^\\s\\.]+", "") %>% str_replace_all(., "\\s", "")
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
  # Check if root folder exists under current working directory.
  if (!dir.exists(root)) {
    message("Root folder does not exist. Please initialize database before storing.")
    return()
  }
  
  allFiles <- list.files(path = folder, include.dirs = FALSE, full.names = FALSE)
  for (oneFile in allFiles) {
    storeObj(obj = oneFile, folder = folder, root = root, verbose = verbose)
  }
  
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
    tmpTagPath <- file.path(root, tag)
    if (!dir.exists(tmpTagPath)) {
      dir.create(tmpTagPath)
    }
    
    # store a copy of object in to current tag folder.
    file.copy(file.path(folder, obj), file.path(tmpTagPath, obj))
  }
  
  # if verbose is true, print some message
  if (verbose) {
    sprintf("Copying %s to %s", fileName, paste(tags, collapse=", "))
  }
  
}

#' Re-install the database
#' 
#' removes all folders and files in the folder specified by root but not the folder for root itself.
#' @param root The database root folder. Should be 'docDB' in this task
#' @return NA

clearDB <- function(root) {
  if (!dir.exists(root)) {
    message("Clear operation is invalid. Root folder is not exist")
    return()
  }
  unlink(file.path(root,"*"), recursive = TRUE)
}

##################

main()
quit()