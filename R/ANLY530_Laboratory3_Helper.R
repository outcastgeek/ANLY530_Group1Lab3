# This is the helper Script for Assignment 1


#Setup X11 Display

#x11(width = 3, height = 3)
Sys.getenv("DISPLAY")
Sys.setenv("DISPLAY"=":0")
capabilities()

#Set Working Directory
#setwd("~/datascience_workspace/MS_Analytics/anly_512/assignment_1")
getwd()

#Set Library Packages Folder
.libPaths()
.libPaths( c( .libPaths(), "~/R/x86_64-pc-linux-gnu-library/3.4") )
.libPaths()

# Obtains the full File Path
fullFilePath <- function(fileName)
{
    fileFolder <- "./data/"
    fileNamePath <- paste(fileFolder, fileName, sep = "")
    fileNamePath
}

# Formats Data
fmt <- function(dt, caption = "") {
  fmt_dt <- dt %>%
    kable("latex", longtable = T, booktabs = T, caption = caption)
  fmt_dt
}

# Style Data
style <- function(dt, full_width = F, angle = 0) {
  style_dt <- dt %>%
    kable_styling(latex_options = "striped", full_width = full_width) %>%
    row_spec(0, angle = angle)
  style_dt
}

# Top Customers
top.n.custs <- function (data,cols,n=5) {  
  
  #Initialize a vector to hold customers being removed 
  idx.to.remove <-integer(0)  
  
  for (c in cols){  
    
    # For every column in the data we passed to this function 
    #Sort column "c" in descending order (bigger on top) 
    #Order returns the sorted index (e.g. row 15, 3, 7, 1, ...) rather than the actual values sorted.     
    
    col.order <-order(data[,c],decreasing=T)  
    
    
    #Take the first n of the sorted column C to 
    #combine and de-duplicate the row ids that need to be removed 
    
    idx <-head(col.order, n) 
    idx.to.remove <-union(idx.to.remove,idx) 
    
  } 
  
  #Return the indexes of customers to be removed 
  return(idx.to.remove)  
  
}
