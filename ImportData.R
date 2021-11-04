#ImportData

importData = function(path){
  library(readxl)
  files = list.files(path, pattern = "*.xlsx", full.names = TRUE)
  data = sapply(files, read_excel, skip=2)
  noExtension = sapply(files, sub, pattern='\\.xlsx$', replacement='')
  names(data) = sapply(noExtension, basename)
  data
  }


renameCols = function(df){
  newNames = df
  for(n in 1:length(chooseData)){
    newNames[[n]] = data.frame(newNames[n])
    colnames(newNames[[n]])[1]  <- "Date"
    }
  newNames
  }





    
    