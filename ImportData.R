#ImportData

importData = function(path){
  library(readxl)
  files = list.files(path, pattern = "*.xlsx", full.names = TRUE)
  data = lapply(files, read_excel, skip=2)
  noExtension = sapply(files, sub, pattern='\\.xlsx$', replacement='')
  names(data) = sapply(noExtension, basename)
  return(data)
}

# import = function(urls){
#   dataNames = c()
#   dataX = c()
#   
#   library(readxl)
#   for(ulr in urls){
#     dataName = dataNames.names(read_excel(url, n_max=1))  #extract the name of the dataset
#     data = read_excel(futuresURL, skip=2)
#     dataNames
#   }
#   return(dataName, data)
# }
# 
# import2 = function(){
#   library(data.table)
#   data =
#     list.files(pattern = "*.xlsx") %>%
#     map_df(~fread(.))
# }
# 
# 
# rbindlist_fread <- function(path) {
#   library(data.table)
#   files = list.files(path, pattern = "*.xlsx", full.names = TRUE)
#   rbindlist(lapply(files, function(x) fread(x)))
# }




    
    