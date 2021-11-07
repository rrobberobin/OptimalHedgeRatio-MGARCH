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


ARCHTest = function(sqrdRes){
  ARCHEffect = summary(lm(sqrdRes ~ Lag(sqrdRes,1) +
                             Lag(sqrdRes,2) + Lag(sqrdRes,3) +
                             Lag(sqrdRes,4) + Lag(sqrdRes,5)))
  TR2 = length(sqrdRes)*ARCHEffect$r.squared
  pchisq(TR2,df=5)
}


EngleNg = function(res){
  library(Hmisc)
  minusDummy = ifelse(Lag(res,1) < 0, 1, 0)
  laggedResiduals = Lag(as.matrix(res),1)
  laggedMinusDummy = minusDummy*laggedResiduals
  laggedPlusDummy = (1-minusDummy)*laggedResiduals
  
  sqrdRes = res^2
  resLen = dim(sqrdRes)[2]
  ENModel = summary(lm(sqrdRes ~ 
                           minusDummy + 
                           laggedMinusDummy + 
                           laggedPlusDummy))
    TRsqrdEngle = length(sqrdRes)*ENModel$r.squared
    pchisq(TRsqrdEngle,df=3)
}



    
    