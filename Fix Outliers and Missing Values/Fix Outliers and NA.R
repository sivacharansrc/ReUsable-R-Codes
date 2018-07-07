### Function for fixing Outliers, and NA's
# fixOutliersNAs(x, "colName", fix = "na", method = "mean")
# x is the dataSource, colName is the name of the column within double quotes, fix can be either "na", 
# "outliers" or "both" within double quotes, and method can be "mean", "median", or "mode" within double quotes
# By default, fix = "both" and method = "mean"
# Note that mode can work for either numeric or catgorical variables
# Outliers are determined by this formula: Values greater than 75th Quartile + 1.5 * IQR or less than 
# 25th Quartile - 1.5 * IQR
# df <- data.frame(NewColumn = c(1,2,3,4,NA,1000))
# fixOutliersNAs(df, "NewColumn", method = "median", fix = "outliers")
# fixOutliersNAs(df, "NewColumn", method = "median", fix = "both")




fixOutliersNAs <- function(x, col, method = "mean", fix = "both") {
  
  ## MODE INFO ORIGINAL FUNCTION
  modeInfo <- function(y) {
    modeData <- unique(y)
    modeData <- modeData[!is.na(modeData)]
    modeData[which.max(tabulate(match(y, modeData)))]
  }
  mn <- mean(x[,col], na.rm = T)
  md <- median(x[,col], na.rm = T)
  mde <- modeInfo(x[,col])
  P25 <- quantile(x[col], na.rm = T,0.25)
  P75 <- quantile(x[col], na.rm = T,0.75)
  IQR <- P75 - P25
  OutlierUpperLimit <- P75 + 1.5*(IQR)
  OutlierLowerLimit <- P25 - 1.5*(IQR)
  newCol <- paste0(col,"_fixed")
  if (fix == "na") {
    for (i in 1:nrow(x)) {
      if (method == "mode") {
        x[i,newCol] <- ifelse(is.na(x[i,col]), mde,x[i,col])
      }
      else if(method == "median") {
        x[i,newCol] <- ifelse(is.na(x[i,col]), md,x[i,col])
      }
      else {
        x[i,newCol] <- ifelse(is.na(x[i,col]), mn,x[i,col])
      }
    }   
  }
  else if (fix == "outliers") {
    for (i in 1:nrow(x)) {
      if (method == "mode") {
        x[i,newCol] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit, mde,x[i,col])
      }
      else if(method == "median") {
        x[i,newCol] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit, md,x[i,col])
      }
      else {
        x[i,newCol] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit, mn,x[i,col])
      }
    }     
  }
  else {
    for (i in 1:nrow(x)) {
      if (method == "mode") {
        x[i,newCol] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit | is.na(x[i,col]), mde,x[i,col])
      }
      else if(method == "median") {
        x[i,newCol] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit | is.na(x[i,col]), md,x[i,col])
      }
      else {
        x[i,newCol] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit | is.na(x[i,col]), mn,x[i,col])
      }
    }  
  }
  
  return(x)
}
