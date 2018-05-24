# The date column should be in POSIXct format, and named PERIOD
# The Time Series value data should be named Value
# The table should be in data.table format


ssacf<- function(x) sum(acf(x, na.action = na.fail, plot = FALSE)$acf^2)


compare_ssacf<-function(add,mult) ifelse(ssacf(add)< ssacf(mult), 
                                         "Additive", "Multiplicative") 
additive_or_multiplicative <- function(dt){
  m<-copy(dt)
  m[,trend := zoo::rollmean(Value, 8, fill="extend", align = "right")]
  m[,`:=`( detrended_a = Value - trend,  detrended_m = Value / trend )]
  m[Value==0,detrended_m:= 0]
  m[,`:=`(seasonal_a = mean(detrended_a, na.rm = TRUE),
          seasonal_m = mean(detrended_m, na.rm = TRUE)), 
    by=.(quarter(PERIOD)) ]
  m[is.infinite(seasonal_m),seasonal_m:= 1]
  m[,`:=`( residual_a = detrended_a - seasonal_a, 
           residual_m = detrended_m / seasonal_m)]
  compare_ssacf(m$residual_a, m$residual_m )
}

# Applying it to all time series in table View(df)


df <- fread("./Source/Demand Forecasting Auto Data Pull.csv", header = T, stringsAsFactors = F)
df$PERIOD <- as.Date(InputData$PERIOD, format = "%m/%d/%Y")
df <- df[df$VARIETY_NM == "SAINT" & df$CNTRY_CD == "IN",]

df <- df[df$PERIOD < Sys.Date(),]


setnames(df, old = "SALES_TOTAL_KCS_QTY", new="Value")

df$Value[is.na(df$Value)] <- 0

sample_ts <- df[ , .(Type=additive_or_multiplicative(.SD)),
                   .(VARIETY_NM, CNTRY_CD)]

View(m[is.na(m$residual_a),])

sum(acf(m$residual_m, na.action = na.omit, plot = FALSE)$acf^2)

unique(demand$VARIETY_NM)

