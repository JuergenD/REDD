#explore Rolling Economic Drawdown - Controlled Optimal Portfolio Strategy (REDD-COPS)
#from Yang, Z. George and Zhong, Liang,
#Optimal Portfolio Strategy to Control Maximum Drawdown - 
#The Case of Risk Based Dynamic Asset Allocation (February 25, 2012).
#Available at SSRN: http://ssrn.com/abstract=2053854 or
#http://dx.doi.org/10.2139/ssrn.2053854

require(quantmod)
require(PerformanceAnalytics)
require(RColorBrewer)

#file input

roc=read.csv("assets.csv",header=TRUE)
roc=as.xts(roc[,-1],order.by=as.POSIXct(roc[,1], format="%Y-%m-%d"))

assetcolumn=14
asset=colnames(roc)[assetcolumn]

# from="1990-01-01"
# #get asset
# getSymbols(asset,src="yahoo" ,from = from)
# asset.monthly <- to.monthly(eval(as.name(asset)))[,6]
# index(asset.monthly) <- as.Date(index(asset.monthly))
# roc <- ROC(asset.monthly, n = 1, type = "discrete")

#get 1 year t-bill for risk-free

getSymbols("GS1", src = "FRED")
idx=seq(as.Date("1953/05/01"), by="month", along.with=GS1)-1
index(GS1)<-idx

#combine the monthly asset return with a monthly return of GS1 1 year treasury

returns <- na.omit(merge(roc[,assetcolumn], ((1+lag(GS1,1) / 100) ^ (1/12)) - 1))
cumreturns <- cumprod(1+returns)

#calculate REDD assuming 1st column is risky asset and 2nd is risk-free

REDD <- function(x, rf) {
  rf <- rf[index(x)]
  result <- 1 - last(x) / 
    (coredata(max(x)) * coredata(last(rf)) / coredata(first(rf[index(x[which(x==max(x))])]))) 
  return(result)
}

#get REDD for asset
#paper says 
#"Intuitively, a drawdown look-back period H somewhat shorter than or similar to the
#market decline cycle is the key to achieve optimality. Substituting EDD with a lower
#REDD in equation (1), we have higher risky asset allocation to improve portfolio return
#during a market rebound phase. In the examples followed, we'll use H = 1 year throughout."
asset.redd <- rollapplyr(cumreturns[,1], width = 12, FUN = REDD, rf=cumreturns[,2])

#experiment with a couple different Sharpe options
asset.sharpe <- na.omit( runMax(lag(rollapplyr(returns[,1], width = 36, FUN = SharpeRatio, Rf = 0, p = 0.95, "StdDev"),12), n = 12) )

#another sharpe alternative
#asset.sharpe <-  1 -  na.omit( runMin(lag(rollapplyr(returns[,1], width = 36, FUN = SharpeRatio, Rf = 0, p = 0.95, "StdDev"),12), n = 12) )
#                               n = 12) )

#if you would like to use a constant Sharpe, specify here and uncomment
#the paper uses a little hindsight to use the historic 0.403 Sharpe
#asset.sharpe <- 0.403

#feel free to experiment here

drawdown.limit <- .3
position.size <- as.xts(apply(( (asset.sharpe/drawdown.limit + 0.5) / (1-drawdown.limit^2) ) * 
                                ((drawdown.limit  - asset.redd) / (1 - asset.redd)), MARGIN = 1, FUN = max, 0), order.by = index(asset.sharpe))

avSize=sum(position.size)/NROW(position.size)

plot(position.size,main=paste("average Pos.Size: ",sprintf("%1.2f%%", 100*avSize),sep=""))

cagr <- function(x) {
  x=cumprod(na.omit((x+1)))
  na.omit((coredata(x[nrow(x)])/coredata(x[1]))^(1/((as.numeric(index(x[nrow(x)])-index(x[1])))/365.25))-1)
}

#charts.PerformanceSummary(merge(lag(position.size)*roc, roc))
return.comps <- merge(lag(position.size)*returns[,1] + lag(1-position.size) * returns[,2], returns[,1], returns[,2])
colnames(return.comps) <- c("REDD-COPS",asset,"US1Y")
CAGR=sprintf("%1.2f%%",cagr(return.comps)*100)
charts.PerformanceSummary(return.comps, ylog=TRUE,
                          colorset=brewer.pal(10,"Spectral")[c(2,4,7)], 
                          main="REDD-COPS System Test (http://ssrn.com/abstract=2053854)",sub=CAGR)
CAGR