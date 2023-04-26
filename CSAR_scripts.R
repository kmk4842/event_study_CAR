#Event study methods in R
#This file contains general functions for running event studies in finance using the market model to calculate cumulative abnormal returns


####FUNCTIONS BEGIN

library(chron)

#specify date format to be used
date.format<-"d.m.y"

load.stooq<-function(filename){
  #this function is similar in effect to load.quotes.csv, but takes into account the new layout of stooq files. 
  	quotes<-NULL
	try (quotes<-read.csv(as.character(filename)))
	if (is.null(quotes)==TRUE) {stop("Load error!")}
	names(quotes)<-toupper(names(quotes))
	attach(quotes)	
	TICKER<-toupper(TICKER)
	quotes<-data.frame(TICKER,DATE,CLOSE,VOLUME)
	detach(quotes)
	quotes$DATE<-dates(as.character(quotes$DATE),format="ymd", out.format=date.format)
	quotes
}	

load.quotes.csv<-function(filename){
	#loads a cvs file with quotes in ascii format
	#as downloaded in metastock from stooq.pl and converted into ascii

	quotes<-NULL
	try (quotes<-read.csv(as.character(filename),skip=1)) #skips the first line that usually contains variable names
	if (is.null(quotes)==TRUE) {stop("Load error!")}

	quotes<-data.frame(quotes[1],quotes[2],quotes[4],quotes[6:10])#take only close PRICE and Voluem
	names(quotes)<-c("TICKER","NAME","DATE","OPEN","HIGH","LOW","CLOSE","VOL")
	quotes<-subset(quotes, (TICKER!="<TICKER>"))#remove headers
	
	#to get rid of useless factor levels
	#quotes$DATE<-as.factor(as.character(quotes$DATE))
	#quotes$TICKER<-as.factor(as.character(quotes$TICKER))
	#quotes$CLOSE<-as.double(as.character(quotes$CLOSE))
	
	#convert DATE
	d<-quotes$DATE
	d<-paste(substr(d,1,4),substr(d,5,6),substr(d,7,8), sep="-")
	d<-chron(d, format="y-m-d", out.format=date.format)
	quotes$DATE<-d
		quotes
}

counter<-function(TICKER,DATE){
	result<-rep(1,length(DATE))
	DOWN<-down(DATE,TICKER)	
	if (DOWN==TRUE){
	for (i in 2:length(DATE)){
		if (TICKER[i]==TICKER[i-1]){
			result[i]<-result[i-1]+1
		}
	}
	}else{
	l<-length(DATE)
	for (i in 1:(l-1)){
		if (TICKER[l-i]==TICKER[l-i+1]){
			result[l-i]<-result[l-i+1]+1
			}
		}
	}
	result
}

down<-function(DATE,TICKER){
	#determine quote history direction

	if (length(TICKER)<2||length(TICKER)!=length(DATE))
		{stop("All vectors have to be of equal length greater than 1.")}
	#if (is.chron(DATE)==FALSE) 
	#	{stop("Dates vector has to be a chron-DATE type.")}
	
	i<-1
	if (TICKER[1]==TICKER[2])
		{
			if (DATE[1]<DATE[2])
				{DOWN<-TRUE}
			else {DOWN<-FALSE}
		}else{
			while(TICKER[i]!=TICKER[i+1])
			{
				i=i+1
				if (TICKER[1]==TICKER[2])
				{
					if (DATE[1]<DATE[2])
					{DOWN<-TRUE}
					else {DOWN<-FALSE}
				}
			}
		}
	DOWN
}

rates.daily.tickers<-function(PRICE,DATE,TICKER){
	#calculates daily rates of return
	#can be used for vectors with many tickers
	#DATE have to be chron
	
	if (length(PRICE)<2||length(PRICE)!=length(TICKER)||length(TICKER)!=length(DATE))
		{stop("All vectors have to be of equal length greater than 1.")}
	#this gives an error, because DATE is actually double
	#if (is.chron(DATE)==FALSE) 
	#	{stop("Dates vector has to be a chron-DATE type.")}
		
	#calculate rates of return
	l<-length(PRICE)
	x<-as.double(rep(NA,l)) #NA is default value
	DOWN<-down(DATE,TICKER)
	for (i in 1:(l-1))
	{
		if (TICKER[i]==TICKER[i+1]){
			if(DOWN==TRUE) 
			{try(x[i+1]<-log(PRICE[i+1]/PRICE[i]))}
			else{x[i]<-log(PRICE[i]/PRICE[i+1])}
		}	
	}
	x #the returns vector
}

rates.daily<-function(PRICE,DATE){
	#wrapper for rates.daily.TICKERs for a case of one TICKER only
	rates.daily.tickers(PRICE,DATE,rep("TICKER",length(DATE)))
}
match.index<-function(indices,DATE){
	#returns a list of quotes for the index on the given dates vector
	result<-as.double(rep(NA,length(DATE)))
	for (i in 1:length(DATE)){
		try(result[i]<-indices$CLOSE[indices$DATE==DATE[i]])
	}
	result
}

check.dates<-function(x,DATE){
	#this version takes care of cases where x$DATE has some dates that DATE doesn't
	dateStart<-min(x$DATE,na.rm=TRUE)
	dateEnd<-max(x$DATE,na.rm=TRUE)
	
	result<-x
	DATE<-subset(DATE,DATE>=dateStart)
	DATE<-subset(DATE,DATE<=dateEnd)

#	browser()
	for (i in 2:length(DATE)){
		i<-i+1
		dateFound<-NULL
		dateFound<-x$DATE[x$DATE==DATE[i]]
		if(length(dateFound)==0){
			j<-0
			r<-NULL
			while (length(r)==0){
				j<-j+1
				r<-which(result$DATE==(DATE[i]-j))
			}
			result<-rbind(result[1:I(r),],NA,result[I(r+1):length(result[[1]]),])
			result$DATE[r+1]<-DATE[i]	
			result$TICKER[r+1]<-result$TICKER[r]
			result$NAME[r+1]<-result$NAME[r]			
		}
	}
	row.names(result)<-seq(from=1,to=length(result[[1]]))

	result
}

market.model.counter<-function(x,dateEvent){
	#wrapper for market.model, to work only with this set
	#x is the dataframe, d is the eventDate,offset is back from d

	offsetStart<-21
	endOffSet<-135
	i<-x$COUNTER[x$DATE==dateEvent]
	dateEnd<-x$DATE[x$COUNTER==(i-offsetStart)]
	dateStart<-x$DATE[x$COUNTER==(i-endOffSet)]
	market.model(x$R,x$MR,x$DATE,dateStart,dateEnd)
}

market.model<-function(R,MR,DATE,dateStart,dateEnd){
	#returns a list object with alpha and beta
	#browser()
	x<-subset.quotes(R,MR,DATE,dateStart,dateEnd)
	model<-lm(R~MR,x)
	result<-list(model,dateStart,dateEnd,coefficients(model)[[1]],coefficients(model)[[2]],
		length(residuals(model)),mean(x$MR,na.rm=TRUE),
		var(x$MR)*(length(x$MR)+1),residuals(model))
	names(result)<-c("model","dateStart","dateEnd","alpha","beta","T","meanIndex","indexSSD","residuals")
	result #note: slight difference with Excel because narcyz uses only two digits of the price
}

subset.quotes<-function(R,MR,DATE,dateStart,dateEnd){
	#creates a data.frame with a subset of quote for a given period
	x<-data.frame(R,MR,DATE)
	names(x)<-c("R","MR","DATE")
	x<-subset(x,x$DATE>=dateStart)
	x<-subset(x,x$DATE<=dateEnd)
}

AR.calc<-function(x,model){
	#calculates abnormal returns
	#requires a market.model object "model"
	
	result<-x$R-predict(model$model,x)
	result
}

SAR.calc<-function(x,model){
	#requires a market.model object "model"
	#calculates standardised abnormal returns following methodology of 
	#Roztocki et al. 2008

	AR<-AR.calc(x,model) #abnormal returns
	sie<-sum((model$residuals-mean(model$residuals,na.rm=TRUE))^2, na.rm=TRUE)/(model$T-2)  #residual standard error - NAs removed, consider trimming
	AR.sd<-sqrt(sie*(1+1/model$T+(x$MR-model$meanIndex)^2/model$indexSSD))   #standard deviation for standardizing
	result<-AR/AR.sd
	result
}

SAR.dataframe<-function(x,dateStart,dateEnd,model){
	#returns a dataframe similar to x, but with a SAR variable
	x<-subset(x,x$DATE>=dateStart)
	x<-subset(x,x$DATE<=dateEnd)
	AR<-AR.calc(x,model)
	SAR<-SAR.calc(x,model)
	result<-data.frame(x,AR,SAR)
	names(result)[length(names(result))-1]<-'AR'
	names(result)[length(names(result))]<-'SAR'
	result
}
SAR.dataframe.window<-function(x,dateEvent,W,model){
	#wrapper that takes a window instead of dates
	#window W should be two arguments e.g. "c(0,1)"
	if (length(W)!=2) {stop("Window (W) should be two arguments e.g. c(0,1)")}
	if (is.null(x$COUNTER)){
		x$COUNTER<-counter(rep('tick',length(x[[1]])),x[[2]])
	}
	i<-x$COUNTER[x$DATE==dateEvent]
	if (max(x$COUNTER)>=i+W[2]&min(x$COUNTER)<=i+W[1]) {
		dateStart<-x$DATE[x$COUNTER==(i+W[1])]
		dateEnd<-x$DATE[x$COUNTER==(i+W[2])]
	}else{
		if (min(x$COUNTER)>(i+W[1])&max(x$COUNTER)>=i+W[2]) {
			dateStart<-min(x$DATE)
			dateEnd<-x$DATE[x$COUNTER==(i+W[2])]
			print(paste("Warning in SAR.dataframe.window for ", x$TICKER[[1]], ": dateStart in window exceeds data availability. Taking minimum date available: ", dateStart))
		}
		if (max(x$COUNTER)<(i+W[2])&min(x$COUNTER)<=i+W[1]){
			dateStart<-x$DATE[x$COUNTER==(i+W[1])]
			dateEnd<-max(x$DATE)
			print(paste("Warning in SAR.dataframe.window for ", x$TICKER[[1]], ": dateEnd in window exceeds data availability. Taking maximum date available: ", dateEnd))
		}
		if (min(x$COUNTER)>(i+W[1])&max(x$COUNTER)<(i+W[2])){
			dateStart<-min(x$DATE)
			dateEnd<-max(x$DATE)			
			print(paste("Warning in SAR.dataframe.window for ", x$TICKER[[1]], ": dateStart and dateEnd in window exceeds data availability. Taking what's available: ", dateStart, " and ", dateEnd))
		}
	}
	result<-SAR.dataframe(x,dateStart,dateEnd,model)
}


cumulate.returns<-function(returns,DATE,dateStart,dateEnd){
	#browser()
	x<-data.frame(returns,DATE)
	x<-subset(x,x$DATE>=dateStart)
	x<-subset(x,x$DATE<=dateEnd)
	result<-sum(x$returns)
	result
}

cumulate.returns.window<-function(returns,DATE,dateEvent,W){
	#wrapper that takes a window instead of dates
	#window W should be two arguments e.g. "c(0,1)"
	if (length(W)!=2) {stop("Window (W) should be two arguments e.g. c(0,1)")}
	x<-data.frame(returns,DATE)
	x$COUNTER<-counter(rep('tick',length(x[[1]])),x[[2]])
	names(x)<-c('R','DATE','COUNTER')
	i<-x$COUNTER[x$DATE==dateEvent]
	dateStart<-x$DATE[x$COUNTER==(i+W[1])]
	dateEnd<-x$DATE[x$COUNTER==(i+W[2])]
	
	result<-cumulate.returns(returns,DATE,dateStart,dateEnd)
	result
}

CSAR.window<-function(returns,DATE,dateEvent,W){
	#wrapper for the above functions, includes scaling by sqrt(T), as in Loderer&Mauer A6
	result<-cumulate.returns.window(returns,DATE,dateEvent,W)/sqrt(W[2]-W[1]+1)
	result
}

counter.event<-function(DATE,dateEvent){
	result<-rep(1,length(DATE))
	COUNTER<-counter(rep('a',length(DATE)),DATE)
	result[DATE==dateEvent]<-0
	x<-COUNTER[DATE==dateEvent]
	if (is.null(x)){stop("Event date not in range.")}
	
	for (i in 1:length(DATE)){
		result[i]<-COUNTER[i]-x
	}
	
	result
}

###FUNCTIONS END

