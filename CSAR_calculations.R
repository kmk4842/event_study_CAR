#Event study example in R for stocks from the Warsaw Stock Exchange

#load required library
library(chron)

#This input file requires functions from the CSAR_script file to be pre-loaded in R. 
source("CSAR_scripts.R") 

#set working directory - remember to change \ to /
setwd("./")

#specify date format to be used
date.format<-"d.m.y"

#load stock data from stooq.pl
#remember you need to clear the first line of <brackets> header should be TICKER,PER,DATE,TIME,OPEN,HIGH,LOW,CLOSE,VOLUME,OPENINT
quotes<-load.stooq("wse_stocks.csv") # this is a csv with all tickers in one file, merged using cmd line: *.txt > wse_stocks.csv - remember to eliminate duplicate header lines
indices<-load.stooq("wse_wig.csv") # stock index for the market model in the same format

#keep only useful data (save memory, files are big)
#quotes<-subset(quotes,quotes$DATE>=dates("01/01/2000"))
#indices<-subset(indices,indices$DATE>=dates("01/01/2000"))

#round up to two decimal places
quotes$CLOSE<-round(quotes$CLOSE,2)
indices$CLOSE<-round(indices$CLOSE,2)

#add return and market return variables - moved to per-company basis
#because dates were missing
	#quotes$R<-rates.daily.tickers(quotes$CLOSE,quotes$DATE,quotes$TICKER)
	#quotes$WIG<-match.index(indices,quotes$DATE)
	#quotes$MR<-rates.daily.tickers(quotes$WIG,quotes$DATE,quotes$TICKER)
	#quotes$COUNTER<-counter(quotes$TICKER,quotes$DATE)

	#sink("WIG_NA.txt")#NAs due to missing WIG quotes
	#	subset(quotes,is.na(quotes$WIG))
	#sink()

#calculate abnormal returns on a per-company basis
quotes.split<-split(quotes,quotes$TICKER)

for (i in 1:length(names(quotes.split))){
	try(quotes.split[[i]]<-check.dates(quotes.split[[i]],indices$DATE))
	try(quotes.split[[i]]$R<-rates.daily(quotes.split[[i]]$CLOSE,quotes.split[[i]]$DATE))
	try(quotes.split[[i]]$WIG<-match.index(indices,quotes.split[[i]]$DATE))
	try(quotes.split[[i]]$MR<-rates.daily(quotes.split[[i]]$WIG,quotes.split[[i]]$DATE))
	try(quotes.split[[i]]$COUNTER<-counter(quotes.split[[i]]$TICKER,quotes.split[[i]]$DATE))
}

# add R/VOLUME to calculate Amihud illiquidity stats - note that volume is in million PLN to make it comparable accross companies with potentially different stock prices
for (i in 1:length(names(quotes.split))){
	try(quotes.split[[i]]$AMIHUD<-((quotes.split[[i]]$R*100)/(quotes.split[[i]]$VOLUME*quotes.split[[i]]$CLOSE))*10^6)
}



#save data files after modifications
	write.csv2(indices,file="indices.csv")
	write.csv2(quotes,file="quotes.csv")
	
	save(quotes.split,file="quotes_split.R")

	#load(file="quotes_split.R")

#list of companies and event dates
 	events<-read.csv("events.csv",header=TRUE)
	names(events)<-toupper(names(events))
	
	#convert dates
	events$DATE<-dates(as.character(events$DATE), format="y-m-d", out.format=date.format)

	#cleanup tickers to get rid of space characters
	events$TICKER<-strtrim(events$TICKER,3)
		
#adding empty vectors for the volume variable and the eventDate variable
	events$AVGVOLUME<-as.double(rep(NA,length(events[[1]])))
	events$eventDate<-chron(rep(dates("07/07/77"),length(events[[1]])))

#adding empty vectors for model properties variables
	events$ALPHA<-as.double(rep(NA,length(events[[1]])))
	events$BETA<-as.double(rep(NA,length(events[[1]])))

#perform calculations for all companies in the list
	event_calculations<-as.list(as.character(events$TICKER)) #list with all results
	errors<-list("Observations which trigerred errors:")
for (i in 1:length(event_calculations)){
	last_i<-i
	ticker<-as.character(events$TICKER[i])
	x<-quotes.split[[ticker]]

	#determine the event date in case quotes are missing on original date
		eventDate<-events$DATE[i]		
		dateFound<-x$DATE[x$DATE==eventDate]
		if (length(dateFound)==1){
			events$eventDate[i]<-eventDate
		}else{
			for (j in 1:5) { #only look at five following days
				if (length(dateFound)==0){
					dateFound<-x$DATE[x$DATE==eventDate+j]
					if (length(dateFound)==1){
						eventDate<-eventDate+j
						events$eventDate[i]<-eventDate
					}
				}
			}
		}
	
	if (length(dateFound)==0||is.na(eventDate)==TRUE){
		errors[length(errors)+1]<-paste(i,"- Event date not within quote history for company",ticker)
		for (n in 3:length(names(events))) {events[[i,n]]<-NA}
	}else{
		if (which(x$DATE==eventDate)<271){
			errors[length(errors)+1]<-paste(i,"- Quote history too short for company",ticker)
			for (n in 3:length(names(events))) {events[[i,n]]<-NA}
		}else{
			#this only runs if there is enough quote history and eventDate in range
			model<-NULL #this to take care of problems with data
			try(
				model<-market.model.counter(x,eventDate))
			if (is.null(model)==FALSE){
				#set max window for SAR calculations
				calculations<-SAR.dataframe.window(x,eventDate,c(-250,250),model)
				calculations$COUNTER.EVENT<-counter.event(calculations$DATE,eventDate)
				event_calculations[[i]]$eventDate<-eventDate
				event_calculations[[i]]$model<-model
				event_calculations[[i]]$calculations<-calculations
				#CSARs per Window:
				events$W10_0[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(-10,0))
				events$W9_0[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(-9,0))
				events$W8_0[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(-8,0))
				events$W7_0[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(-7,0))
				events$W6_0[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(-6,0))
				events$W5_0[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(-5,0))
				events$W4_0[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(-4,0))
				events$W3_0[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(-3,0))
				events$W2_0[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(-2,0))
				events$W1_0[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(-1,0))
				events$W1_1[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(-1,1))
				events$W0_1[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(0,1))
				events$W0_2[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(0,2))
				events$W0_3[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(0,3))
				events$W0_4[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(0,4))
				events$W0_5[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(0,5))
				events$W0_6[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(0,6))
				events$W0_7[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(0,7))
				events$W0_8[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(0,8))
				events$W0_9[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(0,9))
				events$W0_10[i]<-CSAR.window(calculations$SAR,calculations$DATE,eventDate,c(0,10))
				#volume variables - mean daily event volume to non-event volume
				events$AVGVOLUME[i]<-mean(calculations$VOL,trim=0.1,na.rm=TRUE)#trims one decile off each tail before taking the mean
				#Volume before publication
				events$VOLUMEM2[i]<-mean(calculations$VOL[calculations$COUNTER.EVENT<0 & calculations$COUNTER.EVENT>-4], na.rm=TRUE)
				events$VOLUMEM5[i]<-mean(calculations$VOL[calculations$COUNTER.EVENT<0 & calculations$COUNTER.EVENT>-7], na.rm=TRUE)
				events$VOLUMEM10[i]<-mean(calculations$VOL[calculations$COUNTER.EVENT<0 & calculations$COUNTER.EVENT>-12], na.rm=TRUE)
				events$ABNVOLM2[i]<-events$VOLUMEM2[i]/events$AVGVOLUME[i]
				events$ABNVOLM5[i]<-events$VOLUMEM5[i]/events$AVGVOLUME[i]
				events$ABNVOLM10[i]<-events$VOLUMEM10[i]/events$AVGVOLUME[i]
				#Volume after publication
				events$VOLUME2[i]<-mean(calculations$VOL[calculations$COUNTER.EVENT>=0 & calculations$COUNTER.EVENT<3], na.rm=TRUE)
				events$VOLUME5[i]<-mean(calculations$VOL[calculations$COUNTER.EVENT>=0 & calculations$COUNTER.EVENT<6], na.rm=TRUE)
				events$VOLUME10[i]<-mean(calculations$VOL[calculations$COUNTER.EVENT>=0 & calculations$COUNTER.EVENT<11], na.rm=TRUE)
				events$ABNVOL2[i]<-events$VOLUME2[i]/events$AVGVOLUME[i]
				events$ABNVOL5[i]<-events$VOLUME5[i]/events$AVGVOLUME[i]
				events$ABNVOL10[i]<-events$VOLUME10[i]/events$AVGVOLUME[i]
				#model properties
				events$ALPHA[i]<-model$alpha
				events$BETA[i]<-model$beta
				#AMIHUD Illiquidity stats for absolute values in percentage points per PLN million, removing NAs (cases where volume is 0 cause NAs):
				if (length(calculations$AMIHUD==500)){
					events$AMIHUD_Y_PRE[i]<-mean(abs(calculations$AMIHUD[0:250]), na.rm=TRUE)
					events$AMIHUD_Y_POST[i]<-mean(abs(calculations$AMIHUD[250:500]), na.rm=TRUE)
				}else{
					errors[length(errors)]<-paste("Data availability limited for: ",i,ticker, ". Taking maximum range available for AMIHUD statistics.")
					events$AMIHUD_Y_PRE[i]<-mean(abs(calculations$AMIHUD[0:250]), na.rm=TRUE)
					events$AMIHUD_Y_POST[i]<-mean(abs(calculations$AMIHUD[250:length(calculations$AMIHUD)]), na.rm=TRUE)
				}
				#rm variables just in case
				rm(model,calculations)
			}else{
				errors[length(errors)]<-paste("Unknown error in observation:",i,ticker)
				for (n in 3:length(names(events))) {events[[i,n]]<-NA}
				}
		}
	}
	rm(eventDate,x,ticker,dateFound)
}
	errors
	#reasons for errors not included in above: 
		#1.NA values within CSAR window (returns NA for CSAR)
		#still have problems with TPS calculations for the first two events

  write.csv2(events,file="events_out_new_table.csv")

#Logging
sink("events_calculations_new.txt")
	errors
	event_calculations
sink()

#Statistical analysis of the results
library(corrplot)
events_clean <- na.omit(events[,!(names(events) %in% c("VOLUME2","VOLUME5","VOLUME10","VOLUMEM2","VOLUMEM5","VOLUMEM10","AVGVOLUME"))])
events_clean$YEAR<-format(as.POSIXct(events_clean$eventDate), format="%Y")

sink("statistical_analysis.txt")
summary(events_clean)
for (n in names(events_clean[,(names(events_clean) %in% c("BETA", "ALHPA", "W5_0", "W1_1", "W0_5", "ABNVOL2", "ABNVOLM2", "AMIHUD_Y_PRE", "AMIHUD_Y_POST"))])){
	print(paste("Summary statistics by YEAR for: ", n))
	print(tapply(events_clean[[n]], events_clean$YEAR, summary))
}

CARsign<-rep(NA,length(events_clean[[1]]))
CARsign[events_clean$W1_1>0]<-"CAR+"
CARsign[events_clean$W1_1<0]<-"CAR-"
for (n in names(events_clean[4:(length(events_clean)-1)])){
	#print(paste("Summary statistics by sign of CAR 1-1 for: ", n))
	#print(summary(events_clean[[n]][CARsign=="CAR-"]))
	#print(summary(events_clean[[n]][CARsign=="CAR+"]))
	if (abs(mean(events_clean[[n]][CARsign=="CAR-"])-mean(events_clean[[n]][CARsign=="CAR+"]))>sd(events_clean[[n]])*0.5){
		print(paste("Difference in means detected for positive vs. negative CAR 1-1 in variable ", n, " means are: ", mean(events_clean[[n]][CARsign=="CAR-"]), " and ", mean(events_clean[[n]][CARsign=="CAR+"])))
	}
}

print("Winsorizing at 2%")
for (n in names(events_clean[,!(names(events_clean) %in% c("TICKER", "DATE", "eventDate", "YEAR"))])){
	q = quantile(events_clean[[n]],probs=c(0.02,0.98))
	events_clean[[n]][events_clean[n]<q[[1]]]<-q[[1]]
	events_clean[[n]][events_clean[n]>q[[2]]]<-q[[2]]
}

cortable<-cor(events_clean[4:length(events_clean)-1])
corrplot(cortable, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

cortable<-cor(events_clean[,(names(events_clean) %in% c("BETA", "ALHPA", "W5_0", "W1_1", "W0_5", "ABNVOL2", "ABNVOLM2", "AMIHUD_Y_PRE", "AMIHUD_Y_POST"))])
cortable
corrplot(cortable, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
print("Correlations for positive CAR")
cor(subset(events_clean[,(names(events_clean) %in% c("BETA", "ALHPA", "W5_0", "W1_1", "W0_5", "ABNVOL2", "ABNVOLM2", "AMIHUD_Y_PRE", "AMIHUD_Y_POST"))], CARsign=="CAR-"))
print("Correlations for negative CAR")
cor(subset(events_clean[,(names(events_clean) %in% c("BETA", "ALHPA", "W5_0", "W1_1", "W0_5", "ABNVOL2", "ABNVOLM2", "AMIHUD_Y_PRE", "AMIHUD_Y_POST"))], CARsign=="CAR+"))

sink()