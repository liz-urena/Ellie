states=c("AL",
"AK",
"AZ",
"AR",
"CA",
"CO",
"CT",
"DE",
"DC",
"FL",
"GA",
"HI",
"ID",
"IL",
"IN",
"IA",
"KS",
"KY",
"LA",
"ME",
"MD",
"MA",
"MI",
"MN",
"MS",
"MO",
"MT",
"NE",
"NV",
"NH",
"NJ",
"NM",
"NY",
"NC",
"ND",
"OH",
"OK",
"OR",
"PA",
"RI",
"SC",
"SD",
"TN",
"TX",
"UT",
"VT",
"VA",
"WA",
"WV",
"WI",
"WY")
n<-c("01","02","04","05","06","08","09",10:13,15:42,44:51,53:56)
years<-c('2000','2004','2008')
for(i in 1:51){
	tmp<-read.table(paste('ST-EST00INT-03-',n[i],'.csv.txt',sep=""),sep=',',colClasses="character")
	assign(states[i], tmp[4:76,c(1,3,7,11)])

}

final_df<-data.frame(cbind(state=states[1],
	year=years[1],
	Hispanic = gsub(",","",as.character(AL[18,2])),
	Black = gsub(",","",as.character(AL[13,2])),
	Asian = gsub(",","",as.character(AL[15,2])),
	White = gsub(",","",as.character(AL[12,2])),
	AmericanIndian = gsub(",","",as.character(AL[14,2])),
	Male=gsub(",","",as.character(get(states[i])[26,2])),
	Female=gsub(",","",as.character(get(states[i])[50,2]))))

for(j in c(2,3,4)){
	for(i in 1:51){
		final_df=rbind(final_df,cbind(state=states[i],
	year=years[j-1],
	Hispanic = gsub(",","",as.character(get(states[i])[18,j])),
	Black = gsub(",","",as.character(get(states[i])[13,j])),
	Asian = gsub(",","",as.character(get(states[i])[15,j])),
	White = gsub(",","",as.character(get(states[i])[12,j])),
	AmericanIndian = gsub(",","",as.character(get(states[i])[14,j])),
	Male=gsub(",","",as.character(get(states[i])[26,j])),
	Female=gsub(",","",as.character(get(states[i])[50,j]))))

	}
}


write.xlsx(final_df,'Ellie_Final.xlsx')
