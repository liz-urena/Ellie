library(foreign)
library(plm)

# y1999<-read.table('1999.txt',header=TRUE,sep=';')

# df<- data.frame(state=0, Unemployment = 0)

# for(i in unique(y1999$Code.1)){
# 	tmp<-subset(y1999,Code.1==i, select = c('Force', 'Unemp_Level'))
# 	unemp<-sum(tmp[,2])/sum(tmp[,1])
# 	df<-rbind(df , c(state=i,Unemployment = unemp))
# }


data<-read.spss('VoterIdentificationData.sav',to.data.frame=TRUE)
data<-data[2:length(data[,1]),]

VoterIDSeverity2<-rep(0,204)
VoterIDSeverity2[which(data$LevelsVoterIDSeverity == "Voter must present proof of identification and his/her signature must match thesignature on the identification provided")]<-6
VoterIDSeverity2[which(data$LevelsVoterIDSeverity == "Voter must present proof of identification or voter registration card. Range varies")]<-5
VoterIDSeverity2[which(data$LevelsVoterIDSeverity == "Voter is requested to present proof of identification or voter registration card (An affidavit may be signed )")]<-4
VoterIDSeverity2[which(data$LevelsVoterIDSeverity == "Voter must sign his/her name in a poll-book and it must match a signature on file")]<-3
VoterIDSeverity2[which(data$LevelsVoterIDSeverity == "Voter must sign his/her name in a poll-book.")]<-2
VoterIDSeverity2[which(data$LevelsVoterIDSeverity == "Voter must state his/her name.")]<-1
VoterIDSeverity2[which(data$LevelsVoterIDSeverity == "Voter is requested to present photo identification")]<-7
VoterIDSeverity2[which(data$LevelsVoterIDSeverity == "Voter is required to present photo identification")]<-8

data$VoterIDSeverity<-VoterIDSeverity2


tmp<-rep(0,204)
tmp[which(data$ChangeinTurnoutrepublicans=='increase in turnout')]<-1
tmp[which(data$ChangeinTurnoutrepublicans=='decrease in turnout')]<-0
data$ChangeinTurnoutrepublicans <- tmp


pdata<-pdata.frame(data,index=c('State','Year'))
summary(data.fe<-plm(VEPHighestOfficeTurntoutRate~factor(VoterIDSeverity),model='random',data=pdata,effect='individual'))

summary(data.fe<-plm(PercentRepublicanVotes~factor(VoterIDSeverity)
			+ElectionYearPercentUnemployed
			+OneYearBeforeElectionPercentUnemployed
			+TwoYearsBeforeElectionPercentUnemployed
			+RealPerCapitaGDPChained2005Dollars
			+PerecentOf25YearOldswithHighSchoolDegreeOrHigher
			+PerecentOf25YearOldswithCollegelDegreeOrHigher
			+I((Hispanic+Black+Asian)/StatePop),model='random',data=pdata,effect='individual'))

summary(data.fe<-plm(PercentDemocratVotes~factor(VoterIDSeverity)
			+ElectionYearPercentUnemployed
			+OneYearBeforeElectionPercentUnemployed
			+TwoYearsBeforeElectionPercentUnemployed
			+RealPerCapitaGDPChained2005Dollars
			+PerecentOf25YearOldswithHighSchoolDegreeOrHigher
			+PerecentOf25YearOldswithCollegelDegreeOrHigher
			+I((Hispanic+Black+Asian)/StatePop),model='random',data=pdata,effect='individual'))


summary(data.fe<-plm(VEPHighestOfficeTurntoutRate~factor(VoterIDSeverity)
			+ElectionYearPercentUnemployed
			+OneYearBeforeElectionPercentUnemployed
			+TwoYearsBeforeElectionPercentUnemployed
			+RealPerCapitaGDPChained2005Dollars
			+PerecentOf25YearOldswithHighSchoolDegreeOrHigher
			+PerecentOf25YearOldswithCollegelDegreeOrHigher
			+I((Hispanic+Black+Asian)/StatePop),model='random',data=pdata,effect='individual'))



summary(data.fe<-lm(log(VEPHighestOfficeTurntoutRate)~(VoterIDSeverity)
			+ElectionYearPercentUnemployed
			+OneYearBeforeElectionPercentUnemployed
						+TwoYearsBeforeElectionPercentUnemployed

			+RealPerCapitaGDPChained2005Dollars
			+PerecentOf25YearOldswithHighSchoolDegreeOrHigher
						+PerecentOf25YearOldswithCollegelDegreeOrHigher

			+I(Hispanic/StatePop)
			+I(White/StatePop)
			+I(Asian/StatePop)
			+I(Black/StatePop),data=subset(data,Year==2012)))
summary(data.fe<-lm(log(VEPHighestOfficeTurntoutRate)~(VoterIDSeverity)
			+ElectionYearPercentUnemployed
			+OneYearBeforeElectionPercentUnemployed
						+TwoYearsBeforeElectionPercentUnemployed

			+RealPerCapitaGDPChained2005Dollars
			+PerecentOf25YearOldswithHighSchoolDegreeOrHigher
						+PerecentOf25YearOldswithCollegelDegreeOrHigher

			+I(Hispanic/StatePop)
			+I(White/StatePop)
			+I(Asian/StatePop)
			+I(Black/StatePop),data=subset(data,Year==2008)))
summary(data.fe<-lm(log(VEPHighestOfficeTurntoutRate)~(VoterIDSeverity)
			+ElectionYearPercentUnemployed
			+OneYearBeforeElectionPercentUnemployed
						+TwoYearsBeforeElectionPercentUnemployed

			+RealPerCapitaGDPChained2005Dollars
			+PerecentOf25YearOldswithHighSchoolDegreeOrHigher
						+PerecentOf25YearOldswithCollegelDegreeOrHigher

			+I(Hispanic/StatePop)
			+I(White/StatePop)
			+I(Asian/StatePop)
			+I(Black/StatePop),data=subset(data,Year==2004)))
summary(data.fe<-lm(log(VEPHighestOfficeTurntoutRate)~(VoterIDSeverity)
			+ElectionYearPercentUnemployed
			+OneYearBeforeElectionPercentUnemployed
						+TwoYearsBeforeElectionPercentUnemployed

			+RealPerCapitaGDPChained2005Dollars
			+PerecentOf25YearOldswithHighSchoolDegreeOrHigher
						+PerecentOf25YearOldswithCollegelDegreeOrHigher

			+I(Hispanic/StatePop)
			+I(White/StatePop)
			+I(Asian/StatePop)
			+I(Black/StatePop),data=subset(data,Year==2000)))

############################################################################

summary(data.fe<-glm(ChangeinTurnoutDems~(VoterIDSeverity)
			+ElectionYearPercentUnemployed
			+OneYearBeforeElectionPercentUnemployed
						+TwoYearsBeforeElectionPercentUnemployed

			+RealPerCapitaGDPChained2005Dollars
			+PerecentOf25YearOldswithHighSchoolDegreeOrHigher
						+PerecentOf25YearOldswithCollegelDegreeOrHigher

			+I(Hispanic/StatePop)
			+I(White/StatePop)
			+I(Asian/StatePop)
			+I(Black/StatePop),family=binomial(),data=subset(data,Year==2012)))
summary(data.fe<-glm(ChangeinTurnoutDems~(VoterIDSeverity)
			+ElectionYearPercentUnemployed
			+OneYearBeforeElectionPercentUnemployed
						+TwoYearsBeforeElectionPercentUnemployed

			+RealPerCapitaGDPChained2005Dollars
			+PerecentOf25YearOldswithHighSchoolDegreeOrHigher
						+PerecentOf25YearOldswithCollegelDegreeOrHigher

			+I(Hispanic/StatePop)
			+I(White/StatePop)
			+I(Asian/StatePop)
			+I(Black/StatePop),family=binomial(),data=subset(data,Year==2008)))
summary(data.fe<-glm(ChangeinTurnoutDems~(VoterIDSeverity)
			+ElectionYearPercentUnemployed
			+OneYearBeforeElectionPercentUnemployed
						+TwoYearsBeforeElectionPercentUnemployed

			+RealPerCapitaGDPChained2005Dollars
			+PerecentOf25YearOldswithHighSchoolDegreeOrHigher
						+PerecentOf25YearOldswithCollegelDegreeOrHigher

			+I(Hispanic/StatePop)
			+I(White/StatePop)
			+I(Asian/StatePop)
			+I(Black/StatePop),family=binomial(),data=subset(data,Year==2004)))
summary(data.fe<-glm(ChangeinTurnoutDems~(VoterIDSeverity)
			+ElectionYearPercentUnemployed
			+OneYearBeforeElectionPercentUnemployed
						+TwoYearsBeforeElectionPercentUnemployed

			+RealPerCapitaGDPChained2005Dollars
			+PerecentOf25YearOldswithHighSchoolDegreeOrHigher
						+PerecentOf25YearOldswithCollegelDegreeOrHigher

			+I(Hispanic/StatePop)
			+I(White/StatePop)
			+I(Asian/StatePop)
			+I(Black/StatePop),family=binomial(),data=subset(data,Year==2000)))