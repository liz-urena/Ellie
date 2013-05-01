y1999<-read.table('1999.txt',header=TRUE,sep=';')

df<- data.frame(c(state=0, Unemployment = 0))

for(i in 1:50){
	tmp<-subset(y1999,Code.1==i, select = c('Force', 'Unemp_Level'))
	unemp<-sum(tmp[,2])/sum(tmp[,1])
	df<-rbind(df , c(state=i,Unemployment = unemp))
}
