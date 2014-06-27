
#read in s2s data & log2_netCellAssociation 
s2s.dat0<- read.csv('s2snet_data0.csv',row.names=4,header=T)#Scaled_

s2s.indx<- colnames(s2s.dat0)[4:dim(s2s.dat0)[2]]
s2s.dat1<- s2s.dat0[2:dim(s2s.dat0)[1],4:dim(s2s.dat0)[2]]
s2s.dat1<- apply(s2s.dat1,1,function(x)as.numeric(as.character(x)))
#rownames(s2s.dat1); colnames(s2s.dat1)
#dim(s2s.dat1) ; class(s2s.dat1)

net.c<- s2s.dat0[2:dim(s2s.dat0)[1],2]
net.c<- as.numeric(as.character(net.c))
#length(net.c)

library(caret)

#1)divide data in train and test sets
	set.seed(1)
	my.datf<- as.data.frame(cbind(net.c,t(s2s.dat1)))
	inTrain <- createDataPartition(1:dim(my.datf)[1], p = 3/4, list = FALSE)
	my.datf.train<- my.datf[inTrain,]
	#write.csv(my.datf.train,'s2s_train.csv')
	my.datf.test<- my.datf[-inTrain,]
	#write.csv(my.datf.test,'s2s_test.csv') 

#2) get rid off near zero variance columns
	s2s.var<- nearZeroVar(my.datf.train[,-1])
	my.datf.train<- my.datf.train[,-(s2s.var+1)]
	my.datf.test<- my.datf.test[,-(s2s.var+1)]

#3) get rid of redundant features, 
#i.e. remove features with more than 0.7 correlation
	s2s.cor<- cor(my.datf.train[,-1])
	high.cor<- findCorrelation(s2s.cor,0.7)
	my.datf.train<- my.datf.train[,-(high.cor+1)]
	my.datf.test<- my.datf.test[,-(high.cor+1)]

#specify CV parameters	
	ctrl<- trainControl(method = 'repeatedcv', number = 10,repeats = 10,
	summaryFunction = defaultSummary)
	
#run glm stepwise based on AIC
	set.seed(2)
	lm.fit<- train(net.c~.,data=my.datf.train,
	method = 'glmStepAIC', tuneLength = 10, trControl = ctrl,
	metric = 'RMSE')#,tuneGrid=expand.grid(.fraction= seq(0.1,1,by=0.1)),
	#preProc = c('center', 'scale'))
	lm.fit ; predictors(lm.fit)

#RMSE & R^2, for train/ test respectively
lm.train.res<- getTrainPerf(lm.fit)
lm.test.res<- postResample(predict(lm.fit,my.datf.test),my.datf.test[,1])

lm.train.res; lm.test.res

raw1<-read.csv('LCMSMS_Table1_t_gold.csv',header=T,row.names=1)
raw2<- raw1[3:dim(raw1)[1],]
prot1<- read.csv('129selected.csv')
raw2<- raw2[,which(colnames(raw2) %in% as.character(prot1[,1]))]

co5<- which(colnames(raw2)=='CO5')
raw2<- raw2[-co5,]
np.freq<- length(dim(raw2)[2])
for(i in 1:dim(raw2)[2]){
	np.freq[i]<- length(which(raw2[,i]!=0))/dim(raw2)[1]
}

ex1<- which(np.freq<=0.4)
ex1
ex1.nam<- colnames(raw2[-co5,])[ex1]
ex1.nam

rnam2<- as.vector(s2s.dat0[1,4:dim(s2s.dat0)[2]])
rnam2<- as.character(unlist(rnam2))
rownames(s2s.dat1)<- rnam2

s2s.dat2<- s2s.dat1[ex1,]
rownames(s2s.dat2) 
s2s.dat2<- s2s.dat2[order(ex1.nam),]

prot1<- read.table('prot1.txt')
prot1<- as.character(prot1[,1])
ex2<- which(rownames(s2s.dat1) %in% prot1)
s2s.dat2<- s2s.dat1[ex2,]

my.lm<- lm(as.matrix(net.c)~as.matrix(t(s2s.dat2)))
my.lm.sum<- summary(my.lm)
	#coef(my.lm)
	#step(my.lm)
	


