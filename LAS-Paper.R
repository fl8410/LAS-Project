source("~network.mixtures.R")
data.list<-list(data1,data2,HMDat1,HMDat2,IU.Advice,IU.Friends,Updated.Pac.Adv2,Updated.Pac.Fr2)
LASI.data<-lapply(data.list,consensus,method="LAS.intersection")
LASU.data<-lapply(data.list,consensus,method="LAS.union")
#Mod.list<-list(Mod1,Mod2,Mod3,Mod4,IU.Advice.Model,IU.Friend.Model,Pacific.Advice.Model,Pacific.Friendship.Model)
Mod.list<-lapply(1:8,function(i){bbnam.mix(data.list[[i]],c(.5,.5,.5))})
LASI.Hdists<-lapply(1:8,function(i){apply(Mod.list[[i]]$net,1,hdist,LASI.data[[i]])})
LASU.Hdists<-lapply(1:8,function(i){apply(Mod.list[[i]]$net,1,hdist,LASU.data[[i]])})
network.names<-c("SS-AD","SS-FR","HM-AD","HM-FR","IU-AD","IU-FR","PD-AD","PD-FR")
png(filename="C:/Users/francis/Desktop/LASComparison/figures/LASPerformance.png",width=10,height=6,units="in",res=300)
par(font.axis = 2)
par(mfrow=c(3,3))
par(mar=c(2,1,2,1))
for(i in 1:8){
  if(i==8){
    plot(density(New.LASU.Hdists[[i]]),main=bquote(bold(.(network.names[i])~ "Network ")),yaxt="n" ,xlab=c("Hamming Distance"),xlim=c(0,max(New.LASU.Hdists[[i]])+10),col=rgb(0,0,1,1))
    polygon(density(New.LASU.Hdists[[i]]),col="blue",border="blue")
    lines(density(New.LASI.Hdists[[i]]),col=rgb(1,0,0,1))
    polygon(density(New.LASI.Hdists[[i]]),col="red",border="red")    
  } else{
  plot(density(New.LASI.Hdists[[i]]),main=bquote(bold(.(network.names[i])~ "Network ")),yaxt="n" ,xlab=c("Hamming Distance"),xlim=c(0,max(New.LASU.Hdists[[i]])+10),col=rgb(1,0,0,1))
  polygon(density(New.LASI.Hdists[[i]]),col="red",border="red")
  lines(density(New.LASU.Hdists[[i]]),col=rgb(0,0,1,1))
  polygon(density(New.LASU.Hdists[[i]]),col="blue",border="blue")
  #hist(LASI.Hdists[[i]],main=bquote(bold(.(network.names[i])~ "Network ")),xlab=c("Hamming Distance"),xlim=c(0,max(LASU.Hdists[[i]])+10),col=rgb(1,0,0,1))
  #hist(LASU.Hdists[[i]],col=rgb(0,0,1,1),add=TRUE)

  }
}
plot.new()
legend("topleft",c("LAS-I","LAS-U"),col=c("red","blue"),lty=1,lwd=3,cex=2)
dev.off()

png(filename="C:/Users/francis/Desktop/LASComparison/figures/SRPerformance.png",width=10,height=6,units="in",res=300)
par(font.axis = 2)
par(mfrow=c(3,3))
par(mar=c(2,1,2,1))
for(i in 1:8){
    plot(density(New.SR.Hdists[[i]]),main=bquote(bold(.(network.names[i])~ "Network ")),yaxt="n" ,xlab=c("Hamming Distance"),col=rgb(0,0,1,1))
    polygon(density(New.SR.Hdists[[i]]),col="blue",border="blue")
    #hist(LASI.Hdists[[i]],main=bquote(bold(.(network.names[i])~ "Network ")),xlab=c("Hamming Distance"),xlim=c(0,max(LASU.Hdists[[i]])+10),col=rgb(1,0,0,1))
    #hist(LASU.Hdists[[i]],col=rgb(0,0,1,1),add=TRUE)
  }
dev.off()

Self.Error.Rates<-lapply(1:8,function(i){Self.Error(Mod.list[[i]],data.list[[i]])})
FP.Error.Rates<-lapply(1:8,function(i){apply(Mod.list[[i]]$ep,2,mean)})
FN.Error.Rates<-lapply(1:8,function(i){apply(Mod.list[[i]]$em,2,mean)})

FP.Diff<-lapply(1:8,function(i){Self.Error.Rates[[i]]$FPR-FP.Error.Rates[[i]]})
FN.Diff<-lapply(1:8,function(i){Self.Error.Rates[[i]]$FNR-FN.Error.Rates[[i]]})

#PLot the False Positive Self and Global Rates against each other
par(mfrow=c(3,3))
par(mar=c(2,1,1,2))
for(i in 1:8){
  plot(density(Self.Error.Rates[[i]]$FPR),main=bquote(bold(.(network.names[i])~ "Network ")),xlab=c("Error Rate"),xlim=c(0,1),col=rgb(1,0,0,.2))
  polygon(density(Self.Error.Rates[[i]]$FPR),col=rgb(1,0,0,.2),border=rgb(1,0,0,.2))
  lines(density(FP.Error.Rates[[i]]),col=rgb(0,0,1,.2))
  polygon(density(FP.Error.Rates[[i]]),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
  ##lines(density(Self.Error.Rates[[i]]$FNR),col=rgb(0,0,1,.2))
  ##polygon(density(Self.Error.Rates[[i]]$FNR),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
  #hist(LASI.Hdists[[i]],main=bquote(bold(.(network.names[i])~ "Network ")),xlab=c("Hamming Distance"),xlim=c(0,max(LASU.Hdists[[i]])+10),col=rgb(1,0,0,1))
  #hist(LASU.Hdists[[i]],col=rgb(0,0,1,1),add=TRUE)
}
plot.new()
legend("topleft",c("False Pos","False Neg"),col=c(rgb(1,0,0,.2),rgb(0,0,1,.2)),lty=1,lwd=3,cex=2)

#Try cross validation bandwidth selection.
#molInf
#Unprocess the data, convenient to split the data into different bins
#Ego reporting on themselves that doesn't' handles NA's fine. Fit the bayesian model.
#How different the graph estimates are.
#Differences between the Self Error Rates and the Global Error Rates
par(mfrow=c(3,3))
par(mar=c(2,1,1,2))
for(i in 1:8){
  if(i==3){
    plot(density(FN.Diff[[i]]),main=bquote(bold(.(network.names[i])~ "Network ")),xaxt="n",xlab=c("Error Rate"),xlim=c(-1,1),col=rgb(1,0,0,.2))
    polygon(density(FN.Diff[[i]]),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
    lines(density(FP.Diff[[i]]),col=rgb(1,0,0,.2))
    polygon(density(FP.Diff[[i]]),col=rgb(1,0,0,.2),border=rgb(1,0,0,.2))
  }else{
  plot(density(FP.Diff[[i]]),main=bquote(bold(.(network.names[i])~ "Network ")),xlab=c("Error Rate"),xlim=c(-1,1),col=rgb(1,0,0,.2))
  polygon(density(FP.Diff[[i]]),col=rgb(1,0,0,.2),border=rgb(1,0,0,.2))
  lines(density(FN.Diff[[i]]),col=rgb(0,0,1,.2))
  polygon(density(FN.Diff[[i]]),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
  }
  
  
  ##lines(density(Self.Error.Rates[[i]]$FNR),col=rgb(0,0,1,.2))
  ##polygon(density(Self.Error.Rates[[i]]$FNR),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
  #hist(LASI.Hdists[[i]],main=bquote(bold(.(network.names[i])~ "Network ")),xlab=c("Hamming Distance"),xlim=c(0,max(LASU.Hdists[[i]])+10),col=rgb(1,0,0,1))
  #hist(LASU.Hdists[[i]],col=rgb(0,0,1,1),add=TRUE)
}
plot.new()
legend("topleft",c("False +","False -"),col=c(rgb(1,0,0,.2),rgb(0,0,1,.2)),lty=1,lwd=3,cex=1.5)


#Plot the False Negative Rates with the Self and Global Rates against each  plot(density(Self.Error.Rates[[i]]$FNR),main=bquote(bold(.(network.names[i])~ "Network ")),xlab=c("Error Rate"),xlim=c(0,1),col=rgb(1,0,0,.2))

par(mfrow=c(3,3))
par(mar=c(2,1,1,2))
for(i in 1:8){
  plot(density(Self.Error.Rates[[i]]$FNR),main=bquote(bold(.(network.names[i])~ "Network ")),xlab=c("Error Rate"),xlim=c(0,1),col=rgb(1,0,0,.2))
  polygon(density(Self.Error.Rates[[i]]$FNR),col=rgb(1,0,0,.2),border=rgb(1,0,0,.2))
  lines(density(FN.Error.Rates[[i]]),col=rgb(0,0,1,.2))
  polygon(density(FN.Error.Rates[[i]]),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
  ##lines(density(Self.Error.Rates[[i]]$FNR),col=rgb(0,0,1,.2))
  ##polygon(density(Self.Error.Rates[[i]]$FNR),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
  #hist(LASI.Hdists[[i]],main=bquote(bold(.(network.names[i])~ "Network ")),xlab=c("Hamming Distance"),xlim=c(0,max(LASU.Hdists[[i]])+10),col=rgb(1,0,0,1))
  #hist(LASU.Hdists[[i]],col=rgb(0,0,1,1),add=TRUE)
}
plot.new()
legend("topleft",c("False +","False -"),col=c(rgb(1,0,0,.2),rgb(0,0,1,.2)),lty=1,lwd=3,cex=2)





plot(NA,type="n",axes=FALSE,legend=legend(c("LAS Intersection","LAS Union")))




#Preprocessing, need to separate the ego vs alter error rates rather than the global error rates
#Creating the Edge By Informant Matrix
MolPreProcess<-function(data){
  nactors<-dim(data)[1]
  FMatrix<-matrix(NA,nrow=nactors*nactors,ncol=nactors*2)#Rows are edges, Columns are informants
  new.data<-data #Create a new dataset to easily get the alter values
  for(i in 1:nactors){
    diag(data[i,,])<-9
    diag(new.data[i,,])<-9
    new.data[i,i,]<-NA
    new.data[i,,i]<-NA
    tmp.matrix<-matrix(NA,nrow=nactors,ncol=nactors) #This is for the ego reports on ego
    tmp.matrix[i,]<-data[i,i,]
    tmp.matrix[,i]<-data[i,,i]
    FMatrix[,i*2-1]<-c(tmp.matrix) #This is ego's reports on ego
    FMatrix[,i*2]<-c(new.data[i,,]) #This is ego's reports on alters
  }
  diag.indices<-unlist(lapply(1:nactors,function(x){x+((x-1)*nactors)}))#Remove Diagonals
  FMatrix<-FMatrix[-diag.indices,]
  return(FMatrix)
}


MolPreProcess.Check1<-function(data){
  nactors<-dim(data)[1]
  FMatrix<-matrix(NA,nrow=nactors*nactors,ncol=nactors)#Rows are edges, Columns are informants
  new.data<-data #Create a new dataset to easily get the alter values
  for(i in 1:nactors){
    diag(data[i,,])<-9
    FMatrix[,i]<-data[i,,] #This is ego's reports on ego
  }
  diag.indices<-unlist(lapply(1:nactors,function(x){x+((x-1)*nactors)}))#Remove Diagonals
  FMatrix<-FMatrix[-diag.indices,]
  return(FMatrix)
}




MolPreProcess.Check<-function(data){
  nactors<-dim(data)[1]
  FMatrix<-matrix(NA,nrow=nactors*nactors,ncol=nactors)#Rows are edges, Columns are informants
  for(i in 1:nactors){
    diag(data[i,,])<-NA
    FMatrix[,i]<-c(data[i,,]) #This is ego's reports on ego
  }
  return(FMatrix)
}

MolData.Ch1<-lapply(data.list,MolPreProcess.Check1)
MolModels.Ch1<-lapply(MolData.Ch1,molInf,sprior=c(.5,.5),suppress.perversion=FALSE)



MolData<-lapply(data.list,MolPreProcess)

MolModels<-lapply(MolData,molInf,sprior=c(.5,.5),suppress.perversion=FALSE)

graph.maker<-function(x){ #Creates all the central graphs
  nactors<-dim(x$em)[2]/2
  blank.mat<-matrix(NA,nrow=nactors,ncol=nactors)
  centralgraphvector<-apply(x$s,2,mean)>.5 #If the posterior mean prob of the state is greater than .5, the edge exists, otherwise no
  for(i in 1:(nactors)){
    blank.mat[-i,i]<-centralgraphvector[((i-1)*(nactors-1)+1):((nactors-1)*i)]
  }
  #blank.mat<-t(blank.mat)
  return(blank.mat)
}

graph.maker.array<-function(x){ #Creates all the central graphs
  nactors<-dim(x$em)[2]/2
  ngraphs<-dim(MolModels[[1]]$s)[1]
  blank.array<-array(dim=c(ngraphs,nactors,nactors))
  for(j in 1:ngraphs){
    for(i in 1:(nactors)){
      blank.array[j,-i,i]<-x$s[j,((i-1)*(nactors-1)+1):((nactors-1)*i)]
    }
  }
  #blank.mat<-t(blank.mat)
  return(blank.array)
}

RevModel.array<-lapply(MolModels,graph.maker.array)

New.LASI.Hdists<-lapply(1:8,function(i){apply(RevModel.array[[i]],1,hdist,LASI.data[[i]])})
New.LASU.Hdists<-lapply(1:8,function(i){apply(RevModel.array[[i]],1,hdist,LASU.data[[i]])})


New.SR.Hdists<-lapply(1:8,function(i){apply(RevModel.array[[i]],1,hdist,SR.data[[i]])})

CSS.Hdists<-lapply(1:8,function(i){apply(data.list[[i]],1,hdist,CG.MolM[[i]])})

png(filename="C:/Users/francis/Desktop/LASComparison/figures/CSSPerformance.png",width=10,height=6,units="in",res=300)
par(font.axis = 2)
par(mfrow=c(3,3))
par(mar=c(2,1,2,1))
for(i in 1:8){
    plot(density(CSS.Hdists[[i]]),main=bquote(bold(.(network.names[i])~ "Network ")),yaxt="n",xlab=c("Hamming Error"),xlim=c(0,max(CSS.Hdists[[i]])+10),col=rgb(1,0,0,.2))
    polygon(density(CSS.Hdists[[i]]),col=rgb(1,0,0,.2),border=rgb(1,0,0,.2))
    lines(density(CSS.Hdists[[i]]),col=rgb(1,0,0,.2))
    abline(v=mean(CSS.Hdists[[i]]),col="red",lwd=2)
    abline(v=SR.Mean.HDists[[i]],col="blue",lwd=2)
}
plot.new()
par(font=2)
legend("topleft",c("CSS Report","Self-Report"),col=c(rgb(1,0,0,.2),rgb(0,0,1,.2)),lty=1,lwd=3,cex=1.5)
dev.off()





CG.MolM<-lapply(MolModels,graph.maker)

# graph.maker.ch<-function(x){ #Creates all the central graphs
#   nactors<-dim(x$em)[2]
#   blank.mat<-matrix(NA,nrow=nactors,ncol=nactors)
#   centralgraphvector<-apply(x$s,2,mean)>.5 #If the posterior mean prob of the state is greater than .5, the edge exists, otherwise no
#   blank.mat<-matrix(centralgraphvector,nrow=nactors,ncol=nactors)
#   #blank.mat<-t(blank.mat)
#   return(blank.mat)
# }
#CG.MolM<-lapply(MolModels,graph.maker)
# 
# CG.MolM.ch<-lapply(MolModels.Ch,graph.maker.ch)
# 
# graph.maker.ch1<-function(x){ #Creates all the central graphs
#   nactors<-dim(x$em)[2]
#   blank.mat<-matrix(NA,nrow=nactors,ncol=nactors)
#   centralgraphvector<-apply(x$s,2,mean)>.5 #If the posterior mean prob of the state is greater than .5, the edge exists, otherwise no
#   for(i in 0:(nactors)){
#     blank.mat[i,-i]<-centralgraphvector[((nactors-1)*i+1):((nactors-1)*i+nactors-1)]
#   }
#   #blank.mat<-t(blank.mat)
#   return(blank.mat)
# }
# 
# CG.MolM.ch1<-lapply(MolModels.Ch1,graph.maker.ch1)
# 
# unlist(lapply(1:8,function(i){hdist(CG.MolM.ch[[i]],centralgraph(Mod.list[[i]]$net))}))
# 
# error.rates<-lapply(MolModels,function(x){fn<-apply(x$em,2,mean);for(i in 1:length)})
# 
# MolPreProcess1<-function(data){
#   nactors<-dim(data)[1]
#   FMatrix<-matrix(NA,nrow=nactors*nactors,ncol=nactors*2)#Rows are edges, Columns are informants
#   new.data<-data #Create a new dataset to easily get the alter values
#   for(i in 1:nactors){
#     diag(data[i,,])<-9
#     diag(new.data[i,,])<-9
#     new.data[i,i,]<-NA
#     new.data[i,,i]<-NA
#     tmp.matrix<-matrix(NA,nrow=nactors,ncol=nactors) #This is for the ego reports on ego
#     tmp.matrix[i,]<-data[i,i,]
#     tmp.matrix[,i]<-data[i,,i]
#     FMatrix[,i*2-1]<-c(tmp.matrix) #This is ego's reports on ego
#     FMatrix[,i*2]<-c(new.data[i,,]) #This is ego's reports on alters
#   }
#   return(FMatrix)
# }
# #A Miserable Failure
# # MolData1<-lapply(data.list,MolPreProcess1)
# # MolModels1<-lapply(MolData1,molInf,sprior=c(.5,.5),suppress.perversion=FALSE)
# # CG.MolM1<-lapply(MolModels1,graph.maker.ch)
# # unlist(lapply(1:8,function(i){hdist(CG.MolM1[[i]],centralgraph(Mod.list[[i]]$net))}))

unlist(lapply(1:8,function(i){sum((apply(MolModels[[i]]$s,2,mean)>.5)==na.omit(c(centralgraph(Mod.list[[i]]$net))))}))

unlist(lapply(1:8,function(i){hdist(CG.MolM[[i]],centralgraph(Mod.list[[i]]$net))}))


unlist(lapply(1:8,function(i){hdist(t(centralgraph(Mod.list[[i]]$net)),centralgraph(Mod.list[[i]]$net))}))

FP.Error.Rates<-lapply(MolModels,function(x){apply(x$ep,2,mean)[2*(1:(x$m/2))-1]-apply(x$ep,2,mean)[2*(1:(x$m/2))]})
FN.Error.Rates<-lapply(MolModels,function(x){apply(x$em,2,mean)[2*(1:(x$m/2))-1]-apply(x$em,2,mean)[2*(1:(x$m/2))]})

FP.Error.Rates<-lapply(MolModels,function(x){apply(x$ep,2,mean)[2*(1:(x$m/2))]})
FN.Error.Rates<-lapply(MolModels,function(x){apply(x$em,2,mean)[2*(1:(x$m/2))]})




Exp.LAS<-function(Graph,FPR,FNR,Data){
  tmp.graph<-Graph
  tmp.graph[is.na(tmp.graph)]<-0
  #     FPR<-apply(Mod$ep,2,mean)
  #     FNR<-apply(Mod$em,2,mean)
  Int.Exp.Graph<-Graph*(outer(FNR,FNR,FUN="+")-FNR%*%t(FNR))+(1-Graph)*(FPR%*%t(FPR))
  diag(Int.Exp.Graph)<-0
  Union.Exp.Graph<-Graph*(FNR%*%t(FNR))+(1-Graph)*(outer(FPR,FPR,FUN="+")-FPR%*%t(FPR))
  diag(Union.Exp.Graph)<-0
  Exp.Int.Error<-sum(Int.Exp.Graph)
  Exp.Union.Error<-sum(Union.Exp.Graph)  
  Actual.Int.Error<-hdist(Graph,consensus(Data,method="LAS.intersection"))
  Actual.Union.Error<-hdist(Graph,consensus(Data,method="LAS.union"))
  return(list(Exp.Int.Error=Exp.Int.Error,Actual.Int.Error=Actual.Int.Error,Exp.Union.Error=Exp.Union.Error,Actual.Union.Error=Actual.Union.Error))
}

lapply(1:8,function(i){Exp.LAS(CG.MolM[[i]],FPR=FP.Error.Rates[[i]],FNR=FN.Error.Rates[[i]],Data=data.list[[i]])})



png(filename="C:/Users/francis/Desktop/LASComparison/figures/GlobalDiff.png",width=10,height=6,units="in",res=300)
par(font.axis = 2)
par(mfrow=c(3,3))
par(mar=c(2,1,2,1))
for(i in 1:8){
  # if(i %in% 3:7){
  #   plot(density(FN.Error.Rates[[i]]),main=bquote(bold(.(network.names[i])~ "Network ")),yaxt="n",xlab=c("Error Rate"),xlim=c(-1,1),col=rgb(0,0,1,.2))
  #   polygon(density(FN.Error.Rates[[i]]),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
  #   lines(density(FP.Error.Rates[[i]]),col=rgb(1,0,0,.2))
  #   polygon(density(FP.Error.Rates[[i]]),col=rgb(1,0,0,.2),border=rgb(1,0,0,.2))
  #   abline(v=mean(FP.Error.Rates[[i]]),col="red",lwd=2)
  #   abline(v=mean(FN.Error.Rates[[i]]),col="blue",lwd=2)
  # }else{
    plot(density(FP.Error.Rates[[i]]),main=bquote(bold(.(network.names[i])~ "Network ")),cex.main=1.5,yaxt="n",xlab=c("Error Rate"),xlim=c(-1,1),col=rgb(1,0,0,.2))
    polygon(density(FP.Error.Rates[[i]]),col=rgb(1,0,0,.2),border=rgb(1,0,0,.2))
    lines(density(FN.Error.Rates[[i]]),col=rgb(0,0,1,.2))
    polygon(density(FN.Error.Rates[[i]]),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
    abline(v=mean(FP.Error.Rates[[i]]),col="red",lwd=2)
    abline(v=mean(FN.Error.Rates[[i]]),col="blue",lwd=2)
#  }
}
plot.new()
par(font=2)
legend("topleft",c("False Positive Diff","False Negative Diff"),col=c(rgb(1,0,0,.2),rgb(0,0,1,.2)),lty=1,lwd=3,cex=1.5)
dev.off()


png(filename="C:/Users/francis/Desktop/LASComparison/figures/EgoErrorRates.png",width=10,height=6,units="in",res=300)
par(font.axis = 2)
par(mfrow=c(3,3))
par(mar=c(2,1,2,1))
for(i in 1:8){
  if(i==9){
    plot(density(FN.Error.Rates[[i]]),main=bquote(bold(.(network.names[i])~ "Network ")),yaxt="n",xlab=c("Error Rate"),xlim=c(0,1),col=rgb(1,0,0,.2))
    polygon(density(FN.Error.Rates[[i]]),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
    lines(density(FP.Error.Rates[[i]]),col=rgb(1,0,0,.2))
    polygon(density(FP.Error.Rates[[i]]),col=rgb(1,0,0,.2),border=rgb(1,0,0,.2))
    abline(v=mean(FP.Error.Rates[[i]]),col="red",lwd=2)
    abline(v=mean(FN.Error.Rates[[i]]),col="blue",lwd=2)
  }else{
    plot(density(FP.Error.Rates[[i]]),main=bquote(bold(.(network.names[i])~ "Network ")),yaxt="n",xlab=c("Error Rate"),xlim=c(0,1),col=rgb(1,0,0,.2))
    polygon(density(FP.Error.Rates[[i]]),col=rgb(1,0,0,.2),border=rgb(1,0,0,.2))
    lines(density(FN.Error.Rates[[i]]),col=rgb(0,0,1,.2))
    polygon(density(FN.Error.Rates[[i]]),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
    abline(v=mean(FP.Error.Rates[[i]]),col="red",lwd=2)
    abline(v=mean(FN.Error.Rates[[i]]),col="blue",lwd=2)
  }
}
plot.new()
par(font=2)
legend("topleft",c("False Positive","False Negative"),col=c(rgb(1,0,0,.2),rgb(0,0,1,.2)),lty=1,lwd=3,cex=1.5)
dev.off()


png(filename="C:/Users/francis/Desktop/LASComparison/figures/ProxyErrorRates.png",width=10,height=6,units="in",res=300)
par(font.axis = 2)
par(mfrow=c(3,3))
par(mar=c(2,1,2,1))
for(i in 1:8){
  # if(i %in% 3:7){
  #   plot(density(FN.Error.Rates[[i]]),main=bquote(bold(.(network.names[i])~ "Network ")),yaxt="n",xlab=c("Error Rate"),xlim=c(0,1),col=rgb(1,0,0,.2))
  #   polygon(density(FN.Error.Rates[[i]]),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
  #   lines(density(FP.Error.Rates[[i]]),col=rgb(1,0,0,.2))
  #   polygon(density(FP.Error.Rates[[i]]),col=rgb(1,0,0,.2),border=rgb(1,0,0,.2))
  #   abline(v=mean(FP.Error.Rates[[i]]),col="red",lwd=2)
  #   abline(v=mean(FN.Error.Rates[[i]]),col="blue",lwd=2)
  # }else{
    plot(density(FP.Error.Rates[[i]]),main=bquote(bold(.(network.names[i])~ "Network ")),yaxt="n",xlab=c("Error Rate"),xlim=c(0,1),col=rgb(1,0,0,.2))
    polygon(density(FP.Error.Rates[[i]]),col=rgb(1,0,0,.2),border=rgb(1,0,0,.2))
    lines(density(FN.Error.Rates[[i]]),col=rgb(0,0,1,.2))
    polygon(density(FN.Error.Rates[[i]]),col=rgb(0,0,1,.2),border=rgb(0,0,1,.2))
    abline(v=mean(FP.Error.Rates[[i]]),col="red",lwd=2)
    abline(v=mean(FN.Error.Rates[[i]]),col="blue",lwd=2)
#  }
}
plot.new()
par(font=2)
legend("topleft",c("False Positive","False Negative"),col=c(rgb(1,0,0,.2),rgb(0,0,1,.2)),lty=1,lwd=3,cex=1.5)
dev.off()



#Varies across network.size
#FP-FN
#THe sum of the the two informants, 1,11 is not that strong a prior, you start putting a lot of weight on the probability of really high error rates.
#Priors are really flat and turn on perversity checking and exclusion
#Its going ot make the priors uniform on the simplex
#Dramatically overestimate the error rates
#FN do dominate when rating third party, but they don't dominate as systematicallly'


#apply(MolModels$s,2,mean)>.5==centralgraph()

