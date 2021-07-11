library(cluster)
library(fpc)
library(foreign)
library(factoextra)
library(gridExtra)
# Function to test the numbers of clusters to find the suggest cluster number

clustTest = function(respondentData,print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(scale){ respondentData = scale(respondentData);}
  set.seed(seed);
  wss <- (nrow(respondentData)-1)*sum(apply(respondentData,2,var))
  for (i in 2:maxClusts) wss[i] <- sum(kmeans(respondentData,centers=i,nstart=nstart,iter.max=iter.max)$withinss)
  gpw = fviz_nbclust(respondentData,kmeans,method="wss",iter.max=iter.max,nstart=nstart,k.max=maxClusts)
  pm1 = pamk(respondentData,scaling=TRUE)
  gps = fviz_nbclust(respondentData,kmeans,method="silhouette",iter.max=iter.max,nstart=nstart,k.max=maxClusts) 
  if(print){
    grid.arrange(gpw,gps, nrow = 1)
  }
  list(wss=wss,pm1=pm1$nc,gpw=gpw,gps=gps)
}

# cluster analysis function with cluster output and visualization
runClusts = function(respondentData,nClusts,print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(length(nClusts)>4){
    warning("Using only first 4 elements of nClusts.")
  }
  kms=list(); ps=list();
  for(i in 1:length(nClusts)){
    kms[[i]] = kmeans(respondentData,nClusts[i],iter.max = iter.max, nstart=nstart)
    ps[[i]] = fviz_cluster(kms[[i]], geom = "point", data = respondentData) + ggtitle(paste("k =",nClusts[i]))
    
  }
  if(print){
    tmp = marrangeGrob(ps, nrow = 2,ncol=2)
    print(tmp)
  }
  list(kms=kms,ps=ps)
}



#The mean of attribute inside the data and visualization

plotClust = function(km,respondentData,
                     discPlot=FALSE,standardize=TRUE,margins = c(7,4,4,2)){
  nc = length(km$size)
  
  percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
  pie(km$size,labels=percsize,col=1:nc)
  
  gg = fviz_cluster(km, geom = "point", data = respondentData) + ggtitle(paste("k =",nc))
  print(gg)
  
  if(discPlot){
    plotcluster(respondentData, km$cluster,col=km$cluster);
  }
  if(standardize){
    kmc = (km$centers-rep(colMeans(respondentData),each=nc))/rep(apply(respondentData,2,sd),each=nc)
    rng = range(kmc)
    dist = rng[2]-rng[1]
    locs = kmc+.05*dist*ifelse(kmc>0,1,-1)
    par(mar=margins)
    bm = barplot(kmc,col=1:nc,beside=TRUE,las=2,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
    text(bm,locs,formatC(kmc,format="f",digits=1))
  } else {
    rng = range(km$centers)
    dist = rng[2]-rng[1]
    locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
    bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
    text(bm,locs,formatC(km$centers,format="f",digits=1))
  }
  vs = data.table(Segment = 1:nrow(km$centers),km$centers,Size = km$size/sum(km$size))
  vs[order(-Size),]
}

# Writing function to obtain the Market share
market_simulation = function(a = c(1:16),data=market,highrateisbetter=T){
  if(!highrateisbetter){market = -market}
  var = length(data$ID)
  data = data[,-1]
  test = matrix(0,nrow=var,ncol=length(data))
  max_value = apply(data[,a],1,max)
  
  for (i in 1:var){
    
    test[i,which(data[i,]==max_value[i]) ]= 1/sum(data[i,a]==max_value[i])
    
  }
  test[,-(a)]=NA
  
  total = apply( test,2,sum)
  d = as.data.frame(matrix(total/var,nrow=1))
  names(d)=1:length(data)
  return(d)
}

comparison = function(sec=scen,data){
  
  total = market_simulation(sec[[1]],market)
  if(length(sec)>1){
    for (i in 2: length(sec)){
      new = market_simulation(sec[[i]],market)
      total = rbind(total,new)
    }}
  return(total)
}


#Function for profit calculation
profit = function(data = test,origin=c(5,13),competitor=7,table = profit_table,unit=4000,fixed_cost=20000,switch=7000){
  profit = c()
  comp=c()
  for(i in 1:nrow(data)){
    margin =table$margin*unit*data[i,]
    profit[i] = sum(margin[-competitor],na.rm = T)-(sum(!is.na(data[i,]))-length(competitor))*fixed_cost-switch*sum(!which(!is.na(data[i,])) %in% c(origin,competitor))
    comp[i]= sum(as.numeric(margin[competitor]-fixed_cost))
  }
  return(data.frame(company_profit = profit, comp_profit = comp))
}
