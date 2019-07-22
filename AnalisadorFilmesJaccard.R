ObtainDataFrame<-function(){
filmes=list.files(recursive=TRUE,pattern = "*.pdf")
aux=combn(filmes,2)
source("ComparacaoDestringsJaccard.R")
vec=c()
for(i in 1:ncol(aux))
  vec[i]=ProcessoCompletoJaccard(aux[,i][1],aux[,i][2],"en")
df=data.frame(aux[1,],aux[2,],vec)
#df2=data.frame(unique(df[,1]),unique(df[,1]),1)
df2=data.frame(filmes,filmes,1)
df3=data.frame(aux[2,],aux[1,],vec)
names(df)=names(df2)=names(df3)=c('Filme1','Filme2','Valor')
df=rbind(df,df2,df3)
return(df)
}

ObtainMatrix<-function(){
  filmes=list.files(recursive=TRUE,pattern = "*.pdf")
  aux=combn(filmes,2)
  source("ComparacaoDestringsJaccard.R")
  vec=c()
  for(i in 1:ncol(aux))
    vec[i]=ProcessoCompletoJaccard(aux[,i][1],aux[,i][2],"en")
  aux=combn(1:length(filmes),2)
  df=data.frame(aux[1,],aux[2,],vec)
  #df2=data.frame(unique(df[,1]),unique(df[,1]),1)
  df2=data.frame(1:length(filmes),1:length(filmes),1)
  df3=data.frame(aux[2,],aux[1,],vec)
  names(df)=names(df2)=names(df3)=c('Filme1','Filme2','Valor')
  df=rbind(df,df2,df3)
  m=matrix(1,length(filmes),length(filmes))
  for(i in 1:nrow(df))
    m[df[i,1],df[i,2]]=df[i,3]
  rownames(m)=colnames(m)=filmes
  return(1-m)
}

OptimumClustering<-function(matrix,kmin,kmax){
  vec=c()
  require(cluster)
  for(i in kmin:kmax){
    clustercriado=kmeans(matrix,i)
    vec[i-kmin+1]=mean(silhouette(clustercriado$cluster,matrix)[,3])
  }
  Otimo=min(which(vec==max(vec)) ) + kmin
  return(Otimo)
}

Clustering<-function(Matrix,kmin,kmax){
  clusters<-OptimumClustering(Matrix,kmin,kmax)
  Separador=kmeans(Matrix,clusters)
  clusplot(Matrix,Separador$cluster,labels=2,color=TRUE,shade=TRUE,lines=0)
  
}
GenerateVizualization<-function(df){
  require(plotly)
  p1=ggplot(data = df, aes(x=Filme1, y=Filme2, fill=Valor)) + geom_tile() + labs(x="",y=""  ) + theme(axis.title.x=element_blank(),
                                                                                                      axis.text.x=element_blank(),
                                                                                                      axis.ticks.x=element_blank() ,axis.title.y=element_blank(),
                                                                                                      axis.text.y=element_blank(),
                                                                                                      axis.ticks.y=element_blank() )    
  p1=p1+scale_fill_gradientn(colours = rainbow(20)) 
  ggplotly(p1)
  
}
#ggplotly(p1)
