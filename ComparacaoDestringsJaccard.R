Separação=function(Palavra){
Palavra=strsplit(Palavra,split="")
Palavra=unlist(Palavra)
x5=list()
k1=2
if(length(Palavra)>=k1+1){
 for(i in 1:(length(Palavra)-k1)){
 x5[[i]]=Palavra[i:(i+k1)]
 }
}
else
	x5[[1]]=Palavra
x6=list()
for(i in 1:length(x5))
x6[[i]]=Reduce(paste,x5[[i]])
return(unlist(x6))
}



Jaccard<-function(Separacao1,Separacao2){

inter1=sum(Separacao1 %in% Separacao2)/length(Separacao1)
inter2=sum(Separacao2 %in% Separacao1)/length(Separacao2)
return((inter1+inter2)/2)

}

Jaccard2<-function(Separacao1,Separacao2){
Separacao1=tolower(Separacao1)
Separacao2=tolower(Separacao2)
intersecao=length( intersect(Separacao1,Separacao2) )
uniao=length( union(Separacao1,Separacao2) )
return(intersecao/uniao)

}


ProcessoCompletoJaccard<-function(Texto1,Texto2,lingua="en"){
source("~/scripts_R/Analise_texto.R")
Texto1=Palavras(leitura(Texto1),lingua)
Texto2=Palavras(leitura(Texto2),lingua)
Texto2=tolower(Texto2)
Texto1=tolower(Texto1)

Sep1=unlist(lapply(Texto1,Separação))
Sep2=unlist(lapply(Texto2,Separação))
return(Jaccard2(Sep1,Sep2))
}
