ToTheGraph_timeShiftmax2 <-
function (network,name){
	row<-dim(network)[1];
  	col<-dim(network)[2];
  	title2<-name;
  	graph<-array(0, dim=c(1000,4));
	w<-1;
  	for(x in 1:row){
    	for(y in 1:col){
    		if(network[x,y]==1){
    			if(is.null(rownames(network))==TRUE){
    				graph[w,]<-c(x,"->",y,";");					}else{
        			graph[w,]<-c(rownames(network)[x],"->",colnames(network)[y],";");
        		}
        		w<-w+1;		
        	}
      	}
    }
	graph<-(graph[(1:(w-1)),]);
	row<-dim(graph)[1];
	color = array(0, dim=1);
	color[1]<-'[color="black"];';
	riga2<-"";
	for(i in 1:row){
		riga<-"";
		if(graph[i,1] != 0){
			for(j in 1:3){
				riga<-paste(riga,graph[i,j]);
			}			
			riga2<-paste(riga2,paste(riga,"",color[1],"\n"));
		}
	}
	riga<-"";
	title<-paste("digraph net{","\n");
	end<-"}";
	cat(title,riga2,end,file=paste("net",title2,".dot",sep=""),append=TRUE);
	riga<-"";
return('ok');	
}

