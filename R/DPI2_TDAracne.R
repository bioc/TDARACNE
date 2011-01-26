DPI2_TDAracne <-
function (MItab,tolerance){
	row <-dim(MItab)[1];
	Interaction<- array(0, dim=c(row,row));
	MIinteraction<-array(0, dim=c(row,row));
	for(i in 1:row){
		for (j in 1:row) {
			if(MItab[i,j] !=0){
				#this "if" delete AvsB feedback
				if(MItab[i,j] >= MItab[j,i]){
					MIinteraction[i,j]<-MItab[i,j];
# evaluate DPI using 15% tolerance to save loops(so we multiplicate "min" for 0.85)
					for(k in 1:row){
          			if(k != i && k!= j){
					     if(MItab[i,j] <=min(max(MItab[i,k],MItab[k,i]),max(MItab[k,j],MItab[j,k]))*tolerance){
						  		MIinteraction[i,j]<-0;
					     }
					 	}
				  	}
				}
			}
		}
	}
	for(i in 1:row) {
		for (j in 1:row) {
		  if(MIinteraction[i,j] != 0){
				Interaction[i,j]<-1;
		  }
    	}
	}
return(Interaction)
}

