DPI_TDAracne <-
function (MItab,delta,tolerance){
  row <-dim(MItab)[1];
  MIinteraction<-array(0, dim=c(row,row,delta));
  MIinteractionMax<- array(0, dim=c(row,row));
  for(d in 1:delta){
	 for(i in 1:row){
		for (j in 1:row) {
			if(MItab[i,j,d] !=0){
				if(MItab[i,j,d] >= MItab[j,i,d]){     #make impossible A to B and B to A;
				  MIinteraction[i,j,d]<-MItab[i,j,d];
# evaluate DPI using 15% tollerance to save loops(so we multiplicate "min" for 0.85)
					for(k in 1:row){
            		if(k != i && k!= j){      
	              	if(MItab[i,j,d] <=min(max(MItab[i,k,d],MItab[k,i,d]),max(MItab[k,j,d],MItab[j,k,d]))*tolerance){
                  		MIinteraction[i,j,d]<-0;
					      }
					    }
				   }
				}
      	}
		}
	 }
  }
	for(i in 1:row) {
		for (j in 1:row) {
			for(d in 1:delta){
				if(MIinteraction[i,j,d] !=0){
          maxx<-max(MIinteraction[i,j,]);
				  MIinteractionMax[i,j]<-maxx;
				}
			}
		}
	}
Interaction<-DPI2_TDAracne(MIinteractionMax,tolerance);
return(Interaction);
}

