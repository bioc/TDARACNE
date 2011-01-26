saveTime <-
function (newz,delta){
	row <-dim(newz)[1];
	bandx=0;
	bandy=0;
	jump<-array(0,dim=row);
	for(i in 1:row){
		x<-newz[i,];
		z<-x[order(x)];
		rowx<-length(x);
		k<-1;
		for(q in 1:(rowx-1)) {
	   	if(z[q]!=z[q+1]){
	     		k<-k+1;
    		}
		}
		if(k > (1+delta)){
   		jump[i]<-1;
   	}
   }
return(jump)
}

