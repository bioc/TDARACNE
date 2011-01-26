MItimeIcE2 <-
function (z,N,delta,norm,threshold,ksd,IcE){
	row<-0;
	row <-dim(z)[1];
	col<-0;
	col<-dim(z)[2];
  	if(norm == 1){
    	z<- PercentileC(z,N);
  	}else{
    	z<-RangeRank2(z,N);
  	} 
	Mean<-threshold[1];
	mysd<-threshold[2];
	thresh<-Mean+ksd*mysd;
 	MI = array(0, dim=c(row,row,delta));
 	k<-0;
	jump<-saveTime(z,delta);
	for(d in 1:delta){
		for(i in 1:row) {
			for (j in 1:row) {
				if(i!=j){
					if(IcE[i] <= IcE[j] && IcE[i] != 0){ #IcE è un vettore di zeri fino all'individuazione dell'initial change expression
            			if(jump[i] == 1 && jump[j] == 1){
            				MI[i,j,d]<-CalcMI_time2(z[i,],z[j,],d);
       					}
             		}
        		}
				if (MI[i,j,d] < thresh){
					MI[i,j,d] <-0;
				}		
			}
		}
	}
return(MI);
}

