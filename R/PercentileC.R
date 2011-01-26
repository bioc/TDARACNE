PercentileC <-
function (z,N){
# order row, make coloumn quantile discretization, put the value k/N in the k-th bin for all value under k-th quantile.	
	row<-0;
	row <-dim(z)[1];
	col<-0;
	col<-dim(z)[2];
	r<-0;
	  for(i in 1:col) {
	  	r <- z[,i];
		w<-r[order(r)];
		quan<-quantile(w,probs=c((1:N)/N),names = FALSE);
		k<-1;
		for(p in 1:row){
			while(w[p] > quan[k]){
				k<-k+1;
				}
			w[p]<-(k/N);
			p<-p+1;
		}
		r[order(r)]<-w;
		z[,i]<-r;
	}
	return(z)
}

