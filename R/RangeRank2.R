RangeRank2 <-
function (z,N){
# order row, set max and min, set bins range as (max-min)/N, put value k/N in bin k, reorder row as initial
	row <-dim(z)[1];
	col<-dim(z)[2];
	for(i in 1:row) {
		r <- z[i,];
		w<-r[order(r)];
		maxx<-max(w);
		minn<-min(w);
		bin<-(maxx-minn)/N;
		k<-1;
		j<-1;
		for(k in 1:N){
			while(w[j] <= (minn+k*bin) && j <= col){
				w[j] <-k/N;
				j<-j+1;
			}
		}
		r[order(r)]<-w;
		z[i,]<-r;
    }
return(z)
}

