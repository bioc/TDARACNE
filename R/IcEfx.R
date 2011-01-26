IcEfx <-
function (z,likehood,logarit){
# logarit = 0 if z is log2	
  row<-0;
	row <-dim(z)[1];
	col<-0;
	col<-dim(z)[2];
	r<-0;
	change <-0;
  if(logarit == 0){
	 likehood<-log2(likehood);
	 change <- 1;
	 }
	FCmatrix<- array(0, dim=c(row,col));
	IcE <- array(0, dim=row);
# order any row ascendent
  for(i in 1:row) {
		r <- z[i,];
		 go<- 0;
		 add<-0;
   a <-r[1];
# check if the k-th element differs of likehood
		for(k in 2:col){
		  if(go == 0){
		    if(change == 1){
		      if(abs(r[k]-r[1]) > likehood){
            FCmatrix[i,k]<-k;
            IcE[i]<-k;
            contatore<-1;
            go<-1;
            }
          }else{
            if (a != 0){
                if((r[k]/a) > likehood || (r[k]/a) < (1/likehood)){
                FCmatrix[i,k]<-k;
                IcE[i]<-k;
                go<-1;
                }
              }else{
                if(r[k] != 0){
                a<-r[k];
                }
              }  
          }
      }
    }
  }
  print("IcE done")
return(IcE)
}

