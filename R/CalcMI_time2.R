CalcMI_time2 <-
function (l,t,delta) {
	library(GenKern)
	x <- l[1:(length(l)-delta)];
	y <- t[(1+delta):length(t)];
	xgs<-50;
	ygs<-50;
	if (length(x) !=length(y)) {
		print("incorrect input dim: %d %d");
		return(-1); 
	}
#check if auto-bandwidth with dpik is possible otherwise band=0.2
	bandx=0;
	bandy=0;
	bandx<- tryCatch(dpik(x),  error=function(err) 0.2);
#	print(c("bandx:",bandx));
	bandy<- tryCatch(dpik(y), error=function(err) 0.2);
#	print(c("bandy:",bandy)); 
# kernel functions to obtain densities 
		Px<-KernSec(x,xgridsize=xgs,xbandwidth=bandx);
#		print(c("PX:",Px));
		Py<-KernSec(y,xgridsize=ygs,xbandwidth=bandy);
		Pxy<-KernSur(x,y,xgridsize=xgs,ygridsize=ygs,xbandwidth=bandx,ybandwidth=bandy);
#Normalization of densities
		s<-sum(Px$yden);
		Px$yden<-Px$yden/s;
		s<-sum(Py$yden);
		Py$yden<-Py$yden/s;
		s<-sum(Pxy$zden);
		Pxy$zden<-Pxy$zden/s;	
#Compute MI
		MI<-0.0;
		for(i in 1:xgs) {
			for (j in 1:ygs) {
				tmp <- Pxy$zden[i,j]*log2(Pxy$zden[i,j]/(Px$yden[i]*Py$yden[j]));
				if (tmp != "NaN"){
					MI <-MI+tmp;
				}
			}
		}
return(MI);
}

