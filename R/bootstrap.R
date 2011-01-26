bootstrap <-
function (TS){
row<-length(TS);
SumBlength<-0;
BSTS<-0;
TS<-c(TS,TS);
while(SumBlength < row){
  p<-runif(1:1000,min=0.1, max=1)[1];
  Blength<- rgeom(1:100,p)[1];
  if(Blength != 0 && Blength != "NaN"){
    Blength<-as.integer(Blength);
    BiV<-sample(1:row)[1];
    bootTS<- TS[BiV:(BiV+(Blength-1))];
    BSTS<-c(BSTS,bootTS);
    SumBlength<-SumBlength+Blength;
    }
  }
BSTS<-BSTS[-1];
return(BSTS[1:row])
}

