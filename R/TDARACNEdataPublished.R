TDARACNEdataPublished <-
function () {

data(dataYeast, package="TDARACNE")
data(dataSOSmean)
data(dataIRMAon)
data(threshIRMAon)
data(threshSOSmean)
data(threshYeast)

TDARACNE(dataYeast,11,delta=3,likehood=1.2,norm=1,logarithm=0,thresh=threshYeast,ksd=1,tolerance=0,dot=TRUE,name="netYeast");
print("1");
TDARACNE(dataSOSmean,15,delta=3,likehood=1.4,norm=2,logarithm=1,thresh=threshSOSmean,ksd=0,tolerance=0,dot=TRUE,name="netSOS");
print("2");
TDARACNE(dataIRMAon,11,delta=3,likehood=1.2,norm=2,logarithm=1,thresh=threshIRMAon,ksd=0,tolerance=0.15,dot=TRUE,name="netIRMAon");
print("3");
return("see in your working directory for .dot files")
}

