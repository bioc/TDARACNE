TDARACNE <-
function (eSet,N,delta=3,likehood=1.2,norm=2,logarithm=1,thresh=0,ksd=1,tolerance=0.15,plot=FALSE,dot=FALSE,name="youHaveForgottenIt",adj=FALSE){

	library(GenKern)
	library(Biobase) 
# eSet is the ExpressionSet object
# N is respectively the number of bins in percentile normalization or in rank normalization
# delta is the maximum time delay allowed to infer connections
# likehood is the fold change used as threshold to state the initial change expression (IcE) 
# if you want column percentile normalization put norm == 1;
# if you want Rank normalization put norm == 2;
# if z is log put logarithm == 0;
# if you don't have threshold put 0 in thresh;
# ksd is the standard deviation multiplier;
# tolerance is the DPI tolerance;
# plot must be TRUE to obtain automatically the graph
# dot must be TRUE to obtain a .dot file 
# name must be written with quotation marks(like this:'examplename') and is the name of the .dot file produced;
# adj must be TRUE to obtain an adjacent matrix
	z<-exprs(eSet)

# take gene names if there are;
	Namesz<-rownames(z);
# initial change expression;
  	IcE<-IcEfx(z,likehood,logarithm);
# calculate threshold if there isn't;
	if(thresh[1] == 0){
    	thresh<-MItimeThreshperm2(z,N,delta,norm);
        print.default('1')
    }
# calculate Mutual information
	MItab<-MItimeIcE2(z,N,delta,norm,thresh,ksd,IcE);
	print.default('2')
	tolerance<-(1-tolerance);
# make MI-DPI, find Influence, make Influence-DPI and give adj
	Influence<-DPI_TDAracne(MItab,delta,tolerance);
	print.default('3')
# put gene names in the adj;
	rownames(Influence)<-Namesz;
	colnames(Influence)<-Namesz;
# make the graph;
	if(plot==TRUE){
		plotRgraphviz(Influence)
	}
	if(dot==TRUE){
		ToTheGraph_timeShiftmax2(Influence,name);
	}
	print.default('4')
	if(adj==TRUE){
		print.default('5')
		return(Influence)
	}else{
		nel.graph <-as(Influence, Class="graphNEL")
		print.default('5')
		return(nel.graph)
	}
}

