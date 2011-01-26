plotRgraphviz <- function(Influence) {
	library("Rgraphviz")
	am.graph<-new("graphAM", adjMat=Influence, edgemode="directed")
	plot(am.graph, attrs = list(node = list(fillcolor = "lightblue"), edge = list(arrowsize=0.5)))
	}