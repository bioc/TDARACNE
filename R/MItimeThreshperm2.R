MItimeThreshperm2 <- function (z, N, delta, norm) 
{
	# delta is not used in the end. You obtain a threshold for d=1 
    row <- 0
    row <- dim(z)[1]
    col <- 0
    col <- dim(z)[2]
    r <- 0
    p <- 1
    sdev <- 0
    threshold <- array(0, dim = delta)
    sdev <- array(0, dim = delta)
    SUMsdev <- array(0, dim = delta)
    thresh <- array(0, dim = c(delta, 2))
    Ngenes <- row
    if (is.numeric(z)) {
        print("z is ok")
    }
    else {
        print("z in not numeric")
    }
    if (Ngenes > 100) {
        Ngenes <- 100
#        print("more than 100 genes in the matrix")
    }
    cycles <- 10
    k <- 1
    newz = array(0, dim = c(Ngenes, col))
    divAll <- array(0, dim = c(delta))
    for (w in 1:cycles) {
        for (i in 1:Ngenes) {
            if (dim(z)[2] > 100) {
#            	print("100 time points")
                newz <- newz[, 1:100]
                k <- sample(row)[1]
                r <- z[k, ]
                bootstrap(r)
                newz[i, ] <- r[1:100]
            }
            else {
                k <- sample(row)[1]
                r <- z[k, ]
                bootstrap(r)
                newz[i, ] <- r
            }
        }
        r <- 0
        if (is.numeric(newz)) {
            print("shuffled matrix is ok")
        }
        if (norm == 1) {
            newz <- PercentileC(newz, N)
        }
        else {
            newz <- RangeRank2(newz, N)
        }
        if (is.numeric(newz)) {
            print("shuffled matrix after discretization is ok")
        }
        jump <- saveTime(newz, delta)

        shuffleMIv <- array(0, dim = c(delta))
        MIv <- 0
        shuffleMI = array(0, dim = c((row * row), delta))
        div <- array(0, dim = delta)
        d <- 1
        p <- 1
        Zero <- 0
        for (i in 1:Ngenes) {
            for (j in 1:Ngenes) {
                if (i != j) {
                  if (jump[i] == 1 && jump[j] == 1) {
                    MIv <- CalcMI_time2(newz[i, ], newz[j, ], 
                      d)
                  }
                  else {
                    Zero <- Zero + 1
                  }
                  if (MIv >= 0) {
                    shuffleMI[p, d] <- MIv
                    shuffleMIv[d] <- shuffleMIv[d] + MIv
                    p <- p + 1
                  }
                }
            }
        }
        print(Zero)
        div[d] <- (p - 1)
        shuffleMIv[d] <- shuffleMIv[d]/div[d]
        TakeIt <- 0
        if (shuffleMIv[d] != "Inf") {
            threshold[d] <- threshold[d] + shuffleMIv[d]
            sdev[d] <- sd(shuffleMI[, d])
            SUMsdev[d] <- SUMsdev[d] + sdev[d]
            divAll[d] <- divAll[d] + 1
            print(c("cycle:", w))
        }
        else {
            print(c("Inf in ", "cycle:", w))
            TakeIt <- 1
        }
    }
    if (TakeIt != 1) {
        if (divAll[d] != 0) {
            threshold[d] <- (threshold[d]/divAll[d])
            SUMsdev[d] <- (SUMsdev[d]/divAll[d])
            thresh[d, 1] <- threshold[d]
            thresh[d, 2] <- SUMsdev[d]
        }
        else {
            thresh[d, 1] <- 0
            thresh[d, 2] <- 0
        }
    }
    else {
        thresh[d, 1] <- -1
    }
    return(thresh[1, ])
}