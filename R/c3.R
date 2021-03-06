#This file belongs to
#c3net: C3NET, <https://r-forge.r-project.org/projects/c3net/>
#This R package allows inferring regulatory networks from expression data using C3NET.
#The inferred network consists of only direct physical interactions.
## Copyright (C) January 2011 Gokmen Altay <altayscience@gmail.com>
## This program is a free software for only academic useage but not for commercial useage; you can redistribute it and/or
## modify it under the terms of the GNU GENERAL PUBLIC LICENSE
## either version 3 of the License, or any later version.
##
## This program is distributed WITHOUT ANY WARRANTY; 
## You can get a copy of the GNU GENERAL PUBLIC LICENSE
## from
## http://www.gnu.org/licenses/gpl.html
## See the licence information for the dependent package from
## igraph package itself.


c3 <- function(mim, n, sym=TRUE) # symetric MI matrix is input
{
diag(mim) <- 0
c3mim <- mim
c3mim[,] <- 0

numgene <- ncol(mim)

if(sym==TRUE){
for(i in 1:numgene)
{
if(sum(mim[i,]) != 0)
{
# set m number of neighbours
	sorted = sort(mim[i,], TRUE)
	for(j in 1:n)
	{
		ind <- which(mim[i,]==sorted[j])
		maxVal = max(c3mim[i,ind], c3mim[ind, i], mim[i,ind])
		c3mim[i,ind] <- maxVal
		c3mim[ind,i] <- maxVal
	}
 
}#endif
} 
}

if(sym!=TRUE){
for(i in 1:numgene)
{
if(sum(mim[i,]) != 0)
{
# set m number of neghbours
sorted = sort(mim[i,], TRUE)
	for(j in 1:n)
	{
		ind <- which(mim[i,]==sorted[j])
		c3mim[i,ind] <- mim[i,ind]
	}

# end of edit
 
}
} 
}

c3mim
}


