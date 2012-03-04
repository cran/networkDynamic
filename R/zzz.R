######################################################################
#
# zzz.R
#
# Written by Carter T. Butts <buttsc@uci.edu>.
#
# Last Modified 3/16/09
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/networkDynamic package
#
# .onLoad is run when the package is loaded with library(networkDynamic)
#
######################################################################

.onLoad<- function(lib, pkg){
  # load c library code
	#library.dynam("networkDynamic", package=pkgname, lib.loc=libname)
     dscr <- utils::packageDescription('networkDynamic')
     packageStartupMessage("\n")
     packageStartupMessage(paste('Package ',dscr$Package,': ',dscr$Title,"\n",
               "Version ",dscr$Version,
               " created on ", dscr$Date ,".\n", sep=""))
    packageStartupMessage(paste("Copyright (c) 2012, The Stanet Project (statnet.org), \n (c) 2009, Carter T. Butts, University of California-Irvine\n",sep=""))
    packageStartupMessage('For citation information, type citation("networkDynamic").')
    packageStartupMessage('Type help("networkDynamic-package") to get started.')
    packageStartupMessage('NOTE: This is an initial public release of an EXPERIMENTAL PACKAGE which contains known bugs.')
}
