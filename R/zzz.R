#  File networkDynamic/R/zzz.R
#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2012 the statnet development team
######################################################################
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
    packageStartupMessage(paste("Copyright (c) 2012,2009 The Stanet Project (statnet.org), 
      Ayn Leslie-Cook, University of Washington
      Zack Almquist, University of California-Irvine
      Pavel N. Krivitsky, Penn State University
      Skye Bender-deMoll
      David R. Hunter, Penn State University
      Martina Morris, University of Washington
      Carter T. Butts, University of California-Irvine\n",sep=""))
    packageStartupMessage('For citation information, type citation("networkDynamic").')
    packageStartupMessage('Type help("networkDynamic-package") to get started.')
    packageStartupMessage('NOTE: This is an initial public release of an EXPERIMENTAL PACKAGE which contains known bugs.')
}
