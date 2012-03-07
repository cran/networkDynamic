#  File networkDynamic/R/utilities.R
#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2012 the statnet development team

################################################
# utilities.R
# Author: Zack W. Almquist, Pavel 
#
# Includes:
#		     c.network #overloaded c operator for network objects
#
#	misc helper functions not for general use
#
################################################





################
### start networkDynamic-> other formats
################
as.data.frame.networkDynamic<-function(x,row.names = NULL, optional = FALSE, start=NULL, end=NULL, ...){
  if(is.null(start) && !is.null(attr(x,"start"))) start <- attr(x,"start")
  if(is.null(end) && !is.null(attr(x,"end"))) end <- attr(x,"end")
  
  tm<-lapply(x$mel,function(y){
    if(is.null(y)) NULL else{
      active<-y$atl$active
      ac<-matrix(rep(cbind(y$outl,y$inl),nrow(active)),ncol=2,byrow=TRUE)
      cbind(active,ac)
    }
  })
  out <- do.call(rbind,tm)
  colnames(out)<-c("start","end","tail","head")
  out<-data.frame(out)
  
  out$left.censored <- out$start==-Inf
  out$right.censored <- out$end==Inf

  if(!is.null(start)) out$start[out$left.censored] <- start
  
  if(!is.null(end)) out$end[out$right.censored] <- end
  
  out$duration <- out$end-out$start  
  out
}
################
### end networkDynamic-> other formats
################


print.networkDynamic <- function(x, ...){
  times <- sort(unique(c(lapply(x$mel, function(e) e$atl$active),recursive=TRUE)))
  cat("networkDynamic with", length(times), "distinct change times:\n")
  print(times)
  NextMethod("print")
}


##############
### as.networkDynamic
### converts various objects to networkDynamic
##############

is.networkDynamic <- function(x){
  "networkDynamic" %in% class(x)
}

as.networkDynamic <- function(object,...){
  UseMethod("as.networkDynamic")
}

as.networkDynamic.networkDynamic <- function(object,...){
  return(object)
}

as.networkDynamic.network <- function(object, spells=NULL, toggles=NULL, start=min(toggles[,1])-1, end=max(toggles[,1]),...){
  if(is.null(spells)){
    if(is.null(toggles)){
      stop("Either spell list or toggle list must be given.")
    }
    toggles <- as.data.frame(toggles)
    spells <- duration.matrix(object, toggles, start, end)
  }else spells <- as.data.frame(spells)

  newedges <- as.matrix(spells[,3:4])
  if(!is.directed(object)) newedges <- cbind(pmin(newedges[,1],newedges[,2]),pmax(newedges[,1],newedges[,2]))
  newedges <- unique(newedges)
  nw <- network.copy(object)
  nw[,]<-0
  nw <- add.edges(nw,newedges[,1],newedges[,2])
  men<-mapen(nw,spells[,3:4])
  #colnames(spells)<-c("start","end","tail","head","left.censored","right.censored")
  x<-cbind(men,spells)
  out <- networkDynamicInternal(nw,x)
  set.nD.class(out)
}


as.networkDynamic.data.frame <- function(object,nodeInfo=NULL, n=NULL, directed = TRUE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = FALSE,...){
  spellInfo <- as.data.frame(object)
  if(is.null(nodeInfo)){
    if(is.null(n)){
      nodeIDs  <- sort(unique(c(spellInfo[,3:4],recursive=TRUE)))
      n <- length(nodeIDs)
    }else{
      nodeIDs <- seq_len(n)
    }
    net <- network.initialize(n,directed=directed,hyper=hyper,loops=loops,multiple=multiple,bipartite=bipartite)
    net %v% "vertex.names" <- nodeIDs
  }else{
    nodeInfo <- as.data.frame(nodeInfo)
    net<-networkDynamicInterV(nodeInfo,directed=directed,hyper=hyper,loops=loops,multiple=multiple,bipartite=bipartite)
  }
        colnames(spellInfo) <- c("start","end","tail","head","left.censored","right.censored")[1:ncol(spellInfo)]
        out <- as.networkDynamic(net, spellInfo)
	
	if(is.network(nodeInfo)){
	if(length(unique(sapply(spellInfo,network.size)))==1){
		out<-addAttributes(out,nodeInfo)
	}
	}
	## Check for vertex dynamics
	## if no vertex dynamics add attributes
	## addAttributes(out,)
	###
  set.nD.class(out)
}

##############
### ndConverter.network.list
### converts a list of network objects to networkDynamic and back
##############
as.networkDynamic.network.list<-as.networkDynamic.list<-function(object,...){
	warning("Keeps only attributes on first network! Temporally extended attribute (TEA) methods coming soon.\n")
	if(is.network(object[[1]])){
		out<-listSpell(object,buildNet(object))
		## Check if vertex length is same if so copy attributes
		if(length(unique(sapply(object,function(y){if(!is.network(object)){return(NULL)}
			network.size(y)})))==1){out<-addAttributes(out,object[[1]])}
		return(set.nD.class(out))
		}
	
}




###########################################################
### Helper functions, internal functions
### These do not need to exported if a specific NAMESPACE file is
### provided
###########################################################

#### copies attributes over
addAttributes<-function(x,net){
	x%n%"directed"<-net%n%"directed"
	m<-match(x%v%"vertex.names",net%v%"vertex.names")
	attr<-list.vertex.attributes(net)
	for(i in 1:length(attr)){
		out<-set.vertex.attribute(x,attr[i],(net%v%attr[i])[m])
		}
	out
}

### Spell dynamics from vertex list
networkDynamicInterV<-function(x,...){
	warning("All non-unique spells to Vertices dropped")
	nam<-unique(x$NodeId)
	net<-network.initialize(length(nam),...)
	net%v%"vertex.names"<-nam
	net<-activate.vertices(net,onset=x[,2],terminus=x[,3],v=x[,1])	
	### need to manage issue of multiple spells in one vertex
	net
}



##### add spell to bn using x
listSpell<-function(x,bn){
	test1<-sapply(x,is.network)
	test2<-sapply(x,function(x){ifelse(is.network(x),x%n%"mnext">1,FALSE)})
	for(i in 1:length(x)){
		if(test1[i]){if(test2[i]){bn<-addSpellnl(x[[i]],bn,i-1,i)}}
		}
	bn
}

### adds spell 
addSpellnl<-function(net,bn,time1,time2){
	### Edge Spell
	elt<-mvnum(snel(as.edgelist(net)),bn%v%"vertex.names") ## builds edgelist 
	eids<-mapen(bn,elt)
	os<-rep(time1,length(eids))
	ts<-rep(time2,length(eids))
	bn<-activate.edges(bn,onset=os,terminus=ts,e=eids)
	
	### Vertex spell
	vid<-match(net%v%"vertex.names",bn%v%"vertex.names")
	os<-rep(time1,length(vid))
	ts<-rep(time2,length(vid))
	bn<-activate.vertices(bn,onset=os,terminus=ts,v=vid)
	bn
}

### builds basis network
buildNet<-function(x){
	nam<-getNames(x)
	base<-baseNetwork(nam,x[[1]])
	nel<-namedEL(x)
	m<-mvnum(nel,nam)
	base<-add.edges(base,tail=m[,1],head=m[,2])
	base
}

### gets unique namelist
getNames<-function(ln){
  out<-unique(unlist(lapply(ln,
            function(x){
              if(is.network(x)){return(x%v%"vertex.names")}
            })))
  opt <- options(warn=-1) # as.numeric produces warnings if not coerceable; turn them off
  # If out can be coerced to numeric without any NAs, then sort it as numeric
  if (all(!is.na(tmp<-as.numeric(out)))) {
    out <- tmp
  }
  options(opt)
  as.character(sort(out))
}

#### copies network attributes to empty network
baseNetwork<-function(nam,x){
out<-network.initialize(length(nam), directed = x%n%"directed", hyper = x%n%"hyper", loops = x%n%"loops", multiple = x%n%"multiple", bipartite = x%n%"bipartite")
out%v%"vertex.names"<-nam
out
}

### single named edge list
snel<-function(el){cbind(attr(el,"vnames")[el[,1]],attr(el,"vnames")[el[,2]])}


### full named edgelist
namedEL<-function(ln){
	fout<-vector()
for(i in 1:length(ln)){
	if(is.network(ln[[i]])){
		out<-as.edgelist(ln[[i]])
		fout<-rbind(fout,snel(out))
		}
	}
unique(fout)
}

## match named edgelist to id'd edgelist
mvnum<-function(nel,nam){
cbind(match(nel[,1],nam),match(nel[,2],nam))
}

#### Builds edge index map 
mapen<-function(net,e){apply(e,1,function(y){get.edgeIDs(net,v=y[1],alter=y[2])})}

###############
#### Builds a networkDynamic object from edgelist + spell and network object
###############
networkDynamicInternal<-function(net,x){

dupf<-function(x,y,v=">"){as.numeric(names(y)[do.call(v,list(y,x))])}

if("right.censored"%in%colnames(x)){x$end[x$right.censored]<-Inf}
if("left.censored"%in%colnames(x)){x$start[x$left.censored]<--Inf}

net<-activate.edges(net,onset=x$start,terminus=x$end,e=x[,1])

###########
## Step 2: add multispells
###########
tab<-table(x[,1])
### 2.1 add all 2< spells
if(any(tab>1)){
for(i in 2:max(tab)){
	index<-dupf(i,tab,"==")
	if(sum(index)!=0){
	temp<-x[x[,1]%in%index,]
	temp<-temp[order(temp[,1]),]
	#cat("i",i,"\n")
	for(j in 1:i){
	t<-temp[seq(j,nrow(temp),i),]
	net<-activate.edges(net,onset=t[,2],terminus=t[,3],e=t[,1])	
	}
	#cat(" j",j,"\n")
	}
}
}
net
}

set.nD.class <- function(x){
  if(!is.networkDynamic(x)) class(x) <- c("networkDynamic", class(x))
  x
}

as.edgelist <- function(nw, attrname = NULL, as.sna.edgelist = FALSE,...){
  el <- as.matrix.network.edgelist(nw, attrname=attrname, as.sna.edgelist=as.sna.edgelist,...)
  if(!is.directed(nw)) el[,1:2] <- cbind(pmin(el[,1],el[,2]),pmax(el[,1],el[,2]))
  el
}
