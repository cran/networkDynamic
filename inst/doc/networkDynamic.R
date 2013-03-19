### R code from vignette source 'networkDynamic.Rnw'

###################################################
### code chunk number 1: foo
###################################################

foo <- packageDescription("networkDynamic")


###################################################
### code chunk number 2: trivial_triangle
###################################################
library(networkDynamic)          # load the dynamic extensions
triangle <- network.initialize(3)  # create a toy network
add.edge(triangle,1,2)    # add an edge between vertices 1 and 2
add.edge(triangle,2,3)               # add a more edges
activate.edges(triangle,at=1) # turn on all edges at time 1 only
activate.edges(triangle,onset=2, terminus=3, 
               e=get.edgeIDs(triangle,v=1,alter=2))
add.edges.active(triangle,onset=4, length=2,tail=3,head=1)


###################################################
### code chunk number 3: trivial_triangle2
###################################################
class(triangle)
print(triangle)


###################################################
### code chunk number 4: triangle_degree
###################################################
degree<-function(x){as.vector(rowSums(as.matrix(x))
             +colSums(as.matrix(x)))} # handmade degree function
degree(triangle)  # degree of each vertex, ignoring time
degree(network.extract(triangle,at=0)) 
degree(network.extract(triangle,at=1)) # just look at t=1
degree(network.extract(triangle,at=2))
degree(network.extract(triangle,at=5))
degree(network.extract(triangle,at=10))


###################################################
### code chunk number 5: fig1
###################################################
par(mfrow=c(2,2))   #show multiple plots
plot(triangle,main='ignoring dynamics',displaylabels=T)  
plot(network.extract(
  triangle,onset=1,terminus=2),main='at time 1',displaylabels=T)
plot(network.extract(
  triangle,onset=2,terminus=3),main='at time 2',displaylabels=T)
plot(network.extract(
  triangle,onset=5,terminus=6),main='at time 5',displaylabels=T)


###################################################
### code chunk number 6: triangle_vert_activate
###################################################
activate.vertices(triangle,onset=1,terminus=5,v=1) 
activate.vertices(triangle,onset=1,terminus=10,v=2)
activate.vertices(triangle,onset=4,terminus=10,v=3)
network.size.active(triangle,at=1) # how big is it?
network.size.active(triangle,at=4)
network.size.active(triangle,at=5)


###################################################
### code chunk number 7: triangle_check
###################################################
network.dynamic.check(triangle)


###################################################
### code chunk number 8: triangle_deactivate
###################################################
deactivate.edges(triangle,onset=1,terminus=4,
          e=get.edgeIDs(triangle,v=3,neighborhood="combined"))
network.dynamic.check(triangle)


###################################################
### code chunk number 9: triangle_get_times
###################################################
get.vertex.activity(triangle) # vertex spells
get.edge.activity(triangle) # edge spells


###################################################
### code chunk number 10: discrete_vs_cont
###################################################

disc <- network.initialize(2)
disc[1,2]<-1
activate.edges(disc,onset=4,terminus=6) # terminus = t+1
is.active(disc,at=4,e=1)
is.active(disc,at=5,e=1)
is.active(disc,at=6,e=1)


###################################################
### code chunk number 11: discrete_vs_cont2
###################################################
is.active(disc,at=5.5,e=1)


###################################################
### code chunk number 12: discrete_vs_cont3
###################################################
cont <- network.initialize(2)
cont[1,2]<-1
activate.edges(cont,onset=3.0,terminus=6.0001)
is.active(cont,at=4,e=1)
is.active(cont,at=6,e=1)
is.active(cont,at=6.5,e=1)


###################################################
### code chunk number 13: discrete_vs_cont4
###################################################
point <- network.initialize(2) # continuous waves
point[1,2]<-1
activate.edges(point,at=4)
activate.edges(point,at=5)
is.active(point,at=4,e=1)
is.active(point,at=4.5,e=1) # this doesn't makes sense
is.active(point,at=4,e=1)


###################################################
### code chunk number 14: is_active
###################################################
is.active(triangle, onset=1, length=1,v=2:3)
is.active(triangle, onset=1, length=1,e=get.edgeIDs(triangle,v=1))


###################################################
### code chunk number 15: get_active
###################################################
get.edgeIDs.active(triangle, onset=2, length=1,v=1)
get.neighborhood.active(triangle, onset=2, length=1,v=1)
is.adjacent.active(triangle,vi=1,vj=2,onset=2,length=1)


###################################################
### code chunk number 16: active_default
###################################################
static<-network.initialize(3)
is.active(static,at=100,v=1:3)
is.active(static,at=100,v=1:3,active.default=FALSE)
dynamic<-activate.vertices(static,onset=0,terminus=200,v=2)
is.active(dynamic,at=100,v=1:3)
is.active(dynamic,at=100,v=1:3,active.default=FALSE)


###################################################
### code chunk number 17: active_default2
###################################################
inactive<-network.initialize(2)
deactivate.vertices(inactive,onset=-Inf,terminus=Inf,v=2)
is.active(inactive,onset=Inf,terminus=Inf,v=1:2,active.default=TRUE)


###################################################
### code chunk number 18: get_change_times
###################################################
get.change.times(triangle)
get.change.times(triangle,vertex.activity=FALSE)
get.change.times(triangle,edge.activity=FALSE)


###################################################
### code chunk number 19: size_and_edgecount
###################################################
network.size.active(triangle,onset=2,terminus=3)
network.edgecount.active(triangle,at=5)


###################################################
### code chunk number 20: extract
###################################################
get.change.times(triangle)
network.edgecount(triangle)
notflat <- network.extract(triangle,onset=1,terminus=3,trim.spells=TRUE)
is.networkDynamic(notflat)
network.edgecount(notflat) # did we lose edge2?
get.change.times(notflat)


###################################################
### code chunk number 21: collapse
###################################################
flat <-network.collapse(triangle,onset=1,terminus=3)
is.networkDynamic(flat)
get.change.times(flat)
network.edgecount(flat)
list.edge.attributes(flat)
flat%e%'activity.count'
flat%e%'activity.duration'


###################################################
### code chunk number 22: delete_times
###################################################
delete.edge.activity(triangle)
delete.vertex.activity(triangle)
get.change.times(triangle)
get.vertex.activity(triangle)


###################################################
### code chunk number 23: any_all
###################################################
query <- network.initialize(2)
query[1,2] <-1
activate.edges(query, onset=1, terminus=2)
is.active(query,onset=1,terminus=2,e=1)
is.active(query,onset=1,terminus=3,rule='all',e=1)
is.active(query,onset=1,terminus=3,rule='any',e=1)


###################################################
### code chunk number 24: newcomb
###################################################
require(networkDynamic)
data(newcomb)             # load the data
length(newcomb)   # how many networks?
is.network(newcomb[[1]]) # is it really a network?
as.sociomatrix(newcomb[[1]]) # peek at sociomatrix       
newcombDyn <- networkDynamic(network.list=newcomb)  # make dynamic  
get.change.times(newcombDyn)


###################################################
### code chunk number 25: newcomb2
###################################################
all(as.sociomatrix(newcomb[[5]]) == 
      as.sociomatrix(network.extract(newcombDyn,at=5)))
all(as.sociomatrix(newcomb[[5]]) == 
      as.sociomatrix(network.extract(newcombDyn,at=4)))



###################################################
### code chunk number 26: newcomb3
###################################################
newcombGaps <- networkDynamic(network.list=newcomb,
                      onsets=c(1:8,10:15),termini=c(2:9,11:16))
get.vertex.activity(newcombGaps)[[1]] # peek at spells for v1


###################################################
### code chunk number 27: alterNewcombNetObs
###################################################
nobs <-get.network.attribute(newcombGaps,'net.obs.period')
names(nobs)
nobs$'time.unit'<-'week'
nobs$'mode'<-'discrete'
set.network.attribute(newcombGaps,'net.obs.period',nobs)


###################################################
### code chunk number 28: toggles
###################################################
toggles <-cbind(time=1:1000,
                    tail=sample(1:10,1000,replace=TRUE),
                    head=sample(1:10,1000,replace=TRUE))
head(toggles) # peek at begining
empty<-network.initialize(10,loops=TRUE) # to define initial states
randomNet <-networkDynamic(base.net=empty,edge.toggles=toggles)


###################################################
### code chunk number 29: toggles2
###################################################
edgeDurations<-get.edge.activity(randomNet,as.spellList=TRUE)$duration
hist(edgeDurations)
summary(edgeDurations)


###################################################
### code chunk number 30: toggles3
###################################################
sum(get.edge.activity(randomNet,as.spellList=TRUE)$terminus.censored)


###################################################
### code chunk number 31: toggles4
###################################################
nEdgesActive<-sapply(0:1000,
              function(t){network.edgecount.active(randomNet,at=t)})
plot(nEdgesActive,xlab='timestep',ylab='number of active edges')


###################################################
### code chunk number 32: file_spells
###################################################
vertexData <-read.table(system.file('extdata/cls33_10_16_96_vertices.tsv', 
            package='networkDynamic'),,header=T)
vertexData[1:5,] # peek
edgeData <-read.table(system.file('extdata/cls33_10_16_96_edges.tsv', 
            package='networkDynamic'),header=T)
edgeData[1:5,] # peek


###################################################
### code chunk number 33: assemble_mcfarland
###################################################
classDyn <- networkDynamic(vertex.spells=vertexData[,c(3,4,1)],
                           edge.spells=edgeData[,c(3,4,1,2)])


###################################################
### code chunk number 34: checkDiscrete
###################################################
get.change.times(classDyn)[1:10]


###################################################
### code chunk number 35: alterNetObs
###################################################
nobs <-get.network.attribute(classDyn,'net.obs.period')
names(nobs)
nobs$'time.unit'<-'minutes'
set.network.attribute(classDyn,'net.obs.period',nobs)


###################################################
### code chunk number 36: mcfarland_vertattr
###################################################
nrow(vertexData)==length(unique(vertexData$vertex_id))



###################################################
### code chunk number 37: mcfarland_vertexattr2
###################################################
set.vertex.attribute(classDyn,"data_id",vertexData$data_id)
set.vertex.attribute(classDyn,"sex",as.character(vertexData$sex))
set.vertex.attribute(classDyn,"role",as.character(vertexData$role))


###################################################
### code chunk number 38: classroom_binning
###################################################
startTimes <-seq(from=0,to=40,by=5) # make a list of times 
classNets <- lapply(startTimes, function(t){ 
  network.extract(classDyn,onset=t,terminus=t+5)})
classMats <- lapply(classNets,as.sociomatrix) # make into matrices
classDensity <- sapply(classNets, network.density) 
plot(classDensity,type='l',xlab='network slice #',ylab='density')


###################################################
### code chunk number 39: class_plots
###################################################
par(mfrow=c(2,2))   # show multiple plots
plot(network.extract(
  classDyn,onset=0,length=40,rule="any"),
  main='entire 40 min class period',displaylabels=T)
plot(network.extract(
  classDyn,onset=0,length=5,rule="any"),
  main='a 5 min chunk',displaylabels=T)
plot(network.extract(
  classDyn,onset=0,length=2.5,rule="any"),
  main='a 2.5 min chunk',displaylabels=T)
plot(network.extract(
  classDyn,onset=0,length=.1,rule="any"),
  main='a single conversation turn',displaylabels=T)


###################################################
### code chunk number 40: newcomb_spells
###################################################
newcombEdgeSpells<-get.edge.activity(newcombDyn,as.spellList=TRUE)
newcombEdgeSpells[1:5,] # peek at the beginning


###################################################
### code chunk number 41: newcomb_dataframe
###################################################
newcombEdgeSpells<-as.data.frame(newcombDyn)
newcombEdgeSpells[1:5,] # peek at the beginning


###################################################
### code chunk number 42: newcomb_vertspells
###################################################
vertSpells <- get.vertex.activity(newcombDyn,as.spellList=TRUE)
vertSpells[1:5,]


###################################################
### code chunk number 43: slice_nets
###################################################
onsets<-0:2
slices<-lapply(onsets,function(t){
          network.extract(randomNet,onset=t,terminus=t+1)
        })
is.network(slices[[1]])
lapply(slices,as.matrix) # print 'em out as matrices


###################################################
### code chunk number 44: tea1
###################################################
net <-network.initialize(5)
activate.vertex.attribute(net,"happiness", -1, onset=0,terminus=1)
activate.vertex.attribute(net,"happiness", 5, onset=1,terminus=3)
activate.vertex.attribute(net,"happiness", 2, onset=4,terminus=7)
list.vertex.attributes(net)    # what are they actually named?
get.vertex.attribute.active(net,"happiness",at=2)
get.vertex.attribute(net,"happiness.active",unlist=FALSE)[[1]]


###################################################
### code chunk number 45: tea1.1
###################################################
activate.network.attribute(net,'colors',"red",
                           onset=0,terminus=1)
activate.network.attribute(net,'colors',"green",
                           onset=1,terminus=5)
add.edges(net,tail=c(1,2,3),head=c(2,3,4)) # need edges to activate-
activate.edge.attribute(net,'weight',c(5,12,7),onset=1,terminus=3)
activate.edge.attribute(net,'weight',c(1,2,1),onset=3,terminus=7)


###################################################
### code chunk number 46: tea2
###################################################
get.vertex.attribute.active(net,"happiness",at=3.5)
get.vertex.attribute.active(net,"happiness",
                            onset=2.5,terminus=3.5)
get.vertex.attribute.active(net,"happiness",
                            onset=2.5,terminus=3.5,rule="all")


###################################################
### code chunk number 47: tea3
###################################################
get.vertex.attribute.active(net,"happiness",onset=2.5,terminus=4.5)


###################################################
### code chunk number 48: tea3_hidden
###################################################
cat('Warning message:
In get.vertex.attribute.active(net, "happiness", onset = 2.5, 
    terminus = 4.5) : Multiple attribute values matched query 
    spell  for some vertices, only earliest value used')


###################################################
### code chunk number 49: tea4
###################################################
get.vertex.attribute.active(net,"happiness",onset=2.5,terminus=4.5,
                        return.tea=TRUE)[[1]]


###################################################
### code chunk number 50: tea5
###################################################
sapply(get.vertex.attribute.active(net,"happiness",onset=0,terminus=7,
                  return.tea=TRUE),function(splist){
                     sum(unlist(splist[[1]]))
                  })


###################################################
### code chunk number 51: tea5.1
###################################################
get.edge.attribute.active(net,'weight',at=2)
get.edge.attribute.active(net,'weight',at=5)


###################################################
### code chunk number 52: listtea
###################################################
list.vertex.attributes.active(net,at=2)
list.edge.attributes.active(net,at=2)
list.network.attributes.active(net,at=2,dynamic.only=TRUE)


###################################################
### code chunk number 53: tea6
###################################################
activate.vertex.attribute(net, "happiness",100, onset=0,terminus=10,v=1)
get.vertex.attribute.active(net,"happiness",at=2)


###################################################
### code chunk number 54: tea7
###################################################
deactivate.vertex.attribute(net, "happiness",onset=1,terminus=10,v=2)
get.vertex.attribute.active(net,"happiness",at=2)


###################################################
### code chunk number 55: windsurfers
###################################################
data(windsurfers)    # let's go to the beach!
range(get.change.times(windsurfers))
sapply(0:31,function(t){ # how many people in net each day?
  network.size.active(windsurfers,at=t)})


###################################################
### code chunk number 56: windsurfers_meta
###################################################
list.network.attributes.active(windsurfers,-Inf,Inf,dynamic.only=TRUE)
par(mfcol=c(2,1)) # show multiple plots
plot(sapply(0:31,function(t){ # how many people in net each day?
  network.size.active(windsurfers,at=t)}),
     type='l',xlab="number on beach",ylab="day"
)
plot(sapply(0:31,function(t){ # how many people in net each day?
  get.network.attribute.active(windsurfers,'atmp',at=t)}),
     type='l',xlab="air temp",ylab="day"
)
par(mfcol=c(1,1))


###################################################
### code chunk number 57: windsurfers_meta2
###################################################
day3 <-network.collapse(windsurfers,at=2)
day3%n%'day' # what day of the week is day 3?
day3%n%'atmp' # air temp?


###################################################
### code chunk number 58: windsim
###################################################
runSim<-function(net,timeStep,transProb){
  # loop through time, updating states
  times<-seq(from=0,to=max(get.change.times(net)),by=timeStep)
  for(t in times){
    # find all the people who know and are active
    knowers <- which(!is.na(get.vertex.attribute.active(
      net,'knowsRumor',at=t,require.active=TRUE)))
    # get the edge ids of active friendships of people who knew
    for (knower in knowers){
      conversations<-get.edgeIDs.active(net,v=knower,at=t)
      for (conversation in conversations){
        # select conversation for transmission with appropriate prob
        if (runif(1)<=transProb){
          # update state of people at other end of conversations
          # but we don't know which way the edge points so..
          v<-c(net$mel[[conversation]]$inl,
                 net$mel[[conversation]]$outl)
          # ignore the v we already know 
          v<-v[v!=knower]
          activate.vertex.attribute(net,"knowsRumor",TRUE,
                                    v=v,onset=t,terminus=Inf)
          # record who spread the rumor
          activate.vertex.attribute(net,"heardRumorFrom",knower,
                                  v=v,onset=t,length=timeStep)
          # record which friendships the rumor spread across
          activate.edge.attribute(net,'passedRumor',
                    value=TRUE,e=conversation,onset=t,terminus=Inf)
        }
      }
    }  
  }
  return(net)
}


###################################################
### code chunk number 59: setseed
###################################################
set.seed(123) # so we will get the same results each time the document is built


###################################################
### code chunk number 60: windsim_params
###################################################
timeStep <- 1  # units are in days
transProb <- 0.2 # how likely to tell in each conversation/day
# start the rumor out on vertex 1
activate.vertex.attribute(windsurfers,"knowsRumor",TRUE,v=1,
                          onset=0-timeStep,terminus=Inf)
activate.vertex.attribute(windsurfers,"heardRumorFrom",1,v=1,
                          onset=0-timeStep,length=timeStep)
windsurfers<-runSim(windsurfers,timeStep,transProb) # run it!


###################################################
### code chunk number 61: windsim_plots
###################################################
par(mfcol=c(1,2)) # show two plots side by side
wind7<-network.extract(windsurfers,at=7)
plot(wind7,
     edge.col=sapply(!is.na(get.edge.value.active(wind7,
      "passedRumor",at=7)), function(e){ switch(e+1,"darkgray","red")}),
     vertex.col=sapply(!is.na(get.vertex.attribute.active(wind7,
      "knowsRumor",at=7)), function(v){switch(v+1,"gray","red")}),
     label.cex=0.5,displaylabels=TRUE,main="gossip at time 7")
wind30<-network.extract(windsurfers,at=30)
plot(wind30,
     edge.col=sapply(!is.na(get.edge.value.active(wind30,
      "passedRumor",at=30)),function(e){switch(e+1,"darkgray","red")}),
     vertex.col=sapply(!is.na(get.vertex.attribute.active(wind30,
      "knowsRumor",at=30)),function(v){switch(v+1,"gray","red")}),
     label.cex=0.5,displaylabels=TRUE,main="gossip at time 30")
par(mfcol=c(1,1))


###################################################
### code chunk number 62: windsim_stats
###################################################
get.vertex.attribute.active(windsurfers,'knowsRumor',at=15)
plot(sapply(0:31,function(t){
  sum(get.vertex.attribute.active(windsurfers,'knowsRumor',at=t),
      na.rm=TRUE)}),
  main='windsurfers who know',ylab="# people",xlab='time'
)


###################################################
### code chunk number 63: windsim_extract
###################################################
# pull TEA from v3, extract values from 1st part and unlist
unlist(get.vertex.attribute.active(windsurfers,'heardRumorFrom',
                onset=0,terminus=31,return.tea=TRUE)[[3]][[1]])
# pull TEA from v3, extract times from 2nd part and pull col 1
get.vertex.attribute.active(windsurfers,'heardRumorFrom',
              onset=0,terminus=31,return.tea=TRUE)[[3]][[2]][,1]


###################################################
### code chunk number 64: windsim_tree
###################################################
transTree<-function(net){
  # for each vertex in net who knows
  knowers <- which(!is.na(get.vertex.attribute.active(net,
                                        'knowsRumor',at=Inf)))
  # find out who the first transmission was from
  transTimes<-get.vertex.attribute.active(net,"heardRumorFrom",
                      onset=-Inf,terminus=Inf,return.tea=TRUE)
  # subset to only ones that know
  transTimes<-transTimes[knowers]
  # get the first value of the TEA for each knower
  tellers<-sapply(transTimes,function(tea){tea[[1]][[1]]})
  # create a new net of appropriate size 
  treeIds <-union(knowers,tellers)
  tree<-network.initialize(length(treeIds),loops=TRUE)
  # copy labels from original net
  set.vertex.attribute(tree,'vertex.names',treeIds)
  # translate the knower and teller ids to new network ids   
  # and add edges for each transmission                
  add.edges(tree,tail=match(tellers,treeIds), 
            head=match(knowers,treeIds) )               
  return(tree)                
}
plot(transTree(windsurfers),displaylabels=TRUE,
     label.cex=0.5,label.col='blue',loop.cex=3)


###################################################
### code chunk number 65: citation
###################################################
citation(package='networkDynamic')


###################################################
### code chunk number 66: package_listing
###################################################
cat(ls("package:networkDynamic"),sep="\n")


