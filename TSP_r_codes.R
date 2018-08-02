remove(destinations);
destinations<-"Times Square,New York,New York"
destinations<-append(destinations,"Golden Gate Bridge,San Francisco Bay Area,California")
destinations<-append(destinations,"Union Station,Washington, D.C.")
destinations<-append(destinations,"Great Smoky Mountains National Park,Tennessee")
destinations<-append(destinations,"Epcot,Orlando,Florida")
destinations<-append(destinations,"Pike Place Market,Seattle,Washington")
bignumber<-10000

library(ggmap)

#Building the distance matrix
destinationsDist<-matrix(nrow=length(destinations),ncol=length(destinations))
destinationsDistv<-vector()
for(i in 1:length(destinations)){
  for(j in 1:length(destinations)){
    if(i==j){
      destinationsDist[i,j]<-bignumber;
    }
    else if (i<j){
      destinationsDist[i,j]<-mapdist(destinations[i],destinations[j],mode="driving")$miles 
      Sys.sleep(1)
    }
    else{
      destinationsDist[i,j]<-destinationsDist[j,i]
    }
  }  
  Sys.sleep(1)
}

#destinationsDist[is.na(destinationsDist)]<-bignumber

#turning the distance matrix into a vector (to be used as the obj function)
destinationsDistv<-as.vector(t(destinationsDist))
destinationsDist
######################################################  
#Rows equal to 1 constraint 
constraints1<-matrix(nrow = ncol(destinationsDist),ncol = ncol(destinationsDist)^2)
for(i in 1:ncol(destinationsDist)){
  for(j in 1:ncol(destinationsDist)^2){
    if((j>=(i-1)*ncol(destinationsDist)+1)&&
       (j<=i*ncol(destinationsDist))){
      constraints1[i,j]<-1
    }
    else{
      constraints1[i,j]<-0
    }
  }
}

#Columns equal to 1 constraint
constraints2<-diag(x = 1, nrow=ncol(destinationsDist), ncol = ncol(destinationsDist))
for(i in 2:ncol(destinationsDist)){
  constraints2<-cbind(constraints2,diag(x = 1, nrow=ncol(destinationsDist), ncol = ncol(destinationsDist)))
}

constraints<-matrix()
constraints<-rbind(constraints1,constraints2)

remove(signs)
rhs<-rep(1,nrow(constraints));
signs<-rep('=',nrow(constraints))

#Solving the LP
solving<-function(){
  library(lpSolve)
  res<<-lp("min",
           destinationsDistv,
           constraints,
           signs,
           rhs,
           all.bin = TRUE,
           presolve = TRUE
  )
  ressolution<<-matrix(res$solution,ncol = ncol(destinationsDist),byrow = TRUE)
  
  #converting matrix into order
  routeorder<<-which(ressolution==1, arr.ind=TRUE)
  ord.destinations<<-vector()
  for(i in 1:(nrow(routeorder))){
    if(i==1){
      ord.destinations[i]<<-routeorder[i,1]
    }else{
      ord.destinations[i]<<-routeorder[ord.destinations[i-1],1]
    }
  }
  ord.destinations_address<<-destinations[ord.destinations]
}

solving()

getsubroute<-function(routeorder){
  routeorder1<-routeorder
  routeorder1<-as.data.frame(routeorder1)
  routeorder1$subroute<-0
  routeorder1<-as.data.frame(routeorder1)
  ord.destinations1<-rep(0,nrow(routeorder1))
  i<-1
  subroutenum<-1
  while(sum(routeorder1$subroute==0)!=0){#-1
    if(i==1){
      ord.destinations1[i]<-routeorder1[i,1]
      routeorder1[i,3]<-subroutenum
    }else{
      ord.destinations1[i]<-routeorder1[ord.destinations1[i-1],1]
      
      if(routeorder1[ord.destinations1[i-1],3]==0){
        routeorder1[ord.destinations1[i-1],3]<-subroutenum
        
      }else{
        routeorder1[ord.destinations1[i-1],3]<-subroutenum
        subroutenum<-subroutenum+1
        ord.destinations1[i]<-routeorder1[match(0,routeorder1$subroute),1]  
      }
    }
    i<-i+1
  }
  #routeorder1<-routeorder1[order(routeorder1[,3],routeorder1[,2]),]
  numsubroute<-unique(routeorder1[,3])
  for(i in 1:length(numsubroute)){#eliminate single city subroute
    if(sum(routeorder1[,3]==i) == 1){
      routeorder1<-routeorder1[!(routeorder1[,3])==i,]
    }
  }
  printrouteorder<<-routeorder1
  names(printrouteorder)<-c("City I","City J","SubRoute")
  for(i in 1:length(unique(routeorder1[,3]))){
    newconstraint<-rep(0,ncol(constraints))
    for(j in 1:sum(routeorder1[,3]==i)){
      newconstraint[((routeorder1[routeorder1[,3]==i,1][j]-1)*length(destinations))+routeorder1[routeorder1[,3]==i,2][j]]<-1
      newconstraint[((routeorder1[routeorder1[,3]==i,2][j]-1)*length(destinations))+(routeorder1[routeorder1[,3]==i,1][j])]<-1
    }
    constraints<<-rbind(constraints,newconstraint)
    signs<<-append(signs,"<=")
    rhs<<-append(rhs,sum(routeorder1[,3]==i)-1)
  }
}



#Subtour Elimination
iteration<-0
ptm <- proc.time()
#while(length(unique(ord.destinations))<length(destinations)){
getsubroute(routeorder)
a<-length(unique(printrouteorder[,3]))>1 && length(unique(ord.destinations))<length(destinations)
while(a!=0){
  #while( length(unique(ord.destinations))<length(destinations)){
  getsubroute(routeorder)
  
  solving()
  iteration<-1+iteration
  print(paste("ITERATION: ",iteration))
  print("Subtours Eliminated:")
  print(printrouteorder)
  print("ord.destinations:")
  print(length(unique(ord.destinations)))
  print(paste("Constraints:",nrow(constraints)))
  print(res)
  print(paste("Unique(ord.destinations):",length(unique(ord.destinations))))
  print(paste("Length(ord.destinations):",length(ord.destinations)))
  print(paste("length(unique(printrouteorder[,3])):",length(unique(printrouteorder[,3]))))
  
  a<-sum(length(unique(printrouteorder[,3]))>1 , length(unique(ord.destinations))<length(destinations))
  #a<-length(unique(ord.destinations))<length(destinations)
  #a<-length(unique(printrouteorder[,3]))>1 
  print(paste("a:",a))
}


proc.time() - ptm

#Go back to first node
ord.destinations_address<-append(ord.destinations_address,ord.destinations_address[1])

#driving route
remove(route_df1)
remove(route_df)
route_df<-vector()
route_df1<-vector()
for(i in 1:(length(ord.destinations_address)-1)){
  route_df1 <- route(ord.destinations_address[i], ord.destinations_address[i+1], structure = "route")
  route_df<-rbind(route_df,route_df1)
  Sys.sleep(.1)
}

route_df<-route_df[,8:9]
gcode<-as.data.frame(geocode(ord.destinations_address))

map1 <- qmap(location = c(-130,8,-60,53),source = "stamen", maptype = "toner",zoom=3)
map1 <- map1 + geom_path(aes(x = lon, y = lat),  colour = "#00ACC1", size = 2,data = route_df, lineend = "round",alpha=.75)
map1 <- map1 + geom_point(data=gcode, aes(x=gcode$lon, y=gcode$lat),size=2, colour= "red")
map1

#Getting the Optimal Solution
OS<-sum(destinationsDistv*res$solution)
OS
##########################################################################
dev.off()
#useful code
distQueryCheck()
geocode(destinations[1])#getting long and lat of a destination
options(scipen=999)

#Debug
destinationsDist
destinationsDistv
constraints
signs
rhs

routeorder
ord.destinations
ord.destinations_address
destinationsDist1#up to (19,19)





