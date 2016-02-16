# Read unemployment rate data from http://www.bls.gov/

source("panelFunctions.r")

 job= read.csv('unemployment.csv',row.names=1,header=T)
names(job) # column label


# Read file with state names, Fips codes abbreviations
stateNamesFips = read.csv('stateNamesFips.csv',row.names=1,header=T)


# Read polygons files used for state boundaries and for US. outline
#Mark Monmonier created and published State Visibility maps
#For example See "Mapping It Out."  On request he sent his coordinates
#to your instructor who adapted them for use in micromaps.


stateVBorders = read.csv('stateVisibilityBorders.csv',row.names=NULL,header=T)
nationVBorders = read.csv('nationVisibilityBorders.csv',
                 blank.lines.skip=F,row.names=NULL,header=T)
names(stateVBorders)   # st= state ids, x and y are polygon coordinates
names(nationVBorders)   


# Sort the recovery data.frame by the current unempoyment rate and
   #extract the state abbreviations for later use
ord = rev(order(job$Current))   #rev() reverses the order to be descending
job=job[ord,]
stateDataId = row.names(job)


#  Start Graphics Device and Define Colors

windows(width=12,height=10)

wgray = rgb(.82,.82,.82)  #white gray

rgbColors = matrix(c(
 1.00, .30, .30,
 1.00, .50, .00,
  .25,1.00, .25,
  .10, .65,1.00,
  .80, .45,1.00,
  .35, .35, .35,
  .85, .85, .85,
  .50, .50, .50,
 1.00,1.00, .85,
  .85, .85, .85),ncol=3,byrow=T)
hdColors=rgb(rgbColors[,1],rgbColors[,2],rgbColors[,3])


# Define panel layouts
bot = .65
top = .8
left = 0
right = 0
panels = panelLayout(nrow=11,ncol=5,
	       topMar=top,bottomMar=bot,
	       leftMar=left,rightMar=right,
          rowSep=c(0,0,0,0,0,.05,.05,0,0,0,0,0),
          rowSize=c(8,8,8,8,8,1.5,8,8,8,8,8),
          colSize=c(1.5,1.5,3.5,3.5,3.5),
          colSep=c(0.01,0.1,0,.1,.1,0))

panelBlock = panelLayout(nrow=3,ncol=4,
 	       topMar=top,bottomMar=bot,
	       leftMar=left,rightMar=right,
             rowSep=c(0,.05,.05,0),
             rowSize=c(40,1.5,40),
             colSize=c(3,3.5,3.5,3.5),
             colSep=c(0.01,0.1,.1,.1,0))


#  Define graphics parameters

dcex = 1.15
tCex = 1.08
cex = .65
fontsize = 12
font = 1
line1 = .2
line2 = 1.0
line3 = .2
ypad = .65
nameShift = .12


# Set up indices for perceptual groups of states


iBegin = c(1, 6,11,16,21,26,27,32,37,42,47)  #group beginning  subscript
iEnd   = c(5,10,15,20,25,26,31,36,41,46,51)  #group ending     subscript
nGroups = length(iEnd)


# Plot maps


# range for scale the polygon panels
rxpoly = range(stateVBorders$x,na.rm=T)
rypoly = range(stateVBorders$y,na.rm=T)

# polygon id's one per polygon
polygonId = stateVBorders$st[is.na(stateVBorders$x)]

# panel titles
panelSelect(panels,1,1)
panelScale()
mtext(side=3,line=line1,'Above Median States',cex=cex)
mtext(side=3,line=line2,'Micromaps',cex=cex)

panelSelect(panels,11,1)
panelScale()
mtext(side=1,line=line3,'Below Median States',cex=cex)

# drawing the maps
for (i in 1:nGroups){
if(i==6){    #map not drawn, median state in adjacent panel
   panelSelect(panels,6,1)
   panelScale()
   panelFill(col=wgray)
   panelOutline(col="black")
   text(.5,.55,'Median',cex=cex)
   next
}
panelSelect(panels,i,1)
panelScale(rxpoly,rypoly)
gsubs = iBegin[i]:iEnd[i]
if(i==5)gsubs = c(gsubs,26)   #median state added here
if(i==7)gsubs = c(gsubs,26)   #median state added here
if(i < 6) cont = stateDataId[1:26] else cont = stateDataId[26:51]
          # cont means above or below median contour
panelNames = stateDataId[gsubs]

# plot background (out of contour) states in gray with white outlines
back = is.na(match(stateVBorders$st,cont))
polygon(stateVBorders$x[back], stateVBorders$y[back],col=wgray,border=F) #
polygon(stateVBorders$x[back], stateVBorders$y[back],col="white",density=0) # outline states

# plot foreground states for the panel in their special colors pens 1:5
# and other in contour states in light yellow pen 9
fore = !back
pen = match(polygonId,panelNames,nomatch=9)[!is.na(match(polygonId,cont))]
polygon(stateVBorders$x[fore], stateVBorders$y[fore],col=hdColors[pen],border=F) # outline states
polygon(stateVBorders$x[fore], stateVBorders$y[fore],col="black",density=0,lwd=1) # outline states

# outline U.S.
polygon(nationVBorders$x,nationVBorders$y,col="black",density=0,lwd=1) # outside boundary
}



# Plot labels

# get full state names in rate order
ord = match(stateDataId,stateNamesFips$ab)   # match abbreviations
   # match abbreviations
   stateNames = stateNamesFips$ab[ord]  # get the state name
   cbind(stateDataId,stateNames)        # check that the matching has worked


# title column
panelSelect(panels,1,2)
panelScale()
mtext(side=3,line=line1,'',cex=cex)
mtext(side=3,line=line2,'States',cex=cex)

# draw state names
for (i in 1:nGroups){
   gsubs = iBegin[i]:iEnd[i]
   gnams = stateNames[gsubs]
   nsubs = length(gnams)
   pen = 1:nsubs
   laby = nsubs:1

   panelSelect(panels,i,2)
   panelScale(c(0,1),c(1-ypad,nsubs+ypad))
   if(i==6){pen =6}
   for(j in 1:length(pen)){
      points(.1,laby[j],pch=16,col=hdColors[pen[j]],cex=1)
      text(0.4,laby[j]+nameShift,gnams[j],cex=.6,adj=0,col="black",font=font)
   }
}


#  Plot rates

# compute bounds if necssary
#rateRange = range(lower,upper)

rateRange = range(job$Current,job$X2009)
# expand the range by 4%
rateRange = mean(rateRange) +1.20*diff(rateRange)*c(-.5,.5)
rateGrid = c(0,2,4,6,8,10,12,14,16)   # empirical determination

panelSelect(panels,1,5)
panelScale()
mtext(side=3,line=line1,'2009 to 2010',cex=cex)
mtext(side=3,line=line2,'Change from',cex=cex)

for (i in 1:nGroups){
   gsubs = iBegin[i]:iEnd[i]
   nsubs = length(gsubs)
   pen = 1:nsubs
   laby = nsubs:1
   panelSelect(panels,i,5)
   panelScale(rateRange,c(1-ypad,nsubs+ypad))
   panelFill(col=wgray)
   panelGrid(x=rateGrid,col="white",lwd=1)

   panelOutline(col="black")

   if(i==nGroups){axis(side=1,at=rateGrid,
             labels=as.character(rateGrid),
             col="black",mgp=c(1.,-.1,0),tck=-.04,cex.axis=cex)
             mtext(side=1,line=.7,'Percentage',font=font,cex=cex)
   }

   if(i==6)pen = 6

   for(j in 1:length(pen)){
	   m = gsubs[j]
    points(job$X2009[m],laby[j],pch=16,
         cex=dcex,col=hdColors[pen[j]])
      arrows(job$X2009[m],laby[j],job$Current[m],laby[j],length = 0.05, angle = 45, code =2,col="black")
          points(job$Current[m],laby[j],pch=20,
         cex=0.8,col="black")
   }
}


countRange = range(job$X2009,job$X2008)

countRange = mean(countRange)+1.20*diff(countRange)*c(-.5,.5)  # just use the coefficeince to change the scale of the axis
countGrid = c(0,2,4,6,8,10,12,14,16)  # used pretty values that are in bounds

panelSelect(panels,1,4)
panelScale()
mtext(side=3,line=line1,'2008 to 2009',cex=cex)
mtext(side=3,line=line2,'Change from',cex=cex)

for (i in 1:nGroups){
   gsubs = iBegin[i]:iEnd[i]
   nsubs = length(gsubs)
   pen = 1:nsubs
   laby = nsubs:1
   panelSelect(panels,i,4)
   panelScale(countRange,c(1-ypad,nsubs+ypad))
   panelFill(col=wgray)
   panelGrid(x=countGrid,col="white")
   ##panelGrid(x=,col="black",lty=2) # hard code U.S. average  if necessary
   panelOutline(col="black")
   if(i==nGroups){axis(side=1,at=countGrid,
         labels=as.character(countGrid),
         col="black",mgp=c(1,-.1,0),tck=-.04,cex.axis=cex)
        mtext(side=1,line=.7,'Percentage',font=font,cex=cex)
   }
   if(i==6)pen = 6

   for(j in 1:length(pen)){
    points(job$X2008[gsubs[j]],laby[j],pch=16,
         cex=dcex,col=hdColors[pen[j]])
      arrows(job$X2008[gsubs[j]],laby[j],job$X2009[gsubs[j]],laby[j],length = 0.05, angle = 45, code = 2,col="black")
        points(job$X2009[gsubs[j]],laby[j],pch=20,
         cex=0.8,col="black")
   }
}

countRange1 = range(job$X2009,job$X2007)

countRange1 = mean(countRange1)+1.20*diff(countRange1)*c(-.5,.5)  # just use the coefficeince to change the scale of the axis
countGrid = c(0,2,4,6,8,10,12,14,16)  # used pretty values that are in bounds

panelSelect(panels,1,3)
panelScale()
mtext(side=3,line=line1,'2007 to 2008',cex=cex)
mtext(side=3,line=line2,'Change from',cex=cex)

for (i in 1:nGroups){
   gsubs = iBegin[i]:iEnd[i]
   nsubs = length(gsubs)
   pen = 1:nsubs
   laby = nsubs:1
   panelSelect(panels,i,3)
   panelScale(countRange1,c(1-ypad,nsubs+ypad))
   panelFill(col=wgray)
   panelGrid(x=countGrid,col="white")
   panelOutline(col="black")
   if(i==nGroups){axis(side=1,at=countGrid,
         labels=as.character(countGrid),
         col="black",mgp=c(1,-.1,0),tck=-.04,cex.axis=cex)
         mtext(side=1,line=.7,'Percentage',font=font,cex=cex)
   }
   if(i==6)pen = 6

   for(j in 1:length(pen)){
   points(job$X2007[gsubs[j]],laby[j],pch=16,
         cex=dcex,col=hdColors[pen[j]])
      arrows(job$X2007[gsubs[j]],laby[j],job$X2008[gsubs[j]],laby[j],length = 0.05, angle = 45, code = 2,col="black")
        points(job$X2008[gsubs[j]],laby[j],pch=20, 
         cex=0.8,col="black")
   }
   }
 



#10 Add Title and Legend _____________________________________________

##Run
panelSelect(panels,margin='top')
panelScale()
text(.5,.9,'United States Unemployment Rate by States,2007-March 2010',
     cex=tCex)
text(.5,.6,'Annual Average ranking by the seasonly average rate till March 2010',
     cex=0.75) 


# Outline groups of Panels

for (i in 1:3){
for (j in 2:4){
   panelSelect(panelBlock,i,j)
   panelScale()
   panelOutline(col="black")
}}

panelSelect(panelBlock,2,1)
panelScale()
#panelOutline(col="black")    #no outline
