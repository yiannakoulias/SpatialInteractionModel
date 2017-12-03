#-----------------------------------------------------
#A simple and somewhat clumsy method for spatial choice using
#a Huff model; the probably of contact between 'traveller' and 'contact'
#point is inversely proportional to distance
#sample data is random
#-----------------------------------------------------

#library(gsheet)
#library(reshape2)
#library(dplyr)
#library(ggplot2)

#inputs
n=10 #travellers
no_cont <- 50 #contacts
beta <- 5 #distance decay rate

id <- c(1:1000)
x <- runif(1000)
y <- runif(1000)
df <- data.frame(cbind(id, x, y))

#distance matrix
d <- dist(df[,c("x","y")],diag=TRUE, method="euclidean")
distmat <- as.matrix(d)
dist_long <- melt(distmat)#reshape
names(dist_long) <- c("idfrom", "idto", "Edist")

#identify the traveller(s) without replacement
travellers <- data.frame(sample(1:100,n,replace=FALSE))
names(travellers) <- "trav"
dist_sampled <- merge(dist_long,travellers, by.x="idfrom", by.y="trav")
dist_sampled <- dist_sampled[dist_sampled$Edist > 0,]

#sample from within groups
id <- 0
contacts <- data.frame(id)

#sample from within groups with a weight
for(i in 1:n)
{
  group <- dist_sampled[dist_sampled$idfrom==travellers[i,],]
  group$w <- 1/group$Edist^beta
  group$sum_w <- sum(group$w)
  group$p <- group$w/group$sum_w
  c <- data.frame(group[sample(nrow(group),no_cont, prob=group$p),"idto"])
  names(c) <- "id"
  contacts <- rbind(c,contacts)
}
contacts <- data.frame(contacts[contacts$id > 0,])
names(contacts) <- "id"

#merge together files to obtain coordinates
names(travellers) <- "id"
contacts$type <-1
travellers$type <-2
travellers <- merge(df,travellers,by.x="id", by.y="id")
contacts <- merge(df,contacts,by.x="id", by.y="id")
final <- rbind(travellers, contacts)

#basic plot
#plot(final$x,final$y, cex=final$type, xlim=c(0,1), ylim=c(0,1))

#plot
ggplot(final, aes(x=x, y=y, color=type, size=type)) + geom_point()
