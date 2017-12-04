#-----------------------------------------------------
#A simple and somewhat clumsy method for spatial choice using
#a Huff model; the probably of contact between 'traveller' and 'contact'
#point is inversely proportional to distance
#sample data is random
#-----------------------------------------------------

#inputs
no_cont <- 5 #contacts
beta <- 2 #distance decay rate
periods <- 1
a <- runif(4)
b <- runif(4)
c <- runif(100)
d <- runif(100)

#data1
df1 <- data.frame(cbind(a, b))
names(df1) <- c("x", "y")
df1$trav <- 1

#data2
df2 <- data.frame(cbind(c, d))
names(df2) <- c("x", "y")
df2$trav <- 0

#df
df <- rbind(df1,df2)
l <- nrow(df)
df$id <- c(1:l)

#distance matrix
d <- dist(df[,c("x","y")],diag=TRUE, method="euclidean")#do I know order is preserved?
distmat <- as.matrix(d)
dist_long <- reshape2::melt(distmat)#reshape library
names(dist_long) <- c("idfrom", "idto", "Edist")
dist_long <- dist_long[dist_long$Edist > 0,]

#identify the traveller(s) without replacement
dist_sampled <- merge(dist_long,df[df$trav==1,], by.x="idfrom", by.y="id")

#sample from within groups
id <- 0
contacts <- data.frame(id)

travellers <- as.data.frame(df[df$trav==1,"id"])
n <- nrow(travellers)

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
ggplot2::ggplot(final, ggplot2::aes(x=x, y=y, color=type, size=type)) + 
  ggplot2::geom_point() +
  ggplot2::xlim(0,1) + 
  ggplot2::ylim(0,1)
  

