#####################################
######### Yelp project ##############
#####################################
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(BHC)
library(parallel)
library(cluster)
library(NbClust) 
library(fpc)
setwd('H:/')

#Use test data for now
df = fread('user_reviews_business_10kSamp.csv', verbose = TRUE)
source('category_dummy_vars.r')

#Reshape data/ make new variable
tmp = tbl_df(df) %>%
	  select(c(seq(1,40), c(seq(1227,1248)))) %>%
	  arrange(user_id) %>%
	  gather(user_id);
	  colnames(tmp) = c('user_id', 'var', 'value')

#Matrix of all possible id combinations and features needing reshaping
combos = expand.grid(
			unique(tmp$user_id),
			c("ambience_romantic.business",
			  "ambience_intimate.business",
	     	  "ambience_touristy.business",
	     	  "ambience_hipster.business",
	     	  "ambience_diveybusiness",
	     	  "ambience_classybusiness",
	     	  "ambience_trendy.business",
	     	  "ambience_upscale.business",  
	     	  "ambience_casualbusiness",
	     	  "active",
			  "arts",
			  "auto",
			  "beautysvc",
			  "education",
			  "eventservices",
			  "financialservices",
			  "food",
			  "health",
			  "homeservices",
			  "hotelstravel",
			  "localflavor",
			  "localservices",
			  "massmedia",
			  "nightlife",
			  "pets",
			  "professional",
			  "publicservicesgovt",
			  "realestate",
			  "religiousorgs",
			  "restaurants",
			  "shopping"))  %>%
		mutate(Var1 = as.character(Var1),
			   Var2 = as.character(Var2)) %>%
		rename(id = Var1, vars = Var2) %>%
		filter(!(id %in% c("#NAME?", "#VALUE!")))

#Function for checking if individual has rated business of x types
catR = function(id, vars){
	return(
		ifelse(TRUE %in% tmp$value[
			which(tmp$user_id == id &
	   			  tmp$var     == vars)], 
			1, 0)
		)
}

#Parallel-ize and apply 
clus = makeCluster(6)
clusterExport(clus, "catR")
clusterExport(clus, "tmp")

hold = parRapply(clus, combos, function(x) catR(x[1], x[2]))
#saveRDS(hold, 'test_hold.RDS')
hold = readRDS('test_hold.RDS')
hold2 = as.vector(hold)

#Spread into vars
dat.1 = cbind(combos, hold2) %>% tbl_df %>% 
	  spread(vars,hold2)

dat = merge(dat.1, as.data.frame(data)[,1:40], 
		  by.x=c('id'), by.y = c('user_id')) %>%
	distinct(id) %>%
	mutate(
		yelpSince  = as.numeric(substr(yelping_since.user, 1,4)),
		votesFunny = as.numeric(votes.funny.user),
		votesUseful = as.numeric(votes.useful.user),
		votesCool = as.numeric(votes.cool.user),
		fans = as.numeric(fans.user),
		reviewCount = as.numeric(review_count.user),
		meanStars = as.numeric(average_stars.user)
		) %>%
	select(id, 
		   ambience_casualbusiness.x,
		   ambience_classybusiness.x,
		   ambience_diveybusiness.x,
		   ambience_hipster.business.x,  
		   ambience_intimate.business.x,
		   ambience_romantic.business.x, 
		   ambience_touristy.business.x,
		   ambience_trendy.business.x, 
		   ambience_upscale.business.x,
		   yelpSince,
		   votesFunny,
		   votesUseful,
		   votesCool,
		   fans,
		   reviewCount,
		   meanStars
		   ) %>%
	na.omit() %>%
	tbl_df %>%
	filter(reviewCount >= 10)

dat2 = select(dat, -c(id))

################################
######### Clustering ###########
################################
#Names 
items = dat$id 

#Test run
test = bhc(dat2[1:500,], items[1:500], randomised = TRUE, m = 200)


#Compare to K-means
NbClust(dat2,  method = "kmeans")

test = NbClust(dat2, method = 'kmeans')

test2 = kmeans(dat2[-2551,], 5)

dat3 = select(dat2, c(ambience_casualbusiness.x,
		   ambience_classybusiness.x,
		   ambience_diveybusiness.x,
		   ambience_hipster.business.x,  
		   ambience_intimate.business.x,
		   ambience_romantic.business.x, 
		   ambience_touristy.business.x,
		   ambience_trendy.business.x, 
		   ambience_upscale.business.x))

test2 = kmeans(dat3[-2551,], 5)

clusplot(dat2[-2551,], test2$cluster, 
		 color=TRUE, shade=TRUE, labels=2, lines=0)

plotcluster(dat2[-2551,], test2$cluster)
