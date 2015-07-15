require(plyr)

### Reading in toy data set with initial parent/child category conversions

full_set <- read.csv("toy_set_converted_cats.csv")


### Function for sampling non-na categories from the full toy set
### Some rows have all NAs after category conversion - using PLACEHOLDER to address for now

category_sampling <- function(x){
  if (length(x[!is.na(x)]) == 0) {
    return("PLACEHOLDER")
  } else {
    return(sample(x[!is.na(x)],size=1))
  }
}


### Function for converting full set into different sets with only one category sampled for each row
### Using set.seed to allow for reproduction of the sampled sets

create_cat_samp_set <- function(seed){
  set.seed(seed)
  cat_samps <- apply(full_set[,40:50], 1, function(x) category_sampling(x))
  new_set <- cbind(full_set[,1:39], cat_samps)
  return(new_set)
}



### Examples of category sampled sets
cat_samped_set <- create_cat_samp_set(seed = 443556)
cat_samped_set2 <- create_cat_samp_set(seed = 3435556)
cat_samped_set3 <- create_cat_samp_set(98076)

