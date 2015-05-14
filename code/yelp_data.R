#Group the review data by user ID → business ideas → category of business → link the business to the 

setwd("~/yelp_culture_project/data/yelp_dataset_challenge_academic_dataset")
library("rjson")

## For the review file 

json_file <- "yelp_academic_dataset_review.json"
lines<-readLines("yelp_academic_dataset_review.json")
set.seed(576)
index<-sample(1:length(lines), 1000)
sample_lines<-lines[index]

review_data<-NULL

for(i in 1:length(review_lines)){
  json_data <- fromJSON(review_lines[i])
  row<-as.data.frame(t(unlist(json_data)))
  review_data<-rbind(review_data, row)
}


## For the business file 

biz_file <- "yelp_academic_dataset_business.json"
biz_lines<-readLines(biz_file)
set.seed(398)
index<-sample(1:length(biz_lines), 1000)
biz_lines<-biz_lines[index]

## might eliminate hours/open (doesn't matter for the sake of this data)

biz_data<-NULL

for(i in 1:length(biz_lines)){
  json_data<-fromJSON(biz_lines[i])
  vars<-as.data.frame(t(unlist(json_data[c(1,2,6,7,8,10:13)])))
  categories<-paste(unlist(json_data$categories), collapse=",")
  
  ## attributes vary so we'll have to request them separately
  alcohol<-json_data$attributes$Alcohol
  if (is.null(alcohol)){
    alcohol<-NA
  }
  noise_level<-json_data$attributes$'Noise Level'
  if (is.null(noise_level)){
   noise_level<-NA
  }
  attire<-json_data$attributes$Attire
  if (is.null(attire)){
    attire<-NA
  }
  wheelchair_accessible<- json_data$attributes$'Wheelchair Accessible'
  if (is.null(wheelchair_accessible)){
    wheelchair_accessible<-NA
  }
  delivery<-json_data$attributes$Delivery
  if (is.null(delivery)){
    delivery<-NA
  }
  good_for_dancing<-json_data$attributes$'Good For Dancing'
  if (is.null(good_for_dancing)){
    good_for_dancing<-NA
  }
  byob<-json_data$attributes$BYOB
  if (is.null(byob)){
    byob<-NA
  }
  coat_check<-json_data$attributes$'Coat Check'
  if (is.null(coat_check)){
    coat_check<-NA
  }
  smoking<-json_data$attributes$'Smoking'
  if (is.null(smoking)){
    smoking<-NA
  }
  accepts_credit_cards<-json_data$attributes$'Accepts Credit Cards'
  if (is.null(accepts_credit_cards)){
    accepts_credit_cards<-NA
  }
  byob_corking<-json_data$attributes$'BYOB/Corkage'
  if (is.null(byob_corking)){
    byob_corking<-NA
  }
  takeout<-json_data$attributes$'Take-out'
  if (is.null(takeout)){
    takeout<-NA
  }
  price_range<-json_data$attributes$'Price Range'
  if (is.null(price_range)){
    price_range<-NA
  }
  corkage<-json_data$attributes$Corkage
  if (is.null(corkage)){
    corkage<-NA
  }
  happy_hour<-json_data$attributes$'Happy Hour'
  if (is.null(happy_hour)){
    happy_hour<-NA
  }
  outdoor_seating<-json_data$attributes$'Outdoor Seating'
  if (is.null(outdoor_seating)){
    outdoor_seating<-NA
  }
  takes_reservations<-json_data$attributes$'Takes Reservations'
  if (is.null(takes_reservations)){
    takes_reservations<-NA
  }
  waiter_service<-json_data$attributes$'Waiter Service'
  if (is.null(waiter_service)){
    waiter_service<-NA
  }
  WiFi<-json_data$attributes$'Wi-Fi'
  if (is.null(WiFi)){
    WiFi<-NA
  }
  caters<-json_data$attributes$Caters
  if (is.null(caters)){
    caters<-NA
  }
  order_at_counter<-json_data$attributes$'Order at Counter'
  if (is.null(order_at_counter)){
    order_at_counter<-NA
  }
  has_tv<-json_data$attributes$'Has TV'
  if (is.null(has_tv)){
    has_tv<-NA
  }
  good_for_kids<-json_data$attributes$'Good For Kids'
  if (is.null(good_for_kids)){
    good_for_kids<-NA
  }
  good_for_groups<-json_data$attributes$'Good For Groups'
  if (is.null(good_for_groups)){
    good_for_groups<-NA
  }

attributes<-as.data.frame(cbind(alcohol, noise_level, attire, wheelchair_accessible, delivery, good_for_dancing, byob, coat_check, smoking,accepts_credit_cards,byob_corking,takeout,price_range,corkage,happy_hour,outdoor_seating,takes_reservations,waiter_service,WiFi,caters,order_at_counter,has_tv,good_for_kids,good_for_groups))

  if(is.null(json_data$attributes$Ambience)){
  ambience_romantic<-NA
  ambience_intimate<-NA
  ambience_touristy<-NA
  ambience_hipster<-NA
  ambience_divey<-NA
  ambience_classy<-NA
  ambience_trendy<-NA
  ambience_upscale<-NA
  ambience_casual<-NA
  }
  if(!is.null(json_data$attributes$Ambience)){
    ambience_romantic<-json_data$attributes$Ambience$romantic
    ambience_intimate<-json_data$attributes$Ambience$intimate
    ambience_touristy<-json_data$attributes$Ambience$touristy
    ambience_hipster<-json_data$attributes$Ambience$hipster
    ambience_divey<-json_data$attributes$Ambience$divey
    ambience_classy<-json_data$attributes$Ambience$classy
    ambience_trendy<-json_data$attributes$Ambience$trendy
    ambience_upscale<-json_data$attributes$Ambience$upscale
    ambience_casual<-json_data$attributes$Ambience$casual
  }

ambience<-as.data.frame(cbind(ambience_romantic,ambience_intimate,ambience_touristy,ambience_hipster,ambience_divey,ambience_classy,ambience_trendy,ambience_upscale,ambience_casual))

  row<-cbind(vars, categories, attributes, ambience)
  biz_data<-rbind(biz_data, row)  
}






allAttrs<-NULL
allLen<-NULL
for(i in 1:length(biz_lines)){
  json_data<-fromJSON(biz_lines[i])
  attrs<-paste(names(json_data$attributes), collapse=",")
  len<- length(json_data$attributes)
  allAttrs<-rbind(allAttrs, attrs)
  allLen<-c(allLen, len)
}

write.csv(allAttrs, "business_attributes_1k_samp.csv", row.names=FALSE)


## For the user file 

user_file <- "yelp_academic_dataset_user.json"
user_lines<-readLines(user_file)
set.seed(398)
index<-sample(1:length(user_lines), 1000)
user_lines<-user_lines[index]

user_data<-NULL

for(i in 1:length(user_lines)){
  json_data <- fromJSON(user_lines[i])
  vars<-as.data.frame(t(unlist(json_data[-c(6,7,10,11)])))
  friends<-paste(unlist(json_data[6]), frmcollapse=",")
  fans<-paste(unlist(json_data[7]), collapse=",")
  compliments<-paste(names(unlist(json_data[10])), collapse=",")
  compliments<-gsub("compliments.", "", compliments)
  elite<-paste(unlist(json_data[11]), collapse=",")
  row<-cbind(vars, friends, fans, compliments, elite)
  print(length(row))
  user_data<-rbind(user_data, row)
}

## Find parent categories and identify that as a separate variable  
## T
  