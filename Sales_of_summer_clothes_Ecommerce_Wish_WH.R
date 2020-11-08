# Installing required libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(wordcloud)) install.packages("wordcloud", repos = "http://cran.us.r-project.org")
if(!require(tm)) install.packages("tm", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(caretEnsemble)) install.packages("caretEnsemble", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")

# Loading required libraries
library(tidyverse)
library(caret)
library(stringr)
library(ggpubr)
library(corrplot)
library(matrixStats)
library(randomForest)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(caretEnsemble)
library(factoextra)


#Loading and saving of the data set "summer-products-with-rating-and-performance-2020-08.zip" from my github account:
raw_data<-read_csv("https://raw.githubusercontent.com/whessarey/edx_capstone_CYO/master/summer-products-with-rating-and-performance_2020-08.csv")
save(raw_data, file="summer-products-with-rating-and-performance_2020-08.Rdata")
load("summer-products-with-rating-and-performance_2020-08.Rdata")

# Data explonatory analysis
class(raw_data)
dim(raw_data)
glimpse(raw_data)

# NA values detection and removal
N_A<-sort(sapply(raw_data, function(x) sum(is.na(x))), decreasing=TRUE)
N_A[N_A>0]

# Formatting
# Features with unique values as crawl_month, currency_buyer, and theme do not impact units_sold and are consequently removed
# creating new variables
processed_data<- raw_data %>% mutate(merchant_has_profile_picture = ifelse(is.na(merchant_has_profile_picture),0,merchant_has_profile_picture),
                                     has_urgency_banner= ifelse(is.na(has_urgency_banner),0,has_urgency_banner),
                                     rating=ifelse(rating_count==0, 0, rating),
                                     rating_five_count=ifelse(is.na(rating_five_count), 0, rating_five_count),
                                     rating_four_count=ifelse(is.na(rating_four_count), 0, rating_four_count),
                                     rating_three_count=ifelse(is.na(rating_three_count), 0, rating_three_count),
                                     rating_two_count=ifelse(is.na(rating_two_count), 0, rating_two_count),
                                     rating_one_count=ifelse(is.na(rating_one_count), 0, rating_one_count),
                                     merchant_rating= round(merchant_rating, digits = 1),
                                     rating=round(rating, digits = 1),
                                     merchant_pos_feedback_rate=as.numeric(sub("%.*","",merchant_info_subtitle)),
                                     merchant_has_feedback_rate=ifelse(is.na(merchant_pos_feedback_rate), 0, 1),
                                     tags_number=as.numeric(str_count(tags, ",")),
                                     product_variation_size_id=str_to_upper(str_trim(str_replace(product_variation_size_id, ".*-|.*/|\\(.*|\\s.*|\\..*|\\..|WOMEN", ""), side = "both")),
                                     product_variation_size_id=str_replace(product_variation_size_id,"2XL|3XL|4XL|5XL|6XL", "XL"),
                                     product_variation_size_id= ifelse(str_detect(product_variation_size_id, "\\bS\\b|\\bM\\b|\\bL\\b|\\bXL\\b|\\bXS\\b|\\bXXS\\b|\\bXXXS\\b|\\bXXL\\b|\\bXXXL\\b|\\bXXXXL\\b")==TRUE, product_variation_size_id, "others" ),
                                     product_variation_size_id= ifelse(product_variation_size_id=="M(CHILD)" | is.na(product_variation_size_id), "others", product_variation_size_id),
                                     product_color= str_to_sentence(product_color),
                                     discount_percent=as.numeric(ifelse(retail_price>price, round((retail_price - price)/retail_price, digits = 2), 0)),
                                     
                                     shipping_price_percentage= as.numeric(round(shipping_option_price/price, digits = 2))) %>%
  select(-merchant_info_subtitle, -tags, -merchant_profile_picture, -urgency_text)

n_month<-n_distinct(processed_data$crawl_month)
n_buyer<-n_distinct(processed_data$currency_buyer)
n_theme<-n_distinct(processed_data$theme)

# Product url is not a valuable feature for a model since it does not impact units_sold. This applies to title and orig_title. 
# Since all products have product pictures and there is no other detailled informations about the pictures this feature will be removed, too.

processed_data<-processed_data %>% select(-c(crawl_month, currency_buyer, theme, product_picture, product_url, title, title_orig))
dim(processed_data)
summary(processed_data)

## Number of unique products
n_distinct(processed_data$product_id)
## Number of unique merchants
n_distinct(processed_data$merchant_id)

# Creating train and validation data sets
set.seed(1, sample.kind = "Rounding")
test_index<- createDataPartition(processed_data$units_sold, times = 1, p=.2, list = F)
train_set<-processed_data[-test_index,]
validation<-processed_data[test_index,]
rm(test_index)


# Distribution of the feature 'units sold'
summary(train_set$units_sold)
rd_hist<-train_set %>% ggplot(aes(units_sold)) + geom_histogram(bins = 30) + scale_x_log10() + ggtitle("Histogram of units sold") + xlab("units_sold [log]")
rd_boxplot<-train_set %>% ggplot(aes(x="", y=units_sold)) + geom_boxplot() + geom_jitter() + ggtitle("Boxplot of units sold") + coord_flip()
ggarrange(rd_boxplot, rd_hist, labels = c("A", "B"), ncol = 1, nrow = 2)

# Price sensitivity analysis
summary(train_set$price)
disc_plot<-train_set %>% group_by(discount_percent) %>% ggplot(aes(x=discount_percent, y=units_sold)) + geom_point() + geom_smooth() +
  ggtitle("Units_sold vs. discount percentage") + xlab("Discount percentage") + scale_x_continuous(labels = scales::percent_format())
price_plot1<-train_set %>% ggplot(aes(x=price, y=units_sold)) + geom_point() + geom_smooth() + ggtitle("units_sold vs. price")
price_plot2<-train_set %>% ggplot(aes(x=retail_price, y=units_sold)) + geom_point() + geom_smooth() + ggtitle("units_sold vs. retail price")

ggarrange(disc_plot, price_plot1, price_plot2, labels = c("A", "B", "C"), ncol = 1, nrow = 4)
train_set %>% mutate(price_category=cut(price, breaks = seq(0, 50, 10))) %>% ggplot(aes(x=price_category, y=units_sold)) + geom_point() + geom_smooth() + ggtitle("units_sold vs. price categories")

# Product attribute analysis
prod_rating_plot1<-train_set %>% ggplot(aes(x=as.factor(rating), y=units_sold)) + geom_bar(stat = "identity") + ggtitle("Units sold") +
  theme(axis.text.x = element_text(angle=90, hjust=1))
prod_rating_plot2<-train_set %>% group_by(rating) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=as.factor(rating), y= avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() + ggtitle("Avg. units sold") +
  theme(axis.text.x = element_text(angle=90, hjust=1))
ggarrange(prod_rating_plot1, prod_rating_plot2, labels = c("A", "B"),ncol = 1, nrow = 2)

prod_rating_count<-train_set %>%ggplot(aes(x=rating_count, y=units_sold)) + geom_point() + geom_smooth() + ggtitle("Units sold vs. rating count")
prod_rating_count_p1<-train_set %>% ggplot(aes(x=rating_one_count, y=units_sold)) + geom_point() + geom_smooth() + ggtitle("Units sold vs. one rating count")
prod_rating_count_p2<-train_set %>% ggplot(aes(x=rating_two_count, y=units_sold)) + geom_point() + geom_smooth()  + ggtitle("Units sold vs. rating two count")
prod_rating_count_p3<-train_set %>% ggplot(aes(x=rating_three_count, y=units_sold)) + geom_point() + geom_smooth() + ggtitle("Units sold vs. rating three count")
prod_rating_count_p4<-train_set %>% ggplot(aes(x=rating_four_count, y=units_sold)) + geom_point() + geom_smooth() + ggtitle("Units sold vs. rating four count")
prod_rating_count_p5<-train_set %>% ggplot(aes(x=rating_five_count, y=units_sold)) + geom_point() + geom_smooth() + ggtitle("Units sold vs. rating five count")

ggarrange(prod_rating_count, prod_rating_count_p1, prod_rating_count_p2, prod_rating_count_p3, prod_rating_count_p4,prod_rating_count_p5,
          labels = c("C", "D", "E", "F", "G", "H"),ncol = 2, nrow = 3)

# Features as merchant uses adboosts, tags number or urgency banner do not seemingly impact units sold.
# Hypothetically, merchants having low level of units sold use adboosts expacting higher volumns of revenue.
prod_urgban_plot<-train_set %>% group_by(has_urgency_banner) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=as.factor(has_urgency_banner), y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +
  ggtitle("Avg. units_sold") + xlab("Merchant uses urgency banner") +  ylab("Avg. units_sold")
prod_adboost_plot<-train_set %>% group_by(uses_ad_boosts) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=as.factor(uses_ad_boosts), y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +
  ggtitle("Avg. units_sold") + xlab("Merchant uses adboosts")  + ylab("Avg. units sold")
prod_tags<-train_set %>% group_by(tags_number) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=tags_number, y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +
  ggtitle("Avg. units_sold")

ggarrange(prod_urgban_plot, prod_adboost_plot, prod_tags, labels = c("I", "J", "K"), ncol = 2, nrow = 2)

# Product variation sizes 'S' and 'M' are the most prevalent sizes regarding units sold. The most prevalent product variation inventory is '50'.
# However, these features very poorly affect units sold.
df_size<-train_set %>% group_by(product_variation_size_id) %>% summarize(product_variation_size_number = n())
wordcloud(words = df_size$product_variation_size_id, freq =df_size$product_variation_size_number, 
          min.freq = 10, max.words = 10, random.order = FALSE, random.color = FALSE, 
          rot.per = 0.35, scale = c(5, 0.2), font = 4, colors = brewer.pal(8, "Dark2"), 
          main = "Most product variation sizes")
df_inventory<-train_set %>% group_by(product_variation_inventory) %>% summarize(product_variation_inventory_number = n())
wordcloud(words = df_inventory$product_variation_inventory, freq =df_inventory$product_variation_inventory_number, 
          min.freq = 10, max.words = 10, random.order = FALSE, random.color = FALSE, 
          rot.per = 0.35, scale = c(5, 0.2), font = 4, colors = brewer.pal(8, "Dark2"), 
          main = "Most product variation sizes")

prod_varsize_plot<-train_set %>% group_by(product_variation_size_id) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=product_variation_size_id, y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +
  ggtitle("Avg. units_sold") + theme(axis.text.x = element_text(angle=45, hjust=1))
prod_varinvent_plot<-train_set %>% group_by(product_variation_inventory) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=product_variation_inventory, y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +
  ggtitle("Avg. units_sold") + theme(axis.text.x = element_text(angle=45, hjust=1))
ggarrange(prod_varsize_plot, prod_varinvent_plot,labels = c("L", "M"), ncol = 1, nrow = 2)

## Product colors as balck, white, grey, blue , or green are very popular.
## Diagrams show that the feature product color do not necessarily impact units sold.
prod_col_plot1<-train_set %>% group_by(product_color) %>% summarize(n=units_sold) %>% mutate(product_color=reorder(product_color, -n)) %>%
  ggplot(aes(x=product_color, y= n)) + geom_bar(stat="identity") + ggtitle("Units sold vs. product color") + theme(axis.text.x = element_text(angle=90, hjust=1))

prod_col_plot2<-train_set %>% group_by(product_color) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% mutate(product_color=reorder(product_color, avg))%>%
  ggplot(aes(x=product_color, y= avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +theme(axis.text.x = element_text(angle=90, hjust=1))+
  ggtitle("Avg. units sold")

ggarrange(prod_col_plot1, prod_col_plot2, labels = c("N", "O"),ncol = 1, nrow = 2)

## A highr number of badges do not necessarily lead to higher voloumns of units sold. This applies to badges as local product or fast shipping.
## Not surprisingly, the badge product quality seems to impact units sold.
badge1<-train_set %>% group_by(badges_count) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=as.factor(badges_count), y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() + ggtitle("Avg. units_sold") + xlab("Badges count")
badge2<-train_set %>% group_by(badge_product_quality) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=as.factor(badge_product_quality), y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +  ggtitle("Avg. units_sold") + xlab("Badges product quality")
badge3<-train_set %>% group_by(badge_local_product) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=as.factor(badge_local_product), y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +  ggtitle("Avg. units_sold") + xlab("Badges local product") 
badge4<-train_set %>% group_by(badge_fast_shipping) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=as.factor(badge_fast_shipping), y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +  ggtitle("Avg. units_sold") + xlab("Badges fast shipping") 

ggarrange(badge1, badge2, badge3, badge4, labels = c( "P", "Q","R", "S"), ncol = 2, nrow = 2)


## Products marked as 'shipping  is express' do not lead to higher volumns of unit sold.
## Shipping price percentage as proportion of shipping price and product price has a slight negative impact on units sold.  
express_plot<-train_set %>% group_by(shipping_is_express) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=as.factor(shipping_is_express), y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() +geom_errorbar() +  ggtitle("Avg. units_sold") + xlab("Shipping is express")
ship_price_plot<-train_set %>% group_by(shipping_price_percentage, shipping_is_express) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=shipping_price_percentage, y=avg, ymin=avg-2*se, ymax=avg+2*se, color=  as.factor(shipping_is_express))) + geom_point() + geom_errorbar() + ggtitle("Avg. units_sold") + xlab("Shipping price percentage") +  theme(legend.position="top")

ggarrange(express_plot, ship_price_plot, labels = c("A", "B"), ncol = 2, nrow = 1)

## Units sold do not differ regarding geographical features as origin country or numer of countries shipped to. 
orig_plot1<-train_set %>% ggplot(aes(x= origin_country, y= units_sold)) + geom_bar(stat = "identity") + ggtitle("Units_sold") + xlab("Origin countries")
orig_plot2<-train_set %>% group_by(origin_country) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>%
  ggplot(aes(x=origin_country, y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +  ggtitle("Avg. units_sold") + xlab("Origin countries")
ship_to_plot<-train_set %>% group_by(countries_shipped_to) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>%
  ggplot(aes(x=countries_shipped_to, y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +  ggtitle("Avg. units_sold") + xlab("Countries shipped to")

ggarrange(orig_plot1, orig_plot2, ship_to_plot, labels = c("C", "D", "E"), ncol = 1, nrow = 3)

# Merchant attribute analysis
merch_plot1<-train_set %>% group_by(merchant_rating) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=as.factor(merchant_rating), y= avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +  ggtitle("Avg. units sold") + xlab("Merchant rating") + theme(axis.text.x = element_text(angle=90, hjust=1))
merch_plot2<-train_set %>% group_by(merchant_pos_feedback_rate) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>%
  ggplot(aes(x=merchant_pos_feedback_rate, y=avg, ymin=avg-2*se, ymax=avg+2*se)) +  geom_point() + geom_errorbar() +  ggtitle("Avg. units sold") + xlab("Merchant pos. feedback rate")
merch_plot3<-train_set %>% group_by(merchant_rating_count) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>%
  ggplot(aes(x=merchant_rating_count,  y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() + geom_smooth() +  scale_x_log10() + scale_y_log10() + ggtitle("Avg. units sold vs. rating count")
merch_plot4<-train_set %>% ggplot(aes(y=merchant_pos_feedback_rate, x=merchant_rating)) + geom_point() + geom_smooth() +  ggtitle("Merchant feedback and rating")

ggarrange(merch_plot1, merch_plot2, merch_plot3, merch_plot4, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)

merch_plot5<-train_set %>% group_by(merchant_has_profile_picture) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=as.factor(merchant_has_profile_picture), y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +  ggtitle("Avg. units sold") + xlab("Merchant has profile picture") 
merch_plot6<-train_set %>% group_by(merchant_has_feedback_rate) %>% summarize(avg=mean(units_sold), se=sd(units_sold)/sqrt(n())) %>% 
  ggplot(aes(x=as.factor(merchant_has_feedback_rate), y=avg, ymin=avg-2*se, ymax=avg+2*se)) + geom_point() + geom_errorbar() +  ggtitle("Avg. units sold") + xlab("Merchant has feedback rate")
ggarrange(merch_plot5, merch_plot6, labels = c("E", "F"), ncol = 2, nrow = 1)

## A correlation heat map is an adequate method for summarizing correlations between all numerical variables.
## This analysis is used to identify irrelvant features as well as highly correlated features. 
S<- ifelse(train_set$product_variation_size_id=="S", 1, 0)
L <- ifelse(train_set$product_variation_size_id=="L", 1, 0)                           
M <- ifelse(train_set$product_variation_size_id=="M", 1, 0)                          
others<- ifelse(train_set$product_variation_size_id=="others", 1, 0)                                               
XL <- ifelse(train_set$product_variation_size_id=="XL", 1, 0)                          
XS  <- ifelse(train_set$product_variation_size_id=="XS", 1, 0)                        
XXL<- ifelse(train_set$product_variation_size_id=="XXL", 1, 0)                          
XXS <- ifelse(train_set$product_variation_size_id=="XXS", 1, 0)                        
XXXL<- ifelse(train_set$product_variation_size_id=="XXXL", 1, 0)                          
XXXS<- ifelse(train_set$product_variation_size_id=="XXXS", 1, 0)                          
XXXXL<- ifelse(train_set$product_variation_size_id=="XXXXL", 1, 0)

train_set<-train_set %>% select(-c(rating_count, merchant_rating_count)) %>% add_column(S, L, M, others, XL, XS, XXL, XXS, XXXL, XXXS, XXXXL)

S<- ifelse(validation$product_variation_size_id=="S", 1, 0)
L <- ifelse(validation$product_variation_size_id=="L", 1, 0)                           
M <- ifelse(validation$product_variation_size_id=="M", 1, 0)                          
others<- ifelse(validation$product_variation_size_id=="others", 1, 0)                                               
XL <- ifelse(validation$product_variation_size_id=="XL", 1, 0)                          
XS  <- ifelse(validation$product_variation_size_id=="XS", 1, 0)                        
XXL<- ifelse(validation$product_variation_size_id=="XXL", 1, 0)                          
XXS <- ifelse(validation$product_variation_size_id=="XXS", 1, 0)                        
XXXL<- ifelse(validation$product_variation_size_id=="XXXL", 1, 0)                          
XXXS<- ifelse(validation$product_variation_size_id=="XXXS", 1, 0)                          
XXXXL<- ifelse(validation$product_variation_size_id=="XXXXL", 1, 0)
validation<-validation %>% select(-c(rating_count, merchant_rating_count))%>% add_column(S, L, M, others, XL, XS, XXL, XXS, XXXL, XXXS, XXXXL)

# Heat map analysis
train_mat<-select_if(train_set, is.numeric) %>%as.matrix()
dim(train_mat)
Mx <- cor(train_mat, use = "pairwise.complete.obs")
corrplot(Mx, method = "color", tl.cex = 0.7)
rm(train_mat)

## Feautures with no correlation with units sold and merchant positive feedback are removed.
train_set<-train_set %>% select(-c(retail_price, badge_local_product, badge_fast_shipping, shipping_is_express, countries_shipped_to, inventory_total, has_urgency_banner, tags_number, discount_percent, merchant_has_feedback_rate, merchant_pos_feedback_rate, product_variation_size_id, S, L, M, others, XL, XS, XXL, XXS, XXXL, XXXS, XXXXL))

validation<-validation %>% select(-c(retail_price, badge_local_product, badge_fast_shipping, shipping_is_express, countries_shipped_to, inventory_total, has_urgency_banner, tags_number, discount_percent, merchant_has_feedback_rate, merchant_pos_feedback_rate, product_variation_size_id, S, L, M, others, XL, XS, XXL, XXS, XXXL, XXXS, XXXXL))

top_6_merchant<-train_set %>% filter(units_sold>=100000)
top_6_merchant

# PCA

train_x<-select_if(train_set, is.numeric) %>% select(-units_sold) %>% as.matrix()
train_y<-train_set$units_sold

fviz_eig(pca)
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
plot(pca$x[,1], pca$x[,2])
plot(pca$x[,3], pca$x[,4])

imp<-8
train_x_imp<-pca$x[,1:imp]
validation_x<-select_if(validation, is.numeric)  %>% select(-units_sold)%>% as.matrix()
validation_y<-validation$units_sold
validation_x_mean_0 <- sweep(validation_x, 2, colMeans(validation_x))
validation_x_standardized <- sweep(validation_x_mean_0, 2, colSds(validation_x), FUN = "/")
validation_x_pca<-validation_x_standardized %*% pca$rotation
validation_x_imp <- validation_x_pca[,1:imp]
rm(validation_x_mean_0, validation_x_standardized, validation_x_pca)

# Prediction models
set.seed(1, sample.kind = "Rounding")
test_index<- createDataPartition(train_y, times = 1, p=.1, list = F)
train_x_imp_sub<-train_x_imp[-test_index,]
test_x_imp<-train_x_imp[test_index,]
train_y_sub<-train_y[-test_index]
test_y<-train_y[test_index]
rm(test_index)

y_hat_baseline<-mean(train_y_sub)
RMSE_baseline<-RMSE(test_y, y_hat_baseline)
RMSE_baseline

set.seed(100, sample.kind = "Rounding")
fit_glm<-train(train_x_imp_sub, train_y_sub, method = "glm")
fit_glm$finalModel
y_hat_glm <- predict(fit_glm, test_x_imp)
RMSE_glm<-RMSE(test_y, y_hat_glm)
RMSE_glm

set.seed(100, sample.kind = "Rounding")
k<-seq(1,20,1)
fit_knn<-train(train_x_imp_sub, train_y_sub, method = "knn", tuneGrid = data.frame(k=k))
ggplot(fit_knn)
fit_knn$finalModel
y_hat_knn <- predict(fit_knn, test_x_imp)
RMSE_knn<-RMSE(test_y, y_hat_knn)
RMSE_knn

set.seed(100, sample.kind = "Rounding")
mtry<-seq(50, 550,50)
fit_rf<-train(train_x_imp_sub, train_y_sub, method = "rf", tuneGrid=data.frame(mtry=mtry), nodesize=1)
ggplot(fit_rf)
fit_rf$finalModel
y_hat_rf <- predict(fit_rf, test_x_imp)
RMSE_rf<-RMSE(test_y, y_hat_rf)
RMSE_rf

test_y_plot<-as.data.frame(test_y) %>% ggplot(aes(x="", y=test_y)) + geom_boxplot() + geom_jitter() + ylim(0, 20000) + coord_flip()
y_hat_glm_plot<-as.data.frame(y_hat_glm) %>% ggplot(aes(x="", y=y_hat_glm)) + geom_boxplot() + geom_jitter() + ylim(0, 20000) + coord_flip()
y_hat_knn_plot<-as.data.frame(y_hat_knn) %>% ggplot(aes(x="", y=y_hat_knn)) + geom_boxplot() + geom_jitter() + ylim(0, 20000) + coord_flip()
y_hat_rf_plot<-as.data.frame(y_hat_rf) %>% ggplot(aes(x="", y=y_hat_rf)) + geom_boxplot() + geom_jitter() + ylim(0, 20000) + coord_flip()

ggarrange(test_y_plot, y_hat_glm_plot, y_hat_knn_plot, y_hat_rf_plot, ncol = 1, nrow = 4)

models <-  c("glm", "svmLinear", "gamboost", "gamLoess", "kknn", "gam", "ranger", "avNNet",
             "mlp", "monmlp", "gbm", "svmRadial", "svmRadialCost", "svmRadialSigma")       

set.seed(100, sample.kind = "Rounding")
control <-  trainControl(method="boot", number=25,  savePredictions="final",  allowParallel = TRUE)
fits_list <- caretList(train_x_imp_sub, train_y_sub,  trControl=control,  methodList= models, 
                       tuneList=list(
                         knn=caretModelSpec(method="knn", tuneGrid=data.frame(k=k)),
                         rf=caretModelSpec(method="rf", tuneGrid=data.frame(mtry=mtry), nodesize=1)))
results <-resamples(fits_list)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales, metric = "RMSE")
modelCor(results)

set.seed(100, sample.kind = "Rounding")
ensemble <- caretEnsemble(fits_list, metric = "RMSE", trControl = control)
summary(ensemble)
plot(ensemble)

y_hat_ensemble <- predict(ensemble, newdata = test_x_imp)
RMSE_ensemble<- RMSE(y_hat_ensemble, test_y)
RMSE_ensemble

set.seed(100, sample.kind = "Rounding")
mtry<-seq(50, 550,50)
fit_rf<-train(train_x_imp, train_y, method = "rf", tuneGrid=data.frame(mtry=mtry), nodesize=1)
ggplot(fit_rf)
fit_rf$finalModel
y_hat_rf <- predict(fit_rf, validation_x_imp)
RMSE_rf<-RMSE(validation_y, y_hat_rf)
RMSE_rf