
setwd ("/Users/ynbsztl/Documents/LaTeX/上课笔记/研二下/贸易与发展工作坊-2023Fall/3.31due")
dat <- read.csv("OTC_Data_forStata.csv",sep = "\t")
head(dat) # inspect structure

# Q1

library(dplyr) # Load dplyr package
litab_num <- data.frame(id=seq(1,11),brand=c(rep(c("Tylenol","Advil","Bayer"),each=3),"Store Brand","Store Brand"),tab=c(rep(c(25,50,100),3),50,100))
dat2 <- dat %>%  left_join(litab_num,c("product"="id")) %>% 
  mutate(price_50_tab=price/tab*50,cost_50_tab=cost/tab*50) %>% 
  group_by(brand,tab) %>% summarize(Price_carton=mean(price),Price_50_tab=mean(price_50_tab),
                                    Whole.Price=mean(cost_50_tab),
                                    sum_q=sum(quantity*tab),sum_c=sum(count),Proportion_promotion=mean(promotion))

dat3 <- dat %>%  left_join(litab_num,c("product"="id")) %>% 
  mutate(price_50_tab=price/tab*50,cost_50_tab=cost/tab*50) %>% 
  group_by(brand,tab)  %>% summarize(sum_q=sum(quantity*tab/50),sum_c=sum(count)) #%>% mutate(market_share=sum_q/sum(sum_q))
dat3$Share_of_potent.mkt <- dat3$sum_q / dat3$sum_c 
dat3$Marketshare <- dat3$Share_of_potent.mkt / sum(dat3$Share_of_potent.mkt)


dat_finals <- dat2 %>% left_join(dat3,by=c("brand"="brand","tab"="tab"),keep = FALSE) %>% select(c("brand","tab","Price_carton",
                                                                                                   "Price_50_tab","Whole.Price",
                                                                                                   "Marketshare",
                                                                                                   "Share_of_potent.mkt","Proportion_promotion"))
dat_finals
 
 #######################################################
 ## Q2
 
dat_new <- dat %>%  left_join(litab_num,c("product"="id")) %>% 
   mutate(price_50_tab=price/tab*50,cost_50_tab=cost/tab*50) %>% group_by(brand,tab,week) %>% 
   summarize(sum_q=sum(quantity*tab/50),sum_c=sum(count),
             Price_carton=mean(price),Price_50_tab=mean(price_50_tab),
             Whole.Price=mean(cost_50_tab),promotion=mean(promotion)) %>% mutate(sjt=sum_q/sum_c)

# (i)
install.packages("gamlss")
library(gamlss)

model_i <- gamlss(sjt ~ Price_carton + promotion, data = dat_new,sigma.formula=~pb(sjt-Price_carton-promotion),family = GU )
summary(model_i)

# (ii)

model_ii <- gamlss(sjt~Price_carton+promotion+factor(brand),data = dat_new,sigma.formula=~pb(sjt-Price_carton-promotion),family = GU)
summary(model_ii)

# (iii)
dat_new$store_product <- paste0(dat_new$brand,dat_new$tab)

model_iii <- gamlss(sjt~Price_carton+promotion+factor(store_product),data = dat_new,sigma.formula=~pb(sjt-Price_carton-promotion),family = GU)
summary(model_iii)

#from r to latex
# 创建一个包含要添加到Latex输出的行的向量
product_fe <- c("Product Fixed Effect","NO","YES","YES")
store_product_fe <- c("Store-product Fixed Effect","NO","NO","YES")
column_labels <- c("benchmark", "product ﬁxed effects", "store-product Fixed Effect")

# 使用stargazer并不支持从gamlss到latex
#stargazer(model_i, model_ii, model_iii, title="Regression Results", 
#          omit.stat=c("f", "ser"), keep=c("Price_carton","promotion"),
#          add.lines = list(product_fe,store_product_fe),column.labels = column_labels)

# (iv)
# reestimate using Whole.Price instrument

model_iiv_1 <- lm(Price_carton~Whole.Price,data = dat_new)

dat_new$fitted_Price_carton <- fitted(model_iiv_1)

model_iiv_11 <- gamlss(sjt~fitted_Price_carton+promotion,data = dat_new,sigma.formula=~pb(sjt-fitted_Price_carton-promotion),family = GU)
summary(model_iiv_11)


model_iiv_12 <- gamlss(sjt~fitted_Price_carton+factor(brand)+promotion,data = dat_new,sigma.formula=~pb(sjt-fitted_Price_carton-promotion),family = GU)
summary(model_iiv_12)

#可以迭代最多30次
con<-gamlss.control(n.cyc = 30)
model_iiv_13 <- gamlss(sjt~fitted_Price_carton+factor(store_product)+promotion,data = dat_new,sigma.formula=~pb(sjt-fitted_Price_carton-promotion),family = GU,control = con)
summary(model_iiv_13)


##################################################
# reestimate using other mean price instrument
tmp_dat <- dat_new %>% group_by(week) %>% summarize(Price_carton_sum=sum(Price_carton)) 

dat_new_2 <- dat_new %>% left_join(tmp_dat,by=c("week"="week"),
                                   keep=FALSE)
dat_new_2$other_Price_carton <- (dat_new_2$Price_carton_sum - dat_new_2$Price_carton) / 10
head(dat_new_2)


model_iiv_2 <- lm(Price_carton~other_Price_carton,data = dat_new_2)

dat_new_2$fitted_Price_carton <- fitted(model_iiv_2)

model_iiv_21 <- gamlss(sjt~fitted_Price_carton+promotion,data = dat_new_2,sigma.formula=~pb(sjt-fitted_Price_carton-promotion),family = GU)
summary(model_iiv_21)


model_iiv_22 <- gamlss(sjt~fitted_Price_carton+factor(brand)+promotion,data = dat_new_2,sigma.formula=~pb(sjt-fitted_Price_carton-promotion),family = GU)
summary(model_iiv_22)

con<-gamlss.control(n.cyc = 30)
model_iiv_23 <- gamlss(sjt~fitted_Price_carton+factor(store_product)+promotion,data = dat_new_2,sigma.formula=~pb(sjt-fitted_Price_carton-promotion),family = GU,control = con)
summary(model_iiv_23)


## (v)
# 
model_v_0 <- gamlss(log(Price_carton)~log(other_Price_carton),data = dat_new_2,sigma.formula=~pb(log(Price_carton)-log(other_Price_carton)),family = GU)
dat_new_2$log_fitted_Price_carton <- fitted(model_v_0)
pe <- c()
for (i in 1:48){
  model_v <- lm(log(sjt)~log_fitted_Price_carton+factor(brand),data = dat_new_2[dat_new_2$week==i,])
  pe <- c(pe,coef(model_v)[2])
}
median(pe)


## cross-price elasticities

sjt_Tylenol50 <- dat_new_2[dat_new_2$store_product=="Tylenol50",c("sjt","week")]
logp_Tylenol25 <- dat_new_2[dat_new_2$store_product=="Tylenol25",c("fitted_Price_carton","week")]
sl_50_25 <- sjt_Tylenol50  %>% left_join(logp_Tylenol25 )
model_v_21 <- gamlss(log(sjt)~fitted_Price_carton,data = sl_50_25,sigma.formula=~pb(log(sjt)-fitted_Price_carton),family = GU)
coef(model_v_21)[2]


sjt_Tylenol50 <- dat_new_2[dat_new_2$store_product=="Tylenol50",c("sjt","week")]
logp_Tylenol100 <- dat_new_2[dat_new_2$store_product=="Tylenol100",c("fitted_Price_carton","week")]
sl_50_100 <- sjt_Tylenol50  %>% left_join(logp_Tylenol100 )
model_v_22 <- gamlss(log(sjt)~fitted_Price_carton,data = sl_50_100,sigma.formula=~pb(log(sjt)-fitted_Price_carton),family = GU)
coef(model_v_22)[2]


sjt_Tylenol50 <- dat_new_2[dat_new_2$store_product=="Tylenol50",c("sjt","week")]
logp_Advil25 <- dat_new_2[dat_new_2$store_product=="Advil25",c("fitted_Price_carton","week")]
sl_50_Advil25 <- sjt_Tylenol50  %>% left_join(logp_Advil25)
model_v_23 <- gamlss(log(sjt)~fitted_Price_carton,data = sl_50_Advil25,sigma.formula=~pb(log(sjt)-fitted_Price_carton),family = GU)
coef(model_v_23)[2]

sjt_Tylenol50 <- dat_new_2[dat_new_2$store_product=="Tylenol50",c("sjt","week")]
logp_Advil50 <- dat_new_2[dat_new_2$store_product=="Advil50",c("fitted_Price_carton","week")]
sl_50_Advil50 <- sjt_Tylenol50  %>% left_join(logp_Advil50)
model_v_24 <- gamlss(log(sjt)~fitted_Price_carton,data = sl_50_Advil50,sigma.formula=~pb(log(sjt)-fitted_Price_carton),family = GU)
coef(model_v_24)[2]


sjt_Tylenol50 <- dat_new_2[dat_new_2$store_product=="Tylenol50",c("sjt","week")]
logp_Advil100 <- dat_new_2[dat_new_2$store_product=="Advil100",c("fitted_Price_carton","week")]
sl_50_Advil100 <- sjt_Tylenol50  %>% left_join(logp_Advil100)
model_v_25 <- gamlss(log(sjt)~fitted_Price_carton,data = sl_50_Advil100,sigma.formula=~pb(log(sjt)-fitted_Price_carton),family = GU)
coef(model_v_25)[2]
