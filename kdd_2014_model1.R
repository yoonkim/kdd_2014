require(gbm)
require(sqldf)
options(java.parameters = "-Xmx32g")
require(extraTrees)
my_cap<-function(v, l, h){
  v[v<l]<-l
  v[v>h]<-h
  return(v)
}
folder <- "-path"
setwd(folder)
outcomes_df <- read.csv("outcomes.csv")
projects_df <- read.csv("projects.csv")
resources_df <- read.csv("resources.csv")
teacher_acctid_exp_df <- read.csv("teacher_acctid_exp_20100401.csv")
schoolid_exp_df <- read.csv("schoolid_exp_20100401.csv")
school_district_exp_df <- read.csv("school_district_exp_20100401.csv")
school_city_exp_df <- read.csv("school_city_exp_20100401.csv")
school_county_exp_df <- read.csv("school_county_exp_20100401.csv")
school_zip_exp_df <- read.csv("school_zip_exp_20100401.csv")
school_state_exp_df <- read.csv("school_state_exp_20100401.csv")
text_preds_y_df <- read.csv("essays_pred_val_y.csv")
donor_teacher_acctid_df <- read.csv("donor_teacher_acctid.csv")
donor_teacher_acctid_df <- donor_teacher_acctid_df[,!(colnames(donor_teacher_acctid_df) %in% c("teacher_acctid_days","teacher_acctid_cnt", "teacher_acctid_cnt_train"))]
donor_schoolid_df <- read.csv("donor_schoolid.csv")
donor_schoolid_df <- donor_schoolid_df[,!(colnames(donor_schoolid_df) %in% c("schoolid_days","schoolid_cnt", "schoolid_cnt_train"))]
resources_df$item_price_total <- with(resources_df, item_unit_price*item_quantity)
resources_df$resource_type_mod <- with(resources_df, ifelse(project_resource_type=="","Other", as.character(project_resource_type)))
resources_books_df <- sqldf("select projectid, sum(1) as books_cnt, sum(item_price_total) as books_item_amount_total
                            from resources_df where resource_type_mod='Books' group by projectid")
resources_supplies_df <- sqldf("select projectid, sum(1) as supplies_cnt, sum(item_price_total) as supplies_item_amount_total
                            from resources_df where resource_type_mod='Supplies' group by projectid")
resources_trips_df <- sqldf("select projectid, sum(1) as trips_cnt, sum(item_price_total) as trips_item_amount_total
                            from resources_df where resource_type_mod='Trips' group by projectid")
resources_other_df <- sqldf("select projectid, sum(1) as other_cnt, sum(item_price_total) as other_item_amount_total
                            from resources_df where resource_type_mod='Other' group by projectid")
resources_tech_df <- sqldf("select projectid, sum(1) as tech_cnt, sum(item_price_total) as tech_item_amount_total
                            from resources_df where resource_type_mod='Technology' group by projectid")
resources_visitors_df <- sqldf("select projectid, sum(1) as visitors_cnt, sum(item_price_total) as visitors_item_amount_total
                            from resources_df where resource_type_mod='Visitors' group by projectid")
resources_agg_df <- sqldf("select projectid, sum(1) as agg_cnt, sum(item_price_total) as agg_item_amount_total
                          from resources_df group by projectid")

teacher_acctid_exp_df <- teacher_acctid_exp_df[,!(colnames(teacher_acctid_exp_df)=="date_posted")]
schoolid_exp_df <- schoolid_exp_df[,!(colnames(schoolid_exp_df)=="date_posted")]
school_city_exp_df <- school_city_exp_df[,!(colnames(school_city_exp_df)=="date_posted")]
school_county_exp_df <- school_county_exp_df[,!(colnames(school_county_exp_df)=="date_posted")]
school_district_exp_df <- school_district_exp_df[,!(colnames(school_district_exp_df)=="date_posted")]
school_zip_exp_df <- school_zip_exp_df[,!(colnames(school_zip_exp_df)=="date_posted")]
school_state_exp_df <- school_state_exp_df[,!(colnames(school_state_exp_df)=="date_posted")]
school_district_exp_df$school_district_y_prev <- as.numeric(as.character(school_district_exp_df$school_district_y_prev))
school_city_exp_df$school_city_y_prev <- as.numeric(as.character(school_city_exp_df$school_city_y_prev))

df <- merge(projects_df, outcomes_df, by.x="projectid", by.y="projectid", all.x=T)
df <- merge(df, teacher_acctid_exp_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, schoolid_exp_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, school_city_exp_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, school_county_exp_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, school_zip_exp_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, school_district_exp_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, school_state_exp_df, by.x="projectid",by.y="projectid", all.x=T)

replace_vecs <- c(45:144)
for (i in 1:length(replace_vecs)) {
  c <- replace_vecs[i]
  df[,c][is.na(df[,c])] <- 0
}

df <- merge(df, text_preds_y_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, donor_teacher_acctid_df, by.x="projectid", by.y="projectid", all.x=T)
df <- merge(df, donor_schoolid_df, by.x="projectid", by.y="projectid", all.x=T)
df <- merge(df, resources_agg_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, resources_books_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, resources_trips_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, resources_supplies_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, resources_other_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, resources_tech_df, by.x="projectid",by.y="projectid", all.x=T)
df <- merge(df, resources_visitors_df, by.x="projectid",by.y="projectid", all.x=T)

df$agg_cnt[is.na(df$agg_cnt)] <- 0
df$agg_item_amount_total[is.na(df$agg_item_amount_total)] <- 0
df$books_cnt[is.na(df$books_cnt)] <- 0
df$books_item_amount_total[is.na(df$books_item_amount_total)] <- 0
df$trips_item_amount_total <- as.numeric(df$trips_item_amount_total)
df$trips_cnt[is.na(df$trips_cnt)] <- 0
df$trips_item_amount_total[is.na(df$trips_item_amount_total)] <- 0
df$other_cnt[is.na(df$other_cnt)] <- 0
df$other_item_amount_total[is.na(df$other_item_amount_total)] <- 0
df$tech_cnt[is.na(df$tech_cnt)] <- 0
df$tech_item_amount_total[is.na(df$tech_item_amount_total)] <- 0
df$visitors_item_amount_total <- as.numeric(df$visitors_item_amount_total)
df$visitors_cnt[is.na(df$visitors_cnt)] <- 0
df$visitors_item_amount_total[is.na(df$visitors_item_amount_total)] <- 0
df$supplies_cnt[is.na(df$supplies_cnt)] <- 0
df$supplies_item_amount_total[is.na(df$supplies_item_amount_total)] <- 0

rm(outcomes_df)
rm(projects_df)
rm(resources_agg_df)
rm(resources_books_df)
rm(resources_other_df)
rm(resources_trips_df)
rm(resources_supplies_df)
rm(resources_tech_df)
rm(resources_visitors_df)
rm(resources_df)
rm(school_city_exp_df)
rm(school_county_exp_df)
rm(school_district_exp_df)
rm(school_zip_exp_df)
rm(schoolid_exp_df)
rm(school_state_exp_df)
rm(teacher_acctid_exp_df)
rm(text_preds_y_df)
rm(donor_schoolid_df)
rm(donor_teacher_acctid_df)
df$date_posted_posix <- as.POSIXlt(df$date_posted)
df$split <- "train"
df$split[df$date_posted_posix<as.POSIXlt("2011-01-01")] <- "none"
df$split[df$date_posted_posix>=as.POSIXlt("2013-09-01")] <- "val"
df$split[df$date_posted_posix>=as.POSIXlt("2014-01-01")] <- "test"
df$ref_date <- as.POSIXlt("2010-12-31")
df$time <- with(df, as.numeric(difftime(date_posted_posix, ref_date,unit="days")))
df$log_time <- log(df$time + 1)
  
df$schoolid_y_prev_rate <- with(df, ifelse(schoolid_cnt==0,0,schoolid_y_prev/schoolid_cnt))
df$teacher_acctid_y_prev_rate <- with(df, ifelse(teacher_acctid_cnt==0,0,teacher_acctid_y_prev/teacher_acctid_cnt))
df$school_city_y_prev_rate <- with(df, ifelse(school_city_cnt==0,0,school_city_y_prev/school_city_cnt))
df$school_zip_y_prev_rate <- with(df, ifelse(school_zip_cnt==0,0,school_zip_y_prev/school_zip_cnt))
df$school_district_y_prev_rate <- with(df, ifelse(school_district_cnt==0,0,school_district_y_prev/school_district_cnt))
df$school_county_y_prev_rate <- with(df, ifelse(school_county_cnt==0,0,school_county_y_prev/school_county_cnt))
df$school_state_y_prev_rate <- with(df, ifelse(school_state_cnt==0,0,school_state_y_prev/school_state_cnt))

df$schoolid_y2_prev_rate <- with(df, ifelse(schoolid_cnt==0,0,schoolid_y2_prev/schoolid_cnt))
df$teacher_acctid_y2_prev_rate <- with(df, ifelse(teacher_acctid_cnt==0,0,teacher_acctid_y2_prev/teacher_acctid_cnt))
df$school_city_y2_prev_rate <- with(df, ifelse(school_city_cnt==0,0,school_city_y2_prev/school_city_cnt))
df$school_zip_y2_prev_rate <- with(df, ifelse(school_zip_cnt==0,0,school_zip_y2_prev/school_zip_cnt))
df$school_district_y2_prev_rate <- with(df, ifelse(school_district_cnt==0,0,school_district_y2_prev/school_district_cnt))
df$school_county_y2_prev_rate <- with(df, ifelse(school_county_cnt==0,0,school_county_y2_prev/school_county_cnt))
df$school_state_y2_prev_rate <- with(df, ifelse(school_state_cnt==0,0,school_state_y2_prev/school_state_cnt))

df$schoolid_y3_prev_rate <- with(df, ifelse(schoolid_cnt==0,0,schoolid_y3_prev/schoolid_cnt))
df$teacher_acctid_y3_prev_rate <- with(df, ifelse(teacher_acctid_cnt==0,0,teacher_acctid_y3_prev/teacher_acctid_cnt))
df$school_city_y3_prev_rate <- with(df, ifelse(school_city_cnt==0,0,school_city_y3_prev/school_city_cnt))
df$school_zip_y3_prev_rate <- with(df, ifelse(school_zip_cnt==0,0,school_zip_y3_prev/school_zip_cnt))
df$school_district_y3_prev_rate <- with(df, ifelse(school_district_cnt==0,0,school_district_y3_prev/school_district_cnt))
df$school_county_y3_prev_rate <- with(df, ifelse(school_county_cnt==0,0,school_county_y3_prev/school_county_cnt))
df$school_state_y3_prev_rate <- with(df, ifelse(school_state_cnt==0,0,school_state_y3_prev/school_state_cnt))

df$schoolid_y4_prev_rate <- with(df, ifelse(schoolid_cnt==0,0,schoolid_y4_prev/schoolid_cnt))
df$teacher_acctid_y4_prev_rate <- with(df, ifelse(teacher_acctid_cnt==0,0,teacher_acctid_y4_prev/teacher_acctid_cnt))
df$school_city_y4_prev_rate <- with(df, ifelse(school_city_cnt==0,0,school_city_y4_prev/school_city_cnt))
df$school_zip_y4_prev_rate <- with(df, ifelse(school_zip_cnt==0,0,school_zip_y4_prev/school_zip_cnt))
df$school_district_y4_prev_rate <- with(df, ifelse(school_district_cnt==0,0,school_district_y4_prev/school_district_cnt))
df$school_county_y4_prev_rate <- with(df, ifelse(school_county_cnt==0,0,school_county_y4_prev/school_county_cnt))
df$school_state_y4_prev_rate <- with(df, ifelse(school_state_cnt==0,0,school_state_y4_prev/school_state_cnt))

df$schoolid_y5_prev_rate <- with(df, ifelse(schoolid_cnt==0,0,schoolid_y5_prev/schoolid_cnt))
df$teacher_acctid_y5_prev_rate <- with(df, ifelse(teacher_acctid_cnt==0,0,teacher_acctid_y5_prev/teacher_acctid_cnt))
df$school_city_y5_prev_rate <- with(df, ifelse(school_city_cnt==0,0,school_city_y5_prev/school_city_cnt))
df$school_zip_y5_prev_rate <- with(df, ifelse(school_zip_cnt==0,0,school_zip_y5_prev/school_zip_cnt))
df$school_district_y5_prev_rate <- with(df, ifelse(school_district_cnt==0,0,school_district_y5_prev/school_district_cnt))
df$school_county_y5_prev_rate <- with(df, ifelse(school_county_cnt==0,0,school_county_y5_prev/school_county_cnt))
df$school_state_y5_prev_rate <- with(df, ifelse(school_state_cnt==0,0,school_state_y5_prev/school_state_cnt))

df$schoolid_y6_prev_rate <- with(df, ifelse(schoolid_cnt==0,0,schoolid_y6_prev/schoolid_cnt))
df$teacher_acctid_y6_prev_rate <- with(df, ifelse(teacher_acctid_cnt==0,0,teacher_acctid_y6_prev/teacher_acctid_cnt))
df$school_city_y6_prev_rate <- with(df, ifelse(school_city_cnt==0,0,school_city_y6_prev/school_city_cnt))
df$school_zip_y6_prev_rate <- with(df, ifelse(school_zip_cnt==0,0,school_zip_y6_prev/school_zip_cnt))
df$school_district_y6_prev_rate <- with(df, ifelse(school_district_cnt==0,0,school_district_y6_prev/school_district_cnt))
df$school_county_y6_prev_rate <- with(df, ifelse(school_county_cnt==0,0,school_county_y6_prev/school_county_cnt))
df$school_state_y6_prev_rate <- with(df, ifelse(school_state_cnt==0,0,school_state_y6_prev/school_state_cnt))

df$schoolid_y7_prev_rate <- with(df, ifelse(schoolid_cnt==0,0,schoolid_y7_prev/schoolid_cnt))
df$teacher_acctid_y7_prev_rate <- with(df, ifelse(teacher_acctid_cnt==0,0,teacher_acctid_y7_prev/teacher_acctid_cnt))
df$school_city_y7_prev_rate <- with(df, ifelse(school_city_cnt==0,0,school_city_y7_prev/school_city_cnt))
df$school_zip_y7_prev_rate <- with(df, ifelse(school_zip_cnt==0,0,school_zip_y7_prev/school_zip_cnt))
df$school_district_y7_prev_rate <- with(df, ifelse(school_district_cnt==0,0,school_district_y7_prev/school_district_cnt))
df$school_county_y7_prev_rate <- with(df, ifelse(school_county_cnt==0,0,school_county_y7_prev/school_county_cnt))
df$school_state_y7_prev_rate <- with(df, ifelse(school_state_cnt==0,0,school_state_y7_prev/school_state_cnt))

df$schoolid_y8_prev_rate <- with(df, ifelse(schoolid_cnt==0,0,schoolid_y8_prev/schoolid_cnt))
df$teacher_acctid_y8_prev_rate <- with(df, ifelse(teacher_acctid_cnt==0,0,teacher_acctid_y8_prev/teacher_acctid_cnt))
df$school_city_y8_prev_rate <- with(df, ifelse(school_city_cnt==0,0,school_city_y8_prev/school_city_cnt))
df$school_zip_y8_prev_rate <- with(df, ifelse(school_zip_cnt==0,0,school_zip_y8_prev/school_zip_cnt))
df$school_district_y8_prev_rate <- with(df, ifelse(school_district_cnt==0,0,school_district_y8_prev/school_district_cnt))
df$school_county_y8_prev_rate <- with(df, ifelse(school_county_cnt==0,0,school_county_y8_prev/school_county_cnt))
df$school_state_y8_prev_rate <- with(df, ifelse(school_state_cnt==0,0,school_state_y8_prev/school_state_cnt))

df$schoolid_y9_prev_rate <- with(df, ifelse(schoolid_cnt==0,0,schoolid_y9_prev/schoolid_cnt))
df$teacher_acctid_y9_prev_rate <- with(df, ifelse(teacher_acctid_cnt==0,0,teacher_acctid_y9_prev/teacher_acctid_cnt))
df$school_city_y9_prev_rate <- with(df, ifelse(school_city_cnt==0,0,school_city_y9_prev/school_city_cnt))
df$school_zip_y9_prev_rate <- with(df, ifelse(school_zip_cnt==0,0,school_zip_y9_prev/school_zip_cnt))
df$school_district_y9_prev_rate <- with(df, ifelse(school_district_cnt==0,0,school_district_y9_prev/school_district_cnt))
df$school_county_y9_prev_rate <- with(df, ifelse(school_county_cnt==0,0,school_county_y9_prev/school_county_cnt))
df$school_state_y9_prev_rate <- with(df, ifelse(school_state_cnt==0,0,school_state_y9_prev/school_state_cnt))

df$schoolid_y10_prev_rate <- with(df, ifelse(schoolid_cnt==0,0,schoolid_y10_prev/schoolid_cnt))
df$teacher_acctid_y10_prev_rate <- with(df, ifelse(teacher_acctid_cnt==0,0,teacher_acctid_y10_prev/teacher_acctid_cnt))
df$school_city_y10_prev_rate <- with(df, ifelse(school_city_cnt==0,0,school_city_y10_prev/school_city_cnt))
df$school_zip_y10_prev_rate <- with(df, ifelse(school_zip_cnt==0,0,school_zip_y10_prev/school_zip_cnt))
df$school_district_y10_prev_rate <- with(df, ifelse(school_district_cnt==0,0,school_district_y10_prev/school_district_cnt))
df$school_county_y10_prev_rate <- with(df, ifelse(school_county_cnt==0,0,school_county_y10_prev/school_county_cnt))
df$school_state_y10_prev_rate <- with(df, ifelse(school_state_cnt==0,0,school_state_y10_prev/school_state_cnt))


df$y <- 0
df$y[df$is_exciting=="t"] <- 1
df$y2 <- 0
df$y2[df$at_least_1_teacher_referred_donor=="t"] <- 1
df$y3 <- 0
df$y3[df$great_chat=="t"] <- 1
df$y4 <- 0
df$y4[df$fully_funded=="t"] <- 1
df$y5 <- 0
df$y5[df$at_least_1_green_donation=="t"] <- 1
df$y6 <- 0
df$y6[df$donation_from_thoughtful_donor=="t"] <- 1
df$y7 <- 0
df$y7[df$three_or_more_non_teacher_referred_donors=="t"] <- 1
df$y8 <- 0
df$y8[df$one_non_teacher_referred_donor_giving_100_plus=="t"] <- 1
df$y9 <- 0
df$y9[df$teacher_referred_count>=1] <- 1
df$y10 <- 0
df$y10[df$non_teacher_referred_count>=1] <- 1

df$poverty <- 0
df$poverty[df$poverty_level=="moderate poverty"] <- 1
df$poverty[df$poverty_level=="high poverty"] <- 2
df$poverty[df$poverty_level=="highest poverty"] <- 3
df$grade_level_num <- 0
df$grade_level_num[df$grade_level=="Grades 3-5"] <- 1
df$grade_level_num[df$grade_level=="Grades 6-8"] <- 2
df$grade_level_num[df$grade_level=="Grades 9-12"] <- 3
df$weekday <- weekdays(df$date_posted_posix)
df$weekday <- as.factor(df$weekday)

df$title_y_pp <- with(df, ifelse(title_y_pred_partial==0 | is.na(title_y_pred_partial), title_y_pred, title_y_pred_partial))
df$essay_y_pp <- with(df, ifelse(essay_y_pred_partial==0 | is.na(essay_y_pred_partial), essay_y_pred, essay_y_pred_partial))
df$need_statement_y_pp <- with(df, ifelse(need_statement_y_pred_partial==0 | is.na(need_statement_y_pred_partial), need_statement_y_pred, need_statement_y_pred_partial))
df$short_description_y_pp <- with(df, ifelse(short_description_y_pred_partial==0 | is.na(short_description_y_pred_partial), short_description_y_pred, short_description_y_pred_partial))

df$school_district_days <- as.numeric(df$school_district_days)
df$price_per_student <- with(df, total_price_excluding_optional_support/students_reached)

df <- df[order(df$time),]
cred_k <- 10
split <- df$split=="train"|df$split=="val"
df$pred0 <- mean(df$y[split])
df$schoolid_y_prev_rate_cred <- with(df, (schoolid_y_prev+cred_k*pred0)/(schoolid_cnt+cred_k))
df$teacher_acctid_y_prev_rate_cred <- with(df, (teacher_acctid_y_prev+cred_k*pred0)/(teacher_acctid_cnt+cred_k))
df$schoolid_y_prev_rate_cred_cap <- with(df, my_cap(schoolid_y_prev_rate_cred, 0,0.25))

df$students_reached_imp <- with(df,ifelse(is.na(students_reached),0,students_reached))
df$price_per_student_imp <- with(df,ifelse(is.na(price_per_student),0,price_per_student))

kdd_gbm_v1 <- gbm(y~school_latitude+
                 school_longitude+
                 school_metro+
                 teacher_prefix+
                 teacher_teach_for_america+
                 teacher_ny_teaching_fellow+
                 total_price_excluding_optional_support+
                 students_reached_imp+
                 eligible_double_your_impact_match+
                 eligible_almost_home_match+
                 poverty_level+
                 primary_focus_area+
                 resource_type+
                 essay_length+
                 title_length+
                 title_y_pp+
                 essay_y_pp+                      
                 agg_item_amount_total+
                 agg_cnt+                 
                 books_cnt+
                 books_item_amount_total+
                 supplies_item_amount_total+
                 other_item_amount_total+
                 tech_item_amount_total+
                 weekday+
                 teacher_acctid_days+
                 schoolid_days+
                 teacher_acctid_cnt+
                 schoolid_cnt+
                 teacher_acctid_y_prev_rate_cred+
                 schoolid_y_prev_rate_cred_cap+ 
                 school_district_y_prev_rate+                    
                 school_county_y_prev_rate+
                 schoolid_y2_prev_rate+                 
                 teacher_acctid_y2_prev_rate+                            
                 schoolid_y3_prev_rate+                 
                 teacher_acctid_y3_prev_rate+                            
                 school_district_y3_prev_rate+
                 schoolid_y4_prev_rate+                                        
                 schoolid_y5_prev_rate+                 
                 teacher_acctid_y5_prev_rate+
                 price_per_student_imp+
                 schoolid_y11_prev+
                 teacher_acctid_y11_prev+
                 schoolid_y12_prev+
                 teacher_acctid_y12_prev+
                 schoolid_y13_prev+
                 teacher_acctid_y13_prev,                
                        ,
               data = df[split,],
               train.fraction = 1,
               distribution="bernoulli",
               interaction.depth=7,
               n.minobsinnode = 750,
               shrinkage = 0.1,
               bag.fraction = 0.5,
               verbose=T,
               n.trees = 650)
df$pred_v1<- predict(kdd_gbm_v1, newdata=df, n.trees=650, type="response")

df$date_posted_string <- as.character(df$date_posted)
tmp1 <- df[df$date_posted_posix >= as.POSIXlt("2010-09-01"),c("projectid", "date_posted_string","pred_v1")]
tmp1 <- sqldf("select date_posted_string, sum(1) as date_posted_cnt, sum(pred_v1) as sum_pred_v1 from tmp1 group by date_posted_string")
pred_sum <- filter(tmp1$sum_pred_v1,rep(1,14),sides=1)
cnt_sum <- filter(tmp1$date_posted_cnt,rep(1,14),sides=1)
tmp1$avg_biweekly_pred_v1 <- pred_sum/cnt_sum
tmp1$avg_biweekly_pred_v1[2:1350] <- tmp1$avg_biweekly_pred_v1[1:1349]
pred_sum <- filter(tmp1$sum_pred_v1,rep(1,30),sides=1)
cnt_sum <- filter(tmp1$date_posted_cnt,rep(1,30),sides=1)
tmp1$avg_monthly_pred_v1 <- pred_sum/cnt_sum
tmp1$avg_monthly_pred_v1[2:1350] <- tmp1$avg_monthly_pred_v1[1:1349]
pred_sum <- filter(tmp1$sum_pred_v1,rep(1,61),sides=1)
cnt_sum <- filter(tmp1$date_posted_cnt,rep(1,61),sides=1)
tmp1$avg_bimonthly_pred_v1 <- pred_sum/cnt_sum
tmp1$avg_bimonthly_pred_v1[2:1350] <- tmp1$avg_bimonthly_pred_v1[1:1349]
df <- merge(df, tmp1, by.x="date_posted_string",by.y="date_posted_string",all.x=T)
rm(tmp1)
df <- df[order(df$time),]

kdd_gbm_v2 <- gbm(y~school_latitude+
                    school_longitude+
                    school_metro+
                    teacher_prefix+
                    teacher_teach_for_america+
                    teacher_ny_teaching_fellow+
                    total_price_excluding_optional_support+
                    students_reached_imp+
                    eligible_double_your_impact_match+
                    eligible_almost_home_match+
                    poverty_level+
                    primary_focus_area+
                    resource_type+
                    essay_length+
                    title_length+
                    title_y_pp+
                    essay_y_pp+                      
                    agg_item_amount_total+
                    agg_cnt+                 
                    books_cnt+
                    books_item_amount_total+
                    supplies_item_amount_total+
                    other_item_amount_total+
                    tech_item_amount_total+
                    weekday+
                    teacher_acctid_days+
                    schoolid_days+
                    teacher_acctid_cnt+
                    schoolid_cnt+
                    teacher_acctid_y_prev_rate_cred+
                    schoolid_y_prev_rate_cred_cap+ 
                    school_district_y_prev_rate+                    
                    school_county_y_prev_rate+
                    schoolid_y2_prev_rate+                 
                    teacher_acctid_y2_prev_rate+                            
                    schoolid_y3_prev_rate+                 
                    teacher_acctid_y3_prev_rate+                            
                    school_district_y3_prev_rate+
                    schoolid_y4_prev_rate+                                        
                    schoolid_y5_prev_rate+                 
                    teacher_acctid_y5_prev_rate+
                    price_per_student_imp+
                    schoolid_y11_prev+
                    teacher_acctid_y11_prev+
                    schoolid_y12_prev+
                    teacher_acctid_y12_prev+
                    schoolid_y13_prev+
                    teacher_acctid_y13_prev+
                    avg_monthly_pred_v1,                
                  ,
                  data = df[split,],
                  train.fraction = 1,
                  distribution="bernoulli",
                  interaction.depth=7,
                  n.minobsinnode = 750,
                  shrinkage = 0.1,
                  bag.fraction = 0.5,
                  verbose=T,
                  n.trees = 600)
df$pred_v2<- predict(kdd_gbm_v2, newdata=df, n.trees=600, type="response")

kdd_gbm_v4 <- gbm(y~school_latitude+
                    school_longitude+
                    school_metro+
                    teacher_prefix+
                    teacher_teach_for_america+
                    teacher_ny_teaching_fellow+
                    total_price_excluding_optional_support+
                    students_reached_imp+
                    eligible_double_your_impact_match+
                    eligible_almost_home_match+
                    poverty_level+
                    primary_focus_area+
                    resource_type+
                    essay_length+
                    title_length+
                    title_y_pp+
                    essay_y_pp+                      
                    agg_item_amount_total+
                    agg_cnt+                 
                    books_cnt+
                    books_item_amount_total+
                    supplies_item_amount_total+
                    other_item_amount_total+
                    tech_item_amount_total+
                    weekday+
                    teacher_acctid_days+
                    schoolid_days+
                    teacher_acctid_cnt+
                    schoolid_cnt+
                    teacher_acctid_y_prev_rate_cred+
                    schoolid_y_prev_rate_cred_cap+ 
                    school_district_y_prev_rate+                    
                    school_county_y_prev_rate+
                    schoolid_y2_prev_rate+                 
                    teacher_acctid_y2_prev_rate+                            
                    schoolid_y3_prev_rate+                 
                    teacher_acctid_y3_prev_rate+                            
                    school_district_y3_prev_rate+
                    schoolid_y4_prev_rate+                                        
                    schoolid_y5_prev_rate+                 
                    teacher_acctid_y5_prev_rate+
                    price_per_student_imp+
                    schoolid_y11_prev+
                    teacher_acctid_y11_prev+
                    schoolid_y12_prev+
                    teacher_acctid_y12_prev+
                    schoolid_y13_prev+
                    teacher_acctid_y13_prev+
                    avg_biweekly_pred_v1,                
                  ,
                  data = df[split,],
                  train.fraction = 1,
                  distribution="bernoulli",
                  interaction.depth=7,
                  n.minobsinnode = 750,
                  shrinkage = 0.1,
                  bag.fraction = 0.5,
                  verbose=T,
                  n.trees = 600)
df$pred_v4<- predict(kdd_gbm_v4, newdata=df, n.trees=600, type="response")

kdd_gbm_v5 <- gbm(y~school_latitude+
                    school_longitude+
                    school_metro+
                    teacher_prefix+
                    teacher_teach_for_america+
                    teacher_ny_teaching_fellow+
                    total_price_excluding_optional_support+
                    students_reached_imp+
                    eligible_double_your_impact_match+
                    eligible_almost_home_match+
                    poverty_level+
                    primary_focus_area+
                    resource_type+
                    essay_length+
                    title_length+
                    title_y_pp+
                    essay_y_pp+                      
                    agg_item_amount_total+
                    agg_cnt+                 
                    books_cnt+
                    books_item_amount_total+
                    supplies_item_amount_total+
                    other_item_amount_total+
                    tech_item_amount_total+
                    weekday+
                    teacher_acctid_days+
                    schoolid_days+
                    teacher_acctid_cnt+
                    schoolid_cnt+
                    teacher_acctid_y_prev_rate_cred+
                    schoolid_y_prev_rate_cred_cap+ 
                    school_district_y_prev_rate+                    
                    school_county_y_prev_rate+
                    schoolid_y2_prev_rate+                 
                    teacher_acctid_y2_prev_rate+                            
                    schoolid_y3_prev_rate+                 
                    teacher_acctid_y3_prev_rate+                            
                    school_district_y3_prev_rate+
                    schoolid_y4_prev_rate+                                        
                    schoolid_y5_prev_rate+                 
                    teacher_acctid_y5_prev_rate+
                    price_per_student_imp+
                    schoolid_y11_prev+
                    teacher_acctid_y11_prev+
                    schoolid_y12_prev+
                    teacher_acctid_y12_prev+
                    schoolid_y13_prev+
                    teacher_acctid_y13_prev+
                    avg_bimonthly_pred_v1,                
                  ,
                  data = df[split,],
                  train.fraction = 1,
                  distribution="bernoulli",
                  interaction.depth=7,
                  n.minobsinnode = 750,
                  shrinkage = 0.1,
                  bag.fraction = 0.5,
                  verbose=T,
                  n.trees = 600)
df$pred_v5<- predict(kdd_gbm_v5, newdata=df, n.trees=600, type="response")

x <- model.matrix(~ school_metro+
                    teacher_prefix+
                    teacher_teach_for_america+
                    students_reached_imp+
                    price_per_student_imp+
                    teacher_ny_teaching_fellow+
                    total_price_excluding_optional_support+
                    eligible_double_your_impact_match+
                    eligible_almost_home_match+
                    poverty_level+
                    primary_focus_area+
                    resource_type+
                    essay_length+
                    title_length+
                    title_y_pp+
                    essay_y_pp+                      
                    agg_item_amount_total+
                    agg_cnt+                 
                    books_cnt+
                    books_item_amount_total+
                    supplies_item_amount_total+
                    other_item_amount_total+
                    tech_item_amount_total+
                    weekday+
                    teacher_acctid_cnt+
                    schoolid_cnt+
                    teacher_acctid_y_prev_rate_cred+
                    schoolid_y_prev_rate_cred_cap+ 
                    school_district_y_prev_rate+                    
                    school_county_y_prev_rate+
                    schoolid_y2_prev_rate+                 
                    teacher_acctid_y2_prev_rate+                            
                    schoolid_y3_prev_rate+                 
                    teacher_acctid_y3_prev_rate+                            
                    school_district_y3_prev_rate+
                    schoolid_y4_prev_rate+                                        
                    schoolid_y5_prev_rate+                 
                    teacher_acctid_y5_prev_rate+
                    schoolid_y11_prev+
                    teacher_acctid_y11_prev+
                    schoolid_y12_prev+
                    teacher_acctid_y12_prev+
                    schoolid_y13_prev+
                    teacher_acctid_y13_prev+
                    avg_biweekly_pred_v1, 
                    data=df[split,])
y <- df$y[split]

x_test <- model.matrix(~school_metro+
                        teacher_prefix+
                        teacher_teach_for_america+
                        students_reached_imp+
                        price_per_student_imp+
                        teacher_ny_teaching_fellow+
                        total_price_excluding_optional_support+
                        eligible_double_your_impact_match+
                        eligible_almost_home_match+
                        poverty_level+
                        primary_focus_area+
                        resource_type+
                        essay_length+
                        title_length+
                        title_y_pp+
                        essay_y_pp+                      
                        agg_item_amount_total+
                        agg_cnt+                 
                        books_cnt+
                        books_item_amount_total+
                        supplies_item_amount_total+
                        other_item_amount_total+
                        tech_item_amount_total+
                        weekday+
                        teacher_acctid_cnt+
                        schoolid_cnt+
                        teacher_acctid_y_prev_rate_cred+
                        schoolid_y_prev_rate_cred_cap+ 
                        school_district_y_prev_rate+                    
                        school_county_y_prev_rate+
                        schoolid_y2_prev_rate+                 
                        teacher_acctid_y2_prev_rate+                            
                        schoolid_y3_prev_rate+                 
                        teacher_acctid_y3_prev_rate+                            
                        school_district_y3_prev_rate+
                        schoolid_y4_prev_rate+                                        
                        schoolid_y5_prev_rate+                 
                        teacher_acctid_y5_prev_rate+
                        schoolid_y11_prev+
                        teacher_acctid_y11_prev+
                        schoolid_y12_prev+
                        teacher_acctid_y12_prev+
                        schoolid_y13_prev+
                        teacher_acctid_y13_prev+
                        avg_biweekly_pred_v1, data=df[df$split=="test",])
kdd_et_v4 <- extraTrees(x,y,ntree=2000,numThreads=12, nodesize=50, mtry=2)
pred_et_v4 <- predict(kdd_et_v4, newdata = x_test)

df$ref_date <- as.POSIXlt("2014-05-12")
df$time_till_end <- with(df, as.numeric(difftime(ref_date, date_posted_posix,unit="days")))
lower_t <- 0.5 
slope <- (1-lower_t)/131
df$discount <- with(df, time_till_end*slope + lower_t)

df_test <- df[df$split=="test",c("projectid","pred_v1","pred_v2","pred_v4","pred_v5","discount")]
df_test$pred_et_v4 <- pred_et_v4
df_test$is_exciting <- with(df_test, 0.1*pred_v1+0.1*pred_v2+0.45*pred_v4+0.1*pred_v5 + 0.25*pred_et_v4)
write.csv(df_test[,c("projectid","is_exciting","discount")],"model1.csv",row.names=F)