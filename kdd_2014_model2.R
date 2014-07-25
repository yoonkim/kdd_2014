options(java.parameters="-Xmx40g")
require(extraTrees)
require(sqldf)
require(gbm)
require(plyr)
require(dplyr)
require(randomForest)
require(glmnet)

mylift<-function(orderby, pred, actual, w, n) {
  if (length(w)==0) {
    w<-rep(1.0, length(pred))
  }
  v<-data.frame(o=orderby, p=pred, a=actual, w=w)
  v<-v[order(v$o),]
  v2<-v[order(v$a),]
  v$cumm_w<-cumsum(v$w)
  v$cumm_y<-cumsum(v$w*v$a)
  total_w<-sum(v$w)
  gini<-with(v,1-2* sum(cumm_y*w)/(sum(a*w)*total_w))
  print(paste("gini=", with(v,1-2* sum(cumm_y*w)/(sum(a*w)*total_w))))
  v$pidx<-round(v$cumm_w*n/total_w+0.5)
  v$pidx[v$pidx>n]<-n
  v$pidx[v$pidx<1]<-1
  
  sum1<-sqldf("select pidx, sum(w) as w, min(o) as min, max(o) as max, sum(o*w)/sum(w) as o, sum(p*w)/sum(w) as p, sum(a*w)/sum(w) as a, min(a) as min_a, max(a) as max_a, sum(a*w) as a_cnt from v group by pidx")
  sum1$err<-with(sum1, a/p)
  print(sum1)
  with(sum1,plot(p, type='b', ylim=c(min(c(p, a)), max(c(p, a))), col="blue"))
  lines(sum1$a, col="red")
  sum1
  
  v2$cumm_w<-cumsum(v2$w)
  v2$cumm_y<-cumsum(v2$w*v2$a)
  max_gini<-with(v2,1-2*sum(cumm_y*w)/(sum(a*w)*total_w))
  normalized_gini<-gini/max_gini
  result<-c(gini,max_gini,normalized_gini)
  return(result)
}

myleaveoneout<-function(d,y,x1,x2=NULL,x3=NULL,x4=NULL,x5=NULL,x6=NULL,x7=NULL,p,filter,k,r=0.3) {
  d1<-d[,c(y,x1,x2,x3,x4,x5,x6,x7,p)]
  if (!is.null(x1)&!is.null(x2)&!is.null(x3)&!is.null(x4)&!is.null(x5)&!is.null(x6)&!is.null(x7)) {
    names(d1)<-c('y','x1','x2','x3','x4','x5','x6','x7','p')
    d1$filter<-filter
    c<-summarize(group_by(filter(d1, filter==TRUE&!is.na(y)), x1, x2, x3, x4, x5, x6, x7),count_x=n(),sum_y=sum(y,na.rm=TRUE), sum_p=sum(p,na.rm=TRUE))
    d1<-left_join(d1,c,by=c('x1','x2','x3','x4','x5','x6','x7'))
  } else if (!is.null(x1)&!is.null(x2)&!is.null(x3)&!is.null(x4)&!is.null(x5)&!is.null(x6)) {
    names(d1)<-c('y','x1','x2','x3','x4','x5','x6','p')
    d1$filter<-filter
    c<-summarize(group_by(filter(d1, filter==TRUE&!is.na(y)), x1, x2, x3, x4, x5, x6),count_x=n(),sum_y=sum(y,na.rm=TRUE), sum_p=sum(p,na.rm=TRUE))
    d1<-left_join(d1,c,by=c('x1','x2','x3','x4','x5','x6'))
  } else if (!is.null(x1)&!is.null(x2)&!is.null(x3)&!is.null(x4)&!is.null(x5)) {
    names(d1)<-c('y','x1','x2','x3','x4','x5','p')
    d1$filter<-filter
    c<-summarize(group_by(filter(d1, filter==TRUE&!is.na(y)), x1, x2, x3, x4, x5),count_x=n(),sum_y=sum(y,na.rm=TRUE), sum_p=sum(p,na.rm=TRUE))
    d1<-left_join(d1,c,by=c('x1','x2','x3','x4','x5'))
  } else if (!is.null(x1)&!is.null(x2)&!is.null(x3)&!is.null(x4)) {
    names(d1)<-c('y','x1','x2','x3','x4','p')
    d1$filter<-filter
    c<-summarize(group_by(filter(d1, filter==TRUE&!is.na(y)), x1, x2, x3, x4),count_x=n(),sum_y=sum(y,na.rm=TRUE), sum_p=sum(p,na.rm=TRUE))
    d1<-left_join(d1,c,by=c('x1','x2','x3','x4'))
  } else if (!is.null(x1)&!is.null(x2)&!is.null(x3)) {
    names(d1)<-c('y','x1','x2','x3','p')
    d1$filter<-filter
    c<-summarize(group_by(filter(d1, filter==TRUE&!is.na(y)), x1, x2, x3),count_x=n(),sum_y=sum(y,na.rm=TRUE), sum_p=sum(p,na.rm=TRUE))
    d1<-left_join(d1,c,by=c('x1','x2','x3'))
  } else if (!is.null(x1)&!is.null(x2)) {
    names(d1)<-c('y','x1','x2','p')
    d1$filter<-filter
    c<-summarize(group_by(filter(d1, filter==TRUE&!is.na(y)), x1, x2),count_x=n(),sum_y=sum(y,na.rm=TRUE), sum_p=sum(p,na.rm=TRUE))
    d1<-left_join(d1,c,by=c('x1','x2'))
  } else if (!is.null(x1)) {
    names(d1)<-c('y','x1','p')
    d1$filter<-filter
    c<-summarize(group_by(filter(d1, filter==TRUE&!is.na(y)), x1),count_x=n(),sum_y=sum(y,na.rm=TRUE), sum_p=sum(p,na.rm=TRUE))
    d1<-left_join(d1,c,by=c('x1'))
  } else {
    stop("Incorrect X variables")
  }
  d1$count_x[is.na(d1$count_x)]<-0
  d1$sum_y[is.na(d1$sum_y)]<-0
  d1$avg_p<-with(d1, sum_p/count_x)
  d1$avg_p[is.na(d1$avg_p)]<-0
  d1$count_x[filter]<-d1$count_x[filter]-1
  d1$sum_y[filter]<-d1$sum_y[filter]-d1$y[filter]
  d1$exp_y<-with(d1, sum_y/count_x)
  d1$adj_y<-with(d1, (sum_y+p*k)/(count_x+k))
  d1$exp_y[is.na(d1$exp_y)]<-d1$p[is.na(d1$exp_y)]
  d1$adj_y[is.na(d1$adj_y)]<-d1$p[is.na(d1$adj_y)]
  #  set.seed(seed)
  d1$adj_y[filter]<-d1$adj_y[filter]*(1+(runif(sum(filter))-0.5)*r)
  return(d1)
}

folder <- "-path"
setwd(folder)
donations<-read.csv("donations.csv")
outcomes<-read.csv("outcomes.csv")
resources<-read.csv("resources.csv")
projects<-read.csv("projects.csv")
public_school<-read.csv("ELSI_public_school.csv")
names(public_school)[3]<-'school_ncesid'
public_school$school_ncesid<-as.factor(public_school$school_ncesid)

d<-left_join(projects,outcomes,by='projectid')
d$date_posted<-as.Date(d$date_posted)
d$y<-with(d,ifelse(is_exciting=='t',1,0))
d$at_least_1_teacher_referred_donor<-with(d,ifelse(at_least_1_teacher_referred_donor=='t',1,ifelse(at_least_1_teacher_referred_donor=='f',0,NA)))
d$fully_funded<-with(d,ifelse(fully_funded=='t',1,0))
d$at_least_1_green_donation<-with(d,ifelse(at_least_1_green_donation=='t',1,ifelse(at_least_1_green_donation=='f',0,NA)))
d$great_chat<-with(d,ifelse(great_chat=='t',1,0))
d$three_or_more_non_teacher_referred_donors<-with(d,ifelse(three_or_more_non_teacher_referred_donors=='t',1,ifelse(three_or_more_non_teacher_referred_donors=='f',0,NA)))
d$one_non_teacher_referred_donor_giving_100_plus<-with(d,ifelse(one_non_teacher_referred_donor_giving_100_plus=='t',1,ifelse(one_non_teacher_referred_donor_giving_100_plus=='f',0,NA)))
d$donation_from_thoughtful_donor<-with(d,ifelse(donation_from_thoughtful_donor=='t',1,ifelse(donation_from_thoughtful_donor=='f',0,NA)))
d$test<-0
d$test[d$date_posted>=as.Date('2014-01-01','%Y-%m-%d')]<-1
d$validation<-0
d$validation[d$date_posted>=as.Date('2013-07-01','%Y-%m-%d')]<-1
d$last_date_posted<-max(d$date_posted)
d2<-d[d$date_posted>=as.Date('2010-04-01','%Y-%m-%d'),]
d2$ym<-(as.POSIXlt(d2$date_posted)$year+1900)*100+as.POSIXlt(d2$date_posted)$mon+1
d2$row.number<-1:length(d2$projectid)

tmp<-select(d2,projectid,validation,test,y)
write.csv(tmp,file="projectlist.csv",row.names=FALSE,quote=FALSE)

# run python files

count<-read.csv("count.csv")
result_essay<-read.csv("result_essay_20140710.csv")
result_need_statement<-read.csv("result_need_statement.csv")
result_short_description<-read.csv("result_short_description.csv")
result_title<-read.csv("result_title.csv")

d2$days_since_posted<-with(d2,as.numeric(last_date_posted-date_posted))

d2<-left_join(d2,count,by='projectid')
d2<-left_join(d2,result_essay,by='projectid')
d2<-left_join(d2,result_need_statement,by='projectid')
d2<-left_join(d2,result_short_description,by='projectid')
d2<-left_join(d2,result_title,by='projectid')

resources<-inner_join(resources,d2[,c('projectid','date_posted')],by='projectid')

d2$school_ncesid<-as.factor(d2$school_ncesid)
d2$school_zip<-as.factor(d2$school_zip)

d2<-left_join(d2,public_school,by='school_ncesid')

d2$optional_support<-d2$total_price_including_optional_support-d2$total_price_excluding_optional_support

tmp<-summarize(group_by(d2,school_city),price_school_city=mean(total_price_excluding_optional_support,rm.na=TRUE))
d2<-left_join(d2,tmp,by='school_city')
tmp<-summarize(group_by(d2,schoolid),price_schoolid=mean(total_price_excluding_optional_support,rm.na=TRUE))
d2<-left_join(d2,tmp,by='schoolid')

resources$vendorid<-as.factor(resources$vendorid)
resources$item_price<-with(resources,item_unit_price*item_quantity)

resources_summary<-summarize(group_by(resources,projectid),resource_cnt=n(),resource_price=sum(item_price,na.rm=TRUE),resource_quantity=sum(item_quantity,na.rm=TRUE))
tmp<-summarize(group_by(filter(resources,project_resource_type=='Books'),projectid),books_cnt=n(),books_quantity=sum(item_quantity,na.rm=TRUE))
resources_summary<-left_join(resources_summary,tmp,by="projectid")
resources_summary$books_cnt[is.na(resources_summary$books_cnt)]<-0
d2<-left_join(d2,resources_summary,by="projectid")

vendor_summary<-summarize(group_by(resources,projectid,vendorid),project_vendor_price=sum(item_price,na.rm=TRUE),project_vendor_quantity=sum(item_quantity,na.rm=TRUE))
tmp<-summarize(group_by(vendor_summary,projectid),project_vendor_cnt=n())
vendor_summary<-left_join(vendor_summary,tmp,by='projectid')
vendor_summary<-mutate(group_by(vendor_summary,projectid),project_vendor_price_rnk=row_number(desc(project_vendor_price)),project_vendor_quantity_rnk=row_number(desc(project_vendor_quantity)))

tmp<-summarize(group_by(resources,vendorid),vendor_cnt=n(),vendor_price=sum(item_price,na.rm=TRUE),vendor_quantity=sum(item_quantity,na.rm=TRUE))
tmp$vendor_average_price<-tmp$vendor_price/tmp$vendor_quantity

vendor_summary<-left_join(vendor_summary,tmp,by='vendorid')
d2<-left_join(d2,vendor_summary[vendor_summary$project_vendor_price_rnk==1,],by='projectid')

donations<-left_join(donations,d2[,c('projectid','date_posted','teacher_acctid','schoolid','school_ncesid','school_city','school_state','school_zip')],by='projectid')
donations$donation_date<-substr(as.character(donations$donation_timestamp),1,10)
donations$donation_date<-as.Date(donations$donation_date)

tmp<-summarize(group_by(donations,projectid),max_donation_date=max(donation_date,na.rm=TRUE))
d2<-left_join(d2,tmp,by='projectid')
d2$days_to_fully_funding<-as.numeric(d2$max_donation_date-d2$date_posted)

tmp<-summarize(group_by(d2[d2$date_posted>=as.Date('2013-10-01','%Y-%m-%d')&d2$date_posted<as.Date('2014-01-01','%Y-%m-%d')&d2$fully_funded==1,],days_to_fully_funding)
               ,project_cnt=n()
               ,is_exciting_cnt=sum(y,na.rm=T))
tmp<-tmp[tmp$days_to_fully_funding>=0&tmp$days_to_fully_funding<=120&!is.na(tmp$days_to_fully_funding),]
tmp$project_cnt_cumsum<-cumsum(tmp$project_cnt)
tmp$is_exciting_cnt_cumsum<-cumsum(tmp$is_exciting_cnt)
tmp$is_exciting_pct<-with(tmp,is_exciting_cnt_cumsum/project_cnt_cumsum)
tmp$factor<-with(tmp,is_exciting_pct/max(is_exciting_pct))
names(tmp)[1]<-'days_since_posted'
d2<-left_join(d2,tmp[,c('days_since_posted','factor')],by='days_since_posted')

donations$is_teacher_acct<-with(donations,ifelse(is_teacher_acct=='t',1,0))
donations$payment_included_acct_credit<-with(donations,ifelse(payment_included_acct_credit=='t',1,0))
donations$payment_included_campaign_gift_card<-with(donations,ifelse(payment_included_campaign_gift_card=='t',1,0))
donations$payment_included_web_purchased_gift_card<-with(donations,ifelse(payment_included_web_purchased_gift_card=='t',1,0))
donations$payment_was_promo_matched<-with(donations,ifelse(payment_was_promo_matched=='t',1,0))
donations$via_giving_page<-with(donations,ifelse(via_giving_page=='t',1,0))
donations$for_honoree<-with(donations,ifelse(for_honoree=='t',1,0))

tmp<-summarize(group_by(select(donations,projectid),projectid),project_donations_cnt=n())
d2<-left_join(d2,tmp,by='projectid')
d2$project_donations_cnt[is.na(d2$project_donations_cnt)&d2$test==0]<-0
tmp<-left_join(d2[,c('projectid','date_posted','teacher_acctid','schoolid','school_ncesid','school_city','school_state','school_zip')],tmp,by='projectid')
tmp$project_donations_cnt[is.na(tmp$project_donations_cnt)]<-0

tmp2<-summarize(group_by(select(tmp,teacher_acctid,date_posted,project_donations_cnt),teacher_acctid,date_posted),donations_cnt=sum(project_donations_cnt))
tmp2<-tmp2[order(tmp2$teacher_acctid,tmp2$date_posted),]
tmp2<-mutate(group_by(tmp2,teacher_acctid),cnt_donations_teacher_acctid_cumsum=cumsum(donations_cnt))
tmp2$cnt_donations_teacher_acctid_cumsum=tmp2$cnt_donations_teacher_acctid_cumsum-tmp2$donations_cnt
d2<-left_join(d2,select(tmp2,teacher_acctid,date_posted,cnt_donations_teacher_acctid_cumsum),by=c('teacher_acctid','date_posted'))
d2$cnt_donations_teacher_acctid_cumsum[is.na(d2$cnt_donations_teacher_acctid_cumsum)]<-0

tmp2<-summarize(group_by(select(tmp,schoolid,date_posted,project_donations_cnt),schoolid,date_posted),donations_cnt=sum(project_donations_cnt))
tmp2<-tmp2[order(tmp2$schoolid,tmp2$date_posted),]
tmp2<-mutate(group_by(tmp2,schoolid),cnt_donations_schoolid_cumsum=cumsum(donations_cnt))
tmp2$cnt_donations_schoolid_cumsum=tmp2$cnt_donations_schoolid_cumsum-tmp2$donations_cnt
d2<-left_join(d2,select(tmp2,schoolid,date_posted,cnt_donations_schoolid_cumsum),by=c('schoolid','date_posted'))
d2$cnt_donations_schoolid_cumsum[is.na(d2$cnt_donations_schoolid_cumsum)]<-0

#####################################################################################################################################################

tmp<-summarize(group_by(d2,schoolid),i_price_ttl_schoolid=sum(total_price_including_optional_support,na.rm=TRUE),min_date_posted_schoolid=min(date_posted))
d2<-left_join(d2,tmp,by='schoolid')
d2$ttl_days_schoolid<-with(d2,as.numeric(last_date_posted-min_date_posted_schoolid))
d2$i_price_per_day_schoolid<-with(d2,i_price_ttl_schoolid/ttl_days_schoolid)

tmp<-select(d2,projectid,schoolid,date_posted,y,total_price_excluding_optional_support,total_price_including_optional_support)
tmp<-tmp[order(tmp$schoolid,tmp$date_posted),]
tmp$schoolid_staggered<-tmp$schoolid
tmp$schoolid_staggered[2:length(tmp$schoolid)]<-tmp$schoolid[1:length(tmp$schoolid)-1]
tmp$schoolid_staggered[1:1]<-NA
tmp$staggered<-with(tmp,ifelse(schoolid_staggered==schoolid,1,0))
tmp$staggered[1:1]<-0

tmp$date_posted_prev<-tmp$date_posted
tmp$date_posted_prev[2:length(tmp$date_posted)]<-tmp$date_posted[1:length(tmp$date_posted)-1]
tmp$date_posted_prev[tmp$staggered!=1]<-NA
tmp$days_since_prev_schoolid<-with(tmp,as.numeric(date_posted-date_posted_prev))

tmp$total_price_including_optional_support_prev<-tmp$total_price_including_optional_support
tmp$total_price_including_optional_support_prev[2:length(tmp$total_price_including_optional_support)]<-tmp$total_price_including_optional_support[1:length(tmp$total_price_including_optional_support)-1]
tmp$total_price_including_optional_support_prev[tmp$staggered!=1]<-NA
tmp$i_price_prev_dif_schoolid<-with(tmp,as.numeric(total_price_including_optional_support-total_price_including_optional_support_prev))
tmp<-tmp[,c('projectid','days_since_prev_schoolid','i_price_prev_dif_schoolid')]
d2<-left_join(d2,tmp,by='projectid')

#####################################################################################################################################################

tmp<-summarize(group_by(d2,school_city),optional_support_mean_school_city=mean(optional_support,na.rm=TRUE))
d2<-left_join(d2,tmp,by='school_city')

tmp<-select(d2,projectid,school_city,date_posted,y,total_price_excluding_optional_support,total_price_including_optional_support)
tmp<-tmp[order(tmp$school_city,tmp$date_posted),]
tmp$school_city_staggered<-tmp$school_city
tmp$school_city_staggered[2:length(tmp$school_city)]<-tmp$school_city[1:length(tmp$school_city)-1]
tmp$school_city_staggered[1:1]<-NA
tmp$staggered<-with(tmp,ifelse(school_city_staggered==school_city,1,0))
tmp$staggered[1:1]<-0

tmp$date_posted_prev<-tmp$date_posted
tmp$date_posted_prev[2:length(tmp$date_posted)]<-tmp$date_posted[1:length(tmp$date_posted)-1]
tmp$date_posted_prev[tmp$staggered!=1]<-NA
tmp$days_since_prev_school_city<-with(tmp,as.numeric(date_posted-date_posted_prev))

tmp$total_price_including_optional_support_prev<-tmp$total_price_including_optional_support
tmp$total_price_including_optional_support_prev[2:length(tmp$total_price_including_optional_support)]<-tmp$total_price_including_optional_support[1:length(tmp$total_price_including_optional_support)-1]
tmp$total_price_including_optional_support_prev[tmp$staggered!=1]<-NA
tmp$total_price_including_optional_support_prev[is.na(tmp$total_price_including_optional_support_prev)]<--100
tmp<-mutate(group_by(tmp,school_city),i_price_school_city_cummax=cummax(total_price_including_optional_support_prev))
tmp$i_price_school_city_cummax[tmp$i_price_school_city_cummax==-100]<-NA
tmp<-tmp[,c('projectid','days_since_prev_school_city','i_price_school_city_cummax')]
d2<-left_join(d2,tmp,by='projectid')

#####################################################################################################################################################

tmp<-select(d2,projectid,school_zip,date_posted,y,total_price_excluding_optional_support,total_price_including_optional_support)
tmp<-tmp[order(tmp$school_zip,tmp$date_posted),]
tmp$school_zip_staggered<-tmp$school_zip
tmp$school_zip_staggered[2:length(tmp$school_zip)]<-tmp$school_zip[1:length(tmp$school_zip)-1]
tmp$school_zip_staggered[1:1]<-NA
tmp$staggered<-with(tmp,ifelse(school_zip_staggered==school_zip,1,0))
tmp$staggered[1:1]<-0

tmp$total_price_including_optional_support_prev<-tmp$total_price_including_optional_support
tmp$total_price_including_optional_support_prev[2:length(tmp$total_price_including_optional_support)]<-tmp$total_price_including_optional_support[1:length(tmp$total_price_including_optional_support)-1]
tmp$total_price_including_optional_support_prev[tmp$staggered!=1]<-NA
tmp$total_price_including_optional_support_prev[is.na(tmp$total_price_including_optional_support_prev)]<-9999999999
tmp<-mutate(group_by(tmp,school_zip),i_price_school_zip_cummin=cummin(total_price_including_optional_support_prev))
tmp$i_price_school_zip_cummin[tmp$i_price_school_zip_cummin==9999999999]<-NA
tmp<-tmp[,c('projectid','i_price_school_zip_cummin')]
d2<-left_join(d2,tmp,by='projectid')
d2$i_price_dec_over_min_school_zip<-with(d2,i_price_school_zip_cummin-total_price_including_optional_support)

#####################################################################################################################################################

d2$date_posted_m1<-with(d2,date_posted-1)
d2$date_posted_m2<-with(d2,date_posted-2)
d2$date_posted_m3<-with(d2,date_posted-3)
d2$date_posted_m4<-with(d2,date_posted-4)
d2$date_posted_m5<-with(d2,date_posted-5)
d2$date_posted_m6<-with(d2,date_posted-6)
d2$date_posted_p1<-with(d2,date_posted+1)
d2$date_posted_p2<-with(d2,date_posted+2)
d2$date_posted_p3<-with(d2,date_posted+3)
d2$date_posted_p4<-with(d2,date_posted+4)
d2$date_posted_p5<-with(d2,date_posted+5)
d2$date_posted_p6<-with(d2,date_posted+6)

tmp<-summarize(group_by(d2[,c('ym','school_zip','resource_quantity')],ym,school_zip),cnt_mth_zip=n())
d2<-left_join(d2,tmp,by=c('ym','school_zip'))
tmp<-summarize(group_by(d2[,c('ym','schoolid','resource_quantity')],ym,schoolid),cnt_mth_schoolid=n())
d2<-left_join(d2,tmp,by=c('ym','schoolid'))

tmp<-summarize(group_by(d2[,c('date_posted','school_city')],date_posted,school_city),cnt_day_city=n())
d2<-left_join(d2,tmp,by=c('date_posted','school_city'))
names(tmp)<-c('date_posted_m1','school_city','cnt_day_city_m1')
d2<-left_join(d2,tmp,by=c('date_posted_m1','school_city'))
d2$cnt_day_city_m1[is.na(d2$cnt_day_city_m1)]<-0
names(tmp)<-c('date_posted_m2','school_city','cnt_day_city_m2')
d2<-left_join(d2,tmp,by=c('date_posted_m2','school_city'))
d2$cnt_day_city_m2[is.na(d2$cnt_day_city_m2)]<-0
names(tmp)<-c('date_posted_m3','school_city','cnt_day_city_m3')
d2<-left_join(d2,tmp,by=c('date_posted_m3','school_city'))
d2$cnt_day_city_m3[is.na(d2$cnt_day_city_m3)]<-0
names(tmp)<-c('date_posted_m4','school_city','cnt_day_city_m4')
d2<-left_join(d2,tmp,by=c('date_posted_m4','school_city'))
d2$cnt_day_city_m4[is.na(d2$cnt_day_city_m4)]<-0
names(tmp)<-c('date_posted_m5','school_city','cnt_day_city_m5')
d2<-left_join(d2,tmp,by=c('date_posted_m5','school_city'))
d2$cnt_day_city_m5[is.na(d2$cnt_day_city_m5)]<-0
names(tmp)<-c('date_posted_m6','school_city','cnt_day_city_m6')
d2<-left_join(d2,tmp,by=c('date_posted_m6','school_city'))
d2$cnt_day_city_m6[is.na(d2$cnt_day_city_m6)]<-0
names(tmp)<-c('date_posted_p1','school_city','cnt_day_city_p1')
d2<-left_join(d2,tmp,by=c('date_posted_p1','school_city'))
d2$cnt_day_city_p1[is.na(d2$cnt_day_city_p1)]<-0
names(tmp)<-c('date_posted_p2','school_city','cnt_day_city_p2')
d2<-left_join(d2,tmp,by=c('date_posted_p2','school_city'))
d2$cnt_day_city_p2[is.na(d2$cnt_day_city_p2)]<-0
names(tmp)<-c('date_posted_p3','school_city','cnt_day_city_p3')
d2<-left_join(d2,tmp,by=c('date_posted_p3','school_city'))
d2$cnt_day_city_p3[is.na(d2$cnt_day_city_p3)]<-0
names(tmp)<-c('date_posted_p4','school_city','cnt_day_city_p4')
d2<-left_join(d2,tmp,by=c('date_posted_p4','school_city'))
d2$cnt_day_city_p4[is.na(d2$cnt_day_city_p4)]<-0
names(tmp)<-c('date_posted_p5','school_city','cnt_day_city_p5')
d2<-left_join(d2,tmp,by=c('date_posted_p5','school_city'))
d2$cnt_day_city_p5[is.na(d2$cnt_day_city_p5)]<-0
names(tmp)<-c('date_posted_p6','school_city','cnt_day_city_p6')
d2<-left_join(d2,tmp,by=c('date_posted_p6','school_city'))
d2$cnt_day_city_p6[is.na(d2$cnt_day_city_p6)]<-0

tmp<-summarize(group_by(d2[,c('date_posted','school_zip')],date_posted,school_zip),cnt_day_zip=n())
d2<-left_join(d2,tmp,by=c('date_posted','school_zip'))
names(tmp)<-c('date_posted_m1','school_zip','cnt_day_zip_m1')
d2<-left_join(d2,tmp,by=c('date_posted_m1','school_zip'))
d2$cnt_day_zip_m1[is.na(d2$cnt_day_zip_m1)]<-0
names(tmp)<-c('date_posted_m2','school_zip','cnt_day_zip_m2')
d2<-left_join(d2,tmp,by=c('date_posted_m2','school_zip'))
d2$cnt_day_zip_m2[is.na(d2$cnt_day_zip_m2)]<-0
names(tmp)<-c('date_posted_m3','school_zip','cnt_day_zip_m3')
d2<-left_join(d2,tmp,by=c('date_posted_m3','school_zip'))
d2$cnt_day_zip_m3[is.na(d2$cnt_day_zip_m3)]<-0
names(tmp)<-c('date_posted_m4','school_zip','cnt_day_zip_m4')
d2<-left_join(d2,tmp,by=c('date_posted_m4','school_zip'))
d2$cnt_day_zip_m4[is.na(d2$cnt_day_zip_m4)]<-0
names(tmp)<-c('date_posted_m5','school_zip','cnt_day_zip_m5')
d2<-left_join(d2,tmp,by=c('date_posted_m5','school_zip'))
d2$cnt_day_zip_m5[is.na(d2$cnt_day_zip_m5)]<-0
names(tmp)<-c('date_posted_m6','school_zip','cnt_day_zip_m6')
d2<-left_join(d2,tmp,by=c('date_posted_m6','school_zip'))
d2$cnt_day_zip_m6[is.na(d2$cnt_day_zip_m6)]<-0
names(tmp)<-c('date_posted_p1','school_zip','cnt_day_zip_p1')
d2<-left_join(d2,tmp,by=c('date_posted_p1','school_zip'))
d2$cnt_day_zip_p1[is.na(d2$cnt_day_zip_p1)]<-0
names(tmp)<-c('date_posted_p2','school_zip','cnt_day_zip_p2')
d2<-left_join(d2,tmp,by=c('date_posted_p2','school_zip'))
d2$cnt_day_zip_p2[is.na(d2$cnt_day_zip_p2)]<-0
names(tmp)<-c('date_posted_p3','school_zip','cnt_day_zip_p3')
d2<-left_join(d2,tmp,by=c('date_posted_p3','school_zip'))
d2$cnt_day_zip_p3[is.na(d2$cnt_day_zip_p3)]<-0
names(tmp)<-c('date_posted_p4','school_zip','cnt_day_zip_p4')
d2<-left_join(d2,tmp,by=c('date_posted_p4','school_zip'))
d2$cnt_day_zip_p4[is.na(d2$cnt_day_zip_p4)]<-0
names(tmp)<-c('date_posted_p5','school_zip','cnt_day_zip_p5')
d2<-left_join(d2,tmp,by=c('date_posted_p5','school_zip'))
d2$cnt_day_zip_p5[is.na(d2$cnt_day_zip_p5)]<-0
names(tmp)<-c('date_posted_p6','school_zip','cnt_day_zip_p6')
d2<-left_join(d2,tmp,by=c('date_posted_p6','school_zip'))
d2$cnt_day_zip_p6[is.na(d2$cnt_day_zip_p6)]<-0

tmp<-summarize(group_by(d2[,c('date_posted','schoolid')],date_posted,schoolid),cnt_day_schoolid=n())
d2<-left_join(d2,tmp,by=c('date_posted','schoolid'))
names(tmp)<-c('date_posted_m1','schoolid','cnt_day_schoolid_m1')
d2<-left_join(d2,tmp,by=c('date_posted_m1','schoolid'))
d2$cnt_day_schoolid_m1[is.na(d2$cnt_day_schoolid_m1)]<-0
names(tmp)<-c('date_posted_m2','schoolid','cnt_day_schoolid_m2')
d2<-left_join(d2,tmp,by=c('date_posted_m2','schoolid'))
d2$cnt_day_schoolid_m2[is.na(d2$cnt_day_schoolid_m2)]<-0
names(tmp)<-c('date_posted_m3','schoolid','cnt_day_schoolid_m3')
d2<-left_join(d2,tmp,by=c('date_posted_m3','schoolid'))
d2$cnt_day_schoolid_m3[is.na(d2$cnt_day_schoolid_m3)]<-0
names(tmp)<-c('date_posted_m4','schoolid','cnt_day_schoolid_m4')
d2<-left_join(d2,tmp,by=c('date_posted_m4','schoolid'))
d2$cnt_day_schoolid_m4[is.na(d2$cnt_day_schoolid_m4)]<-0
names(tmp)<-c('date_posted_m5','schoolid','cnt_day_schoolid_m5')
d2<-left_join(d2,tmp,by=c('date_posted_m5','schoolid'))
d2$cnt_day_schoolid_m5[is.na(d2$cnt_day_schoolid_m5)]<-0
names(tmp)<-c('date_posted_m6','schoolid','cnt_day_schoolid_m6')
d2<-left_join(d2,tmp,by=c('date_posted_m6','schoolid'))
d2$cnt_day_schoolid_m6[is.na(d2$cnt_day_schoolid_m6)]<-0
names(tmp)<-c('date_posted_p1','schoolid','cnt_day_schoolid_p1')
d2<-left_join(d2,tmp,by=c('date_posted_p1','schoolid'))
d2$cnt_day_schoolid_p1[is.na(d2$cnt_day_schoolid_p1)]<-0
names(tmp)<-c('date_posted_p2','schoolid','cnt_day_schoolid_p2')
d2<-left_join(d2,tmp,by=c('date_posted_p2','schoolid'))
d2$cnt_day_schoolid_p2[is.na(d2$cnt_day_schoolid_p2)]<-0
names(tmp)<-c('date_posted_p3','schoolid','cnt_day_schoolid_p3')
d2<-left_join(d2,tmp,by=c('date_posted_p3','schoolid'))
d2$cnt_day_schoolid_p3[is.na(d2$cnt_day_schoolid_p3)]<-0
names(tmp)<-c('date_posted_p4','schoolid','cnt_day_schoolid_p4')
d2<-left_join(d2,tmp,by=c('date_posted_p4','schoolid'))
d2$cnt_day_schoolid_p4[is.na(d2$cnt_day_schoolid_p4)]<-0
names(tmp)<-c('date_posted_p5','schoolid','cnt_day_schoolid_p5')
d2<-left_join(d2,tmp,by=c('date_posted_p5','schoolid'))
d2$cnt_day_schoolid_p5[is.na(d2$cnt_day_schoolid_p5)]<-0
names(tmp)<-c('date_posted_p6','schoolid','cnt_day_schoolid_p6')
d2<-left_join(d2,tmp,by=c('date_posted_p6','schoolid'))
d2$cnt_day_schoolid_p6[is.na(d2$cnt_day_schoolid_p6)]<-0

d2$cnt_wk_city<-with(d2,cnt_day_city+cnt_day_city_m1+cnt_day_city_m2+cnt_day_city_m3+cnt_day_city_p1+cnt_day_city_p2+cnt_day_city_p3)
d2$cnt_wk_zip<-with(d2,cnt_day_zip+cnt_day_zip_m1+cnt_day_zip_m2+cnt_day_zip_m3+cnt_day_zip_p1+cnt_day_zip_p2+cnt_day_zip_p3)
d2$cnt_wk_schoolid<-with(d2,cnt_day_schoolid+cnt_day_schoolid_m1+cnt_day_schoolid_m2+cnt_day_schoolid_m3+cnt_day_schoolid_p1+cnt_day_schoolid_p2+cnt_day_schoolid_p3)
d2$cnt_bwk_city<-with(d2,cnt_day_city+cnt_day_city_m1+cnt_day_city_m2+cnt_day_city_m3+cnt_day_city_m4+cnt_day_city_m5+cnt_day_city_m6
                      +cnt_day_city_p1+cnt_day_city_p2+cnt_day_city_p3+cnt_day_city_p4+cnt_day_city_p5+cnt_day_city_p6)
d2$cnt_bwk_zip<-with(d2,cnt_day_zip+cnt_day_zip_m1+cnt_day_zip_m2+cnt_day_zip_m3+cnt_day_zip_m4+cnt_day_zip_m5+cnt_day_zip_m6
                     +cnt_day_zip_p1+cnt_day_zip_p2+cnt_day_zip_p3+cnt_day_zip_p4+cnt_day_zip_p5+cnt_day_zip_p6)
d2$cnt_bwk_schoolid<-with(d2,cnt_day_schoolid+cnt_day_schoolid_m1+cnt_day_schoolid_m2+cnt_day_schoolid_m3+cnt_day_schoolid_m4+cnt_day_schoolid_m5+cnt_day_schoolid_m6
                          +cnt_day_schoolid_p1+cnt_day_schoolid_p2+cnt_day_schoolid_p3+cnt_day_schoolid_p4+cnt_day_schoolid_p5+cnt_day_schoolid_p6)

d2$School.Type_v2<-as.character(d2$School.Type)
d2$School.Type_v2[is.na(d2$School.Type_v2)]<-"_NA_"
d2$School.Type_v2<-as.factor(d2$School.Type_v2)

d2$i_price_dec_over_min_school_zip_v2<-d2$i_price_dec_over_min_school_zip
d2$i_price_dec_over_min_school_zip_v2[is.na(d2$i_price_dec_over_min_school_zip_v2)]<-0

d2$i_price_school_city_cummax_v2<-d2$i_price_school_city_cummax
d2$i_price_school_city_cummax_v2[is.na(d2$i_price_school_city_cummax_v2)]<-0

d2$days_since_prev_school_city_v2<-d2$days_since_prev_school_city
d2$days_since_prev_school_city_v2[is.na(d2$days_since_prev_school_city_v2)]<--1

d2$i_price_prev_dif_schoolid_v2<-d2$i_price_prev_dif_schoolid
d2$i_price_prev_dif_schoolid_v2[is.na(d2$i_price_prev_dif_schoolid_v2)]<-0

d2$days_since_prev_schoolid_v2<-d2$days_since_prev_schoolid
d2$days_since_prev_schoolid_v2[is.na(d2$days_since_prev_schoolid_v2)]<--1

d2$students_reached_v2<-d2$students_reached
d2$students_reached_v2[is.na(d2$students_reached_v2)]<--1

d2$y2<-as.factor(d2$y)

d2$teacher_prefix_v2<-NA
d2$teacher_prefix_v2[d2$teacher_prefix=='']<-0
d2$teacher_prefix_v2[d2$teacher_prefix=='Dr.']<-1
d2$teacher_prefix_v2[d2$teacher_prefix=='Mr.']<-2
d2$teacher_prefix_v2[d2$teacher_prefix=='Mr. & Mrs.']<-3
d2$teacher_prefix_v2[d2$teacher_prefix=='Mrs.']<-4
d2$teacher_prefix_v2[d2$teacher_prefix=='Ms.']<-5

d2$teacher_teach_for_america_v2<-NA
d2$teacher_teach_for_america_v2[d2$teacher_teach_for_america=='f']<-0
d2$teacher_teach_for_america_v2[d2$teacher_teach_for_america=='t']<-1

d2$eligible_double_your_impact_match_v2<-NA
d2$eligible_double_your_impact_match_v2[d2$eligible_double_your_impact_match=='f']<-0
d2$eligible_double_your_impact_match_v2[d2$eligible_double_your_impact_match=='t']<-1

d2$eligible_almost_home_match_v2<-NA
d2$eligible_almost_home_match_v2[d2$eligible_almost_home_match=='f']<-0
d2$eligible_almost_home_match_v2[d2$eligible_almost_home_match=='t']<-1

d2$resource_type_v2<-NA
d2$resource_type_v2[d2$resource_type=='']<-0
d2$resource_type_v2[d2$resource_type=='Books']<-1
d2$resource_type_v2[d2$resource_type=='Other']<-2
d2$resource_type_v2[d2$resource_type=='Supplies']<-3
d2$resource_type_v2[d2$resource_type=='Technology']<-4
d2$resource_type_v2[d2$resource_type=='Trips']<-5
d2$resource_type_v2[d2$resource_type=='Visitors']<-6

d2$School.Type_v3<-NA
d2$School.Type_v3[d2$School.Type_v2=='_NA_']<-0
d2$School.Type_v3[d2$School.Type_v2=='1-Regular school']<-1
d2$School.Type_v3[d2$School.Type_v2=='2-Special education school']<-2
d2$School.Type_v3[d2$School.Type_v2=='3-Vocational school']<-3
d2$School.Type_v3[d2$School.Type_v2=='4-Alternative/other school']<-4

set.seed(6744315)
r=0.3
d2$p<-mean(d2$y[d2$test==0])

tmp<-myleaveoneout(d=d2,y='y',x1='teacher_acctid',p='p',filter=(d2$test==0),k=5,r=r)
d2$adj_teacher_acctid<-tmp$adj_y
tmp<-myleaveoneout(d=d2,y='y',x1='school_ncesid',p='p',filter=(d2$test==0),k=25,r=r)
d2$adj_school_ncesid<-tmp$adj_y
tmp<-myleaveoneout(d=d2,y='y',x1='school_state',p='p',filter=(d2$test==0),k=25,r=r)
d2$adj_school_state<-tmp$adj_y
tmp<-myleaveoneout(d=d2,y='y',x1='primary_focus_subject',x2='secondary_focus_subject',p='p',filter=(d2$test==0),k=20,r=r)
d2$adj_subject<-tmp$adj_y
tmp<-myleaveoneout(d=d2,y='y',x1='primary_focus_subject',x2='resource_type',p='p',filter=(d2$test==0),k=25,r=r)
d2$adj_primary_focus_subject_resource_type<-tmp$adj_y
tmp<-myleaveoneout(d=d2,y='y',x1='secondary_focus_subject',x2='resource_type',p='p',filter=(d2$test==0),k=25,r=r)
d2$adj_secondary_focus_subject_resource_type<-tmp$adj_y

d2$adj_teacher_acctid_ln<-log(d2$adj_teacher_acctid)
d2$students_reached_v2_ln<-log(d2$students_reached_v2+2)
d2$total_price_excluding_optional_support_ln<-log(d2$total_price_excluding_optional_support)
d2$cnt_bwk_city_ln<-log(d2$cnt_bwk_city)
d2$cnt_wk_city_ln<-log(d2$cnt_wk_city)
d2$cnt_mth_city_ln<-log(d2$cnt_wk_city)
d2$cnt_mth_zip_ln<-log(d2$cnt_mth_zip)
d2$cnt_bwk_zip_ln<-log(d2$cnt_bwk_zip)
d2$cnt_wk_zip_ln<-log(d2$cnt_wk_zip)
d2$cnt_mth_schoolid_ln<-log(d2$cnt_mth_schoolid)
d2$cnt_bwk_schoolid_ln<-log(d2$cnt_bwk_schoolid)
d2$cnt_wk_schoolid_ln<-log(d2$cnt_wk_schoolid)
d2$days_since_prev_schoolid_v2_ln<-log(d2$days_since_prev_schoolid_v2+2)
d2$i_price_per_day_schoolid_ln<-log(d2$i_price_per_day_schoolid+1)
d2$title_cnt_ln<-log(d2$title_cnt+1)
d2$essay_cnt_ln<-log(d2$essay_cnt+1)
d2$cnt_donations_schoolid_cumsum_ln<-log(d2$cnt_donations_schoolid_cumsum+1)

d2<-d2[order(d2$test),]

set.seed(9299085)
gbm.kdd.day<-gbm(y~
                   adj_teacher_acctid
                 +adj_school_ncesid
                 +adj_school_state
                 +adj_primary_focus_subject_resource_type
                 +adj_secondary_focus_subject_resource_type
                 +price_school_city
                 +school_latitude
                 +school_longitude
                 +teacher_prefix
                 +teacher_teach_for_america
                 +students_reached
                 +eligible_double_your_impact_match
                 +eligible_almost_home_match
                 +total_price_excluding_optional_support
                 +books_cnt
                 +cnt_bwk_city
                 +cnt_wk_city
                 +cnt_mth_zip
                 +cnt_bwk_zip
                 +cnt_wk_zip
                 +cnt_mth_schoolid
                 +cnt_bwk_schoolid
                 +cnt_wk_schoolid
                 +days_since_prev_schoolid
                 +i_price_prev_dif_schoolid
                 +optional_support_mean_school_city
                 +days_since_prev_school_city
                 +price_schoolid
                 +optional_support
                 +i_price_per_day_schoolid
                 +i_price_school_city_cummax
                 +i_price_dec_over_min_school_zip
                 +title_cnt
                 +essay_cnt
                 +resource_type
                 +School.Type
                 +cnt_donations_schoolid_cumsum
                 +cnt_donations_teacher_acctid_cumsum
                 +essay_tfidf
                 ,
                 data=d2[d2$test==0&d2$date_posted>=as.Date('2011-01-01','%Y-%m-%d'),],
                 distribution='bernoulli',
                 n.trees=7300,
                 interaction.depth=6,
                 n.minobsinnode=100,
                 shrinkage=0.01,
                 bag.fraction=0.5,
                 train.fraction=1,
                 keep.data=TRUE,
                 verbose=TRUE
)
d2$pred.day.gbm<-predict(gbm.kdd.day,newdata=d2,n.trees=gbm.kdd.day$n.trees,type="response")

set.seed(447245)
rf.kdd.day<-randomForest(y2~
                           adj_teacher_acctid
                         +adj_school_ncesid
                         +adj_school_state
                         +adj_primary_focus_subject_resource_type
                         +adj_secondary_focus_subject_resource_type
                         +price_school_city
                         +school_latitude
                         +school_longitude
                         +teacher_prefix
                         +teacher_teach_for_america
                         +students_reached_v2
                         +eligible_double_your_impact_match
                         +eligible_almost_home_match
                         +total_price_excluding_optional_support
                         +books_cnt
                         +cnt_bwk_city
                         +cnt_wk_city
                         +cnt_mth_zip
                         +cnt_bwk_zip
                         +cnt_wk_zip
                         +cnt_mth_schoolid
                         +cnt_bwk_schoolid
                         +cnt_wk_schoolid
                         +days_since_prev_schoolid_v2
                         +i_price_prev_dif_schoolid_v2
                         +optional_support_mean_school_city
                         +days_since_prev_school_city_v2
                         +price_schoolid
                         +optional_support
                         +i_price_per_day_schoolid
                         +i_price_school_city_cummax_v2
                         +i_price_dec_over_min_school_zip_v2
                         +title_cnt
                         +essay_cnt
                         +resource_type
                         +School.Type_v2
                         +cnt_donations_schoolid_cumsum
                         +cnt_donations_teacher_acctid_cumsum
                         +essay_tfidf
                         ,
                         data=d2[d2$test==0&d2$date_posted>=as.Date('2011-01-01','%Y-%m-%d'),],
                         ntree=5000,
                         mtry=2,
                         replace=FALSE,
                         nodesize=10,
                         do.trace=TRUE
)
tmp<-predict(rf.kdd.day,newdata=d2,type='prob')
d2$pred.day.rf<-tmp[,2]

mm<-data.matrix(select(d2,adj_teacher_acctid
                       ,adj_school_ncesid
                       ,adj_school_state
                       ,adj_primary_focus_subject_resource_type
                       ,adj_secondary_focus_subject_resource_type
                       ,price_school_city
                       ,school_latitude
                       ,school_longitude
                       ,teacher_prefix_v2
                       ,teacher_teach_for_america_v2
                       ,students_reached_v2
                       ,eligible_double_your_impact_match_v2
                       ,eligible_almost_home_match_v2
                       ,total_price_excluding_optional_support
                       ,books_cnt
                       ,cnt_bwk_city
                       ,cnt_wk_city
                       ,cnt_mth_zip
                       ,cnt_bwk_zip
                       ,cnt_wk_zip
                       ,cnt_mth_schoolid
                       ,cnt_bwk_schoolid
                       ,cnt_wk_schoolid
                       ,days_since_prev_schoolid_v2
                       ,i_price_prev_dif_schoolid_v2
                       ,optional_support_mean_school_city
                       ,days_since_prev_school_city_v2
                       ,price_schoolid
                       ,optional_support
                       ,i_price_per_day_schoolid
                       ,i_price_school_city_cummax_v2
                       ,i_price_dec_over_min_school_zip_v2
                       ,title_cnt
                       ,essay_cnt
                       ,resource_type_v2
                       ,School.Type_v3
                       ,cnt_donations_schoolid_cumsum
                       ,cnt_donations_teacher_acctid_cumsum
                       ,essay_tfidf))
set.seed(605562)
et.kdd.day<-extraTrees(mm[d2$test==0&d2$date_posted>=as.Date('2011-01-01','%Y-%m-%d'),],
                       d2$y[d2$test==0&d2$date_posted>=as.Date('2011-01-01','%Y-%m-%d')],
                       mtry=2,
                       nodesize=100,
                       numTreads=1,
                       ntree=3000,
                       numRandomCuts=1)
d2$pred.day.et<-predict(et.kdd.day,mm)

mm.glmnet<-data.matrix(select(d2,adj_teacher_acctid_ln
                              ,adj_school_ncesid
                              ,adj_school_state
                              ,adj_primary_focus_subject_resource_type
                              ,adj_secondary_focus_subject_resource_type
                              ,price_school_city
                              ,school_latitude
                              ,school_longitude
                              ,teacher_prefix_v2
                              ,teacher_teach_for_america_v2
                              ,students_reached_v2_ln
                              ,eligible_double_your_impact_match_v2
                              ,eligible_almost_home_match_v2
                              ,total_price_excluding_optional_support_ln
                              ,books_cnt
                              ,cnt_bwk_city_ln
                              ,cnt_wk_city_ln
                              ,cnt_mth_zip_ln
                              ,cnt_bwk_zip_ln
                              ,cnt_wk_zip_ln
                              ,cnt_mth_schoolid_ln
                              ,cnt_bwk_schoolid_ln
                              ,cnt_wk_schoolid_ln
                              ,days_since_prev_schoolid_v2_ln
                              ,i_price_prev_dif_schoolid_v2
                              ,optional_support_mean_school_city
                              ,days_since_prev_school_city_v2
                              ,price_schoolid
                              ,optional_support
                              ,i_price_per_day_schoolid_ln
                              ,i_price_school_city_cummax_v2
                              ,i_price_dec_over_min_school_zip_v2
                              ,title_cnt_ln
                              ,essay_cnt_ln
                              ,resource_type_v2
                              ,School.Type_v3
                              ,cnt_donations_schoolid_cumsum_ln
                              ,cnt_donations_teacher_acctid_cumsum
                              ,essay_tfidf))
set.seed(9141260)
glmnet.kdd.day<-cv.glmnet(mm.glmnet[d2$test==0&d2$date_posted>=as.Date('2011-01-01','%Y-%m-%d'),],
                          d2$y[d2$test==0&d2$date_posted>=as.Date('2011-01-01','%Y-%m-%d')],
                          family='binomial',
                          alpha=0,
                          lambda.min.ratio=0.005)
d2$pred.day.glmnet<-predict(glmnet.kdd.day, newx=mm.glmnet, s="lambda.min", type="response")[,1]

d2$pred.day<-(1*d2$pred.day.gbm+1*d2$pred.day.et+0*d2$pred.day.glmnet+0.2*d2$pred.day.rf)/2.2

tmp<-summarize(group_by(d2,date_posted),pred.day.mean=mean(pred.day,rm.na=TRUE))

d2<-left_join(d2,tmp,by='date_posted')

tmp$day<-with(tmp,as.numeric(as.Date('2014-05-12','%Y-%m-%d')-date_posted))
plot(tmp$day[tmp$date_posted>=as.Date('2014-01-01','%Y-%m-%d')],tmp$pred.day.mean[tmp$date_posted>=as.Date('2014-01-01','%Y-%m-%d')])

d2$pred<-with(d2,ifelse(!is.na(factor),pred.day*factor,pred.day))

tmp<-summarize(group_by(d2,date_posted),pred.day.mean=mean(pred,rm.na=TRUE))
tmp$day<-with(tmp,as.numeric(as.Date('2014-05-12','%Y-%m-%d')-date_posted))
plot(tmp$day[tmp$date_posted>=as.Date('2014-01-01','%Y-%m-%d')],tmp$pred.day.mean[tmp$date_posted>=as.Date('2014-01-01','%Y-%m-%d')])

out<-select(d2[d2$test==1,],projectid,is_exciting=pred)
write.csv(out,file="model2.csv",row.names=FALSE,quote=FALSE)