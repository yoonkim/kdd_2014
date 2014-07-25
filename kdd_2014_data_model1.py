# -*- coding: utf-8 -*-
"""
Created on Tue May 20 13:44:46 2014

@author: yookim
"""
import re
import os
import sys
import pandas as pd
import numpy as np
from sklearn import metrics
from sklearn.linear_model import SGDClassifier
from sklearn.feature_extraction.text import TfidfVectorizer
from datetime import date

def build_tfidf_models_cv(df, include_val=False, y="y", givens = None, cv=10, text_vars=["title", "short_description", "need_statement", "essay"]):
    df["r"] = np.random.uniform(0,1,size=len(df))
    if include_val:
        split_tr = (df["split"]=="train") | (df["split"]=="val")
    else:
        split_tr = df["split"]=="train"
    if givens:
        for g in givens:
            split_tr = split_tr & (df[g]==1)   
    y_train = df[y][split_tr].values
    probs = np.arange(0,1. + 1./cv, 1./cv)
    for var in text_vars:
        new_var_name = var+"_pred_partial"
        df[new_var_name] = 0.0
        vectorizer = TfidfVectorizer(min_df=2,
                                     use_idf=1,
                                     smooth_idf=1,
                                     sublinear_tf=1,
                                     ngram_range=(1,2), 
                                     token_pattern=r"(?u)\b[A-Za-z0-9()\'\-?!\"%]+\b",
                                     norm='l2')    
        vectorizer.fit(df[var][(df["split"]=="train") | (df["split"]=="val") | (df["split"]=="test")])
        tfidf_train = vectorizer.transform(df[var][split_tr])
        tfidf_all = vectorizer.transform(df[var])        
        lm_model = SGDClassifier(penalty="l2",loss="log",fit_intercept=True, shuffle=True,n_iter=20, n_jobs=-1,alpha=0.000005)
        lm_model.fit(tfidf_train, y_train)
        df[var+"_pred"] = lm_model.predict_proba(tfidf_all)[:,1]
        for i in range(cv):             
            split_train_test = (split_tr) & ((df['r']>=probs[i]) & (df['r']<probs[i+1]))            
            split_train_train = (split_tr) & ((df['r']<probs[i]) | (df['r']>=probs[i+1]))          
            lm_model_temp = SGDClassifier(penalty="l2",loss="log",fit_intercept=True, shuffle=True,n_iter=20, n_jobs=-1,alpha=0.000005)
            x_train_train = vectorizer.transform(df[var][split_train_train])
            x_train_test = vectorizer.transform(df[var][split_train_test])
            lm_model_temp.fit(x_train_train, df[y][split_train_train].values)                       
            pred_train_test = lm_model_temp.predict_proba(x_train_test)[:,1]
            pred_train_train = lm_model_temp.predict_proba(x_train_train)[:,1]
            df[new_var_name][split_train_test] = pred_train_test                                                
            print 'CV: ' + str(i+1)
            print 'AUC (Train_Train): ' + str(metrics.roc_auc_score(df['y'][split_train_train],pred_train_train))
            print 'AUC (Train_Test): ' + str(metrics.roc_auc_score(df['y'][split_train_test],pred_train_test))  
    
def get_pred_partials(df, y="y",id_var="projectid", text_vars=["title", "short_description", "need_statement", "essay"]):
    df2 = pd.DataFrame(df["projectid"])
    for var in text_vars:
        df2[var+"_"+y+"_pred_partial"] = df[var+"_pred_partial"]
        df2[var+"_"+y+"_pred"] = df[var+"_pred"]
    return df2
    
def get_length(string):
    return len(string.split())
        
def clean_essay(string, lower=False):
    string = re.sub(r"\\t", " ", string)   
    string = re.sub(r"\\n", " ", string)   
    string = re.sub(r"\\r", " ", string)   
    string = re.sub(r"[^A-Za-z0-9\']", " ", string)   
    string = re.sub(r"\s{2,}", " ", string)
    if lower:
        string = string.lower()
    return string.strip()

def get_prev_exp(df, to_file, var="teacher_acctid", responses=["y","y2","y3","y4","y5","y6","y7","y8","y9","y10"]):
    df = df.sort([var, "date_posted"])
    df.index = range(len(df))    
    with open(to_file, "wb") as f:
        f.write(",".join(["projectid",var,"date_posted",var+"_days"] + [var+"_"+r+"_prev" for r in responses] + [var+"_cnt",var+"_cnt_train"])+"\n")   
        prev_var = 0
        for i, v in enumerate(df[var]):
            if prev_var != v:
                prev_var = v
                count_vec = [0]*(len(responses)+2)
                days_btw = 9999                
                f.write(",".join([df["projectid"][i],str(v),df["date_posted"][i],str(days_btw)] + [str(count) for count in count_vec])+"\n")
            else:
                prev_date = date(int(df["year"][i-1]), int(df["month"][i-1]), int(df["day"][i-1]))
                now_date = date(int(df["year"][i]), int(df["month"][i]), int(df["day"][i]))
                days_btw = (now_date - prev_date).days
                f.write(",".join([df["projectid"][i],str(v),df["date_posted"][i],str(days_btw)] + [str(count) for count in count_vec])+"\n")
            for j, r in enumerate(responses):
                count_vec[j] += df[r][i]        
            count_vec[j+1] += 1    
            if df["date_posted"][i] < "2014-01-01":
                count_vec[j+2] += 1
    
if __name__=="__main__":    
    folder = sys.argv[1]
    print "Loading files..."
    outcomes_df = pd.read_csv(os.path.join(folder, "outcomes.csv"))
    projects_df = pd.read_csv(os.path.join(folder, "projects.csv"))
    donations_df = pd.read_csv(os.path.join(folder, "donations.csv"))
   
    print "Getting previous experience features..."
    df = pd.merge(projects_df, outcomes_df, how='left', on='projectid')
    df["split"] = "train"
    df["split"][df["date_posted"]<"2010-04-01"] = "none"
    df["split"][df["date_posted"]>="2013-01-01"] = "val"
    df["split"][df["date_posted"]>="2014-01-01"]= "test"
    df = df[df["split"]!="none"]
    df["y"] = 0
    df["y"][df["is_exciting"]=="t"] = 1
    df["y2"] = 0
    df["y2"][df["at_least_1_teacher_referred_donor"]=="t"] = 1
    df["y3"] = 0
    df["y3"][df["great_chat"]=="t"] = 1
    df["y4"] = 0
    df["y4"][df["fully_funded"]=="t"] = 1
    df["y5"] = 0
    df["y5"][df["at_least_1_green_donation"]=="t"] = 1
    df["y6"] = 0
    df["y6"][df["donation_from_thoughtful_donor"]=="t"] = 1
    df["y7"] = 0
    df["y7"][df["three_or_more_non_teacher_referred_donors"]=="t"] = 1
    df["y8"] = 0
    df["y8"][df["one_non_teacher_referred_donor_giving_100_plus"]=="t"] = 1
    df["y9"] = 0
    df["y9"][df["teacher_referred_count"]>=1] = 1
    df["y10"] = 0
    df["y10"][df["non_teacher_referred_count"]>=1]=1
    df["year"] = df["date_posted"].apply(lambda x: x.split("-")[0])
    df["month"] = df["date_posted"].apply(lambda x: x.split("-")[1])
    df["day"] = df["date_posted"].apply(lambda x: x.split("-")[2])
    for var in ["teacher_acctid", "schoolid", "school_district", "school_city", "school_county", "school_zip", "school_state"]:
        get_prev_exp(df,os.path.join(folder,var+"_exp_20100401.csv"),var,["y","y2","y3","y4","y5","y6","y7","y8","y9","y10"])
    del df
       
    df = pd.merge(projects_df, donations_df, how='left', on='projectid')
    df["y11"] = 1
    df["y12"] = 0
    df["y12"][df["is_teacher_acct"]=="t"] = 1
    df["y13"] = df["donation_total"]
    df["y13"][pd.isnull(df["donation_total"])] = 0
    df2=df.groupby(by=["projectid","date_posted","teacher_acctid","schoolid","school_district","school_city","school_zip"])[["y11","y12","y13"]].sum()
    df2.reset_index(inplace=True)
    df2["year"] = df2["date_posted"].apply(lambda x: x.split("-")[0])
    df2["month"] = df2["date_posted"].apply(lambda x: x.split("-")[1])
    df2["day"] = df2["date_posted"].apply(lambda x: x.split("-")[2])
    for var in ["teacher_acctid", "schoolid", "school_district", "school_city", "school_zip"]:
        get_prev_exp(df2,os.path.join(folder,"donor_"+var+".csv"), var, ["y11","y12","y13"])
    del df
        
    print "Getting essay features..."
    df = pd.read_csv(os.path.join(folder, "essays.csv"))
    df = pd.merge(df, outcomes_df, how = 'left', on = 'projectid')
    df = pd.merge(df, projects_df, how = 'inner', on = 'projectid')
    df["split"] = "train"
    df["split"][df["date_posted"]<"2010-04-01"] = "none"
    df["split"][df["date_posted"]>="2013-01-01"] = "val"
    df["split"][df["date_posted"]>="2014-01-01"]= "test"
    df = df[df["split"]!="none"]
    df["y"] = 0
    df["y"][df["is_exciting"]=="t"] = 1
    text_vars=["title", "short_description", "need_statement", "essay"]
    for var in text_vars:
        df[var][pd.isnull(df[var])] = ""
        df[var] = df[var].apply(clean_essay)
        df[var+"_length"] = df[var].apply(get_length)    
    build_tfidf_models_cv(df, include_val=False, y="y", givens = None, cv=10, text_vars=["title", "short_description", "need_statement", "essay"])
    df2 = get_pred_partials(df, y="y",id_var="projectid", text_vars=["title", "short_description", "need_statement", "essay"])
    df2["title_length"] = df["title_length"]
    df2["essay_length"] = df["essay_length"]
    df2.to_csv(os.path.join(folder,"essays_pred_val_y.csv"), index=False)
    del df
    del df2