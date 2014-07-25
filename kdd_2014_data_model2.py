import os, re, csv, string
import cPickle as pickle
import numpy as np
import sys
from sklearn import metrics, preprocessing, cross_validation
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import SGDClassifier, LogisticRegression
from nltk.tokenize import WhitespaceTokenizer


def clean_file(file_name, new_file_name):
    rf_file = open(file_name, 'rU')
    wf_file = open(new_file_name, 'w')
    count = 0
    for line in rf_file:
        count += 1
        if count % 100000 == 0:
            print count
        wf_file.write(line.replace('\x00', ''))
    print count
    rf_file.close()
    wf_file.close()

def count(filename, delim):
    csv.field_size_limit(99999999)
    rf = open(filename, 'rb')
    rf_csv = csv.reader(rf, delimiter = delim, quotechar = '"')
    count_dic = {}
    count = 0
    for project in rf_csv:
        count += 1
        if count % 10000 == 0:
            print "Lines read in essays.csv: " + str(count)
        if project[0].strip() not in count_dic:
            title_token = WhitespaceTokenizer().tokenize(project[2].strip())
            short_description_token = WhitespaceTokenizer().tokenize(project[3].strip())
            need_statement_token = WhitespaceTokenizer().tokenize(project[4].strip())
            essay_token = WhitespaceTokenizer().tokenize(project[5].strip())
            count_dic[project[0].strip()] = (len(title_token), len(short_description_token), len(need_statement_token), len(essay_token))
    print "Lines read in essays.csv: " + str(count)
    rf.close()
    return count_dic            

def print_rslt(count_dic,folder):
    fn = os.path.join(folder,"count.csv")
    wf = open(fn, 'w')
    wf.write('projectid,title_cnt,short_description_cnt,need_statement_cnt,essay_cnt\n')
    for project in count_dic.keys():
        wf.write(project + ',' + str(count_dic[project][0]) + ',' + str(count_dic[project][1]) + ',' + str(count_dic[project][2]) + ',' + str(count_dic[project][3]) + '\n')
    wf.close()

def import_projectlist(data):
    data = open(data, 'rU')
    data_csv = csv.reader(data, delimiter = ',', quotechar = '"')
    project_dic = {}
    heading = 1
    for line in data_csv:
        if heading:
            heading = 0
        else:
            project_dic[line[0]] = [line[1], line[2], line[3]]
    data.close()
    return project_dic

def read_essays(data, project_dic):
    data = open(data, 'rU')
    data_csv = csv.reader(data, delimiter = ',', quotechar = '"')
    project_dic_train = {}
    project_dic_validation = {}
    project_dic_test = {}
    heading = 1
    count = 0
    count_train = 0
    count_validation = 0
    count_test = 0
    for line in data_csv:
        count += 1
        if count % 100000 == 0:
            print count
        if heading:
            heading = 0
        else:
            if line[0] in project_dic:
                if project_dic[line[0]][0] == '0':
                    project_dic_train[line[0]] = [line[2], line[3], line[4], line[5], project_dic[line[0]][2]]
                    count_train += 1
                else:
                    if project_dic[line[0]][1] == '0':
                        project_dic_validation[line[0]] = [line[2], line[3], line[4], line[5], project_dic[line[0]][2]]
                        count_validation += 1
                    else:
                        project_dic_test[line[0]] = [line[2], line[3], line[4], line[5]]
                        count_test += 1
    print count
    print "Train Count: ", count_train
    print "Validation Count: ", count_validation
    print "Test Count: ", count_test
    data.close()

    title_train = []
    short_description_train = []
    need_statement_train = []
    essay_train = []
    y_train = []
    project_train = []
    count_train = 0
    for project in project_dic_train.keys():
        count_train += 1
        project_train.append(project)
        y_train.append(int(project_dic_train[project][4]))
        title_train.append(project_dic_train[project][0])
        short_description_train.append(project_dic_train[project][1])
        need_statement_train.append(project_dic_train[project][2])
        essay_train.append(project_dic_train[project][3])
    print "Train Count: ", count_train

    title_validation = []
    short_description_validation = []
    need_statement_validation = []
    essay_validation = []
    y_validation = []
    project_validation = []
    count_validation = 0
    for project in project_dic_validation.keys():
        count_validation += 1
        project_validation.append(project)
        y_validation.append(int(project_dic_validation[project][4]))
        title_validation.append(project_dic_validation[project][0])
        short_description_validation.append(project_dic_validation[project][1])
        need_statement_validation.append(project_dic_validation[project][2])
        essay_validation.append(project_dic_validation[project][3])
    print "Validation Count: ", count_validation

    title_test = []
    short_description_test = []
    need_statement_test = []
    essay_test = []
    project_test = []
    count_test = 0
    for project in project_dic_test.keys():
        count_test += 1
        project_test.append(project)
        title_test.append(project_dic_test[project][0])
        short_description_test.append(project_dic_test[project][1])
        need_statement_test.append(project_dic_test[project][2])
        essay_test.append(project_dic_test[project][3])
    print "Test Count: ", count_test

    return np.asarray(y_train), np.asarray(y_validation), np.asarray(project_train), np.asarray(project_validation), np.asarray(project_test), title_train, title_validation, title_test, short_description_train, short_description_validation, short_description_test, need_statement_train, need_statement_validation, need_statement_test, essay_train, essay_validation, essay_test

def score(y_train, y_validation, text_train, text_validation, text_test, project_train, project_validation, project_test, ngrams_max, freq_min, sgd_alpha, sgd_iter, rand_seed):
    tfv = TfidfVectorizer(min_df=freq_min, max_features=None, decode_error = 'ignore', strip_accents='unicode', analyzer='word', token_pattern=r'[a-zA-Z]{1,}', ngram_range=(1, ngrams_max), use_idf=1, smooth_idf=1, sublinear_tf=1)

    rand_state = np.random.RandomState(seed=rand_seed)
    sgd = SGDClassifier(loss="log", penalty="l2", shuffle=True, verbose=0, n_iter=sgd_iter, fit_intercept=True, alpha=sgd_alpha)

    rand = np.ravel(rand_state.rand(len(y_train),1))
    group = []
    for number in rand:
        if number > 0.9:
            group.append(1)
        elif number > 0.8:
            group.append(2)
        elif number > 0.7:
            group.append(3)
        elif number > 0.6:
            group.append(4)
        elif number > 0.5:
            group.append(5)
        elif number > 0.4:
            group.append(6)
        elif number > 0.3:
            group.append(7)
        elif number > 0.2:
            group.append(8)
        elif number > 0.1:
            group.append(9)
        else:
            group.append(10)
    for i in range(1,11):
        text_train_tmp = []
        text_test_tmp = []
        y_train_tmp = []
        y_test_tmp = []
        project_train_tmp = []
        project_test_tmp = []
        for j in range(0,len(group)):
            if group[j] == i:
                text_test_tmp.append(text_train[j])
                y_test_tmp.append(y_train[j])
                project_test_tmp.append(project_train[j])
            else:
                text_train_tmp.append(text_train[j])
                y_train_tmp.append(y_train[j])
                project_train_tmp.append(project_train[j])
        project_test_tmp = np.asarray(project_test_tmp)
        tfv.fit(text_train_tmp)
        x_train = tfv.transform(text_train_tmp)
        sgd.fit(x_train,y_train_tmp)
        x_test = tfv.transform(text_test_tmp)
        prob = sgd.predict_proba(x_test)[:,1]
        if i == 1:
            result_train = np.vstack((project_test_tmp,prob))
            print "Group: ", i
        else:
            tmp = np.vstack((project_test_tmp,prob))
            result_train = np.hstack((result_train,tmp))
            print "Group: ", i

    tfv.fit(text_train)
    x_train = tfv.transform(text_train)
    sgd.fit(x_train,y_train)
    x_validation = tfv.transform(text_validation)
    prob = sgd.predict_proba(x_validation)[:,1]
    result_validation = np.vstack((project_validation,prob))

    text_train_validation = text_train + text_validation
    y_train_validation = np.hstack((y_train, y_validation))
    tfv.fit(text_train_validation)
    x_train_validation = tfv.transform(text_train_validation)
    sgd.fit(x_train_validation, y_train_validation)
    x_test = tfv.transform(text_test)
    prob = sgd.predict_proba(x_test)[:,1]
    result_test = np.vstack((project_test, prob))

    result = np.hstack((result_train, result_validation, result_test))

    return result

def print_result(result, filename, varname):
    wfile = open(filename, "w")
    wfile.write("projectid,"+varname+"\n")
    for i in range(0,len(result[0])):
        wfile.write(result[0][i]+","+result[1][i]+"\n")
    wfile.close()

if __name__=="__main__":    
    folder = sys.argv[1]
    os.chdir(folder)
    csv.field_size_limit(999999999)
    clean_file("essays.csv","essays_v2.csv")
    count_dic = count("essays_v2.csv",',')
    print_rslt(count_dic,folder)
    
    project_dic = import_projectlist("projectlist.csv")
    y_train, y_validation, project_train, project_validation, project_test, title_train, title_validation, title_test, short_description_train, short_description_validation, short_description_test, need_statement_train, need_statement_validation, need_statement_test, essay_train, essay_validation, essay_test = read_essays("essays_v2.csv", project_dic)
    
    result_title = score(y_train, y_validation, title_train, title_validation, title_test, project_train, project_validation, project_test, ngrams_max = 3, freq_min = 10, sgd_alpha = 0.00005, sgd_iter = 20, rand_seed = 3165832)
    result_short_description = score(y_train, y_validation, short_description_train, short_description_validation, short_description_test, project_train, project_validation, project_test, ngrams_max = 2, freq_min = 10, sgd_alpha = 0.00001, sgd_iter = 20, rand_seed = 2105827)
    result_need_statement = score(y_train, y_validation, need_statement_train, need_statement_validation, need_statement_test, project_train, project_validation, project_test, ngrams_max = 2, freq_min = 10, sgd_alpha = 0.00005, sgd_iter = 20, rand_seed = 7392409)
    result_essay = score(y_train, y_validation, essay_train, essay_validation, essay_test, project_train, project_validation, project_test, ngrams_max = 4, freq_min = 2, sgd_alpha = 0.000004, sgd_iter = 20, rand_seed = 3866942)
    
    print_result(result_title, "result_title.csv","title_tfidf")
    print_result(result_short_description, "result_short_description.csv","short_description_tfidf")
    print_result(result_need_statement, "result_need_statement.csv","need_statement_tfidf")
    print_result(result_essay, "result_essay_20140710.csv","essay_tfidf")
