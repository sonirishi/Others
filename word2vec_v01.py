# -*- coding: utf-8 -*-
"""
Created on Tue Jul 19 22:50:17 2016

@author: rsoni106
"""

import pandas as pd
from gensim.models import Word2Vec
import numpy as np
import nltk
from nltk.stem import WordNetLemmatizer
from nltk import word_tokenize

lemma_func = WordNetLemmatizer()

train_df = pd.read_csv("C:\\Users\\rsoni106\\Documents\\Work\Methodology Work\\Kaggle\\Completed\\HomeDepot\\train.csv",
                       encoding = 'iso-8859-1')
                       
test_df = pd.read_csv("C:\\Users\\rsoni106\\Documents\\Work\Methodology Work\\Kaggle\\Completed\\HomeDepot\\test.csv",
                       encoding = 'iso-8859-1')

path_w2v_pretrained_model = "C:\\Users\\rsoni106\\Documents\\Work\Methodology Work\\Kaggle\\GoogleNews-vectors-negative300.bin"

def lemmatize_only(data,var):
    final_data = []
    for i in range(data.shape[0]):
        token = (data.iloc[i][var]).split()
        lemma_temp1 = ""
        for words in token:
            temp = lemma_func.lemmatize(words)
            lemma_temp1 = str(lemma_temp1) + " " + temp
        final_data.append(lemma_temp1.lstrip())
    return np.array(final_data)
    
train_df["search_lemma"] = lemmatize_only(train_df,"search_term")
train_df["product_lemma"] = lemmatize_only(train_df,"product_title")

test_df["search_lemma"] = lemmatize_only(test_df,"search_term")
test_df["product_lemma"] = lemmatize_only(test_df,"product_title")

def calc_w2v_sim(row):
    '''
    Calc w2v similarities and diff of centers of query\title
    '''
    a2 = [x for x in row['search_lemma'].lower().split() if x in similarity_model.vocab]
    b2 = [x for x in row['product_lemma'].lower().split() if x in similarity_model.vocab]
    if len(a2)>0 and len(b2)>0:
        w2v_sim = similarity_model.n_similarity(a2, b2)
    else:
        return((-1, -1, np.zeros(300)))
    
    vectorA = np.zeros(300)
    for w in a2:
        vectorA += similarity_model[w]
    vectorA /= len(a2)

    vectorB = np.zeros(300)
    for w in b2:
        vectorB += similarity_model[w]
    vectorB /= len(b2)

    vector_diff = (vectorA - vectorB)

    w2v_vdiff_dist = np.sqrt(np.sum(vector_diff**2))
    return (w2v_sim, w2v_vdiff_dist, vector_diff)

similarity_model = Word2Vec.load_word2vec_format(path_w2v_pretrained_model, binary=True)

"""
def load_word2vec(file):
    word2vec_new = {}
    fin= open(file)    
    for line in fin:
        items = line.replace('\r','').replace('\n','').split(' ')
        if len(items) < 10: continue
    word = items[0]
    vect = np.array([float(i) for i in items[1:] if len(i) > 1])
    word2vec_new[word] = vect
"""

X_w2v = []
sim_list = []
dist_list = []
for i,row in train_df.iterrows():
    sim, dist, vdiff = calc_w2v_sim(row)
    X_w2v.append(vdiff)
    sim_list.append(sim)
    dist_list.append(dist)
X_w2v_tr = np.array(X_w2v)
train_df['w2v_sim'] = np.array(sim_list)
train_df['w2v_dist'] = np.array(dist_list)

# for test
X_w2v = []
sim_list = []
dist_list = []
for i,row in test_df.iterrows():
    sim, dist, vdiff = calc_w2v_sim(row)
    X_w2v.append(vdiff)
    sim_list.append(sim)
    dist_list.append(dist)
X_w2v_te = np.array(X_w2v)
test_df['w2v_sim'] = np.array(sim_list)
test_df['w2v_dist'] = np.array(dist_list)

train_var = train_df[["w2v_sim","w2v_dist"]]
test_var = test_df[["w2v_sim","w2v_dist"]]

train_var.to_csv("C:\\Users\\rsoni106\\Documents\\Work\Methodology Work\\Kaggle\\Completed\\HomeDepot\\train_word2vec.csv")

test_var.to_csv("C:\\Users\\rsoni106\\Documents\\Work\Methodology Work\\Kaggle\\Completed\\HomeDepot\\test_word2vec.csv")

from sklearn.cross_validation import cross_val_score
from sklearn.ensemble import RandomForestRegressor

rf_model = RandomForestRegressor(max_depth = 6, min_samples_leaf = 10, n_jobs = -1, 
                                  n_estimators = 500, oob_score = True)

scores = cross_val_score(rf_model, train_df[["w2v_sim","w2v_dist"]], np.ravel(train_df["relevance"]))

rf_model.fit(train_df[["w2v_sim","w2v_dist"]], np.ravel(train_df["relevance"]))