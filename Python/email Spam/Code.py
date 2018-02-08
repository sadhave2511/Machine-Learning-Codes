# -*- coding: utf-8 -*-
"""
Created on Fri Feb  2 18:51:56 2018

@author: sadha
"""
import os
from collections import Counter
import random 
import numpy as np
from sklearn.naive_bayes import GaussianNB, MultinomialNB
from sklearn.metrics import accuracy_score
from sklearn.svm import SVC, LinearSVC


def word_extract(root_dir):
    all_w = []
    mail_file = [os.path.join(root_dir, fil) for fil in os.listdir(root_dir)]
    for i in mail_file:
        with open(i) as m:
            for l in m:
                w = l.split()
                all_w += w 
    words = Counter(all_w)
    return words
             
def matrix_form(root_dir_2):
    file = [os.path.join(root_dir_2, fil) for fil in os.listdir(root_dir_2)]
    row_num = 0
    x_matrix = np.zeros((len(file), 3000))
    y_matrix = np.zeros(len(file))
    count = 0
    for f in file:
        with open(f) as open_file:
            for i,line in enumerate(open_file):
                if i == 2:
                    w_split = line.split()
                    for word in w_split:
                        d = 0
                        for j,dicti in enumerate(word_fin):
                            if dicti[0] == word:
                                d = j
                                x_matrix[row_num,d] = w_split.count(word)
        f_split = f.split('\\')
        f_name = f_split[(len(f_split))-1]
        if f_name.startswith("spmsg"):
            y_matrix[row_num] = 1
            count = count+1
        row_num = row_num+1
    return x_matrix, y_matrix

"""Loading data"""

train = "C:/Users/sadha/Desktop/train-mails"
word_y = word_extract(train)
len(word_y)
#20600

test = "C:/Users/sadha/Desktop/test-mails"
word_z = word_extract(test)
len(word_z)
#13633

""" To delete single letters and non words"""
#train
ke = word_y.keys()
to_remove = []
for i in ke:
    if (i.isalpha() == False) or (len(i) == 1):
        to_remove += i
        del word_y[i]
len(to_remove)
for i in to_remove:
    del word_y[i]
len(word_y)
#20533

#test
ke = word_z.keys()
for i in ke:
    if (i.isalpha() == False) or (len(i) == 1):
        to_remove += i
for i in to_remove:
    del word_z[i]
len(word_z)
#13569


"""Most Common words"""
#train
word_fin = word_y.most_common(3000)
len(word_fin)
#3000

#test
word_fin_test = word_z.most_common(3000)
len(word_fin_test)
#3000

"""Matrix formation """
x,y = matrix_form(train)
x_test,y_test = matrix_form(test)

np.sum(x)
#163430
np.sum(y)
#351
np.sum(x_test)
#57311
np.sum(y_test)
#130

"""Model"""

model = GaussianNB()
#train model
model.fit(x, y)
predictions = model.predict(x_test)
accuracy_score(y_test, predictions)
#0.96153846153846156


model2 = MultinomialNB()
model2.fit(x,y)
predictions2 = model2.predict(x_test)
accuracy_score(y_test, predictions2)
#0.95769230769230773

model3 = LinearSVC()
model3.fit(x,y)
predictions3 = model3.predict(x_test)
accuracy_score(y_test, predictions3)
#96153846153846156

