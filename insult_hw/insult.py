
# Usage: python scriptname outputfilename

import numpy as np
import pandas as pd
import csv
import sys

from sklearn.naive_bayes import MultinomialNB
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.cross_validation import cross_val_score
from sklearn.metrics import auc_score

train = pd.read_csv("/Users/loganweir/DataScience/GADS/my_homework/insult_hw/detrain.csv")

test = pd.read_csv("/Users/loganweir/DataScience/GADS/my_homework/insult_hw/detest.csv")

Comment = train['Comment']

TestComment = test['Comment']

ID1 = test['id']

ID2 = np.array(ID1)

ID = np.reshape(ID2, (2235,1))

Combined = Comment.append(TestComment)

All = np.array(Combined)

vectorizer = CountVectorizer(stop_words='english', ngram_range=(1,3))

Insult =  np.array(train['Insult'])

x_train = vectorizer.fit_transform(All)

model = MultinomialNB()

classifier = model.fit(x_train.toarray()[0:3947,:], Insult)

print(cross_val_score(model, x_train.toarray()[0:3947,:], Insult, cv = 10, score_func=auc_score).mean())

close = classifier.predict_proba(x_train.toarray()[3947:,:])

a = close[:,1:]

finalarray = np.hstack((ID,a))

headers = np.array(['id','Insult'])

predictions = np.vstack((headers, finalarray))

outfile = open(sys.argv[1], 'w')

writer = csv.writer(outfile)

for row in predictions:
	writer.writerow(row)

outfile.close()






