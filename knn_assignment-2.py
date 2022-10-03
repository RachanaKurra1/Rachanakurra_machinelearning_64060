import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import accuracy_score, confusion_matrix

#Importing dataset
UniversalBank = pd.read_csv(r"C:\Users\kurra\Downloads\UniversalBank.csv")
UniversalBank.rename(columns={"ZIP Code":'ZIP_Code',"Personal Loan":"Personal_Loan"},inplace = True)

#Exploratory data analysis
UniversalBank.columns
UniversalBank.info()
UniversalBank.describe()

#checking for missing values
UniversalBank.isna().sum()

#Dropping zip code
UniversalBank = UniversalBank.drop({"ZIP_Code","ID"},axis=1)

#Creating Validation and Train data
predictor = np.array(UniversalBank.loc[:,UniversalBank.columns != "Personal_Loan"])
target = np.array(UniversalBank.Personal_Loan)

from sklearn.model_selection import train_test_split
x_train, x_validation , y_train, y_validation = train_test_split(predictor,target, test_size=0.4,random_state=21)


from sklearn.preprocessing import normalize
x_train = normalize(UniversalBank)
x_validation = normalize(UniversalBank) 

y_train = normalize(UniversalBank)
y_validation = normalize(UniversalBank) 


from sklearn.neighbors import KNeighborsClassifier
knn = KNeighborsClassifier(n_neighbors=7)
knn.fit(x_train,y_train)


#Predicting on test and train data
pred_validation = knn.predict(x_validation)
pred_train = knn.predict(x_train)
people_got_loan = pd.DataFrame(pred_train)

#People who got loan

people_got_loan.value_counts()

# error on test data
accuracy_score = accuracy_score(y_validation, pred_validation) * 100
accuracy_score
# error on train data

accuracy_score(y_train, pred_train)



##############################################################################
#Q1
#Testing the data
test_data =  pd.DataFrame({ 'Age':40, 'Experience':10, 'Income':84, 'Family':2, 'CCAvg':2,'Education':[0,1,0], 'Mortgage':0, 'Securities Account':0,'CD Account':0, 'Online':1, 'CreditCard':1})

#Normalizing the data
from sklearn.preprocessing import normalize
test_normalized = normalize(test_data)

test_normalized= pd.DataFrame(test_normalized)

test_normalized.columns = ['Age', 'Experience', 'Income', 'Family', 'CCAvg', 'Education','Mortgage', 'Securities Account', 'CD Account', 'Online', 'CreditCard']

test_predict = knn.predict(test_normalized)



##############################################################################
#Q2

accuracy = []
for i in range(3,60,2):
    knn = KNeighborsClassifier(n_neighbors=i)
    knn.fit(x_train,y_train)
    test_accuracy = np.mean(knn.predict(x_validation) == y_validation)
    train_accuracy = np.mean(knn.predict(x_train) == y_train)
    accuracy.append([test_accuracy,train_accuracy])


import matplotlib.pyplot as plt
plt.plot(np.arange(3,60,2),[i[0] for i in accuracy],"ro-")
plt.plot(np.arange(3,60,2),[i[1] for i in accuracy],"bs-")

# According to the graph for knn we get the best k value worst at 29
# We can say the knn  is overfitted at 29.

##############################################################################
#Q3
# According to the graph for knn we get the best k value to be 3.


from sklearn.neighbors import KNeighborsClassifier
knn = KNeighborsClassifier(n_neighbors=3)
knn.fit(x_train,y_train)

confusion_matrix(y_train, pred_train)


##############################################################################

from sklearn.neighbors import KNeighborsClassifier
knn = KNeighborsClassifier(n_neighbors=3)
knn.fit(x_train,y_train)

#Testing the data Q4
test_data_q4 =  pd.DataFrame({ 'Age':40, 'Experience':10, 'Income':84, 'Family':2, 'CCAvg':2,'Education':[0,1,0], 'Mortgage':0, 'Securities Account':0,'CD Account':0, 'Online':1, 'CreditCard':1})

#Normalizing the data
from sklearn.preprocessing import normalize
test_data_q4 = normalize(test_data_q4)

test_data_q4= pd.DataFrame(test_data_q4)

test_data_q4.columns = ['Age', 'Experience', 'Income', 'Family', 'CCAvg', 'Education','Mortgage', 'Securities Account', 'CD Account', 'Online', 'CreditCard']

#Predicting the test values for model
test_predict_q4 = knn.predict(test_data_q4)

##########################################################################