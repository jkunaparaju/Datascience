
# coding: utf-8

#  ## Recommender Systems: Collaborative Filtering

# In[7]:

import pandas as pd


# In[8]:


names = ['userid', 'itemid', 'rating','id','user_id','item_id' ]
df = pd.read_csv('/Users/jyothi/Desktop/Movie/out.csv', sep=',', names=names ,low_memory=False)
df.tail()


# In[3]:

df.head()


# In[4]:

import matplotlib.pyplot as plt 


# ## Histograms  -- Ratings,  User and Item

# In[5]:

plt.hist(df['rating'])
plt.show()


# In[6]:

plt.hist(df['itemid'])
plt.show()


# In[7]:

plt.hist(df['userid'])
plt.show()


# In[8]:

n_users = df.userid.unique().shape[0]
n_users


# ## Creating Matrix

# In[9]:

df.userid.max()


# In[10]:

df.itemid.max()


# In[11]:


n_items = df['itemid'].unique().shape[ 0]
n_items


# In[12]:

import numpy as np
ratings = np.zeros((df.userid.max(), df.itemid.max()))
for row in df.itertuples():
    ratings[row[5]-1, row[ 6]-1] = row[3]


# In[21]:

ratings.shape


# ## Find the Sparsity of Matrix 

# In[13]:

import sklearn


# In[39]:

#ratings = np.zeros((6040, 3952))


# In[14]:

ratings.shape


# In[15]:

for row in df.itertuples():
    ratings[row[1]-1, row[2]-1] = row[3]
ratings


# ## This means that 4.18% of the user-item ratings have a valu

# In[16]:

sparsity = float(len(ratings.nonzero()[0]))
sparsity /= (ratings.shape[0] * ratings.shape[1])
sparsity *= 100
print( 'Sparsity: {:4.2f}%'.format(sparsity))


# ### Splitting into Train and test. We will split our data into training and test sets by removing 10 ratings per user from the training set and placing them in the test set.

# In[17]:

def train_test_split(ratings):
    test = np.zeros(ratings.shape)
    train = ratings.copy()
    for user in range(ratings.shape[0]):
        test_ratings = np.random.choice(ratings[user, :].nonzero()[0], 
                                        size=10)
        train[user, test_ratings] = 0.
        test[user, test_ratings] = ratings[user, test_ratings]
        
    # Test and training are truly disjoint
    assert(np.all((train * test) == 0)) 
    return train, test


# In[18]:

train, test = train_test_split(ratings)


# ### Sparsity of Train and Test 

# In[19]:

sparsity = float(len(train.nonzero()[0]))
sparsity /= (train.shape[0] * train.shape[1])
sparsity *= 100
print( 'Sparsity: {:4.2f}%'.format(sparsity))


# In[20]:

sparsity = float(len(test.nonzero()[0]))
sparsity /= (test.shape[0] * test.shape[1])
sparsity *= 100
print( 'Sparsity: {:4.2f}%'.format(sparsity))


# ## Find Similarity Matrix

# In[21]:

def fast_similarity(ratings, kind='user', epsilon=1e-9):
    # epsilon -> small number for handling dived-by-zero errors
    if kind == 'user':
        sim = ratings.dot(ratings.T) + epsilon
    elif kind == 'item':
        sim = ratings.T.dot(ratings) + epsilon
    norms = np.array([np.sqrt(np.diagonal(sim))])
    return (sim / norms / norms.T)


# In[35]:

get_ipython().magic("timeit fast_similarity(train, kind='user')")


# In[22]:

user_similarity = fast_similarity(train, kind='user')
item_similarity = fast_similarity(train, kind='item')
print(item_similarity[:4, :4])


# In[37]:

user_similarity


# In[38]:

item_similarity


# ## Predict unknown ratings of each user 

# In[39]:

def predict_fast_simple(ratings, similarity, kind='user'):
    if kind == 'user':
        return similarity.dot(ratings) / np.array([np.abs(similarity).sum(axis=1)]).T
    elif kind == 'item':
        return ratings.dot(similarity) / np.array([np.abs(similarity).sum(axis=1)])


# In[40]:

get_ipython().magic("timeit predict_fast_simple(train, user_similarity, kind='user')")


# In[41]:

from sklearn.metrics import mean_squared_error

def get_mse(pred, actual):
    # Ignore nonzero terms.
    pred = pred[actual.nonzero()].flatten()
    actual = actual[actual.nonzero()].flatten()
    return mean_squared_error(pred, actual)


# ## Find MSE for User and  Item predictions. 

# In[42]:

item_prediction = predict_fast_simple(train, item_similarity, kind='item')
user_prediction = predict_fast_simple(train, user_similarity, kind='user')

print ('User-based CF MSE: ' + str(get_mse(user_prediction, test)))
print ('Item-based CF MSE: ' + str(get_mse(item_prediction, test)))


# ### Item Based Filtering is less efficient than User based Filtering

# In[43]:

def predict_topk(ratings, similarity, kind='user', k=40):
    pred = np.zeros(ratings.shape)
    if kind == 'user':
        for i in range(ratings.shape[0]):
            top_k_users = [np.argsort(similarity[:,i])[:-k-1:-1]]
            for j in range(ratings.shape[1]):
                pred[i, j] = similarity[i, :][top_k_users].dot(ratings[:, j][top_k_users]) 
                pred[i, j] /= np.sum(np.abs(similarity[i, :][top_k_users]))
    if kind == 'item':
        for j in range(ratings.shape[1]):
            top_k_items = [np.argsort(similarity[:,j])[:-k-1:-1]]
            for i in range(ratings.shape[0]):
                pred[i, j] = similarity[j, :][top_k_items].dot(ratings[i, :][top_k_items].T) 
                pred[i, j] /= np.sum(np.abs(similarity[j, :][top_k_items]))        
    
    return pred


# ## Now predict Ratings from top K users ratings. Here we are using Top 40 Users

# In[44]:

user_pred = predict_topk(train, user_similarity, kind='user', k=40)
print ('Top-k User-based CF MSE: ' + str(get_mse(user_pred, test)))

item_pred = predict_topk(train, item_similarity, kind='item', k=40)
print ('Top-k Item-based CF MSE: ' + str(get_mse(item_pred, test)))


# In[32]:

import numpy as np


# In[33]:

np.radians(30)


# ## Now MSE is lesser than previous prection.(Finding rating with total users and Top 40 users) 

# In[60]:

user_pred


# In[61]:

item_pred


# In[45]:


def get_mse(pred, actual):
    pred = pred[actual.nonzero()].flatten()
    actual = actual[actual.nonzero()].flatten()
    return mean_squared_error(pred, actual)


# ## Now we try to find best K value 

# In[46]:

user_train_mse =[]
user_test_mse=[]
item_train_mse =[]
item_test_mse=[]


# In[47]:

user_train_mse  += [get_mse(user_pred, train)]
user_test_mse   += [get_mse(user_pred, test)]


# In[48]:

item_train_mse  += [get_mse(item_pred, train)]
item_test_mse   += [get_mse(item_pred, test)]


# In[49]:

user_test_mse


# In[50]:

user_train_mse


# In[51]:

item_test_mse


# In[52]:

item_train_mse


# In[62]:

user_train_mse =[]
user_test_mse=[]
item_train_mse =[]
item_test_mse=[]
k_array = [5,15, 30, 50]
for k in k_array:
    user_pred = predict_topk(train, user_similarity, kind='user', k=k)  
    user_train_mse += [get_mse(user_pred, train)]
    user_test_mse += [get_mse(user_pred, test)]


# ## MSE values for k - 5,15, 30 , 50 

# In[63]:

user_train_mse


# In[64]:

user_test_mse


# In[65]:

k_array = [5,15, 30, 50]
for k in k_array:
    item_pred = predict_topk(train, item_similarity, kind='item', k=k)
    
    item_train_mse += [get_mse(item_pred, train)]
    item_test_mse += [get_mse(item_pred, test)]


# In[66]:

item_train_mse


# In[67]:

item_test_mse


# ## Plot the values 

# In[68]:

get_ipython().magic('matplotlib inline')
import matplotlib.pyplot as plt
import seaborn as sns
sns.set()

pal = sns.color_palette("Set2", 2)

plt.figure(figsize=(8, 8))
plt.plot(k_array, user_train_mse, c=pal[0], label='User-based train', alpha=0.5, linewidth=5)
plt.plot(k_array, user_test_mse, c=pal[0], label='User-based test', linewidth=5)
plt.plot(k_array, item_train_mse, c=pal[1], label='Item-based train', alpha=0.5, linewidth=5)
plt.plot(k_array, item_test_mse, c=pal[1], label='Item-based test', linewidth=5)
plt.legend(loc='best', fontsize=20)
plt.xticks(fontsize=16);
plt.yticks(fontsize=16);
plt.xlabel('k', fontsize=30);
plt.ylabel('MSE', fontsize=30);


# ## Looks 15 is the best value to choose 

# In[5]:

def top_k_movies(similarity, mapper, movie_idx, k=6):
    return [mapper[x] for x in np.argsort(similarity[movie_idx,:])[:-k-1:-1]]


# In[1]:

idx_to_movie = {}


# In[3]:

with open('/Users/jyothi/Desktop/Movie/movie.dat', encoding = "ISO-8859-1") as f:
    for line in f.readlines():
        info = line.split('::')
        idx_to_movie[int(info[0])-1] = info[1]


# In[23]:

idx = 0
movies = top_k_movies(item_similarity, idx_to_movie, idx)


# In[24]:

movies


# In[25]:

idx = 2
movies = top_k_movies(item_similarity, idx_to_movie, idx)


# In[26]:

movies


# In[27]:

idx = 1
movies = top_k_movies(item_similarity, idx_to_movie, idx)
movies


# In[31]:

idx = 4
movies = top_k_movies(item_similarity, idx_to_movie, idx)
movies


# In[ ]:



