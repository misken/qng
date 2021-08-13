import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Make the graphs a bit prettier, and bigger
# pd.set_option('display.mpl_style', 'default')
# pd.set_option('display.width', 5000)
# pd.set_option('display.max_columns', 60)


# In[2]:




# In[3]:

from sklearn import linear_model


# In[4]:

#import hillmaker as hm
from pandas import Timestamp
import re
from datetime import datetime


# In[5]:

# For now I'm just sticking qng.py into this directory
import qng


# In[6]:

## Read training data set
train_df = pd.read_csv('train_exp9_tandem05_nodischadj.csv')
#train_df.info()

# Blocking related approximations

prob_blocked_by_pp_approx = [qng.mgc_prob_wait_erlangc(a,b,c) for (a,b,c)
                        in zip(train_df.lam_pp,1.0/(train_df.alos_pp),train_df.cap_pp)]

prob_blocked_by_pp_approx_df = pd.DataFrame({"scenario":train_df.scenario,
                                             "prob_blocked_by_pp_approx":prob_blocked_by_pp_approx},index=None)

train_df['prob_blocked_by_pp_approx'] = pd.Series(prob_blocked_by_pp_approx)

cond_mean_blockedtime_by_pp_approx = [24.0 * qng.mgc_mean_qwait_kimura(a,b,c,d) for (a,b,c,d)
                        in zip(train_df.lam_pp,1.0/(train_df.alos_pp),train_df.cap_pp, train_df.cv2_pp)]/train_df['prob_blocked_by_pp_approx']


# In[49]:

train_df['cond_mean_blockedtime_by_pp_approx'] = pd.Series(cond_mean_blockedtime_by_pp_approx)
#print(train_df['cond_mean_blockedtime_by_pp_approx'][:20])

p = 0.95
success = 0
failure = 0
for (a,b,c,d) in zip(train_df.lam_pp,1.0/(train_df.alos_pp),train_df.cap_pp, train_df.cv2_pp):
    print(a,b,c,d)
    rho = a / (b * c)
    erlangc = qng.mgc_prob_wait_erlangc(a,b,c)
    print('rho={} and erlangc={}'.format(rho, erlangc))
    if p > 1 - (erlangc - 0.02):
        print(qng.mgc_qcondwait_pctile_firstorder_2moment(0.95,a,b,c,d))
        success += 1
    else:
        print('{} not > 1 - (prob(delay) - 0.02)'.format(p))
        failure +=1

print('{} successes, {} failures'.format(success, failure))
