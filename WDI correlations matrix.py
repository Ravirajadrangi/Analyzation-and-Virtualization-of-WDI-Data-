import sys
import numpy as np
import pandas as pd
import scipy.stats as sp
import matplotlib.pyplot as plt
from itertools import combinations_with_replacement
data = pd.read_csv('/data/home/sw8/WDI_csv/WDI_Data.csv')
num_indicators=30
start_year_col = data.columns.get_loc("1960")
a1=np.arange(1960,2016).astype('S4')
col = data.columns[start_year_col+data[a1].count().argmax()]
cleared=data[['Indicator Code',col]].dropna()
ind_counts=cleared['Indicator Code'].value_counts()
ind_counts_frame=pd.DataFrame({
   'ind_name':ind_counts.index,
    'ind_count':ind_counts})
ind_counts_frame.sort(['ind_count','ind_name'],ascending=[False,True])
indicators=ind_counts_frame['ind_name'][:num_indicators]
corrs=np.ones((num_indicators,num_indicators))
for(x,y) in combinations_with_replacement(np.arange(num_indicators),2):
    d1 = data[data['Indicator Code']==indicators[x]][['Country Code',col]]
    d2 = data[data['Indicator Code']==indicators[y]][['Country Code',col]]
    cd=pd.merge(d1,d2,on = 'Country Code')
    (r,p)=sp.spearmanr(cd[col+'.x'],cd[col+'.y'])
    corrs[x][y]=corrs[y][x]=r
sys.stdout.write("\r{}/{}".format(x*num_indicators+y+1,
                                      num_indicators**2))
sys.stdout.flush()
print
corrs=np.flipud(corrs)
tick_indicators=np.arange(0.5,num_indicators+0.5)
plt.figure()
plt.pcolormesh(corrs,cmap='RdBu',vmin=-1,vmax=1)
colorbar=plt.colorbar()
colorbar.set_label("spearman's Rank Correlation",fontsize=8)
plt.title("World Bank Indicator Correlation",fontsize=8)
plt.xticks(tick_indicators,indicators, rotation='vertical',fontsize=8)
plt.yticks(tick_indicators,indicators[::-1],fontsize=8)
plt.tight_layout()
plt.savefig("wdi_data_corr.pdf")
plt.show()
