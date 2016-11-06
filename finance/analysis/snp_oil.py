from yahoo_finance import Share
from pprint import pprint
from tabulate import tabulate
import pandas as pd
from datetime import datetime as dt
import statsmodels.formula.api as sm
import matplotlib.pyplot as plt
import numpy as np

start_dt = '2011-01-01'
end_dt = dt.today().strftime("%Y-%m-%d")

snp=Share('^GSPC')
oil=Share('BZU16.NYM')

snp_list = snp.get_historical(start_dt,end_dt)
df=pd.DataFrame(snp_list)
#df["Date"] = dt.strptime(df["Date"], '%Y-%m-%d') #Tried to convert string date into date data type but this didn't work
df["Adj_Ret"] = df["Adj_Close"].astype(float)/df["Adj_Close"].astype(float).shift(-1)-1
df["Abs_Adj_Ret"] = abs(df["Adj_Ret"])
df["Volume"] = df["Volume"].astype(float)
df["Low"] = df["Low"].astype(float)
df["High"] = df["High"].astype(float)
df["Band"] = df["High"] - df["Low"]

result = sm.ols(formula="Band ~ Volume", data=df).fit()
print result.summary()

plt.plot(df["Band"])

plt.show()


#http://stackoverflow.com/questions/12433076/download-history-stock-prices-automatically-from-yahoo-finance-in-python
#from pandas.datareader
