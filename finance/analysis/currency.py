#from pandas_datareader import data as web
from yahoo_finance import Currency
#import pandas.io.data as web
from tabulate import tabulate
import pandas as pd
from datetime import datetime as dt

start_dt = '2011-01-01'
end_dt = dt.today().strftime("%Y-%m-%d")

jpy = Currency('EURUSD')
print jpy.get_rate()