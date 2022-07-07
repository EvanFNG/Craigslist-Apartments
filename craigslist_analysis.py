import pandas as pd
import numpy as np
import matplotlib.pylab as pylab
import matplotlib.pyplot as plt
import seaborn as sns

# Required to get the full URL.
pd.set_option('display.max_colwidth', None)

df = pd.read_csv('craigslist.csv')

df = df.drop_duplicates(subset = 'title')

params = {
    'legend.fontsize': 'x-large',
    'figure.figsize': (15, 5),
    'axes.labelsize': 'x-large',
    'axes.titlesize': 'x-large',
    'xtick.labelsize': 'x-large',
    'ytick.labelsize': 'x-large'
}

pylab.rcParams.update(params)

plt.figure(figsize = (12, 8))
plt.legend(fontsize = 12)
plt.xlabel('Price', fontsize = 18)
plt.ylabel('Square Footage', fontsize = 18)
plt.title('Price vs. Square Footage, Western MA', fontsize = 18)
sns.scatterplot(x = 'price', y = 'sqft', hue = 'beds', palette = 'summer', x_jitter = True, y_jitter = True, s = 125, data = df.dropna())

plt.show()