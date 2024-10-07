###################################
## Code for POLSCI 9590 Week 9   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

import pandas as pd
import statsmodels.formula.api as smf
dat = pd.read_stata("example1.dta")
mod_setup =  smf.ols(' y ~ x', data=dat)
mod = mod_setup.fit()

mod.summary(slim=True)

import seaborn as sb
import matplotlib.pyplot as plt
plt.clf()
sb.set_style('ticks')
fig, ax = plt.subplots()
fig.set_size_inches(6, 4)
sb.regplot(x=dat.x, y=dat.y, scatter_kws={"alpha": .1}, ax=ax)
sb.despine()
plt.show()
plt.clf()

demo = pd.read_stata('demo.dta')
mod =  smf.ols('demodays ~ corrupt', data=demo).fit()


mod.summary(slim=True)

plt.clf()
sb.set_style('ticks')
fig, ax = plt.subplots()
fig.set_size_inches(6, 4)
sb.regplot(x=demo.corrupt, y=demo.demodays, scatter_kws={"alpha": .1}, ax=ax)
sb.despine()
plt.show()
plt.clf()

df = pd.read_stata("cat_example.dta")
(df
  .groupby("x", observed=True)
  .agg({'y':['mean', 'std', lambda x: len(x)]})
  .reset_index()
  .droplevel(0, axis=1)
  .rename(columns={'': 'Group', 'mean': 'Mean', 'std': 'SD', '<lambda_0>': 'N'}))


xmod =  smf.ols('y ~ x', data=df).fit()
print(xmod.summary().tables[1].as_text())


import marginaleffects as me
me.avg_predictions(xmod, by="x").to_pandas()
me.avg_predictions(xmod, by="x", hypothesis="pairwise").to_pandas()


from plotnine import geom_point, position_jitter, aes
p = me.plot_predictions(xmod, by="x")  
(p + geom_point(data=df, 
                mapping = aes(x="x", y="y"), 
                position = position_jitter(width=.15), 
                inherit_aes=False)).show()


