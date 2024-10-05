###################################
## Code for POLSCI 9590 Week 8   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

exec(open('../python_functions.py').read())
import statsmodels.api as sm
import pandas as pd
ces = pd.read_stata("ces19.dta")
therms = ces[['leader_ndp', 'leader_lib', 'leader_con']]
therms.corr()


pwcorr(therms)


import seaborn as sb
import numpy as np
import matplotlib.pyplot as plt


g = sb.PairGrid(therms)
g.map_diag(sb.histplot, color="gray")
g.map_offdiag(sb.regplot, 
              lowess=True, 
              line_kws = dict(color="r"), 
              scatter_kws = {"alpha": .05, "color": "gray"}, 
              x_jitter=.5, 
              y_jitter=.5)
g.map_offdiag(sb.regplot, 
              line_kws = dict(color="black"), 
              scatter_kws = {"alpha": 0})
plt.show()
plt.clf()


corr = therms.corr()
rlt = lower_tri(corr)
plt.clf()
sb.heatmap(rlt, 
  cmap = sb.color_palette("vlag", as_cmap=True), 
  vmin = -1, 
  vmax = 1, 
  center = 0, 
  annot=True,
  xticklabels = ['NDP', 'Liberal'], 
  yticklabels = ['Liberal', 'Conservative'])
plt.show()


