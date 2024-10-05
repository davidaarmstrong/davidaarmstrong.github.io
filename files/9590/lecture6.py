###################################
## Code for POLSCI 9590 Week 5   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

import pandas as pd
import numpy as np
import scipy.stats as st
exec(open('../python_functions.py').read())
confidenceInterval(r.samp)
confidenceInterval(r.samp, dist="t")


ces = pd.read_stata("ces19.dta")
confidenceInterval(ces.market)
res = (ces[['vote', 'market']]
  .groupby('vote')
  .apply(lambda x: confidenceInterval(x, dist="t"))
  .reset_index()
  .drop("level_1", axis=1))
res


from plotnine import ggplot, aes, geom_pointrange, theme_classic, labs
res.vote = res.vote.cat.reorder_categories(['Conservative', 'Other', 'Liberal', 'NDP'], ordered=True)
(ggplot(res, aes(x="vote", y="Mean", ymin = "Lower", ymax = "Upper")) + 
  geom_pointrange() + 
  theme_classic() + 
  labs(x="Vote", y="Average Market Liberalism")
  
).show()


