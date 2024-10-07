###################################
## Code for POLSCI 9590 Week 3   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

import pandas as pd
from plotnine import ggplot, aes, geom_bar, position_dodge, scale_y_continuous, theme_classic, labs
import mizani.labels as ml
exec(open('../python_functions.py').read())


data['y'] = value3
caselist = [(data.x.condition1, value1),
            (data.x.condition2, value2)]
data['y'] = data.y.case_when(caselist)


dat = pd.DataFrame(data={'x': [1,2,3,4]})
caselist = [(dat.x.le(3), "yes")]
dat['y'] = 'no'
dat['y'] = dat.y.case_when(caselist)
dat


dat = pd.DataFrame(data={'x': [1,2,3,4]})
caselist = [(dat.x.isin([1,2,3]), "yes")]
dat['y'] = 'no'
dat['y'] = dat.y.case_when(caselist)
dat


dat = pd.DataFrame(data={'x': [1,2,3,4,None]})
caselist = [(dat.x.le(3), "yes")]
dat['y'] = 'no'
dat['y'] = dat.y.case_when(caselist)
dat


dat = pd.DataFrame(data={'x': [1,2,3,4,None]})
caselist = [(dat.x.le(3), "yes"), (dat.x.isna(), None)]
dat['y'] = 'no'
dat['y'] = dat.y.case_when(caselist)
dat


dat = pd.DataFrame(data={'x': [1,2,3,4]})
caselist = [(dat.x.le(2), "l"), (dat.x.ge(2), "g")]
caselistr = [(dat.x.ge(2), "g"), (dat.x.le(2), "l")]
dat['y'] = dat.x.case_when(caselist)
dat['z'] = dat.x.case_when(caselistr)
dat


ces = pd.read_stata("ces19.dta")
caselist = [(ces.market < ces.market.quantile(.4), 0), 
            (ces.market.isna(), None)]
ces['market_01'] = 1
ces['market_01'] = ces.market_01.case_when(caselist)
ces.market_01.value_counts(dropna=False)


caselist = [(ces.leader_ndp.gt(ces.leader_lib) & ces.leader_ndp.gt(ces.leader_con), 1), 
            (ces.leader_ndp.isna() | ces.leader_lib.isna() | ces.leader_con.isna(), None)]
ces['heart_ndp'] = 0
ces['heart_ndp'] = ces.heart_ndp.case_when(caselist)
ces.heart_ndp.value_counts(dropna=False)


dat = freqDist(ces, 'vote', g='heart_ndp')
caselist = [(dat.heart_ndp.eq(1), 'yes'), 
            (dat.heart_ndp.eq(0), 'no')]
dat['heart_ndp'] = (dat.heart_ndp
    .case_when(caselist)
    .astype("category"))
(ggplot(dat,
  aes(x="vote",
      y = "pct", 
      fill="heart_ndp")) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_y_continuous(labels = ml.label_percent()) + 
  theme_classic() + 
  labs(x="Highest Level of Education", 
       y="Percentage", 
       fill="Like NDP\nLeader Most")).show()


