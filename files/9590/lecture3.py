###################################
## Code for POLSCI 9590 Week 2   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

import pandas as pd
import mizani.labels as ml
import numpy as np
from plotnine import aes, after_stat, geom_line, geom_bar, geom_histogram, ggplot, labs, position_dodge, scale_color_brewer, scale_y_continuous, theme_classic


import pandas as pd
ces19 = pd.read_stata("ces19.dta")


def freqDist(d, v):
  tab = (d[v]
          .value_counts()
          .reset_index(name='Frequency')
        )
  tab["pct"] = round((tab["Frequency"]/sum(tab["Frequency"]))*100, 2)
  print(tab)


ces19['educ'].value_counts().reset_index(name='Frequency')
freqDist(ces19, 'educ')


ces19.leader_lib.describe()


exec(open('../python_functions.py').read())
sumStats(ces19, "leader_lib")


(ces19[['leader_lib', 'educ']]
  .groupby('educ',observed=True) 
  .describe() 
  .droplevel(axis=1, level=0) 
  .reset_index())


sumStats(ces19, "leader_lib", g="educ")


ces19.leader_lib.quantile([.62])


ces19.leader_lib.quantile([.62, .38])


crime = pd.read_stata("crime.dta")
from plotnine import ggplot, geom_line, aes

## Line plot 1
(ggplot(crime, 
       aes(x="year", 
           y="incidents", 
           colour="prov")) +
  geom_line()
)


from plotnine import theme_classic

(ggplot(crime, 
       aes(x="year", 
           y="incidents", 
           colour="prov")) +
  geom_line() + 
  theme_classic() #<<
)


from plotnine import scale_color_brewer

(ggplot(crime, 
       aes(x="year", 
           y="incidents", 
           colour="prov")) +
  geom_line() + 
  theme_classic() + 
  scale_color_brewer(type="qualitative", palette="Paired") #<<
)


from plotnine import labs

(ggplot(crime, 
       aes(x="year", 
           y="incidents", 
           colour="prov")) +
  geom_line() + 
  theme_classic() + 
  scale_color_brewer(type="qualitative", palette="Paired")+ 
  labs(x="Year", y="Incidents of Crime", #<<
       colour="Province")#<<
)


from plotnine import scale_y_continuous
import mizani.labels as ml

(ggplot(crime, 
       aes(x="year", 
           y="incidents", 
           colour="prov")) +
  geom_line() + 
  theme_classic() + 
  scale_color_brewer(type="qual", palette=3) + 
  labs(x="Year", y="Incidents of Crime",
     color="Province") + 
  scale_y_continuous(labels = ml.label_comma())#<<
)


(ggplot(crime, 
       aes(x="year", 
           y="rate", #<<
           colour="prov")) +
  geom_line() + 
  theme_classic() + 
  scale_color_brewer(type="qual", palette=3) + 
  labs(x="Year", y="Incidents of Crime",
     color="Province") + 
  scale_y_continuous(labels = ml.label_comma())
)


freqDist(ces19, 'educ')


from plotnine import geom_bar
(ggplot(ces19, aes(x="educ")) + 
  geom_bar() 
)


(ggplot(ces19[ces19['educ'].dropna()], #<<
    aes(x="educ")) + 
    geom_bar() 
)


import numpy as np
from plotnine import after_stat, scale_y_continuous

(ggplot(ces19[ces19['educ'].dropna()], 
  aes(x="educ")) + 
  geom_bar(aes(y=after_stat('count / np.sum(count)'))) + #<<
  scale_y_continuous(labels = ml.label_percent()) #<<
)


import numpy as np
from plotnine import after_stat, scale_y_continuous

(ggplot(ces19[ces19['educ']dropna()], 
  aes(x="educ")) + 
  geom_bar(aes(y=after_stat('count / np.sum(count)'))) + 
  scale_y_continuous(labels = ml.label_percent()) + 
  labs(x="Highest Level of Education", #<<
     y="Percentage") + #<<
  theme_classic() #<<
)


from plotnine import geom_histogram

(ggplot(ces19, aes(x="leader_lib")) + 
  geom_histogram() + 
  theme_classic() + 
  labs(
    x="Liberal Leader Feeling Thermometer", 
    y="# Observations")
)


(ggplot(ces19, aes(x="leader_lib")) + 
  geom_histogram(bins=10) + #<<
  theme_classic() + 
  labs(
    x="Liberal Leader Feeling Thermometer", 
    y="# Observations")
)


gencase=[(ces19.gender == 1, 'M'),
          (ces19.gender == 5, 'F')]
ces19['genderf'] = ces19.gender.case_when(caselist=gencase).astype('category')
tmp = ces19[['educ', 'genderf']].dropna()
tmp = tmp.groupby(['educ', 'genderf'], observed=True).value_counts().reset_index()
tmp['pct'] = tmp[['genderf', 'count']].groupby('genderf').transform(lambda x: x/x.sum())
tmp['genderf'] = tmp.genderf.cat.reorder_categories(['M', 'F'], ordered=True)


from plotnine import position_dodge, after_stat, scale_y_continuous
(ggplot(tmp,
  aes(x="educ",
      y = "pct", 
      fill="genderf")) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_y_continuous(labels = ml.label_percent()) + 
  theme_classic() + 
  labs(x="Highest Level of Education", 
       y="Percentage"))


