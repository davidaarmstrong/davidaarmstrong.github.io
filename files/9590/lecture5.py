###################################
## Code for POLSCI 9590 Week 4   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

exec(open('../python_functions.py').read())
import pandas as pd
import numpy as np
from plotnine import ggplot, aes, geom_point, geom_line, stat_summary, theme_bw, theme_classic, labs, ylim, position_dodge, theme, facet_wrap, scale_colour_manual, scale_x_continuous


ces = pd.read_stata("ces19.dta")
sumStats(ces, "leader_lib")
sumStats(ces, "leader_lib", g="educ")


from plotnine import ggplot, aes, stat_summary, theme_classic, labs, ylim
import numpy as np
(ggplot(ces[['educ', 'leader_lib']].dropna(), 
  aes(x='educ', y='leader_lib')) + 
  stat_summary(geom="point", fun_data="mean_cl_normal") + 
  theme_bw() + 
  labs(x="Highest Level of Educational Attainment", 
       y = "Liberal Leader Feeling Thermometer (0-100)") + 
  ylim(0,100) #<<
).show()



meds = (ces[['educ', 'leader_lib']]
.groupby('educ', observed=True)
.agg(lambda x: x.quantile(.5))
.reset_index()
.rename(columns={"leader_lib": "val"}))
mns = (ces[['educ', 'leader_lib']]
.groupby('educ', observed=True)
.agg(lambda x: np.mean(x))
.reset_index()
.rename(columns={"leader_lib": "val"}))
mns['stat'] = 'Mean'
meds['stat'] = 'Median'
out = meds._append(mns)
out['stat'] = out.stat.astype('category')

(ggplot(out, 
    aes(x="educ", y="val", color="stat", shape="stat")) +  
    geom_point() + 
  theme_bw() + 
  labs(x="Highest Level of Educational Attainment", 
       y = "Liberal Leader Feeling Thermometer (0-100)", 
       colour="Measure", shape="Measure") + 
  ylim(0,100)).show()



meds = (ces[['educ', 'leader_lib']]
.groupby('educ', observed=True)
.agg(lambda x: x.quantile(.5))
.reset_index()
.rename(columns={"leader_lib": "val"}))
mns = (ces[['educ', 'leader_lib']]
.groupby('educ', observed=True)
.agg(lambda x: np.mean(x))
.reset_index()
.rename(columns={"leader_lib": "val"}))
mns['stat'] = 'Mean'
meds['stat'] = 'Median'
out = meds._append(mns)
out['stat'] = out.stat.astype('category')

(ggplot(out, 
    aes(x="educ", y="val", color="stat", shape="stat")) +  
    geom_point(position = position_dodge(width=.25)) + #<<
  theme_bw() + 
  labs(x="Highest Level of Educational Attainment", 
       y = "Liberal Leader Feeling Thermometer (0-100)", 
       colour="Measure", shape="Measure") + 
  ylim(0,100)).show()



x = pd.DataFrame(data = {"country": ['A', 'B', 'C'], 
                     "1999": [1,2,3], 
                     "2000": [4,5,6]})
x
x.melt(id_vars="country")


xl = x.melt(id_vars="country")
xl 
xl.pivot(index='country', columns='variable', values='value').reset_index()


cesl = (ces[ces.filter(regex="educ|leader").columns]
  .melt(id_vars = "educ")
)
mns =  (cesl
  .groupby(['educ', 'variable'], observed=True)
  .agg(lambda x: np
  .mean(x))
  .reset_index()
  .rename(columns={"value": "mean"})
  )
meds = (cesl
  .groupby(['educ', 'variable'], observed=True)
  .agg(lambda x: x
  .quantile())
  .reset_index()
  .rename(columns={"value": "median"}))

out = mns.merge(meds).rename(columns = {'variable': 'party'})

outl = pd.DataFrame(out).melt(id_vars = ['educ', 'party'])
caselist = [(outl.party.eq("leader_lib"), "Liberal"), 
            (outl.party.eq("leader_ndp"), "NDP"), 
            (outl.party.eq("leader_con"), "Conservative")]
outl.party = outl.party.case_when(caselist)

(ggplot(outl, aes(x="educ", y="value", 
              colour="party")) + 
  geom_point(position=position_dodge(width=.25)) + 
  theme_bw() + 
  facet_wrap("variable", ncol=2) + 
  scale_colour_manual(values=[ "#003F72", "#d71920", "#F58220"]) + 
  theme(legend_position="top") + 
  labs(x="Highest Level of Educational Attainment", 
       y = "Liberal Leader Feeling Thermometer (0-100)") + 
  ylim(0,100))



outl['educn'] = outl.educ.cat.codes + 1

(ggplot(outl, aes(x="educn", y="value", 
              colour="party")) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  facet_wrap("variable", ncol=2) + 
  scale_colour_manual(values=[ "#003F72", "#d71920", "#F58220"]) + 
  scale_x_continuous(breaks = [1, 2, 3], labels=["<HS", "HS/College", "College\nGrad"]) + 
  theme(legend_position="top", 
        panel_spacing=.05) + 
  labs(x="Highest Level of Educational Attainment", 
       y = "Liberal Leader Feeling Thermometer (0-100)") + 
  ylim(0,100)).show()


