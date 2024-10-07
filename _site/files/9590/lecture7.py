###################################
## Code for POLSCI 9590 Week 6   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

import pandas as pd
exec(open('../python_functions.py').read())
dat = pd.read_stata("dat_1samp.dta")
tTest(dat.x)


tTest(dat.x, alternative="less")


tTest(dat.x, alternative="greater")


propTest1(110, 250, .5)


import pandas as pd
ces = pd.read_stata("ces19.dta")
ces_v = ces[['market', 'vote']].dropna()
ces_v['vote_con'] = (ces_v.vote == "Conservative").astype('int')
ces_v.vote_con = ces_v.vote_con.case_when([(ces_v.vote.isna(), np.nan)])
tTest(ces_v.market, group=ces_v.vote_con, equal_var=False)


ces_g = ces[['vote', 'educ']].dropna()
ces_g['coll_grad'] = (ces_g.educ == "College Grad").astype('int')
ces_g['vote_con'] = (ces_g.vote == "Conservative").astype('int')
propTest2(ces_g.vote_con, ces_g.coll_grad)


from plotnine import ggplot, aes, geom_boxplot, theme_classic, labs
ces_v['vc'] = (ces_v
  .vote_con
  .case_when([(ces_v.vote_con.eq(0), 'No'), 
              (ces_v.vote_con.eq(1), 'Yes')])
  .astype('category')
  .cat
  .reorder_categories(['No', 'Yes'], ordered=True)
)
(ggplot(ces_v.dropna(), aes(x="vc", y="market")) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x="Vote Conservative", y="Market Liberalism")
).show()


from plotnine import geom_bar
ces_gf = (ces_g[['coll_grad', 'vote_con']]
  .apply(lambda x: x
    .case_when([(x.eq(0), "No"), 
                (x.eq(1), "Yes")])
    .astype('category')
    .cat
    .reorder_categories(['No', 'Yes'], 
        ordered=True)))
cts = (ces_gf
  .value_counts()
  .reset_index()
)
cts['pct'] = (cts[['coll_grad', 'count']]
  .groupby('coll_grad', observed=True)
  .transform(lambda x: x/x.sum()))

(ggplot(cts, aes(x="coll_grad", y="pct", fill="vote_con")) + 
  geom_bar(stat="identity", position="stack") + 
  theme_classic() + 
  labs(x="College Graduate?", fill="Vote\nConservative?")
).show() 



