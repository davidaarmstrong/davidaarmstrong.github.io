###################################
## Code for POLSCI 9590 Week 7   ##
## Dave Armstrong                ##
## University of Western Ontario ##
## dave.armstrong@uwo.ca         ##
## 2024                          ##
###################################

exec(open('../python_functions.py').read())
import statsmodels.api as sm
import pandas as pd
ces = pd.read_stata("ces19.dta")
ces.relig = ces.relig.cat.rename_categories(['Atheist', 'Protestant', 'Catholic', 'Other'])
ces.relig = ces.relig.cat.reorder_categories(['Protestant', 'Atheist', 'Other', 'Catholic'], 
      ordered=True)
tab = sm.stats.Table.from_data(ces[['vote', 'relig']])
tab_stats = cat_stats(ces[['vote', 'relig']], 
                       ordinal = False, 
                       perm = True, 
                       niter = 500)

print_table(tab)
tab_stats

tab = sm.stats.Table.from_data(ces[['educ', 'agegrp']])
tab_stats = cat_stats(ces[['educ', 'agegrp']], 
                       ordinal = True, 
                       perm = True, 
                       niter = 500)

print_table(tab)
tab_stats

from plotnine import ggplot, geom_bar, aes, theme_classic, theme, labs, scale_y_continuous
import mizani.labels as ml
tab = sm.stats.Table.from_data(ces[['vote', 'relig']])
plot_dat = (tab
  .table_orig
  .apply(lambda x: x/sum(x))
  .reset_index()
  .melt(id_vars="vote"))
plot_dat.relig = (plot_dat.relig.astype('category').cat
  .reorder_categories(['Protestant', 'Atheist', 'Other', 'Catholic']))
(ggplot(plot_dat, aes(x="vote", y="value", fill="relig")) + 
geom_bar(stat="identity", position = "dodge") + 
theme_classic() + 
theme(legend_position = "top") + 
  labs(x="Vote", fill="Religion",
       y="Percentage (by Religion)") + 
  scale_y_continuous(labels = ml.label_percent())
).show()



## mosaic_plot is in python_functions.py
colors = {'Liberal': '#d71920', 
          'Conservative': '#003F72', 
          'NDP': '#F58220', 
          'Other': '#7f7f7f'}
mosaic_plot(ces[['relig', 'vote']], colors=colors)


from plotnine import ggplot, aes, geom_tile, scale_fill_brewer, labs
plot_dat = tab.standardized_resids.reset_index().melt(id_vars="vote")
plot_dat.relig = (plot_dat.relig.astype('category').cat
  .reorder_categories(['Catholic', 'Protestant', 'Other', 'Atheist']))
plot_dat.vote = (plot_dat.vote.astype('category').cat
  .reorder_categories(['NDP', 'Liberal', 'Conservative', 'Other']))
plot_dat['sigres'] = (plot_dat
  .value
  .case_when([(plot_dat.value.lt(-3), 'e < -3'), 
              (plot_dat.value.ge(-3) & plot_dat.value.lt(-2), '-3 <= e < -2'), 
              (plot_dat.value.ge(-2) & plot_dat.value.lt(-1), '-2 <= e < -1'), 
              (plot_dat.value.ge(-1) & plot_dat.value.lt(0), '-1 <= e < 0'), 
              (plot_dat.value.ge(0) & plot_dat.value.lt(1), '0 <= e < 1'), 
              (plot_dat.value.ge(1) & plot_dat.value.lt(2), '1 <= e < 2'), 
              (plot_dat.value.ge(2) & plot_dat.value.le(3), '2 <= e < 3'), 
              (plot_dat.value.gt(3), '3 < e')])
  .astype('category')
  .cat
  .reorder_categories(['e < -3', '-3 <= e < -2', '-2 <= e < -1', 
                       '-1 <= e < 0', '0 <= e < 1', '1 <= e < 2', 
                       '2 <= e < 3', '3 < e']))
(ggplot(plot_dat, aes(x="relig", y="vote", fill="sigres")) + 
geom_tile(color="white") + 
scale_fill_brewer("diverging", "RdBu") + 
labs(x="Religious Affiliation", y="Vote", fill="Std. Resid.")).show()


