# reticulate::use_python("/Users/david/.pyenv/shims/python")
# python -m pip install -i https://pypi.anaconda.org/scientific-python-nightly-wheels/simple statsmodels --upgrade --use-deprecated=legacy-resolver


import pandas as pd

def freqDist(d, v, g=None):
  if not pd.isna(g):
    tab = (d[[v,g]]
      .groupby([v,g], observed=True)
      .value_counts()
      .reset_index())
    tab['pct'] = (tab[[g, 'count']]
      .groupby(g, observed=True)
      .transform(lambda x: x/x.sum())
      )  
    # print the result
    return(tab)
  else:  
    tab = (d[v]
      # count number of values for each group of v
      .value_counts()
      # make the group variable a variable in the table
      .reset_index(name='Frequency')
      )
    # Make a percentage variable
    tab["pct"] = tab[['Frequency']].transform(lambda x: x/x.sum())
    # print the result
    return(tab)

def sumStats(d, v, g=None):
  if not pd.isna(g):
    out = (d[[g,v]] 
            ## group by the grouping variable 
            .groupby(g,observed=True) 
            ## make summary statistics for `v` by group of `g`
            .describe() 
            ## drop the top level of the table
            .droplevel(axis=1, level=0) 
            ## make the row names a variable
            .reset_index()
          )
    n_miss = (d[[g,v]]
                ## make grouping variable the index
                .set_index(g)
                ## identify missing values as True, non-missing as False
                .isna()
                ## group by the index
                .groupby(level=0, observed=True)
                ## sum where True = 1, False = 0
                .sum()
                ## make the index a variable in the table 
                .reset_index()
                ## Rename the variable being counted to nNA
                .rename(columns = {v: "nNA"})
              )
    out = (out
            ## merge in the non-missing data
            .merge(n_miss)
            ## rename count (which comes from describe()) to nValid
            .rename(columns = {"count": "nValid"})
          )
    ## put the nValid column as the last one
    out.insert(len(out.columns)-1, 'nValid', out.pop('nValid'))
    ## change nVZalid to an integer
    out['nValid'] = pd.to_numeric(out['nValid'], downcast='integer')
  else:
    out = (d[[v]] 
            .describe()
          )
    out = out.T.rename(columns = {0: 'nValid', 1: 'mean', 2: 'std', 
                                  3: 'min', 4: '25%', 5: '50%', 
                                  6: '75%', 7: 'max'})
    out = out.rename(columns = {'count': 'nValid'})                                  
    n_miss = (d[[v]]
                .isna()
                .sum())
    out['nNA'] = n_miss
    out = out.reindex(columns = ['mean', 'std', 'min', '25%', '50%', '75%', 'max', 'nNA', 'nValid'])

  ## change rouding for each float column to 2 decimal places
  for col in out.columns:
    if isinstance(out[col].iloc[0], float):
        out[col] = out[col].apply(lambda x: round(x, 2))
  print(out)

import scipy.stats as st
import numpy as np
import pandas as pd
def confidenceInterval(data, dist="normal", confidence=.95):
  mu = np.mean(data)
  se = st.sem(data, nan_policy = 'omit')
  df = len(data)-1
  if dist == "t": 
    interval = st.t.interval(confidence=confidence, df=df, loc=mu, scale=se)
    out = pd.DataFrame(data = {'Mean': mu, 'SE': se, 'Lower': interval[0], 'Upper': interval[1]}, index=[0])
    return(out)
  elif dist == "normal": 
    interval = st.norm.interval(confidence=confidence, loc=mu, scale=se)
    out = pd.DataFrame(data = {'Mean': mu, 'SE': se, 'Lower': interval[0], 'Upper': interval[1]}, index=[0])
    return(out)
  else: 
    print("dist must be one of 'normal' or 't'")


def tTest(x, y=None, group=None, h0 = 0, nan_policy='omit', alternative='two-sided', alpha=.05, equal_var=False): 
  from scipy import stats
  import numpy as np
  from pandas import DataFrame
  x = np.array(x)
  if np.all(y == None) & np.all(group == None): 
    x = x[np.where(~np.isnan(x))]
    tt = stats.ttest_1samp(x, popmean = h0, nan_policy = nan_policy, alternative=alternative)
    sig = ""
    if tt.pvalue < alpha: 
      sig = "*"
    out = {"Mean": np.mean(x), "SE": stats.sem(x), "H0": h0, "t": tt.statistic, "df": tt.df, "p-val": tt.pvalue.round(3).astype('str') + sig}
    out = DataFrame(out, index=[0])
  if np.all(y == None) & np.any(group != None):
    X = pd.DataFrame(data={"x": x, "group": group}).dropna()
    if len(x) != len(group): 
      print("x and group must be the same length")
    else:
      ung = np.unique(group)  
      if len(ung) != 2: 
        print("group must contain only two values")
      else: 
        y = x[group == ung[1]]
        y = y[y != None]
        y = y[np.where(~np.isnan(y))]
        x = x[group == ung[0]]
        x = x[x != None]
        x = x[np.where(~np.isnan(x))]
        tt = stats.ttest_ind(x, y, nan_policy = nan_policy, alternative=alternative, equal_var = equal_var)      
        sig = ""
        if tt.pvalue < alpha: 
          sig = "*"
        n1 = len(x)
        n2 = len(y)
        v1 = np.var(x)
        v2 = np.var(y)
        if equal_var: 
          pse = np.sqrt((v1/n1) + (v2/n2))
        else: 
          sp = np.sqrt((((n1-1)*v1) + ((n2-1)*v2))/(n1 + n2 - 1))
          pse = sp*np.sqrt(1/n1 + 1/n2)
        out = {"Mean": [np.mean(x), np.mean(y), np.mean(y) - np.mean(x)], 
               "SE": [stats.sem(x), stats.sem(y), pse],  
               "N": [n1, n2, n1+n2],
               "t": ["", "", tt.statistic], 
               "df": ["", "", tt.df], 
               "p-val": ["", "", tt.pvalue.round(3).astype('str') + sig]}
        out = DataFrame(out, index = ['x', 'y', 'difference (y-x)'])
  if np.any(y != None) & np.all(group == None):
    y = np.array(y)
    y = y[y != None]
    y = y[np.where(~np.isnan(y))]
    x = x[np.where(~np.isnan(y))]
    tt = stats.ttest_ind(x, y, nan_policy = nan_policy, alternative=alternative, equal_var = equal_var)      
    sig = ""
    if tt.pvalue < alpha: 
      sig = "*"
    n1 = len(x)
    n2 = len(y)
    v1 = np.var(x)
    v2 = np.var(y)
    if equal_var: 
      pse = np.sqrt((v1/n1) + (v2/n2))
    else: 
      sp = np.sqrt((((n1-1)*v1) + ((n2-1)*v2))/(n1 + n2 - 1))
      pse = sp*np.sqrt(1/n1 + 1/n2)
    out = {"Mean": [np.mean(x), np.mean(y), np.mean(y) - np.mean(x)], 
           "SE": [stats.sem(x, nan_policy = nan_policy), stats.sem(y, nan_policy = nan_policy), pse],  
           "N": [n1, n2, n1+n2],
           "t": ["", "", tt.statistic], 
           "df": ["", "", tt.df], 
           "p-val": ["", "", tt.pvalue.round(3).astype('str') + sig]}
    out = DataFrame(out, index = ['x', 'y', 'difference (y-x)'])
  return(out)

def propTest1(k, n, p, alternative='two-sided'): 
  from scipy import stats
  import numpy as np
  from pandas import DataFrame
  pt = stats.binomtest(k, n, p, alternative=alternative)
  out = {'k': k, 'N': n, 'H0': p, 'est_p': pt.statistic, 'p-value': pt.pvalue}
  out = DataFrame(out, index=[0])
  return(out)
  
def propTest2(x, group): 
  from scipy import stats
  import numpy as np
  from pandas import DataFrame
  d = DataFrame(data= {"x": x, "group": group}).dropna()
  ung = np.unique(d.group)  
  unx = np.unique(d.x)
  if (len(ung) != 2) | (len(unx) != 2): 
    print("group and x must have only two values each")
  else: 
    tab = pd.crosstab(d.x, d.group)
    x2 = stats.chi2_contingency(tab)
    probs = tab.iloc[1,]/tab.sum(axis=0)
    sig = ""
    if x2.pvalue < .05: 
      sig = "*"
    out = DataFrame(data= {"group": [ung[0], ung[1], "Difference"], 
                           "prob": [probs[0], probs[1], probs[1]-probs[0]],
                           "Chi2": ["", "", x2.statistic.round(3)], 
                           "DF": ["", "", x2.dof], 
                           "p-value": ["", "", x2.pvalue.round(3).astype('str') + sig]})
    return(out)
  
def make_stats(data, ordinal=False):
  import pandas as pd
  import statsmodels.api as sm
  import numpy as np
  from scipy.stats import chi2_contingency
  pd.options.display.float_format = "{:,.3f}".format
  data = data.dropna()
  tab = sm.stats.Table.from_data(data)
  chi2 = chi2_contingency(tab.table_orig)
  n = tab.table_orig.sum().sum()
  crv = np.sqrt(chi2.statistic/(n*(np.min(tab.table_orig.shape)-1)))
  E1 = n-tab.table_orig.sum(axis=1).max()
  E2 = n - tab.table_orig.max().sum()
  lam = (E1-E2)/E1
  out = pd.DataFrame(data= {"Statistic": ["Chi-squared", "Cramer's V", "Lambda"], 
                                  "Value": [chi2.statistic, crv, lam], 
                                  "p-value": [chi2.pvalue, "", ""]})
  if ordinal:                                
    table = tab.table_orig
    unx = table.axes[1].categories
    uny = table.axes[0].categories
    t_dat = table.reset_index()
    t_dat = t_dat.melt(id_vars = t_dat.axes[1][0])
    t_dat['x_code'] = t_dat.iloc[:,1].astype('category').cat.set_categories(unx).cat.codes
    t_dat['y_code'] = t_dat.iloc[:,0].astype('category').cat.set_categories(uny).cat.codes
    discordant = 0
    concordant = 0
    ties_x = 0
    ties_y = 0
    for i in range(t_dat.shape[0]): 
      concordant = concordant + t_dat.value[i]*t_dat[(t_dat.x_code > t_dat.x_code[i]) & (t_dat.y_code > t_dat.y_code[i])].value.sum()
      discordant = discordant + t_dat.value[i]*t_dat[(t_dat.x_code < t_dat.x_code[i]) & (t_dat.y_code > t_dat.y_code[i])].value.sum()
      ties_x = ties_x + t_dat.value[i]*t_dat[(t_dat.x_code == t_dat.x_code[i]) & (t_dat.y_code > t_dat.y_code[i])].value.sum()
      ties_y = ties_y + t_dat.value[i]*t_dat[(t_dat.x_code > t_dat.x_code[i]) & (t_dat.y_code == t_dat.y_code[i])].value.sum()
    gamma = (concordant - discordant)/(concordant + discordant)
    somersd = (concordant - discordant)/(concordant + discordant + ties_x)
    taub = (concordant - discordant)/np.sqrt((concordant + discordant + ties_y)*(concordant + discordant + ties_x))
    ord_out = pd.DataFrame(data= {'Statistic': ['Gamma', "Somer's D", "Kendall's Tau-b"], 
                        'Value': [gamma, somersd, taub], 
                        'p-value': ["", "", ""]})
    out = out._append(ord_out)                      
  return(out)


def make_stats_perm(data, niter=100, ordinal=False): 
  import pandas as pd
  pd.options.display.float_format = "{:,.3f}".format
  data = data.dropna()
  ostat = make_stats(data, ordinal=ordinal)
  out = ostat.Value
  y = data.iloc[:,[0]].reset_index()
  for i in range((niter-1)): 
    x = data.iloc[:,[1]].sample(frac=1).reset_index()
    tmp = pd.DataFrame(data= {"y": y.iloc[:,1], "x": x.iloc[:,1]})
    tmp_stats = make_stats(tmp, ordinal = ordinal).Value
    out = pd.concat([out, tmp_stats], axis=1)
  orig = out.iloc[:,0]
  perms = out.iloc[:,1:niter]
  ngt = perms.apply(lambda x: orig > x).sum(axis=1)
  denom = perms.shape[1]
  pval = 1-ngt/denom
  ostat['p-value'] = pval
  return(ostat)
  
  
def cat_stats(data, ordinal=False, perm=False, niter=100): 
  if perm: 
    res = make_stats_perm(data, ordinal=ordinal, niter = niter)
  else: 
    res = make_stats(data, ordinal=ordinal)
  return(res)


def print_table(result): 
  from numpy import str_
  colpct = result.table_orig
  tmp = pd.DataFrame(np.asarray(colpct.reset_index()))
  tmp.columns = np.asarray(colpct.reset_index().columns.to_series())
  tmp = tmp.set_index(tmp.columns[0])
  tmp_cp = tmp.apply(lambda x: (x*100/sum(x)))
  tmp_cp = tmp_cp.apply(
      lambda series: series.apply(lambda value: f"({value:,.2f}%)")
  )
  tmp['obs'] = range(0, (tmp.shape[1]*2), 2)
  tmp_cp['obs'] = tmp.obs.add(1)
  tmp = tmp._append(tmp_cp)
  tmp = tmp.sort_values('obs')
  nm = tmp.index.name
  tmp = tmp.reset_index(names=nm)
  tmp[nm] = tmp[nm].case_when([(tmp.obs % 2 == 1, "")])
  tmp = tmp.set_index(nm)
  tmp = tmp.iloc[:,0:(tmp.shape[1]-1)]
  return(tmp)

match = lambda a, b: [ x if x in b else None for x in a ]

def make_properties(d, kys): 
    import numpy as np
    k = list(d.keys())
    m = match(np.asarray(kys)[:,1], k)  
    props = {}
    for i in range(len(kys)):
      props[kys[i]] = {'color': d.get(m[i])}
    return(props)

def mosaic_plot(data, colors=None): 
    import numpy as np
    import pandas as pd
    import matplotlib.pyplot as plt
    from itertools import product
    from statsmodels.graphics.mosaicplot import mosaic
    data = data.dropna()
    nms = data.axes[1]
    data = data.apply(lambda x: x.astype('category') if x.dtypes != "category" else x)
    tups = list(product(data[nms[0]].cat.categories,
                          data[nms[1]].cat.categories))
    index = pd.MultiIndex.from_tuples(tups, names=nms)
    vc = data.value_counts()
    vc = pd.Series(vc, index=index)
    labd = {}
    for item in tups:
        key = item
        value = ''
        labd[key] = value
    labelizer = lambda k: labd[k]
    if isinstance(colors, dict):
        props = make_properties(colors, tups)
        mosaic(vc, properties=props, labelizer = labelizer)
    else:
        mosaic(vc, labelizer = labelizer)
    plt.show()

def pwcorr(data, niter=100):
  data = data.dropna()
  orig_r = data.corr()
  orig = data.corr().reset_index().melt(id_vars = "index")
  perm_dat = data.apply(lambda x: x.sample(frac=1, replace=False, ignore_index=True))
  r = perm_dat.corr().reset_index().melt(id_vars="index")
  r['orig'] = orig.value
  r['bigger'] = r.orig.case_when([(r.orig.lt(0) & r.orig.gt(r.value), 1), 
                                        (r.orig.lt(0) & r.orig.lt(r.value), 0), 
                                        (r.orig.gt(0) & r.orig.gt(r.value), 1), 
                                        (r.orig.gt(0) & r.orig.lt(r.value), 0)]) 
  r['iter'] = 0
  for i in range((niter-1)): 
    perm_dat = data.apply(lambda x: x.sample(frac=1, replace=False, ignore_index=True))  
    tmpr = perm_dat.corr().reset_index().melt(id_vars="index")
    tmpr['orig'] = orig.value
    tmpr['bigger'] = tmpr.orig.case_when([(tmpr.orig.lt(0) & tmpr.orig.gt(tmpr.value), 1), 
                                          (tmpr.orig.lt(0) & tmpr.orig.lt(tmpr.value), 0), 
                                          (tmpr.orig.gt(0) & tmpr.orig.gt(tmpr.value), 0), 
                                          (tmpr.orig.gt(0) & tmpr.orig.lt(tmpr.value), 1)]) 
    tmpr['iter'] = i+1
    r = r._append(tmpr)
  z = r.reset_index().groupby(['index', 'variable']).bigger.mean().reset_index().pivot_table(values="bigger", columns="variable", index="index")
  z = z.apply(lambda x: x.case_when([(x.lt(.05), "*"), (x.ge(.05), " ")]))
  orig_r = orig_r.apply(lambda series: series.apply(lambda value: f"{value:,.3f}"))
  out = orig_r + z
  return(out)

def lower_tri(mat): 
  import numpy as np
  mask = np.zeros_like(mat, dtype=bool)
  mask[np.triu_indices_from(mask)] = True
  mat[mask] = np.nan
  nr = mat.shape[0]
  nc = mat.shape[1]-1
  matlt = mat.iloc[range(1, nr, 1), range(0,nc, 1)]
  return(matlt)

