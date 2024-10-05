# pip install pandas

## In R 
## If you want to use a version other than the configured default in RStudio
## reticulate::use_python("/Users/david/.pyenv/shims/python")

import pandas as pd
ces19 = pd.read_stata("ces19.dta")

ces19.agegrp.dtypes
ces19.leader_lib.dtypes
ces19.leader_lib.values


