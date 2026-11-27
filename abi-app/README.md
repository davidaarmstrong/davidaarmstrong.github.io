# Anti-Black Index map

A static web app for the county and state estimates: a map by decade
(hostility or treatment, counties or states), each unit's trajectory on both
dimensions with 95% intervals, and the distribution of percent Black in the
selected decade with the unit marked.

Everything it needs is in this folder; there is no build step.

Colours follow the University of Michigan palette (Maize, Blue, Ross Orange,
Arboretum Blue); `style.css` holds the tokens and `app.js` the map ramps.

| File | Contents |
|---|---|
| `index.html`, `style.css`, `app.js` | the app (D3 7.9.0 and topojson-client 3.1.0 from jsDelivr) |
| `theme.js` | light/dark switch: follows the system setting until the reader picks one, which the browser remembers |
| `docs.html` | documentation: what the two dimensions mean, every indicator behind them, how the estimates are made |
| `data/abi.json` | estimates, column-wise (2.1 MB; about 530 KB as served, compressed) |
| `data/us.topo.json` | 2010 counties, simplified, with a states layer (0.5 MB; about 160 KB compressed) |

## Updating the data

After a new county run (`counties.csv` and `states.csv` from
`R/26_combine_counties.R`), rebuild both data files from the repo's top folder:

```
Rscript R/28_app_data.R
```

It needs `cty_20m.rds` in the top folder, `data/derived/census.csv`, and the
R packages sf, rmapshaper (which bundles mapshaper), jsonlite, dplyr and tidyr.
The boundaries keep the file's Albers projection with Alaska and Hawaii as
insets, so the browser only scales them; `simplify` at the top of the script
sets how much detail is kept.

## Running it locally

Browsers won't load the data from a `file://` page, so serve the folder:

```
python3 -m http.server 8765 --directory app
```

then open http://localhost:8765.

## Hosting on GitHub Pages

The folder is self-contained and uses relative paths, so it works at any URL.
Either:

* **Copy it into your Pages site**: copy `app/` into the site's repository
  (for example as `abi/`), commit and push; it is then at
  `https://<user>.github.io/abi/`.
* **Publish from this repository**: GitHub Pages can serve only the root or
  `docs/` of a branch, so publishing `app/` from here needs a GitHub Actions
  workflow that uploads the folder (`actions/upload-pages-artifact` with
  `path: app`, then `actions/deploy-pages`), with Pages set to "GitHub
  Actions" in the repository's settings.

## Links

The URL keeps the view, so a link opens the same map and selection, e.g.
`#level=county&dim=h&decade=1960&id=16055` (Kootenai County, Idaho, hostility,
1960). Parameters: `level` (county, state), `dim` (h, t), `decade`, `id` (FIPS
code or state abbreviation), `within=1` (quantiles from the selected decade only, the "This decade" button).
