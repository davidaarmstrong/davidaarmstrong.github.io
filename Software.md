<script src="https://kit.fontawesome.com/3b340a2892.js" crossorigin="anonymous"></script>

<script type="text/javascript">
document.addEventListener('DOMContentLoaded', function() {
    document.getElementById('downloads').innerHTML = '<div class="icon-container" style="width: 100%;"><a href="index.html" class="link-item" title="Home" target="_blank" rel="nofollow"><i class="fa-solid fa-house fa-2xl"></i><span style="padding-top: 15px;">Home</span></a><a href="Research.html" class="link-item" title="Research" target="_blank" rel="nofollow"><i class="fa-solid fa-puzzle-piece fa-2xl"></i><span style="padding-top: 15px;">Research</span></a><a href="Teaching.html" class="link-item" title="Teaching" target="_blank" rel="nofollow"><i class="fa-solid fa-user-graduate fa-2xl"></i><span style="padding-top: 15px;">Teaching</span></a><a href="Software.html" class="link-item" title="Software" target="_blank" rel="nofollow"><i class="fa-solid fa-floppy-disk fa-2xl"></i><span style="padding-top: 15px;">Software</span></a></div>';}, false);
</script>
<style>
.icon-container {
    display: flex;
    justify-content: space-evenly;
    align-items: center;
}

.icon-container a {
    text-align: center;
    display: flex;
    flex-direction: column;
    align-items: center;
    text-decoration: none;
    color: inherit;
}

.icon-container i {
    font-size: 24px; /* Adjust the icon size */
    margin-bottom: 5px; /* Space between icon and label */
    margin-top: 5px; /* Space between icon and label */
}

.icon-container span {
    font-size: 14px; /* Adjust the label size */
}
</style>

I maintain several packages for R, including some lightly modified versions of existing packages.  You can see all of them on my [github]{https://github.com/davidaarmstrong} page.  Below are some links to the ones I think are more prominent or useful to a general audience. 

### [ClarkeTest](/files/clarkeTest/index.html)

The `clarkeTest` package implements Kevin Clarke's (2007) distribution-free test for non-nested models.  It does so in a way that is more modular than the `games` package currently supporting models estimated with `lm()`, `glm()` (`binomial`, `poisson` and `negbin` links), `polr()` (from the `MASS` package), `clm()` (from the `ordinal` package), `multinom()` (from the `nnet` package) and `mlogit()` (from the `mlogit` package)

### [DAMisc](/files/damisc/index.html)

The `DAMisc` package has evolved over the past decade to include many of the functions I use when teaching applied stats to social scientists.  I think many of the functions might be useful more broadly so I thought it would be worth discussing the functionality in a sort of thematic way here.  The functions do fall into a few different themes.  

  - Functions that attempt to figure out whether and what kind of unmodeled non-linearities exist.  
  - Functions for investigating interactions in different settings (linear models and binomial GLMs).  
  - Functions for post-model evaluation and examination of non-linear models (GLMs, ordinal data models and unordered data models). 

### [factorplot](/files/factorplot/index.html)

The `factorplot` package provides users with a way to visualize pairwise comparisons that arise from multiple situations.  The package was first developed to plot pairwise comparisons that arise from factors being included in statistical models; that is, to deal with the reference category problem.  It has grown to include multinomial logit models (estimated with `mlogit()` from `nnet`), `eff` objects created with the `effects` package and `glht` objects from the `multcomp` package.  The link to the `pkgdown` site above provides some examples for various different use-cases.  For more information about methods for visualizing paired comparisons, see [my article](https://journal.r-project.org/archive/2013/RJ-2013-021/) in the R Journal.   

### [psre](psre.html)
This R package supports <em>Presenting Statistical Results Effectively</em> (Sage, 2022) by Bob Andersen and Dave Armstrong.  The package contains the data and replication code that recreates all of the visualizations in the book.  

### [asmcjr](https://github.com/davidaarmstrong/asmcjr) 
This R package supports the <em>Analyzing Spatial Models of Choice and Judgment</em> book, which is now in its second edition.  You can see more about it on my [Research page](Research.html).  The [github repo](https://github.com/davidaarmstrong/asmcjr) for the package has helpful hints and potential troubleshooting suggestions. 

### [optiscale](https://cran.r-project.org/web/packages/optiscale/index.html)
The optiscale package was conceived and written by Bill Jacoby.  It performs optimal scaling tasks like those that form the basis of Alternating Least Scales Optimal Scaling (ALSOS).  

### [DyadRatios](https://github.com/davidaarmstrong/DyadRatios)
The DyadRatios package is a packaged version of Jim Stimson's code which can be found on [his website](https://stimson.web.unc.edu/software/).  The packaging simply makes available help files and installing it will automatically install any dependencies.  