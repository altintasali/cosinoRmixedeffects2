# cosinoRmixedeffects2

This package, [cosinoRmixedeffects2](https://github.com/altintasali/cosinoRmixedeffects2), is forked from [cosinoRmixedeffects](https://github.com/maytesuarezfarinas/cosinoRmixedeffects). [cosinoRmixedeffects](https://github.com/maytesuarezfarinas/cosinoRmixedeffects) makes an excellent implementation of mixed effects model to calculate the effect size, confidence intervals, standard errors and p-values using a bootstrapping approach while comparing the rhythmicity parameters *MESOR*, *amplitude* and *acrophase* across two or more groups. In the circadian bioinformatics, this approach is called as **differential rhythmicity** as it compares the **rhythmicity** between 2 or more groups. 

Prior to the differential rhythmicity analysis, a pre-filtering step is required to check whether at least one of the groups show rhythmicity in the measured entity such as gene expression. For example, let's assume that we would like to test the differential rhythmicity of 3 genes (*gene_1*, *gene_2*, *gene_3*) between 2 groups (*group_A*, *group_B*). Below &#10003; and &#10005; shows if a gene is **rhythmic** or **not rhythmic**, respectively.

| Gene          | group_1       | group_2       |
| :-----------: | :-----------: | :-----------: |
| gene_1        | &#10003;      |   &#10003;    |
| gene_2        | &#10003;      |   &#10005;    |
| gene_3        | &#10005;      |   &#10005;    |

Imagine that you ran a differential rhythmicity analysis for *gene_3* and it turned out to be statistically significant. However, remember that *gene_3* did not show any rhytmicity neither in *group_1* nor in *group_2*. This will obviously lead to a false positive discovery. Therefore, genes (or whatever feature you are testing) that do not show any rhythmicity should be discarded from differential rhythmicity analysis. In the toy example above, we should keep *gene_1* and *gene_2* while filtering out *gene_3* for differential rhythmicity analysis.

[cosinoRmixedeffects2](https://github.com/altintasali/cosinoRmixedeffects2) can perform **rhythmicity analysis** while keeping the same methodological principles presented in [cosinoRmixedeffects](https://github.com/maytesuarezfarinas/cosinoRmixedeffects) for **differential rhythmicity analysis**.

## Installation 

## Installation

Please install `devtools` if you haven't yet.

```
install.packages("devtools")
```

Then, you finally install the `cosinoRmixedeffects2` package by using the command below. **Warning:** It will take some time to build the vignettes, therefore sit back and relax (time for a coffee). 

```
library(devtools)
install_github("altintasali/cosinoRmixedeffects2", build_vignettes = TRUE, dependencies = TRUE)
```

If you are in a hurry and you know what to do, you can install the package without building the vignettes: 

```
library(devtools)
install_github("altintasali/cosinoRmixedeffects2")
```

## Quick start

I recommend going through the vignette **Rhythmicity and differential rhythmicity analysis** for a quick start. 

```
vignette("Rhythmicity_Analysis", package = "cosinoRmixedeffects2")
```

## What's new compared to `cosinoRmixedeffects`? 

New features

* Rhythmicity analysis
* Improved acrophase calculation
* New parameter `seed`: random seed assignment for reproducible results
* CRAN style R package

Bug fixes (from `cosinoRmixedeffects`)

* Amplitude calculation
* Small improvements and fixes on function parameters
