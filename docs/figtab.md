---
subtitle: |
  | Variation in broadband access among undergraduate 
  | populations across the United States
title: Figures and Tables
author:
  - Benjamin Skinner^[\noindent_Corresponding author_&#58;
    btskinner@coe.ufl.edu.] 
    <br>University of Florida
  - Taylor Burtch<br>University of Florida
  - Hazel Levy<br>University of Florida
date: | 
  | <br>
  | 22 October 2022
  | <br>
  | <br>
geometry: margin=1in
fontsize: 12pt
resource-path: ['..','figures']
figPrefix:
  - "Figure"
  - "Figures"
tblPrefix:
  - "Table"
  - "Tables"
eqnPrefix:
  - "equation"
  - "equations"
header-includes:
  - \usepackage[T1]{fontenc}
  - \usepackage[utf8]{inputenc}
  - \usepackage{dsfont}
  - \usepackage{amstext}
  - \usepackage{amssymb}
  - \usepackage{amsmath}
  - \usepackage{mathptmx}
  - \usepackage[american]{babel}
  - \usepackage{setspace}
  - \usepackage{appendix}
  - \usepackage{tabularx}
  - \usepackage{longtable}
  - \usepackage{booktabs}
  - \usepackage{caption}
  - \captionsetup{singlelinecheck=off,font=small,labelfont=bf}
  - \usepackage[nolists,tablesfirst,nomarkers]{endfloat}
  - \newcommand{\RR}{\raggedright\arraybackslash}
  - \newcommand{\RL}{\raggedleft\arraybackslash}
  - \newcommand{\CC}{\centering\arraybackslash}
  - \setlength{\parindent}{2em}
  - \setlength{\parskip}{0em}
---


![State-level comparison of in-home broadband access (top panel) and
access only through a cellular data plan (bottom
panel). Center dots and lines represent medians and 95% credible
intervals, respectively, for posterior predicted probabilities. The
horizontal dashed line and shaded area show the national median and
95% credible interval.](../figures/stcomp.pdf){#fig:stcomp}

![Race/ethnicity comparison of in-home broadband access (left panel)
and access only through a cellular data plan (right panel). Numbers on
the _y_-axis correspond to U.S. Census codes and can be linked the
names given by the census in Appendix
@tbl:overall.](../figures/recomp.pdf){#fig:recomp}

![In-home broadband access (top panel) and access only through a
cellular plan (bottom panel) for American Indian / Alaska Native
populations. The horizontal dashed line and shaded area represent the
overall median / 95% credible interval for this population. The
horizontal dotted line and shaded area represent the national median /
95% credible interval.](../figures/aiak_mf_disagg.pdf){#fig:aiak}

![In-home broadband access (top panel) and access only through a
cellular plan (bottom panel) for Asian populations. The horizontal
dashed line and shaded area represent the overall median / 95%
credible interval for this population. The horizontal dotted line and
shaded area represent the national median / 95% credible
interval.](../figures/asian_mf_disagg.pdf){#fig:asian}

![In-home broadband access (top panel) and access only through a
cellular plan (bottom panel) for multiracial/multiethnic populations
typically designated as _other_. The horizontal dashed line and shaded
area represent the overall median / 95% credible interval for this
population. The horizontal dotted line and shaded area represent the
national median / 95% credible
interval.](../figures/other_mf_disagg_slides.pdf){#fig:mult}

![In-home broadband access among Hispanic populations across
California, Florida, and Texas. The horizontal dashed line and shaded
area represent the overall median / 95% credible interval for this
population. The horizontal dotted line and shaded area represent the
national median / 95% credible
interval.](../figures/bb_h_st.pdf){#fig:bbhst}

![Broadband access only through a cellular plan among Hispanic
populations across California, Florida, and Texas. The horizontal
dashed line and shaded area represent the overall median / 95%
credible interval for this population. The horizontal dotted line and
shaded area represent the national median / 95% credible
interval.](../figures/do_h_st.pdf){#fig:dohst} 

\processdelayedfloats
\appendix
\setcounter{table}{0}
\renewcommand{\thetable}{A\arabic{table}}
\input{../tables/overall.tex}
\pagebreak
\input{../tables/men.tex}
\pagebreak
\input{../tables/women.tex}
\pagebreak
\input{../tables/hispanic_bb.tex}
\pagebreak
\input{../tables/hispanic_do.tex}
