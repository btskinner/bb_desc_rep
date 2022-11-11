This repository contains the replication files for  

> Skinner, B.T., Burtch, T., and Levy, H. (2022). Variation in
> broadband access among undergraduate populations across the United
> States. _Annenberg Institute at Brown University_ (EdWorkingPaper: 22-667). Doi: [10.26300/8a57-0r97](https://doi.org/10.26300/8a57-0r97) 


## To run

Clone the project repository, `cd` into project directory, and run the `makefile`:

```bash
git clone https://github.com/btskinner/bb_desc_rep.git
cd ./bb_desc_rep
make
```

Or, after cloning the repository, run the R scripts one by one:

1. `make_data_acs.R`
1. `run_stan.R`
1. `poststratify.R`
1. `make_figures.R`
1. `make_tables.R`

Figures and tables found in the final paper can be built from
`figtab.md` after completing all scripts and running from the `docs`
directory:

``` sh
pandoc figtab.md \
--read=markdown \
--write=latex \
--output=./figtab.pdf \
--filter=pandoc-crossref \
--citeproc \
--lua-filter=linebreaks.lua
```
