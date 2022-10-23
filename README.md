This repository contains the replication files for  

> Skinner, B.T., Burtch, T.M., and Levy, H. (2022). Variation in
> broadband access among undergraduate populations across the United
> States 


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

The final paper, `paper.md`, can be built after running all
scripts by running from the `docs` directory:

``` sh
pandoc \
--read=markdown \
--write=latex \
--output=./paper.pdf \
--filter=pandoc-crossref \
--citeproc \
--lua-filter=linebreaks.lua
```

For a shorter document of just figures and tables,
`tabfig.md` can be compiled instead.

``` sh
pandoc \
--read=markdown \
--write=latex \
--output=./figtab.pdf \
--filter=pandoc-crossref \
--citeproc \
--lua-filter=linebreaks.lua
```
