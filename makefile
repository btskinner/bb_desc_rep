# ==============================================================================
# 
# [ PROJ ] Variation in broadband access among undergraduate populations
#          across the United States
# [ FILE ] makefile
# [ AUTH ] Benjamin Skinner (@btskinner), Taylor Burtch, & Hazel Levy
# [ INIT ] 22 October 2022
#
# ==============================================================================

# --- directories --------------------------------

DAT_DIR := data
DOC_DIR := docs
FIG_DIR := figures
SCR_DIR := scripts
TAB_DIR := tables

# --- variables ----------------------------------

# data vars
ipums := $(DAT_DIR)/ipums/ipums_raw.dta
analysis_data := $(DAT_DIR)/clean/ipums_analysis.RDS

# output vars (one example: assumes one change is all change)
pred_output := $(EST_DIR)/mrp_broadband_pred.RDS
post_output := $(EST_DIR)/national_post.RDS
fig_output := $(FIG_DIR)/stcomp.pdf
tab_output := $(TAB_DIR)/overall.tex
doc_figtab_output := $(DOC_DIR)/figtab.md
doc_paper_output := $(DOC_DIR)/paper.md

# --- build targets ------------------------------

all: setup data analysis figures tables docs 

data: $(analysis_data)
analysis: $(pred_output)
figures: $(fig_output)
tables: $(tab_output)
docs: $(doc_figtab_output) $(doc_paper_output)

.PHONY: all setup data analysis figures tables docs

# --- packages -----------------------------------

setup:
	@echo "Checking for and installing necessary R packages"
	Rscript $(SCR_DIR)/r/check_packages.R .

# --- clean data ---------------------------------

$(analysis_data): $(SCR_DIR)/r/make_data.R $(ipums)
	@echo "Making analysis data"
	Rscript $< .

# --- analysis -----------------------------------

$(pred_output): $(SCR_DIR)/r/run_stan.R $(analysis_data) 
	@echo "Running Bayesian models"
	Rscript $< .

$(post_output): $(SCR_DIR)/r/poststratify.R $(pred_output) 
	@echo "Poststratifying predicted posteriors"
	Rscript $< .

# --- tables & figures ---------------------------

$(fig_output): $(SCR_DIR)/r/make_figures.R $(post_output)
	@echo "Making figures"
	Rscript $< .

$(tab_output): $(SCR_DIR)/r/make_tables.R $(post_output)
	@echo "Making tables"	
	Rscript $< .

# --- tab_fig ------------------------------------

$(doc_figtab_output): $(fig_output) $(tab_output)
	@echo "Compiling figures and tables document"
	pandoc \
		--read=markdown \
		--write=latex \
		--output=$@ \
		--filter=pandoc-crossref \
		--citeproc \
		--lua-filter=linebreaks.lua

$(doc_paper_output): $(fig_output) $(tab_output)
	@echo "Compiling paper"
	pandoc \
		--read=markdown \
		--write=latex \
		--output=$@ \
		--filter=pandoc-crossref \
		--citeproc \
		--lua-filter=linebreaks.lua

# --- clean up -----------------------------------

clean:
	@echo "Cleaning up directory"
	$(RM) -r $(EST_DIR)/*.csv
	$(RM) -r $(DAT_DIR)/clean/*.RDS
	$(RM) -r $(TAB_DIR)/*.tex
	$(RM) -r $(FIG_DIR)/*.pdf

# ------------------------------------------------------------------------------
# end makefile
# ==============================================================================
