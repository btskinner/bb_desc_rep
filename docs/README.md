# Documents

The final paper, `paper.md`, can be built after running all
scripts by running:

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
