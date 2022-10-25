# Documents

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
