# Subnational Electoral Deforestation

Replication data and code for _Electoral Deforestation Cycles: A Subnational Analysis_

`analysis.R` runs all analyses included in body text and appendices.

`clea_process.R` processes and clean. Outputs file `clea_final.RData`.

`merge_sp.R`: Code for spatial merge of Hansen GFC with CLEA data; (requires n GB RAM, m cores)

`merge_nonsp.R` joins country-year level non-spatial data (e.g. DPI, Polity) onto the main dataframe.

`gred_glbl.R` merges all of CLEA's GeoReference data into one dataframe.

`plots.R` generates plots included in paper.

To reproduce the final upsampled data, run the scripts in the following order: `clea_process.R`, `gred_glbl.R`, `merge_sp.R`, `merge_nonsp.R`.
