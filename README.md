# Subnational Electoral Deforestation
Repository for subnational electoral deforestation cycles paper

**analysis.R** runs all analyses included in body text and appendices.

**"support.R"** contains all packages used in R-scripts anywhere in the project.

**"clea_process.R"** groups the CLEA data by {ctr,cst,yr} and adds as variables two competitiveness measures for each. Outputs file "clea_final.RData".

**"merge_sp.R"**: Code for spatial merge of deforestation data with CLEA data; (requires HPC)

**merge_nonsp.R** joins the nonspatial data (DPI, Polity, etc.; on the country-year level) onto the main dataframe.

**gred_glbl.R** merges all of CLEA's GeoReference data into one dataframe.

**plots.R** generates plots shown in paper.

To reproduce the final upsampled data, run the scripts in the following order: **clea_process.R**, **gred_glbl.R**, **merge_sp.R**, **merge_nonsp.R**
