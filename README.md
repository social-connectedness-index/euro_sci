# Replication code for ''The Determinants of Social Connectedness in Europe'' and Online Appendix

This repository includes the replication code for the paper [The Determinants of Social Connectedness in Europe](https://doi.org/10.1007/978-3-030-60975-7_1) and the [Online Appendix with additional results](https://arxiv.org/abs/2007.12177}). You can find additional example code and shapefiles for using the SCI data at: <https://github.com/social-connectedness-index/example-scripts>.

When using this repository, please cite the original paper.

NOTE 1: Because of privacy restrictions added to the public release of the SCI data and a different data generation date, the results in this folder differ in small ways from those in the actual paper.

NOTE 2: This repository uses [git-lfs](https://git-lfs.github.com/) for versioning large files. You will need it installed to clone the repository.

## Repository Structure

All results can be generated from running three scripts in the master directory: 
1. `1_setup_map_andSCI.R` downloads administrative shapefiles from (c) EuroGeographics and the [Facebook SCI data](https://data.humdata.org/dataset/social-connectedness-index?) from the Humanitarian Data Exchange. 
2. `2_run_all_R_analysis_scripts.R` produces a number of results in R.
3. `3_run_all_stata_analysis_scripts.do` produces a number of results in Stata.

All final output is saved to `_output`.

The script `1x_run_all_prep_scripts.R` generates the intermediate data used in all analysis. We do not sync the files in the `_raw_data` folder that this script uses, as it includes no additional information used in our final analysis. We do include a key that shows the source of each dataset. Upon request we can share a zip file of this folder.

In addition to the paper, this repository includes the presentation slides from the 2020 International Conference on Social Informatics. You can find a recording of the presentation [here](https://drive.google.com/file/d/1cPQFFAfvfXMaYFqR2_3ojMmtOSMMcrKI/view).
