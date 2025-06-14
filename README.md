# Resource Rational Prediction Experiment

## Overview

This repository contains data and code for the following paper in preparation:

Ferdinand, V., Yu, A., and Marzen, S. (in prep). Humans are resource rational predictors in a sequence learning task.

All of the experimental data and code for the analyses written in R by Ferdinand can be found in this repository.

All code for the calculation of theoretical PRA curves and related analyses, written in python by Marzen and Yu, can be found here:
<a href="https://github.com/smarzen/resource-rational-prediction"> smarzen/resource-rational-prediction </a>

## Contents

### Data

The experimental data can be found in the file `experiment.csv` in the folder `Data`.
See the README in the `Data` folder for a detailed explanation of the contents of `experiment.csv`.

Some of the outputs of the python code in Sarah's github repository is also archived in the `Data` folder. These are the numpy files `NoisyPeriodic_PRA.npz`, `DoubleProcess_PRA.npz`, and `EvenProcess_PRA.npz`, which contain the theoretical PRA curves and the coordinates of participants' behavior in the PRA space. I have written a script to convert these files to Rdata format (see `npz_converter.R` in the Code section below).


### Code

How to run the code:

The code is written in base R and stored in `.R` files. One way to run these is by installing R Studio and opening the R project file in the root directory of this repository: `resource-rational-prediction.Rproj`. Then, in the R Studio menu bar click on `File` and open the `.R` file you want to run, for example: File > Open File > Code > `load_data.R`.

What's in each file:

`load_data.R` Run this code to load `experiment.csv` into your R workspace as a dataframe called `df`. It also loads 6 objects from `PRA.Rdata`.

`npz_converter.R` This code converts the `.npz` files from Sarah's respository into the `PRA.Rdata` file. It's not necessary to run this code because the results are already stored in `PRA.Rdata`. However, the script itself contains a good explanation of what is included in these `.npz` files and `PRA.Rdata`.

`participation.R` This code contains basic descriptives about how participants engaged with the experiment, for example: how many participants took the experiment, how many sessions and trials each participant completed, and how participation was distributed across the three experimental conditions.

`results.R` This file contains everything that appears in the results section of the paper. The code is divided into sections that match each subsection in the paper, in order of appearance in the paper.

`functions.R` This file contains some support functions I wrote, which get loaded by the files above.


### Plots

The folder `Plots` contains all of the plots that are generated by the code above. They correspond to panels a-e in Figure 1 of the paper.

All other figures (Figures 2 and 3) were generated by the code in Sarah's repository.

