## __ShinySurfer__ - Software tool for the Analysis and Visualization of MRI images of the brain

#### Start
Load .txt files created by [FreeSurfer](https://freesurfer.net) using the [aparcstats2table](https://surfer.nmr.mgh.harvard.edu/fswiki/aparcstats2table) command for the left and right hemisphere. In these files each line represents an individual participant and each column represents the mean value for a cortical area (cortical thickness, volume, area, or curvature). At present, ShinySurfer only accepts data files based on the brain parcellation developed by C. Destrieux et al. ( [Neuroimage 2010;53:1-15](https://www.sciencedirect.com/science/article/abs/pii/S1053811910008542)). You may also load a .csv file containing additional information for each participant, such as sex, age or clinical information. Example datasets can be found at [GitHub](https://github.com/SandraKla/ShinySurfer/tree/master/example%20data). 

#### Quality Control
The data files are combined and displayed as one table on the right. In the table you may sort all variables in ascending or descending order by clicking on the small arrows after each variable name.
After clicking on __Raincloud Plot__, ShinySurfer produces raincloud plots for all cortical areas in a new tab based on code provided by M. Allen et al. ( [Wellcome Open Res 2021;4:63](https://wellcomeopenresearch.org/articles/4-63/v2)). The raincloud plots combine a dot plot of raw data, a plot of probability density, and a box plot, showing the median and the quartiles. Before creating raincloud plots, you may filter your dataset based on every variable in the table to display subsets of your data.

#### Descriptive Statistics
After clicking on __Brain Map__, ShinySurfer displays the central tendency (mean or median) or dispersion (standard deviation or standard error of the mean) on a semi-inflated 3D standard brain using code provided by A. Mowinckel et al. ( [Adv Methods Pract Psychol Sci 2020;3:466-483](https://journals.sagepub.com/doi/full/10.1177/2515245920928009)). When you move the pointer inside the Brain Map tab, a toolbar appears in the right upper corner, allowing you to zoom, pan, and rotate the brain model. Before creating the 3D brain map, you may filter your dataset based on every variable in the table to display subsets of your data.

#### Linear Regression
After clicking on __Regression Plots__, ShinySurfer creates scatter plots for every cortical area with the cortical parameter on the y-axis and the chosen explanatory variable on the x-axis. ShinySurfer also displays the linear regression line and the 95% confidence interval of the regression line.

#### Lasso Regression
ShinySurfer is able to perform a bootstrapped lasso (least absolute shrinkage and selection operator) regression analysis for variable selection ( [Tibshirani 1996](https://rss.onlinelibrary.wiley.com/doi/abs/10.1111/j.2517-6161.1996.tb02080.x)). After choosing the explanatory variable and clicking on __Lasso Regression__, ShinySurfer creates a table containing all cortical areas. The table lists the proportion of all non-zero regression coefficients after 1000 bootstraps. In addition, the table states if all regression coefficients for a given cortical area were positive or negative (true).

#### Restart
To restart ShinySurfer click on Restart.

#### Documentation
For more information use the [Homepage](https://sandrakla.github.io/ShinySurfer_Homepage/).