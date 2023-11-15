# Forest-Trhough-Trees
This Projects is Contains functions to create AP-trees portfolios, and  to performs robust mean variance optimization on them.

1.  AP-Trees are based on Bryzgalova et. all (2021) and sort assets into managed portofolios using observable characteristics at time (t-1) it consists basically on recursively splitting the cross section of assets on the median, with respect tocharacteristic, up a prespecified depth.
2. Bryzgalova et. all (2021) aims to recover the SDF from this tree portfolios, so they propose a robust mean variance optimization algorithm, to select the SDF weights.
3. In this project we provid this original algorithm, but we advocate for using as an input a covarianca matrix based on a technique for dimension reduction (Factor Models), in particular we use the POET estimator from Fan, Liao, and Mincheva (2013) to build an input for the covariance Matrix.
4. Furthermore, this project contains functions to estimate a multivariate Factor GARCH models, which enables to estimate large dimension conditional covariance matrices


Data:

1. Data set span from jan 1995 to april 2023, and consists of monthly observations for assets returns and some characteristics from brazilian stock market
2. Form this raw data frame, we build more characteristics, and constructed from eight characteristics another data frame of up to depth 2 trees portfolios using the functions constructed in this project the output data frame is called "retuns_df"


Functions: There is a lot of functions for many purposes. Since the goal of this project evolved and changed, the  demand for new functions evolved and changed aswel. Not everything is well comented, therefore it is probably a little confusing. If you have any  questions, please contact me at: bernardoscbraganca00@gmail.com


The Nom_port file is used to identify a portfolio by its composition in the stock level! there are some interdependencies with the Pruning final, since it uses objects defined there
