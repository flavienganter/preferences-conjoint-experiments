# Replication materials for the draft "Identification of Preferences in Forced-Choice Conjoint Experiments: Reassessing the Quantity of Interest" (Ganter, 2021)

*Abstract.* Forced-choice conjoint experiments have become a standard component of the experimental toolbox in political science and sociology. Yet the literature has largely overlooked the fact that conjoint experiments are used for two distinct purposes: to uncover respondents' multidimensional preferences, and to estimate the causal effects of some attributes on a profile's selection probability in a multidimensional choice setting. This paper makes the argument that this distinction is both analytically and practically relevant, because the quantity of interest is contingent on the purpose of the study. The vast majority of social scientists relying on conjoint analyses, including most scholars interested in studying preferences, have adopted the average marginal component effect (AMCE) as their main quantity of interest. The paper shows that the AMCE is neither conceptually nor practically suited to explore respondents' preferences. Not only is it essentially a causal quantity conceptually at odds with the goal of describing patterns of preferences, but it also does generally not identify preferences, mixing them with compositional effects unrelated to preferences. This paper proposes a novel estimand&mdash;the average component preference (ACP)&mdash;designed to explore patterns of preferences, and it presents a method for estimating it.

The paper is available on [_Political Analysis_' website](https://www.cambridge.org/core/journals/political-analysis/article/identification-of-preferences-in-forcedchoice-conjoint-experiments-reassessing-the-quantity-of-interest/E6C7719AD2EF30514C2EC4396FD0D928), and the preprint on [SocArXiv](https://osf.io/preprints/socarxiv/e638u/).

This repository contains the following replication materials:
* the R function used to estimate ACPs, differences in ACPs, and direct pairwise preferences, along with its documentation;
* the R code that replicates my partial replication of Hainmueller and Hopkins's immigrant experiment. The original data are available on the [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/THJYQR);
* the R code that replicates my partial replication of Mummolo and Nall's community experiment. The original data are available on the [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/EDGRDC);
* the R code that replicates the Monte Carlo simulations presented in the supplemental information D.

***

### Computational platforms:

1. I tested my code on a MacBook Pro 2019 with Quad-Core Intel Core i5 and 16G RAM. The operation system is macOS 11.4 (BigSur).
2. The R version is 3.6.1.

### Packages

Please install packages “doParallel”, “foreach”, “haven”, “patchwork”, “sandwich”, “sjlabelled”, “stringr”, “tidyverse”, and “xlsx” from CRAN.

### Folders

| **Folder** | **Description** |
| ---------- | --------------- |
| Codes | Stores code files required to replicate Hainmueller and Hopkins (2015) and Mummolo and Nall (2017), to run the Monte Carlo simulations, and to generate the graphs and tables included in the paper |
| Data | Stores data from Hainmueller and Hopkins (2015) and Mummolo and Nall (2017) |
| Functions | Stores required additional functions |
| Output | Stores graphs and tables from code files |

### Notes

1. Please make sure to set path to the root replication folder in R.
2. Each code file is independent.
3. All graphs and tables are saved in the “Output” folder. When you run the code, existing files in
this folder will be overwritten, which is normal.
4. Figure 1, Table A1, and Table D1 present information on the data format, on the literature review,
and on the parameters of the simulation. Replication is not applicable.
5. Simulation results may not be exactly be replicated due to the randomness of the simulation
process.
6. The Monte Carlo simulation takes hours to finish. To speed things up, you can use the
intermediate files that store simulation results and can be used to construct the simulation result tables.

### File names and Tables / Figures in the paper

| **In Paper** | **File name**|
| --- | --- |
| Figure 1 | - |
| Figure 2 | acpamce_imm.pdf |
| Figure 3 | acpamce_comm.pdf |
| Table A1 | - |
| Table D1 | - |
| Table D2 | simulations_acp.csv |
| Table D3 | simulations_dacp.csv |
| Table D4 | simulations_p.csv |
| Figure B1 | relimp_reg.pdf |
| Figure B2 | relimp_cond.pdf |

### Code files

| **File name** | **Description** | **Output** | **Execution time**|
| --- | --- | --- | --- |
| CommunityReplication.R | Replicates partially Mummolo and Nall (2017) | acpamce_comm.pdf | Minutes |
| ImmigrantReplication.R | Replicates partially Hainmueller and Hopkins (2015) | acpamce_imm.pdf relimp_reg.pdf relimp_cond.pdf | Minutes |
| Simulations.R | Runs the Monte Carlo simulations | SimulationIntermediate.RData SimulationIntermediate_int.RData simulations_acp.csv simulations_dacp.csv simulations_p.csv | Hours |
