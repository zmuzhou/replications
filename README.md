This is a repository of some replication exercises. 

### `hong_yang_2020_bjps` 
Replicated article:

Hong, Ji Yeon, and Wenhui Yang. 2020. “Oilfields, Mosques and Violence: Is There a Resource Curse in Xinjiang?” _British Journal of Political Science_ 50(1): 45-78. https://doi.org/10.1017/S0007123417000564

The article finds oil and gas sales in Xinjiang soothe ethnic violence but mosque density weakens this effect.

This was a part of an assignment of GV508: Analysis of Conflict and Peace at Essex (then taught by [Martin Steinwand](https://sites.google.com/site/martincsteinwand/)).

| `output.pdf`     | article          |
| ---------------- | ---------------- |
| Table 1          | Table A.1        |
| (1)–(3), Table 2 | (7)–(9), Table 1 |
| (1)–(3), Table 3 | (7)–(9), Table 3 |
| (1)–(3), Table 4 | (4)–(6), Table 7 |

The replicated standard errors are not identical with that reported in the article.

### `rogowski_tucker_2019_psrm`
This is done with Joseph Kelly.

Replicated article:

Rogowski, Jon C., and Patrick D. Tucker. 2019. “Critical Events and Attitude Change: Support for Gun Control After Mass Shootings.” _Political Science Research and Methods_ 7(4): 903–11. https://doi.org/10.1017/psrm.2018.21

The article finds American's gun control support was unchanged following the 2012 Sandy Hook shooting. The data file is downloadable at https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/1SBYFJ/JGX6A7&version=1.0.

This was a part of an assignment of GV915: Applied Research Design at Essex (then taught by [Carolina Garriga](https://sites.google.com/site/carogarriga/)).

| `output.pdf`     | article                 | our extension                  |
| ---------------- | ----------------------- | ------------------------------ |
| Figure 1         | Left panel, Figure 2    | logit instead of OLS           |
| Table 1          | Left panel, Figure A.1  | ordered logit instead of OLS   |
| Table 2          | NA                      | education's conditional effect |
| Table 3          | Left panel, Figure 2    | NA                             |
| Table 4          | Right panel, Figure 2   | NA                             |
| Table 5          | Left panel, Figure A.1  | NA                             |
| Table 6          | Right panel, Figure A.1 | NA                             |
| Table 7          | Figure 3                | NA                             |

We have a research note [[link](https://github.com/zmuzhou/zhang_kelly_2020)] related to this replication. We use the 2016 Orlando shooting as a new case to show that American's gun control attitude was not changed after this either.

### Note
Working directory should be appropriately set. Run `all.R` to execute analysis and produce tables and figures. `input{tabname}` and `includegraphics{figname.pdf}` with necessary preambles to compile `output.pdf`. (I use my customized style file which could be found [[here](https://gist.github.com/zmuzhou/8e06d7887804728c25a908af014f0499)].)