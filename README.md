# MTLComb: multi-task learning combing regression and classification tasks with joint feature selection


## Introduction
Multi-task learning (MTL) is a machine learning technique for the simultaneously learning of multiple, ‘communicating’ algorithms. The currently available MTL algorithms have been designed for either regression or classification tasks, but no solutions exist that allow for the joint learning of these task types, limiting the utility of MTL with regards to interesting biomedical applications. We developed MTLComb, a novel methodology and software for MTL that can learn regression and classification tasks simultaneously and includes regularization to identify predictors of relevance for all tasks. We provide a mathematical derivation of the algorithm and demonstrate its utility using simulation data.

We implemented the efficient solver for solving the mixed objective and the training procedure to estimate the regularization path. The cross-validation is implemented to select the lambda 


## Installation

```r
install.packages("devtools")
library("devtools")
install_github("transbioZI/MTLComb")
```




## Tutorial
The [Tutorial](https://github.com/transbioZI/MTLComb/blob/main/tests/MTLComb_Tutorial.R) demonstrated all functions of MTLComb regarding to solver, training procedure, cross-validation and prediction procedure.
This [script](https://github.com/transbioZI/MTLComb/blob/main/tests/MTLComb_codes4paper.R) demonstrate the codes for generating the results of MTLComb paper.



## Contact

Han Cao (hank9cao@gmail.com)
