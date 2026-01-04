# MTLComb: multi-task learning combing regression and classification tasks with joint feature selection


## Introduction
Multi-task learning (MTL) is a learning paradigm that enables the simultaneous training of multiple communicating algorithms, and has been widely applied in the biomedical analysis for shared biomarker identification. Although MTL has successfully supported either regression or classification tasks, incorporating mixed types of tasks into a unified MTL framework remains challenging, especially in biomedicine, where it can lead to biased biomarker identification. To address this issue, we propose an improved method of multi-task learning, MTLComb, which balances the weights of regression and classification tasks to promote unbiased biomarker identification. We demonstrate the algorithmic efficiency and clinical utility of MTLComb through analyses on both simulated data and actual biomedical studies pertaining to sepsis and schizophrenia.


<p align="center"> 
<img src="case studies/principle.png" style="width: 95%; height: 95%"/>
</p>

## Installation

```r
install.packages("devtools")
library("devtools")
install_github("transbioZI/MTLComb")
```




## Tutorial and Manuscript
The [Tutorial](https://github.com/transbioZI/MTLComb/blob/main/case studies/0_tests_of_MTLComb/test_MTLComb.R) demonstrated all functions of MTLComb regarding to solver, training procedure, cross-validation and prediction procedure. The technical details and more use cases can be found in our [manuscipt](https://arxiv.org/abs/2405.09886) 



## Contact
Han Cao (hank9cao@gmail.com)
