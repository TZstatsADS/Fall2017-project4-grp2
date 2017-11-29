# Project 4: Collaborative Filtering

### [Project Description](doc/project4_desc.md)

Term: Fall 2017

+ Team #2
+ Project title: Collaborative Filtering Algorithms Evaluation
+ Team members
	+ Cavalheiro De Paoli Lyrio, Joaquim
    + Chen, Tiantian
    + Park, Stephanie
    + Zhao, Yufei
    + Zhou, Xiaoyu
+ Project summary: In this project, we implemented several different collaborative filtering algorithms and compared their performance on two data sets: Anonymous Microsoft Web Data and EachMovie. The divided the project on two types of algorithms: memory-based and model-based. In the memory-based algorithms, we implemented different similarity weights (Spearman correlation, vector similairty, entropy and SimRank), variance weighting, selecting neighbors methods (weight threshold, best n-estimator and combined) and rating normalization. The model-based algorithm was a clustering EM model. For the evaluation part we applied MAE to get the results for different combination of neighbours and similiarity weight and prediction method.

	
**Contribution statement**: 
+ Zhou, Xiaoyu: wrote and ran code for Rating Normalization, Variance Weighting, wrote evaluation function MAE and ROC, made prediction for different pairs of similarity weight (spearman correlation and vector similarity). Collaborated with Tiantian to write code and debug for EM algorithm.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
```

Please see each subfolder for a README file.
