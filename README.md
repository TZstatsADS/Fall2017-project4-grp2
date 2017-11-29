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
+ Project summary: In this project, we implemented several different collaborative filtering algorithms and compared their performance on two data sets: Anonymous Microsoft Web Data and EachMovie. The divided the project on two types of algorithms: memory-based and model-based. In the memory-based algorithms, we implemented different similarity weights (Spearman correlation, vector similairty, entropy and SimRank), variance weighting, selecting neighbors methods (weight threshold, best n-estimator and combined) and rating normalization. The model-based algorithm was a clustering EM model. For the evaluation part we applied MAE to get the assess the prediction results for different combinations of neighbour selection methods and similiarity weights.

	
**Contribution statement**: 
+ Zhou, Xiaoyu: collaborated in Rating Normalization and Variance Weighting functions, wrote evaluation function MAE and ROC, made prediction for different pairs of similarity weight (spearman correlation and vector similarity). Collaborated with Tiantian to write code and debug for EM algorithm.
+ Zhao, Yufei: wrote and ran codes for neighborhood selection(Weight Threshold, Best-n-estimator, Combined) in terms of spearman coorelation and vector similarity. Worked closely with Joaquim on data cleanning, matrix transformation, calculation of similarity weights.
+ Chen Tiantian: wrote and debug codes for model-based method. Used EM algorithm to calculate the parameters of probabilities of cluster memberships and the conditional probabilities of votes given cluster. Used cross-validation to choose cluster number. Collaborated with Xiaoyu on the code for MAE and ROC.
+ Lyrio, Joaquim: wrote the functions to read the raw data sets into R; wrote the code to convert the format of Microsoft data set; wrote the Java codes to calculate the similarity measures (Spearman correlation and vector similarity); wrote the R function to calculate entropy  similarity measure; wrote the code for the function collabPredict, which performs the rating normalization; wrote the code to calculate variance weighting; collaborated with Yufei Zhao in writing the main.Rmd file.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
```

Please see each subfolder for a README file.
