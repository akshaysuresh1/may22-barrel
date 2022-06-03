### Files: <br>

1. `npk_cluster_trends.ipynb`: Explore the impact of fertilizer consumption per unit cropped area on annual rice yield for each cluster of districts identified in our data. <br>
2. `npk_slr.ipynb`: Fit a simple linear regression model to NPK consumption (kg/ha) vs. rice yield (kg/ha) for each cluster. <br>
3. `npk_rfr.ipynb`: Fit a random forest regressor to NPK consumption (kg/ha) vs. rice yield (kg/ha) for each cluster. <br>
4. `npk_svr.ipynb`: Fit a support vector regressor to NPK consumption (kg/ha) vs. rice yield (kg/ha) for each cluster. <br>
5. `npk_poly2_ridge.ipynb`: Second order polynomial fitting with ridge regression applied to NPK consumption (kg/ha) vs. rice yield (kg/ha) data for each cluster. <br>
6. `smape_*.csv`: Symmetric mean absolute percent error (SMAPE) values returned by different models for various clusters <br>
7. `barplot_smape.ipynb`: Utilizes SMAPE as a metric to compare performances of different models on the same data <br>
