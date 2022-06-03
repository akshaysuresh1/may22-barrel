Grand data sets constructed for exploratory data analysis and crop yield modeling

---
### .csv files: <br>

1. `rice_yield.csv`: Clean data set constructed from multiple separate ICRISAT databases <br>
2. `rice_yield_clusters.csv`: Column added to `rice_yield.csv` to associate every district with a cluster number identified using hierarchical clustering. <br>
3. `districts_clusters.csv`: Districts in `rice_yield.csv` and their assigned cluster number via hierarchical clustering <br>
4. `rice_yield_KMeans*`: Same as `rice_yield_clusters.csv`, but now the clusters have been detected using the K-means algorithm. <br>

### Shape files: <br>
1. `districts2020.*`: Cleaned shape file <br>
2. `districts2020_clusters.*`: Column added to `districts2020.*` to associate each district with a cluster number identified via hierarchical clustering. <br>
