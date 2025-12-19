# IJC437 — Project page
Author: oscaram92  
Profile: https://github.com/oscaram92  
Repository: https://github.com/oscaram92/IJC437  

Summary  
This project builds a data-driven typology of Great Britain’s **NUTS1** regions by combining **business demography** and **employment dynamics**.  
Using official **ONS Business Demography** tables (births, deaths, active enterprises, 3-year survival, active employer enterprises) and **BRES/Nomis** employment data (2018–2022), the pipeline constructs comparable regional indicators and applies clustering methods to identify coherent groupings.

The workflow:
- harmonises region names to a consistent NUTS1 convention,  
- computes pooled (2018–2022) **birth and death rates** weighted by active enterprises,  
- adds **3-year survival** (converted to proportions if needed),  
- computes **employment growth (2018→2022)**,  
- includes **active employer enterprises (2023)** (optionally log-transformed),  
- standardises variables and estimates clusters via **k-means** and validates them with **Ward hierarchical clustering**.

Data sources  
- ONS Business Demography (Excel tables, 2023 release):
  - Births: `Table 1.1a`–`Table 1.1d`  
  - Deaths: `Table 2.1a`–`Table 2.1d`  
  - Active enterprises: `Table 3.1a`–`Table 3.1d`  
  - 3-year survival: `Table 4.1`  
  - Active employer enterprises (2023): `Table 6.1`  
- BRES/Nomis employment export (Excel):** uses sheet `Data` in wide “year-block” format.



Methods  
- **Pre-processing:** scaling/standardisation via `scale()`; Euclidean distance.  
- **K selection diagnostics (computed for K=2..5):**
  - WSS (Elbow), Silhouette, and Gap Statistic.  
- **Clustering:**
  - k-means (`nstart = 25`, reproducible via `set.seed(123)`)
  - Ward hierarchical clustering (`ward.D2`) + cutree with `k_opt`  
- **Outputs include:**
  - dendrogram (Ward),
  - PCA-space k-means cluster plot,
  - silhouette plot for the chosen k-means solution,
  - console tables for `reg_cluster_df` (region assignments) and `cluster_summary`.

