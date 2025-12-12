# IJC437 — Project page
**Author:** oscaram92  
**Profile:** https://github.com/oscaram92  
**Repository:** https://github.com/oscaram92/IJC437  

## Summary  
This project develops a data-driven typology of Great Britain’s NUTS1 regions by jointly analysing business demography and recent employment dynamics.  
Using official ONS Business Demography tables (births, deaths, 3-year survival) and BRES/Nomis employment data (2018–2022), it constructs comparable regional indicators.  
After standardising variables to ensure scale comparability, the study applies k-means clustering (k=3) and Ward’s hierarchical clustering to identify coherent regional groupings.  
The resulting clusters are interpreted in terms of entrepreneurial churn, firm survival, employment growth, and the scale of active employer enterprises (2023).  
A specific interpretive focus is placed on London, Yorkshire & The Humber, and northern regions in the context of persistent UK regional disparities.

## Key findings  
- The analysis robustly identifies a three-cluster regional structure: a single “hot/dynamic” region (London), a broad “mature and stable” group, and a smaller “lagging/struggling” group.  
- London forms a distinct cluster, combining the highest average business birth rate with high firm turnover and the strongest employment growth over 2018–2022.  
- Yorkshire & The Humber is classified within the “mature and stable” cluster, characterised by moderate entry/exit rates, relatively high 3-year survival, and positive but modest employment growth.  
- North West and West Midlands cluster together as “lagging/struggling”, showing high business churn (high deaths alongside relatively high births) and comparatively weaker post-entry consolidation (lower 3-year survival).  
- Across regions, business entry and exit rates are of similar magnitude (non-trivial churn), while employment growth remains positive everywhere but highly uneven—highlighting London’s outperformance relative to the rest.

## How to run the code  
1. Clone the repository  

   ```bash  
   git clone https://github.com/oscaram92/IJC437.git  
   cd IJC437  
   ```  

2. Open the project and set the working directory  

   - In RStudio, open the repo folder (or an .Rproj file if present) so the working directory is the repository root.  

3. Install required R packages  

   - The script is configured to install any missing packages automatically.  

   - Required packages are:  

     - readxl, dplyr, tidyr, ggplot2, cluster, factoextra, stringr, readr, tibble  

4. Add the required input datasets  

   - Create a folder data/raw/ (if it does not already exist).  

   - Download and place the following Excel files in data/raw/:  

     - ONS Business Demography: businessdemographyexceltables2023.xlsx (uses sheets Table 1.1a, Table 2.1a, Table 3.1a, Table 4.1, Table 6.1).  

     - BRES (Nomis) export: nomis_2025_12_04_201028.xlsx (uses sheet Data in wide layout).  

5. Update file paths inside the script  

   - In R/99_run_all.R, edit the two path variables so they point to your local copies (recommended: use relative paths from the repo root). The script currently uses local absolute paths.  

   - Example:  

     ```r  
     biz_file  <- "data/raw/businessdemographyexceltables2023.xlsx"  
     bres_file <- "data/raw/nomis_2025_12_04_201028.xlsx"  
     ```  

6. Run the analysis end-to-end (single script workflow)  

   - Ensure use_example <- FALSE to run with the real datasets.  

   - Run in RStudio:  

     ```r  
     source("R/99_run_all.R")  
     ```  

   
7. Reproducibility and outputs  

   - The k-means solution is made reproducible via set.seed(123).  

   - Outputs produced when the script finishes:  

     - Console tables/data frames (e.g., reg_cluster_df and cluster_summary).  

     - Figures displayed in the plotting device: hierarchical dendrogram (fviz_dend) and PCA cluster plot (fviz_cluster).  

## R code  
Carpeta de scripts: `R/`  

- Main workflow (regional clustering): the core script reads two Excel inputs (ONS Business Demography and BRES/Nomis employment), cleans region names to a consistent NUTS1 convention, computes regional indicators (average birth and death rates over 2018–2022, 3-year survival for the 2018 cohort, employment growth 2018–2022, and active employer enterprises in 2023), and merges them into a single regional feature table for clustering.  

- Data ingestion helpers (ONS): reusable functions parse ONS tables with irregular headers (births, deaths, active enterprises), extract 3-year survival rates, and retrieve active employer enterprises for 2023, ensuring consistent typing and region filtering.  

- Data ingestion helpers (BRES/Nomis): a dedicated reader handles the wide “year-block” structure in the Nomis download, aggregates total employment across industries, and outputs a clean long panel by region and year (2018–2022).  

- Clustering and visualisation: the pipeline standardises variables, estimates k-means (k=3), validates structure via Ward hierarchical clustering, and produces interpretive plots (dendrogram and PCA-space cluster projection) alongside cluster-level descriptive summaries for interpretation.  

- Reproducibility notes: the script installs/loads required packages, sets a random seed for replicable k-means results, and uses clear variable naming for each constructed indicator (e.g., birth_rate_reg, death_rate_reg, survival_3y_reg, emp_growth_18_22_reg).