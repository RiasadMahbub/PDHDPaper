# PDHDPaper
This repository contains the code, blueprint and dataset of my PD and HD paper


Here are some step by step workflow of the code and the paper

Writing the code to extract the NDVI, EVI and everything from google earth engine
What is the code like 
1. Load the dataset <br> 
    Landsat 8 Dataset (LANDSAT/LC08/C01/T1_SR)<br> 
    Nass Cropscape Dataset (USDA/NASS/CDL)<br> 
    Tiger State collection (TIGER/2016/States)<br> 
2. Load the packages <br> 
    Spectral (Used to calculate the different vegetation indices)<br> 
    Palettes (Used to map divergent color in google earth engine)<br> 
3. Filtering Data<br> 
    Spatial Filter <br> 
        For spatial cover we want the Arkansas region which is obtained from the Tiger dataset<br> 
        From the NASS cropscape dataset choose rice which is crop land cover =3<br> 
    Temporal Filter <br> 
    The minimum day of planting from the histogram of field collected DOP and the maximum day of harvesting from the histogram of field collected DOH<br> 
4. Masking out the bad pixels <br> 
    Cloud shadow<br> 
    Cloud cover <br> 
    Filter bound the <br> 


- Manual export
- Package Loading:
  - spectral and palettes
  - viridis palettes
- Load the arkansas shapefile file from FAO Gaul 2015
- Convert the geometry


## Landsat Images
List  the band names

