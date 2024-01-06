# PDHDPaper
This repository contains the code, blueprint and dataset of my PD and HD paper


Here are some step by step workflow of the code and the paper

Writing the code to extract the NDVI, EVI and everything from google earth engine
What is the code like 
1. Load the dataset 
    Landsat 8 Dataset (LANDSAT/LC08/C01/T1_SR)
    Nass Cropscape Dataset (USDA/NASS/CDL)
    Tiger State collection (TIGER/2016/States)
2. Load the packages 
    Spectral (Used to calculate the different vegetation indices)
    Palettes (Used to map divergent color in google earth engine)
3. Filtering Data
    Spatial Filter 
        For spatial cover we want the Arkansas region which is obtained from the Tiger dataset
        From the NASS cropscape dataset choose rice which is crop land cover =3
    Temporal Filter 
    The minimum day of planting from the histogram of field collected DOP and the maximum day of harvesting from the histogram of field collected DOH
4. Masking out the bad pixels 
    Cloud shadow
    Cloud cover 
    Filter bound the 

