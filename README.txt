This README.txt file was generated on 2023-06-29 by Thomas J. Yamashita


GENERAL INFORMATION

1. Title of Dataset: A Multivariate Approach to Assessing Landscape Structure Effects on Wildlife Crossing Structure Use
	This data is associated with the manuscript, titled A Multivariate Approach to Assessing Landscape Structure Effects on Wildlife Crossing Structure Use, available here: XXXX

2. Author Information
	Thomas J. Yamashita
		Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville
		tjyamashta@gmail.com
		Corresponding Author
	Humberto L. Perotto-Baldivieso
		Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville
	David B. Wester
		Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville
	Kevin W. Ryer
		School of Earth, Environmental, and Marine Sciences, University of Texas Rio Grande Valley
	Richard J. Kline
		School of Earth, Environmental, and Marine Sciences, University of Texas Rio Grande Valley
	Michael E. Tewes
		Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville
	John H. Young Jr. 
		Environmental Affairs Division, Texas Department of Transportation
	Jason V. Lombardi
		Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville

3. Date of Data Collection: 
	December 2019 through November 2020

4. Geographic location of data collection: Eastern Cameron County, Texas, USA around State Highway 100, Farm-to-Market 106, and Farm-to-Market 1847

5. Funding Sources: Texas Department of Transportation


DATA & FILE OVERVIEW

1. File List: 
	LandscapeMetrics_20230629.xlsx: An excel file with the landscape metrics in a 1 km buff around each wildlife crossing and random site
	LandscapeStructureBobcats_R.R: R code used to produce figures and analyze data for the manuscript. This code will not function without the bobcat detection data. Please contact the corresponding author to request bobcat detection data. 
	LandscapeStructureBobcats_SAS.sas: SAS code used to analyze data for the manuscript. This code will not function without the bobcat detection data. Please contact the corresponding author to request bobcat detection data. 
	README.txt: This file

2. Relationship between files: 
	Only one data file is provided in this repository.  

3. Excluded files from repository: 
	Classified Landcover map: Classified landcover map is available upon request. Please contact the corresponding author. 
	Bobcat detection data: Bobcat detection data is available upon request. Please contact the corresponding author. 

METHODOLOGICAL INFORMATION

1. Description of the methods used for collection/generation and processing of data: 
	Methodology for collection and processing of the data can be found in the manuscript

2. Quality Assurance Procedures: 
	Quality assurance is discussed in the manuscript. Classification accuracy was 87%. Bobcat detection data was hand sorted by multiple individuals to ensure accuracy.

3. People involved with data collection, processing, and analysis: 
	Thomas J. Yamashita, Kevin W. Ryer, David B. Wester, Jason V. Lombardi


DATA SPECIFIC INFORMATION FOR: LandscapeMetrics_20230629.xlsx
1. Data Type: Microsoft Excel File

2. Number of Variables: 16

3. Number of Rows: 26

4. Variable List: 
	Site: Location where landscape metrics were calculated
	Type: Was the site a crossing or random location
	PLAND: Percent landcover in a 1 km buffer around the site
	PD: Patch density in a 1 km buffer around the site
	LPI: Largest patch index in a 1 km buffer around the site
	ED: Edge density in a 1 km buffer around the site
	LSI: Landscape shape index in a 1 km buffer around the site
	MPA: Mean patch area in a 1 km buffer around the site
	ENN_MN: Mean euclidean nearest neighbor distance in a 1 km buffer around the site
	AI: Aggregation index in a 1 km buffer around the site
	m0.5: Mean vegetation density at 0.5 m above the ground in a 1 km buffer around the site
	m1.0: Mean vegetation density at 1.0 m above the ground in a 1 km buffer around the site
	m1.5: Mean vegetation density at 1.5 m above the ground in a 1 km buffer around the site
	m2.0: Mean vegetation density at 2.0 m above the ground in a 1 km buffer around the site
	m2.5: Mean vegetation density at 2.5 m above the ground in a 1 km buffer around the site
	m3.0: Mean vegetation density at 3.0 m above the ground in a 1 km buffer around the site

5. Other Information: Landscape metrics were calculated using Fragstats and the lidR package in R


DATA SPECIFIC INFORMATION FOR: LandscapeStructureBobcats_R.R
1. Data Type: R script

5. Other Information: This script provides code for producing figures of PCAs and data management used in this manuscript


DATA SPECIFIC INFORMATION FOR: LandscapeStructureBobcats_SAS.sas
1. Data Type: SAS script

5. Other Information: This script provides code for running the generalized linear mixed models 


