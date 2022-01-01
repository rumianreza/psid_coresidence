Contact: Rumian Reza // rumian.reza@unt.edu
Final data set: analysis_sample_5655_v2.csv (analysis_sample_5655_v2.dta)

***R Scripts (in order of execution):
	
	0A_PSID_VariableNames.R
		Input:
		(1)	File: none
			Desc: N/A
		Output:
		(1)	Files: (many .csv files)
			Desc: containing individual-level and family-level variable
			names. 
		(2)	File: VariableList_v1.csv
			Desc: A list of variables to be downloaded from PSID's data center.

	1A_ImportAndClean_PSID.R
		Input:
		(1)	File: VariableList_v1.csv
			Desc: A list of variable names that were downloaded from PSID's data
			center.
		(2)	File: J299783.xlsx
			Desc: Variables downloaded from the PSID data center.
		(3)	File: J299788.xlsx
			Desc: Summary-level variables downloaded from the PSID data center.
			(Note that these may not be included in VariableList_v1.csv...)
		(4)	File: fim12001_gid_BA_2_BAL_wide.xlsx
			Desc: Retrospective FIMS; see details below.
		(5)	File: PSIDStateCodes.csv
			Desc: State codes linking integer values to state names; see details
			below.
		Output:
		(1)	File: psid_data_v1.csv
			Desc: Cross-wave individual file consisting of re-defined PSID
			variables. See the first table in the Variable Dictionary file.

	1B_ImportAndClean_FRED.R
		Input:
		(1)	Files: UR1.csv, UR2.csv, UR3.csv, UR4.csv, UR5.csv
			Desc: State-level seasonally-adjusted monthly unemployment rates;
			see details below.
		Output:
		(1)	File: annual_state_UR.csv
			Desc: Annual state-level unemployment rates.

	1C_ImportAndClean_FreddieMac.R
		Input:
		(1)	File: fmhpi_master_file.csv
			Desc: Freddie Mac Housing Price Index; see details below.
		Output:
		(1)	File: annual_fmhpi.csv
			Desc: Annual state-level housing price index (averaged monthly
			figures).

	2A_Merge_PSID-FIMS.R
		Input:
		(1)	File: psid_data_v1.csv
			Desc: see output for 1A_ImportAndClean_PSID.R
		(2)	File: fim12001_gid_BA_2_BAL_wide.xlsx
			Desc: Retrospective FIMS; see details below.
		(3)	File: annual_fmhpi.csv
			Desc: see output for 1C_ImportAndClean_FreddieMac.R
		(4)	File: annual_state_UR.csv
			Desc: see output for 1B_ImportAndClean_FRED.R
		Output:
		(1)	File: analysis_sample_5655_v1.csv
			Desc: Merged data set with generational variables and G1 individuals
			as cross-sectional units of analysis.

	3A_Alter_AnalysisSample.R
		Input:
		(1)	File: analysis_sample_5655_v1.csv
			Desc: see output for 2A_Merge_PSID-FIMS.R
		(2)	File: CPIAUCSL.csv
			Desc: CPI figures from FRED; see details below.
		Output:
		(2)	File: analysis_sample_5655_v2.csv 
			Desc: Analysis sample sans anomalies and with inflation adjustments.

*** details about data sources
	PSID:
	- 2019 User Guide:
		https://psidonline.isr.umich.edu/data/Documentation/UserGuide2019.pdf
	- FAQ on imputing Race:
		https://www.psc.isr.umich.edu/dis/data/kb/answer/1025.html
	- PSID main files:
		https://simba.isr.umich.edu/data/data.aspx
	- PSID state codes: 
		https://psidonline.isr.umich.edu/data/Documentation/PSIDStateCodes.pdf
	- FIMS: 
		https://simba.isr.umich.edu/FIMS/default.aspx
		- FIMS options used: Biological and adoptive, Individual to parents, 
		balanced (i.e., include individuals with parents only), Wide
	- FRED:
		- Seasonally-adjusted monthly state-level unemployment rates:
		https://fred.stlouisfed.org/release?rid=112
		- Annual seasonally adjusted inflation (annual percentage change in the CPI,
		aggregated as the average): 
		https://fred.stlouisfed.org/series/CPIAUCSL
		- Consumer Price Index for All Urban Consumers: All Items in U.S. City 
		Average, Seasonally Adjusted:
		https://fred.stlouisfed.org/series/CPIAUCSL
			- Base period: Jan 2011 = 100
			- Time: 2003-01-01 to 2011-12-31
		-	 Annual aggregation method: Average of monthly figures
	- Freddie Mac:
		- Housing price indices: 
		http://www.freddiemac.com/research/indices/house-price-index.page

*** Stata do files:
	- do_5655_linreg_notables_v2
	- do_5655_nonlinreg_notables_v2
