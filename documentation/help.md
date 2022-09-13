# Tumor Volume Analysis Suite

<p align="center">
<img src="images/0.splash_page.png" alt="drawing" width="400"/>
</p>  

From the splash page, or via the top menu users can access "Data Upload and Validation" or "Tumor Volume Analysis." 

## Data Upload and Validation

<p align="center">
<img src="images/1.data_upload.png" alt="drawing" width="400"/>
</p>

On this tab, you can upload your tumor volume data by clicking the 'browse' button and then selecting your file. Files are expected in either CSV or EXCEL format. Other formats are not accepted.  

Files uploaded to this tool must also contain expected columns and data structure as described in the following section: 

---

### Data Format and Structure

The following columns with the **exact names** and **data types** are expected in the dataset uploaded to the tool. 

    1. 'Contributor':  

        Attributes studies to the contributing investigator or institution (e.g., PDTC_1, PDTC_2).  

    2. 'Arms':

        Treatment arms description (e.g., Carboplatin, Control).  
        NOTE: The dataset MUST contain a control arm, and the control arm MUST be named 'Control'. 
              An error will be thrown on upload if the text 'Control' is missing from the treatment arm column.   

    3. 'Times':  

        Numeric study day (e.g., 0, 4, 8, 14, 21).
        NOTE: Numeric or integer values are expected. Calendar dates (e.g., 06.02.2022) are not valid.   

    4. 'Volume':  
    
        Tumor volume in mm<sup>3</sup> (e.g. 200, 140, 23.4). 
        NOTE: Negative values are accepted, but may result in improper plotting or calculations. 
        A warning message from the validator will report the number of values with negative volumes. 

    5. 'Study':  
    
        Study ID corresponding to that particular sample (e.g., Study_1, Study_2).  

    6. 'ID':  
    
        Unique identifier for each individual animal on study (e.g., Mouse_1, Mouse_2).
        NOTE: Animal IDs MUST be unique across studies. 

    7. 'Tumor':  
    
        PDX model ID (e.g., PDX_1, PDX_2).   

    8. 'Disease_Type':  
    
        A descriptor of the tumor type (e.g., Invasive breast carcinoma, Unknown, NA). 
        NOTE: This is a free text field, and is only used in the data filter query.


### Upload Validation Message: 

Once you file is selected, a validator will run during the upload to ensure that all fields are present, and that each field contains the expected data type. 

If data are correctly formatted, a message 'Successfully Validated Your Tumor Volume Data!' will show that you successfully imported and loaded your data to the 'Tumor Volume Analysis' tab.  

If data are improperly formatted, the validator will list errors associated with the upload. Correct the errors in your local file, and retry the upload. 

### Example Expected Tumor Volume File Formatting

An example data table is provided on this page to show users the expected data structure and formatting. This table can be download for your reference. 

## Tumor Volume Analysis

The Tumor Volume Analysis page contains all plot and analysis functions.  

<p align="center">
<img src="images/2.tv_landing_page.png" alt="" width="400"/>
</p>  

By default an example dataset is loaded. To add your own data, see the **Data Upload and Validation** section of this document. 

---

> **NOTE:** For many plots and metrics, a user defined time point is required for the calculation (e.g., RECIST objective response classification). In these cases, measures are calculated based on data sampled from the last available time point for each individual (rounded down). 
>
> **Example:**   
> Tumors for a study are measured at time points 0, 7, 10, 21, 29.   
> 1. If a user selects to calculate RECIST classification at time 21; data from time point 21 will be used.   
> 2. If a user selects to calculate RECIST at time 26; data at time 21 will also be used.
> 
> 3. If data are collected in an asynchronous fashion, (e.g., mouse_1: 0, 7, 10; mouse_2: 0, 6, 9, etc.), the last available time point (rounded down) for each individual animal is used.
> 
> In all cases where a user must input a day for calculation, data underlying the metric or plot can be interpolated as described below. Using interpolation overcomes asynchronous and missing data issues. Note that the use of interpolation should be reported in final results.
> 
> Ultimately, it is up to the user to know their data and select time points for metric calculation that are relevant to the data. 

---

### Interpolation 

In all cases where a user must input a day for calculation (plots and metrics), data underlying the metrics can be interpolated using the 'Interpolate Data' or 'Interpolate Data for Calculation' checkboxes. 

When these boxes are checked, a linear interpolation method is used to adjusts for animals where there is no tumor volume measurement at time *t*, but which have flanking volume measurements at time *t*<sub>0</sub> and *t*<sub>1</sub> such that *t*<sub>0</sub> < *t* < *t*<sub>1</sub>.



### Query and Data Summary

#### Data Selection and Filtering


<p align="center">
<img src="images/3.tv_query.png" alt="" width="700"/>
</p>

This section contains data query filters, which allows the user to select subsets of the loaded data set. For example, select which treatment arms you wish to visualize. 

**NOTE:** When selecting treatment arms, 'Control' **MUST** be selected, or plots and analysis metrics will not be generated. Likewise, if no data is selected, the tool will not function. 

--- 

#### Selected Volume Data - Summary

<p align="center">
<img src="images/4.tv_data_summary.png" alt="" width="700"/>
</p>

This section summarized the selected data. The number of unique mouse IDs, treatment arms, disease types, models, studies, and contributors are shown. 


## Tumor Volume Analysis Pages

Below the data query and summary are three sub-tabs that allow users to access different plots and the filtered data table. 

1. Cross Study Plots and Analysis
2. Individual Study Plots & Analysis
3. Current Data Table

<p align="center">
<img src="images/5.tv_main_subtabs.png" alt="" width="700"/>
</p>

---

### 1. Cross Study Plots and Analysis

There are six tabs that provide access to different visualizations and analysis metrics for cross study comparisons. 

**NOTE:** Data filtering via 'Data Selection and Filtering' dropdowns can be used to select or deselect studies as needed.

<p align="center">
<img src="images/6.tv_cross_study_options.png" alt="" width="700"/>  
</p>

**Individual plots and options are described below.**

---

#### 1a. Response Plot

<p align="center">
<img src="images/7.tv_resp_plot_default.png" alt="" width="600"/>
</p>  

A common way to display tumor growth on treatment and control arms are tumor growth curves, which typically displays tumor volumes calculated by the following formula:  

<p align="center">
<img src="images/a.eq1.gif" alt="$$\frac{\left ( \textbf{Tumor Length} \times \textbf{Tumor Width}^{2} \right )}{2}$$" width="250"/>
</p>


Tumor growth curves in this panel are by default as the 'Study Average' of tumor volume with standard errors. 

* Plots are arranged by 'Study' (default) or 'Treatment' using the 'Plot Facet Type' dropdown.  

* Plots can also be shown by 'Study Average' (default) or 'Individual Animal' using the 'Plot Style' dropdown. 

* Data for all plot combinations / selection can be interpolated with 'Interpolate Data'

* There are four options for 'Plot Type' with that dropdown. The options are:   

---

A. Volume (default)

<p align="center">
<img src="images/8.tv_resp_volume.png" alt="" width="400"/>
</p>

This plot displays tumor volume as provided by the user in 'volume' column of the loaded/filtered dataset. It is assumed that this is tumor volume in mm<sup>3</sup>.

---

B. Scaled  
<p align="center">
<img src="images/9.tv_scaled_plot.png" alt="" width="400"/>
</p>  

This plot re-scales volume data from -100 to 100. Where -100 represents total tumor regression, 0 is neither growth nor regression, and 100 represents when growth has reached a pre-defined end point. 

Users are able to define end endpoint as either a growth factor (e.g., 4x) or volume (e.g., 1200 mm<sup>3</sup>). When 'Scaled' is selected, the 'Scale Plot By' and 'Endpoint Scaling' options will appear. 

<p align="center">
<img src="images/10.tv_scaled_plot_options.png" alt="" width="200"/>
</p> 

---
  
C. Percent Change  

<p align="center">
<img src="images/11.tv_percChange_plot.png" alt="" width="450"/>
</p>  

This plot shows an alternate way to demonstrate growth curves using percent change in tumor volume. Percent change is defined for each individual within study and treatment arm. It is calculated as follows: 

<p align="center">
<img src="images/a.eq2.gif" alt="" width="175"/>
</p>


---

D. Semi-Log  


<p align="center">
<img src="images/12.tv_semi_log_plot.png" alt="" width="400"/>
</p> 


This plot show the natural logarithm transformation of volume measures on the y-axis, and study day along the x-axis.

---

#### 1b. Average Volume Plot

<p align="center">
<img src="images/13.tv_avgStudy_plot.png" alt="" width="400"/>
</p> 

This plot shows cross study average volume across all animals for treatment arms +/- SE at a user defined time point (default "Avg Measure Calc. Day" is 21). Data underlying this plot can be interpolated with the checkbox.  


#### 1c. Log2 Fold Change Plot

<p align="center">
<img src="images/14.tv_log2fold_plot.png" alt="" width="400"/>
</p> 

This plot shows cross study mean log2 fold change [log2(Volume<sub>t</sub> / Volume<sub>t</sub>)] across all animals for treatment arms +/- SE across all measurement time points. Data underlying this plot can be interpolated with the checkbox.  

---

#### 1d. Hybrid Waterfall Plot

<p align="center">
<img src="images/15.tv_hybridWaterfall_plot.png" alt="" width="400"/>
</p> 

This plot shows cross study hybrid waterfall plot. Where model by treatment arm shows progression, T / C ratio +/- SE  is displayed in the plot. T/C is defined within study, tumor, and treatment arm roughly as:  

<p align="center">
<img src="images/a.eq3.gif" alt="" width="175"/>
</p>

For the specific formulation, see the 'Tumor Growth Inhibition' section that follows. 

Where model by treatment arm shows regression, percent change in tumor volume (as defined above) +/- SE is shown. The calculation of these metrics is taken at the user defined time point (default "Waterfall Calc. Day" is 21). Data underlying this plot can be interpolated with the checkbox.  

---

#### 1e. Tumor Growth Inhibition

<p align="center">
<img src="images/16.tv_TGI_plot.png" alt="" width="400"/>
</p> 

This plot and table shows cross study tumor growth inhibition defined as the T/C ratio. 

To measure antitumor activity of the treatment group compared to the control group, we considered the tumor growth treatment-to-control ratio (gamma<sub>t</sub>) estimated by one way ANOVA focused on time *t* as described in Supplemental Materials 1 in (Evrard et al. 2020). 
 
Briefly, after log transforming the change in tumor volume (as defined above), a linear model is fit:

<p align="center">
<img src="images/a.eq4.gif" alt="" width="175"/>
</p>

where *T<sub>i</sub>* is the treatment arm for each individual. 

If we define TGI as the ratio of mean tumor to mean control as in the equation shown in section 1c. above, it can be shown that:

<p align="center">
<img src="images/a.eq5.gif" alt="" width="175"/>
</p>

Where TGI < 1 indicates the degree of growth inhibition of the treatment relative to control. To test for the significance of anti-tumor activity of the treatment group at time *t*, we test the null hypothesis of no treatment effect by comparing

H_0:β=0 (γ=1) vs. H_1: β<0 (γ<1).

    Evrard YA, Srivastava A, Randjelovic J, Doroshow JH, Dean DA 2nd, Morris JS, Chuang JH; NCI PDXNet Consortium. Systematic Establishment of Robustness and Standards in Patient-Derived Xenograft Experiments and Analysis. Cancer Res. 2020 Jun 1;80(11):2286-2297. doi: 10.1158/0008-5472.CAN-19-3101. Epub 2020 Mar 9. PMID: 32152150; PMCID: PMC7272270.



---

#### 1f. Stacked Objective Response Plot

---









### 2. Individual Study Plots & Analysis

There is a study selection dropdown, and four tabs that provide access to different visualizations and analysis metrics for analysis of the selected individual study. 

<p align="center">
<img src="images/999.tv_individual_study_options.png" alt="" width="700"/>
</p>

