# Research Questions
What shapes trust in the European Union and United Nations in Poland, Germany and Slovenia? A comparative Analysis of political attitudes, ideology and socio-demographics?

# Datasets
1. Downloaded data named ESS_all
2. Data Preparation -> Save data set as ESS_prepared
3. Lasso -> save data set as ESS_lasso
4. descriptive statistics on ESS_lasso
5. Models on ESS_lasso
   
# Dashboard 
"About this Project", "General Insights/Descriptives", "General Findings", eventually "Implications/Predictions"
-> 10min per person presentation, so don't do it more complicated than can be presented in 20min
-> for the paper doesn't matter between 8-12 pages 

-> maybe separate the "General Insights/Descriptives" into the groups mentioned in the research question: political attitudes, ideology, and socio-demographics (kann man so Untertabs erstellen?)

# Notes
- UN & EU? Or just EU? -> highly correlated but not perfectly, rather focus on EU
- What drives attitude towards EU? -> lasso regression leaves us with 10 most important; only include ones with not too high correlation; then look at how does this diverge by age group; maybe not trust in politics & in UN; macroeconomic, socio-economic, internet usage, satisfaction/happiness, (trust & satisfaction in government only if we can derive something from it)
- dividing by age makes sense? yes (if different results) "Difference in driving factors in age groups"
- education years: binsurize 5%-99% quantile so put value outside the quantile into it (not loosing observation) or remove if not many
- if 1-10 generally no log if evently distributed, if we see pattern in simple graph, possibly use others
- for numeric variables (income) log for normal distr. in regressions
- for age incl. age_squared because increasing then decreasing
- in dashboard explanation why diff. countries + little bit of macro explanation -> what distinguishes the countries the most?

# Important variables

### Political competence:
- **actrolga** – Able to take active role in political group
- **cptppola** – Confident in own ability to participate in politics
- **vote** - Voted last national election -> not sure but could be good

### Media consumption:
- **nwspol** – Time spent on news about politics and current affairs
- **netustm** – Time spent on internet per day
- **netusoft** – Frequency of internet use

### Political trust:
- **trstplt** – Trust in politicians
- **trstprl** – Trust in country's parliament
- **trstprt** – Trust in political parties

### Control Variables:
- **agea** – Age of respondent
- **cntry** – Country
- **anweight**, **dweight**, **pspwght** – weighting variables

All variables are correct and have data. 

# Country Color Code
- Germany Blue #33658a
- Poland Light Orange #f6ae2d
- Slowenia Pink Dark Orande #f26419

# 1. Descriptives & Visualization / General Insights
- trust plots by country (flowchart, all rounds, all countries)
- plots each confounder variable (over time, all countries)
-   evtl. satisfaction variables in extra plots by country
- age groups for demographics
-   evtl. later country descriptives by data (income, joblevel, ...)

# 2. Analysis
- correlation matrix
- models
![image](https://github.com/user-attachments/assets/c71f68cf-0dd3-43c3-a218-02c36ffb0f7a)

- LASSO Model FIt

# 3. About this Project
Text about main findings & why these countries

# 4. Implications/Prediction

# Questions
- Do we need weights?
