# Research Questions
What shapes trust in the European Union and United Nations in Poland, Germany and Slovenia? A comparative Analysis of political attitudes, ideology and socio-demographics?

# To Do:
*Dashboard*
- App Grundstruktur
- ⁠Layout überlegen für die einzelnen Pages
- CSS file
- Final Dataset festlegen -> filtered? oder lasso?

*Für die Models*
- Verteilung checken für Form in der Regression (e.g. Log, U-shape)
- Multicollinearity checken
- Group variables & group models
- Variance inflation factor / reduce models with robustness check
- Guide the reader with visuals: marginal effects plots, country-specific slopes
- Focus your interpretation on a few core interactions (e.g., trust in legal system, voting behavior, religiosity)
- Ask yourself: What are we learning about Poland, Germany, and Slovenia politically, socially, ideologically? Bring it back to meaning, not just numbers

*Für das Paper & das Dashboard*
- Betonung von Support for unification vs. Trust in Parliament in Ausarbeitung
- Focus interpretation on few core interactions
- Your conclusion is data-rich but a little dry or mechanical — your reader wants to know: What does this mean for EU legitimacy, political divides, or Eastern vs. Western Europe?
- Suggestions:
   - End with a few big-picture insights:
   - Why might government satisfaction matter more in Germany?
   - Why is religiosity politically reversed in Poland and Slovenia?
   - What do gendered patterns of trust suggest?
   - Raise future questions: how would things look post-2024 EU election? Is this stable?

# Datasets
1. Downloaded data named ESS_all
2. Data Preparation -> Save data set as ESS_prepared
3. Lasso -> save data set as ESS_lasso
4. descriptive statistics on ESS_lasso
5. Models on ESS_lasso
6. App on ESS_lasso
   
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

