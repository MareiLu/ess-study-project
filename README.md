# Research Questions
What shapes trust in the European Union and United Nations in Poland, Germany and Slovenia? A comparative Analysis of political attitudes, ideology and socio-demographics over time.

# To Do:
*Notizen zu Dashboard*

Since it is not possible to come to a clear and inter-country-wide prediction of Trust in the EU, we estimate that there must be other underlying factors involved in shaping this trust.

*Dashboard*
  
*Für die Models*
- Verteilung checken für Form in der Regression (e.g. Log, U-shape)
- Multicollinearity checken
- Group variables & group models
- Variance inflation factor / reduce models with robustness check
- Guide the reader with visuals: marginal effects plots, country-specific slopes
- Focus your interpretation on a few core interactions (e.g., trust in legal system, voting behavior, religiosity)
- Ask yourself: What are we learning about Poland, Germany, and Slovenia politically, socially, ideologically? Bring it back to meaning, not just numbers
- **regression ohne intercept laufen lassen**
- welches Jahr, welches Land Baseline: Jahr ist continuous, Land DE

*Für das Paper & das Dashboard*
- Betonung von Support for unification vs. Trust in Parliament in Ausarbeitung
- Wir haben zu Lasso (statistisch wichtige Variablen) auch noch thematisch wichtige Variablen hinzugefügt
- income wurde nicht inkludiert, da es zu viele NAs hatte
- Age hat quadratischen effekt, statistically significant
- fixed effects model: erklären &  R2 oder MSE zur Rechtfertigung
- Your conclusion is data-rich but a little dry or mechanical — your reader wants to know: What does this mean for EU legitimacy, political divides, or Eastern vs. Western Europe?
- Suggestions:
   - End with a few big-picture insights:
   - Why might government satisfaction matter more in Germany?
   - Why is religiosity politically reversed in Poland and Slovenia?
   - What do gendered patterns of trust suggest?
   - Raise future questions: how would things look post-2024 EU election? Is this stable?

 *Fragen*
 
- Should we have a code for the assumptions?
   - Homoscedasticity könnte getestet werden, für unseren Kontext nicht so wichtig --> Ordinary Least Squares (OLS) estimates of the coefficients are still unbiased and consistent — they don't systematically over- or underestimate the true effect
   - Robust standard errors are easy to apply --> You can use robust (heteroskedasticity-consistent) standard errors to correct for any distortions in standard errors without having to test for heteroskedasticity explicitly
 
- Remarks: **Story ist wichtig**
   - die Story die wir erzöhlen wollen ist wichtig, Unterschiede zwischen ländern herausheben, Hauptunterschiede, was sorgt für die Unterschiede, Wo könnte man darauf ansetzen in den jeweiligen Ländern

# Variablen
*Socio-Demograohics*
    - Distribution of Gender
    - Distribution of Age Group
    - Education Years (unterer graph, bar plot)
  - *Political Attitudes*
    - Distribution of voting behavior
    - trust in politicians (rausgenommen)
    - Satisfaction with National government
    - Satisfaction with State of education
    - Satisfaction with State of Health
    - Trust in legal system
    - Unification Progress Satisfaction
  - *Ideology*
    - Attitudes towards Immigraants
    - Trust in People
    - Attendance of religious services
    - Political Interest

   
# Dashboard 
"About this Project", "General Insights/Descriptives", "General Findings", eventually "Implications/Predictions"
-> 10min per person presentation, so don't do it more complicated than can be presented in 20min
-> for the paper doesn't matter between 8-12 pages 

-> maybe separate the "General Insights/Descriptives" into the groups mentioned in the research question: political attitudes, ideology, and socio-demographics (kann man so Untertabs erstellen?)

# Report
- Report ist wie eine Art Anleitung für das Dashboard
- Wenn wir interessante Patterns haben (z.B. für Polen), dann können wir die auch erklären und auch in der Präsi mit einbeziehen
- am Ende Outlooks/Implikationen geben
