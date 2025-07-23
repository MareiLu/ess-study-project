# Research Questions
What shapes trust in the European Union and United Nations in Poland, Germany and Slovenia? A comparative Analysis of political attitudes, ideology and socio-demographics over time.

# To Do:
*Ändern im Dashboard*

- Im Implications tab: "Iniciatives need to be specifically tailored to the country." --> Initiatives
- Text leicht abgeändert: What Role do Macroeconomic Factors and Connected Claims to Sovereignty Play?
It seems that the initial country selection by net recipient or payer status appears to be relevant, which is especially noticeable in the Polish "either-or" effect between the national and the EU level. We suggest that the intangible effects we see in this analysis are partly dependent on the economic status of a country within the EU. Further research should be done on the indicated substitution relationship between trust in national and international institutions and what this might imply about the interplay between EU integration and national claims to sovereignty.
- Alternative Formulierung für "What are Further Open and Future-Oriented Questions" --> "What Are Implications for Policy and Future Research?"
- bei Implications oben im roten Kasten: "Most importantly, this analysis has shown that efforts to foster EU trust must be tailored to specific national contexts and institutional landscapes and have the most effect when tackling lacking trust in the national legal system and general public trust." --> "Most importantly, this analysis has shown that efforts to foster EU trust must be tailored to specific national contexts and institutional landscapes and have the most effect when tackling lacking public trust in the national legal system." weil wir weder in den Results noch in den Implications groß auf Public trust eingehen macht das so mehr sinn
- wir müssen beim Interaction model anweight mitnutzen. Deswegen am besten neuen Model code einfügen und damit die Graphen erstellen. Am besten telefonieren wir gleich einmal



*Notizen zu Dashboard*

Since it is not possible to come to a clear and inter-country-wide prediction of Trust in the EU, we estimate that there must be other underlying factors involved in shaping this trust.

- App; descriptives bzw. data wrangling as "additional code"; Dataset
- in Report figures & tables if necessary in the appendix
- references for context
- Introduction about motivation & stating of the idea
- What, Why to What are the Recommendations
- Highlight key facts

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

# Dashboard 
-> 10min per person presentation, so don't do it more complicated than can be presented in 20min

# Report
- between 8-12 pages 
- Report ist wie eine Art Anleitung für das Dashboard
- Wenn wir interessante Patterns haben (z.B. für Polen), dann können wir die auch erklären und auch in der Präsi mit einbeziehen
- am Ende Outlooks/Implikationen geben
- Explain plots broadly (mean, median, how doing predictions etc.; not necessary to including all models etc. or go into every graph)
- Key elements of the dashboard & key learnings to derive decisions

# Nicht mehr wichtig fürs Dashboard:
*Für das Paper & das Dashboard*
- Betonung von Support for unification vs. Trust in Parliament in Ausarbeitung
- Wir haben zu Lasso (statistisch wichtige Variablen) auch noch thematisch wichtige Variablen hinzugefügt
- income wurde nicht inkludiert, da es zu viele NAs hatte
- Age hat quadratischen effekt, statistically significant
- fixed effects model: erklären &  R2 oder MSE zur Rechtfertigung
- Your conclusion is data-rich but a little dry or mechanical — your reader wants to know: What does this mean for EU legitimacy, political divides, or Eastern vs. Western Europe?
