# Research Questions
What shapes trust in the European Union and United Nations in Poland, Germany and Slovenia? A comparative Analysis of political attitudes, ideology and socio-demographics over time.

# To Do:
*Ändern im Dashboard*
- "On an aggregate level, all countries show a general downward trend with the youngest people >18 having the highest trust in the EU" muss "<18"
- "Support for further EU unification is broadly positive: Respondents in all three countries generally want the EU to move forward, with Poland showing the strongest support. After 2014, all countries followed a similar upward trend, though support declined again slightly after 2020." muss "with Poland showing the strongest support until 2014"
- "Immigration attitudes reveal a surprising pattern: Polish respondents appear the most welcoming toward immigrants across age groups, despite their more conservative political orientation. Slovenian respondents are the least welcoming." muss "Polish respondents appear the most welcoming toward immigrants, despite their..." - ich würde das "across age groups" löschen, weil wir hier ja nur auf den Zeittrend schauen und nicht auf die Altersgruppen
- genauso hier: "Political interest is highest in Germany: Respondents in Germany show consistently higher political interest across all age groups. Interest in Poland is slightly higher than Slovenia, particularly in younger and older age groups." muss zu "Respondents in Germany show consistently higher political interest." und der zweite Satz zu: "Interest in Poland and Slovenia is on the same level, with only small fluctuations. However, a sharp rise in Poland interrupts this pattern."
- Reihenfolge im "Cross-country comparison" tab muss geändert werden. Erst correlation matrix, dann die heat map mit den drei Ländern models, dann die key interaction effects
- Im Implications tab: "What can Politicians Differ from this Analysis?" muss "infer"

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
