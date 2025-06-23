# Research Questions
What shapes trust in the European Union and United Nations in Poland, Germany and Slovenia? A comparative Analysis of political attitudes, ideology and socio-demographics.

# To Do:
*Dashboard*
- App Grundstruktur
- ⁠Layout überlegen für die einzelnen Pages
- CSS file
- Final Dataset festlegen -> filtered? oder lasso?
- Descriptive Plots mit richtiger Codierung/ Scale versehen

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
- income wurde nicht inkludiert, da es zu viele NAs hatte
- Your conclusion is data-rich but a little dry or mechanical — your reader wants to know: What does this mean for EU legitimacy, political divides, or Eastern vs. Western Europe?
- Suggestions:
   - End with a few big-picture insights:
   - Why might government satisfaction matter more in Germany?
   - Why is religiosity politically reversed in Poland and Slovenia?
   - What do gendered patterns of trust suggest?
   - Raise future questions: how would things look post-2024 EU election? Is this stable?

 *Fragen*
- Do we only work with the variables from LASSO or can we use some other variables for our visualizations to get some insights from them, such as Left-Right-Scale
   - überlegen pb es in der Sotry Sinn macht, bringt es was, das zu sehen für die Frage
   - könnten sonst auch LASSO Model anpassen mit zusätzlicher Variable
   - Begründung: wir denken sie ist wichtig, LASSO gibt nur die statistische Wichtigkeit, keine thematische

- Should we test different models?
   - R2 oder MSE zur Rechtfertigung
   - fixed effects model: erklären
   - regression ohne intercept laufen lassen
   - welches Jahr, welches Land Baseline: Jahr ist continuous, Land DE
   - wir wollen keine rediction, sondern eher Causal Effect
   - LASSO Model und ein Model mit usnerern Variablen und dann schauen was EInfluss hat n ddas genauer interpretieren
   - Age könnte quadratischen Effekt, den man mit reinnehmen könnte, reinnehmen und Signifikanz testen: Age hat quadratischen effekt, statistically significant
 
- Should we have a code for the assumptions?
   - Homoscedasticity könnte getestet werden, für unseren Kontext nicht so wichtig
   - Q-Q Plots wenn man denkt dass eine Variable logarithmisch skaliert ist
   - wir könnten Histogram machen, wo man für die einzelnen Variablen auswählen kann ob sie normalverteilt sind oder starke Ausreißer haben
 
- Remarks: Story ist wichtig
   - die Story die wir erzöhlen wollen ist wichtig, Unterschiede zwischen ländern herausheben, Hauptunterschiede, was sorgt für die Unterschiede, Wo könnte man darauf ansetzen in den jeweiligen Ländern

- Visualisierungen
   - Sachen im Zeitverlauf anschauen

- Report
   - Report ist wie eine Art Anleitung für das Dashboard
   - Wenn wir interessante Patterns haben (z.B. für Polen), dann können wir die auch erklären und auch in der Präsi mit einbeziehen
   - am Ende Outlooks/Implikationen geben

# Chatty Models
1. Sinnvolle Interaktionen
Interaktionen prüfen, wenn du erwartest, dass der Effekt eines Prädiktors nicht für alle Gruppen gleich ist. Beispiele:

2. Modellspezifikation nach Gruppen
Gruppenmodelle sind sinnvoll, wenn sich die ganze Struktur der Zusammenhänge je nach Land oder Altersgruppe unterscheiden könnte.

Beispiel: ein Modell getrennt nach Land:
library(broom)
dt_filtered %>%
  group_by(cntry) %>%
  do(tidy(lm(trstep ~ ppltrst + euftf + stfgov + clsprty + age_group + gndr, data = .)))

Alternativ:
library(modelsummary)
models <- dt_filtered %>%
  group_split(cntry) %>%
  map(~ lm(trstep ~ ppltrst + euftf + stfgov + age_group + clsprty, data = .))

modelsummary(models, gof_omit = "IC|Log|Adj")

🔹 3. Transformationen prüfen
Am wichtigsten bei schiefen metrischen Prädiktoren (nicht bei Faktoren). Du könntest z. B.:

# Histogramm oder Dichte ansehen
ggplot(dt_filtered, aes(x = ppltrst)) + geom_histogram(bins = 30)

# Bei Schiefe:
log-Transformation
dt_filtered <- dt_filtered %>%
  mutate(log_ppltrst = log(ppltrst + 1))
Bei ESS-Variablen wie ppltrst, trstlgl, etc. mit Skala 0–10 ist eine Transformation meist nicht nötig, weil die Verteilung halbwegs symmetrisch ist. Nur wenn extrem schief oder sehr viele Nullen auftreten, wäre es sinnvoll.

🔹 4. Weitere sinnvolle Diagnoseschritte
Multikollinearität prüfen
library(car)
vif(model1)

Residuenplot
plot(model1)

Vergleich von Modellen mit/ohne Interaktionen
model2 <- update(model1, . ~ . + ppltrst*cntry)
anova(model1, model2)

🔹 Empfehlung: So könntet ihr jetzt weitermachen
Modelldiagnose & Interpretation von model1

Gezielte Interaktionen auf Basis eurer Forschungsfrage:
z. B. "Hat allgemeines Vertrauen in Institutionen einen anderen Effekt auf EU-Vertrauen je nach Land?"

Modelle nach Ländern und/oder Altersgruppen getrennt schätzen

Optional: Transformationen, falls bei Einzelvariablen nötig

Visuelle Darstellung von Interaktionen (ggplot2: geom_smooth() oder interactions-Paket)

# Datasets
1. Downloaded data named ESS_all
2. Data Preparation -> Save data set as ESS_prepared
3. Lasso -> save data set as ESS_lasso
4. descriptive statistics on ESS_lasso
5. Models on ESS_lasso
6. App on ESS_lasso

# Variable Groups
**political attitudes**
-  **clsprty** (Feel closer to a particular party than all others)
-  **euftf** (European Union: European unification go further or gone too far)
-  **stfgov** (How satisfied with the national government)
-  **trstlgl** (Trust in the legal system)

**ideology** 
- **ppltrst** (Most people can be trusted or you can't be too careful)
- **freehms** (Gays and lesbians free to live as they wish)
- **imwbcnt** (Immigrants make country worse or better place to live)
- **rlgatnd** (How often attend religious services apart from special occations)
  
**socio-demographics**
- **stfedu** (state of education in country nowadays)
- **stfhlth** (State of health services in country nowadays)
- **gndr** (Gender)
- **age_group** (Age group)

**Others**
- **cntry** (Country)
- **year** (Ess round)
   
# Dashboard 
"About this Project", "General Insights/Descriptives", "General Findings", eventually "Implications/Predictions"
-> 10min per person presentation, so don't do it more complicated than can be presented in 20min
-> for the paper doesn't matter between 8-12 pages 

-> maybe separate the "General Insights/Descriptives" into the groups mentioned in the research question: political attitudes, ideology, and socio-demographics (kann man so Untertabs erstellen?)

# Notes
- dividing by age makes sense? yes (if different results) "Difference in driving factors in age groups"
- if 1-10 generally no log if evently distributed, if we see pattern in simple graph, possibly use others
- for numeric variables (income) log for normal distr. in regressions
- for age incl. age_squared because increasing then decreasing
- in dashboard explanation why diff. countries + little bit of macro explanation -> what distinguishes the countries the most?

