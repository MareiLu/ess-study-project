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
- income wurde nicht inkludiert, da es zu viele NAs hatte
- Your conclusion is data-rich but a little dry or mechanical — your reader wants to know: What does this mean for EU legitimacy, political divides, or Eastern vs. Western Europe?
- Suggestions:
   - End with a few big-picture insights:
   - Why might government satisfaction matter more in Germany?
   - Why is religiosity politically reversed in Poland and Slovenia?
   - What do gendered patterns of trust suggest?
   - Raise future questions: how would things look post-2024 EU election? Is this stable?

# Chatty Models
1. Sinnvolle Interaktionen
Interaktionen prüfen, wenn du erwartest, dass der Effekt eines Prädiktors nicht für alle Gruppen gleich ist. Beispiele:

🧩 Ideen für Interaktionen:
Institutionelles Vertrauen × Land

r
Kopieren
Bearbeiten
ppltrst * cntry
stfgov * cntry
→ Fragt: Hat allgemeines oder Regierungstrust einen stärkeren Effekt in manchen Ländern?

Politische Orientierung × Parteiidentifikation

r
Kopieren
Bearbeiten
clsprty * euftf
→ Fragt: Haben Parteibindung und EU-Skepsis gemeinsam einen Effekt?

Religion × Land

r
Kopieren
Bearbeiten
rlgatnd * cntry
Geschlecht oder Altersgruppe × institutionelles Vertrauen

r
Kopieren
Bearbeiten
age_group * stfhlth
gndr * trstlgl
🔹 2. Modellspezifikation nach Gruppen
Gruppenmodelle sind sinnvoll, wenn sich die ganze Struktur der Zusammenhänge je nach Land oder Altersgruppe unterscheiden könnte.

Beispiel: ein Modell getrennt nach Land:

r
Kopieren
Bearbeiten
library(broom)
dt_filtered %>%
  group_by(cntry) %>%
  do(tidy(lm(trstep ~ ppltrst + euftf + stfgov + clsprty + age_group + gndr, data = .)))
Alternativ:

r
Kopieren
Bearbeiten
library(modelsummary)
models <- dt_filtered %>%
  group_split(cntry) %>%
  map(~ lm(trstep ~ ppltrst + euftf + stfgov + age_group + clsprty, data = .))

modelsummary(models, gof_omit = "IC|Log|Adj")
🔹 3. Transformationen prüfen
Am wichtigsten bei schiefen metrischen Prädiktoren (nicht bei Faktoren). Du könntest z. B.:

r
Kopieren
Bearbeiten
# Histogramm oder Dichte ansehen
ggplot(dt_filtered, aes(x = ppltrst)) + geom_histogram(bins = 30)

# Bei Schiefe: log-Transformation
dt_filtered <- dt_filtered %>%
  mutate(log_ppltrst = log(ppltrst + 1))
Bei ESS-Variablen wie ppltrst, trstlgl, etc. mit Skala 0–10 ist eine Transformation meist nicht nötig, weil die Verteilung halbwegs symmetrisch ist. Nur wenn extrem schief oder sehr viele Nullen auftreten, wäre es sinnvoll.

🔹 4. Weitere sinnvolle Diagnoseschritte
Multikollinearität prüfen

r
Kopieren
Bearbeiten
library(car)
vif(model1)
Residuenplot

r
Kopieren
Bearbeiten
plot(model1)
Vergleich von Modellen mit/ohne Interaktionen

r
Kopieren
Bearbeiten
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

