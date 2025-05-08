# Research Questions
A. How do levels of trust in the EU and UN vary across Western vs. Eastern European countries, and what role does immigration play in this?
B. Are younger people more politically competent because of greater media consumption, or despite lower political trust?
C. How do perceptions of political competence (internal efficacy) differ between younger and older people across democracies?

# Dashboard 
"About this Project", "General Insights/Descriptives", "General Findings", eventually "Implications/Predictions"
-> 10min per person presentation, so don't do it more complicated than can be presented in 20min
-> for the paper doesn't matter between 8-12 pages 

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

# Strategy 

## 3. How do perceptions of political competence (internal efficacy) differ between younger and older people across democracies?
  
  B. Digital divide question
  “Are younger people more politically competent because of greater media consumption, or despite lower political trust?”
  
  – Contrasts age groups on polcmpl controlling for polintr, trstplt

  C. Cross-country differences in the age gap
  “Is the age gap in political efficacy larger in countries with low political trust overall?”
  
  – Interaction: agea × cntry, with country-level mean trstplt as contextual variable

## Zeitplan für B
- research question is clear and testable:
> “Are younger people more politically competent because of greater media consumption, or despite lower political trust?”

- You're focusing on the variable **`polcmpl`** (*"Politics is too complicated to understand"*) as a measure of **perceived political competence**, and you'll control for:

- **`polintr`** – interest in politics  
- **`trstplt`** – trust in politicians  
- **age groups** – to analyze the digital divide  

**This Week (by 03.05): Data & Variables**

- [ ] **Select variables**:
  - `polcmpl` (DV – perceived political complexity)
  - `agea` (age) – turn into **age group** (`<30`, `30–59`, `60+`)
  - `polintr` (political interest)
  - `trstplt` (trust in politicians)
  - `cntry` (country)

- [ ] **Clean and prepare data**:
  - Recode `polcmpl` if needed (e.g., reverse code so higher = more competence)
  - Create **age group** variable
  - Handle missing values
  - Optionally center/scale continuous variables

- [ ] **Start R Markdown file**:
  - Title, authors, question, section headings (intro, data, descriptives, models)

 **Next Week (by 10.05): Descriptive Stats & Models**

- [ ] **Descriptive statistics**:
  - Mean `polcmpl` by age group
  - Plot `polcmpl` vs. age or age group
  - Correlation matrix: `polcmpl`, `polintr`, `trstplt`, age

- [ ] **Run models**:
  - **Model 1**: `polcmpl ~ age_group`
  - **Model 2**: `polcmpl ~ age_group + polintr + trstplt`
  - **Model 3 (optional)**: Add interaction `age_group * trstplt`  
    → See if trust matters more/less by age

- [ ] **Visualize key effects**:
  - Use `ggplot2` or `ggeffects` to show predicted values by age group
  - Simple coefficient plot (`sjPlot::plot_model()`)

 **Final Week (11–17.05): Interpretation & Polish**

- [ ] **Write up model results**
  - Explain what affects perceived competence
  - Highlight role of age, interest, and trust
  - Tie back to your digital divide framing

- [ ] **Polish R Markdown file**:
  - Add inline summaries, e.g., `r nrow(data)` observations
  - Clean figures and tables (add titles/labels)
  - Add final section: **"Are we on the right track?"** → reflection and next steps

Recommended Packages

```r
library(tidyverse)
library(lme4)         # If using multilevel modeling
library(ggeffects)    # For plotting marginal effects
library(kableExtra)   # Tables
library(sjPlot)       # Model plots
```



## Code für B

Here's a tailored **R Markdown template** for your research question:

> _“Are younger people more politically competent because of greater media consumption, or despite lower political trust?”_

This template focuses on the variable `polcmpl` (perceived complexity of politics), controlling for `polintr` (political interest) and `trstplt` (trust in politicians), across **age groups**.

**R Markdown Template (Copy into a `.Rmd` file)**

```markdown
---
title: "Digital Divide and Perceived Political Competence"
author: "Your Team Name"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(tidyverse)
library(ggeffects)
library(sjPlot)
library(kableExtra)
```

## Are younger people more politically competent because of greater media consumption, or despite lower political trust?

We explore this using the variable `polcmpl` (*"Politics is too complicated to understand"*) as a proxy for perceived political competence.

# Data Preparation

```{r load-and-clean}
# Load your dataset (replace this with real loading code)
# Example: ess <- read_sav("your_ess_file.sav")

# Simulate structure for testing
set.seed(42)
ess <- tibble(
  agea = sample(18:85, 1000, replace = TRUE),
  polcmpl = sample(0:4, 1000, replace = TRUE),   # 0 = not complicated, 4 = very complicated
  polintr = sample(0:3, 1000, replace = TRUE),   # 0 = not at all, 3 = very
  trstplt = sample(0:10, 1000, replace = TRUE),  # 0–10 trust scale
  cntry = sample(c("DE", "FR", "SE", "ES", "PL"), 1000, replace = TRUE)
)

# Recode age group
ess <- ess %>%
  mutate(age_group = case_when(
    agea < 30 ~ "<30",
    agea >= 30 & agea <= 59 ~ "30–59",
    agea >= 60 ~ "60+"
  )) %>%
  mutate(
    age_group = factor(age_group, levels = c("<30", "30–59", "60+")),
    polcmpl_rev = 4 - polcmpl  # higher = more competence
  )
```

# Descriptive Statistics

```{r descriptives}
ess %>%
  group_by(age_group) %>%
  summarise(
    mean_competence = round(mean(polcmpl_rev, na.rm = TRUE), 2),
    mean_interest = round(mean(polintr, na.rm = TRUE), 2),
    mean_trust = round(mean(trstplt, na.rm = TRUE), 2),
    n = n()
  ) %>%
  kbl() %>%
  kable_styling()
```

```{r plot-competence}
ggplot(ess, aes(x = age_group, y = polcmpl_rev)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "Perceived Political Competence by Age Group",
       y = "Perceived Competence (Higher = More)", x = "Age Group")
```

# Regression Models

**Model 1: Age Group Only**

```{r model1}
mod1 <- lm(polcmpl_rev ~ age_group, data = ess)
summary(mod1)
```

**Model 2: Add Political Interest and Trust**

```{r model2}
mod2 <- lm(polcmpl_rev ~ age_group + polintr + trstplt, data = ess)
summary(mod2)
```

**Model 3 (Optional): Interaction with Trust**

```{r model3}
mod3 <- lm(polcmpl_rev ~ age_group * trstplt + polintr, data = ess)
summary(mod3)
```

-research question focuses on **whether younger people are more politically competent because of their media consumption** --> it's essential to bring the media variables (`nwspol`, `netustm`, `netusoft`) directly into modeling
- **extending current models** to include media use

---

# **Model 4: Add Media Use as Predictors**

```r
mod4 <- lm(polcmpl_rev ~ age_group + polintr_rev + trstplt + nwspol + netustm + netusoft, data = dt_raw)
summary(mod4)
```

**Goal:** Test whether media consumption explains additional variance in perceived competence, beyond age and political trust/interest.

---

# **Model 5: Age × Media Interaction(s)**

```r
mod5 <- lm(polcmpl_rev ~ age_group * nwspol + polintr_rev + trstplt, data = dt_raw)
summary(mod5)
```

**Goal:** See if the effect of news consumption differs by age group. You could repeat this for `netustm` and `netusoft` individually or all at once:

```r
mod5b <- lm(polcmpl_rev ~ age_group * (nwspol + netustm + netusoft) + polintr_rev + trstplt, data = dt_raw)
summary(mod5b)
```

---

# **Model 6: Media × Trust Interaction**

```r
mod6 <- lm(polcmpl_rev ~ age_group + polintr_rev + trstplt * (nwspol + netustm + netusoft), data = dt_raw)
summary(mod6)
```

**Goal:** Explore whether the impact of trust on perceived competence depends on media usage patterns.

---

# **Model 7: Fully Saturated Model (All Interactions)**

Only do this if you have enough data and want to explore complex interactions:

```r
mod7 <- lm(polcmpl_rev ~ age_group * (nwspol + netustm + netusoft) +
                            trstplt * (nwspol + netustm + netusoft) +
                            polintr_rev, data = dt_raw)
summary(mod7)
```

---


# Visualizing Model Effects

```{r effects-plot}
plot_model(mod2, type = "eff", terms = c("age_group", "trstplt")) +
  ggtitle("Effect of Age Group and Trust on Perceived Political Competence")
```

```{r marginal-effects}
ggeffect(mod2, terms = "age_group") %>%
  plot() +
  labs(title = "Predicted Competence by Age Group", y = "Predicted Value")
```

# Interpretation

- Do younger people feel more or less politically competent?
- Does trust in politicians explain this?
- Does interest in politics moderate this relationship?

# Appendix

- Variable definitions:
  - `polcmpl`: Politics too complicated to understand
  - `polintr`: Interest in politics
  - `trstplt`: Trust in politicians
  - `agea`: Age (recoded into `age_group`)

- Notes on data:
  - Only countries X, Y, Z used
  - List any data exclusions

# Next Steps

- Replace the **simulated data** with your actual ESS dataset.
- Adjust variable names if needed (e.g., `polintr` may be `polintr` or similar in your version).
- Start filling in text sections (interpretation, variable definitions, etc.)

Would you like help writing interpretation text or adding additional plots (e.g. for political interest by age)?





## Zeitplan für C
-you’re looking at **cross-level interaction**: does the **individual-level relationship between age and political efficacy** vary **depending on country-level trust in politicians**? That’s a solid multilevel or contextual analysis structure.

**This Week (until ~03.05): Planning & Data Setup**
Goal: Prepare your data and structure your R Markdown file

1. Finalize your variables
   - Political efficacy (DV): e.g. *"can take active role"*, *"politics too complicated"*, or *"confident in ability to participate"*
   - Age: `agea`
   - Country: `cntry`
   - Trust in politicians: `trstplt`

2. Download and prepare ESS data
   - Select only the rounds and countries you’ll include
   - Filter to relevant variables
   - Create new variables:
     - Centered age (e.g. `age_centered`)
     - Country-level average trust: `mean_trstplt_country`
     - Possibly rescale trust and efficacy for interpretation

3. Set up initial R Markdown structure
   - Title, authors, date
   - Sections:
     - Introduction / Research question
     - Data & Variables
     - Descriptive stats
     - Models
     - Interpretation

**Next Week (04.05 – 10.05): Descriptives & First Models**
Goal: Run and interpret basic models to test your interaction idea

1. **Descriptive statistics**
   - Age distribution by country
   - Mean political efficacy by age group and country
   - Mean trust in politicians by country (create a plot)

2. **Visualize the research question**
   - Line plot: political efficacy vs. age by country
   - Scatter plot: age gap size vs. mean country trust

3. **Run basic regression models**
   - Linear regression: efficacy ~ age + country + (age × country)
   - Extract country slopes for age → visualize age gap

4. **Add contextual effect**
   - Add country-level trust as moderator:
     ```r
     lmer(efficacy ~ age_centered * mean_trstplt_country + (1 | cntry), data = ...)
     ```

**Final Week (11.05 – 17.05): Clean-up & Polish**
Goal: Polish your Rmd and interpret key results clearly

1. **Refine plots**
   - Use `ggplot2` for clean visuals
   - Label axes, use facet or color by country

2. **Write up interpretations**
   - Short paragraphs under each model output
   - Focus on: does the interaction support your hypothesis?

3. **Polish R Markdown**
   - Add inline code for summaries (e.g. mean age, number of countries)
   - Render to HTML/PDF
   - Check for reproducibility (clear chunk options, use `set.seed()`)

Suggested R Packages

```r
library(tidyverse)     # For data wrangling and plots
library(lme4)          # For multilevel modeling
library(broom.mixed)   # To tidy model output
library(ggeffects)     # For plotting interaction effects
library(kableExtra)    # For clean tables in Rmd
```



## Beispiel Code für C

code tailored to your project on **age, political efficacy, and country-level trust**. 

**R Markdown Template: Political Efficacy, Age, and Trust**

```markdown
---
title: "Political Efficacy, Age, and Country-Level Trust in Politicians"
author: "Your Name(s)"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(tidyverse)
library(lme4)
library(broom.mixed)
library(ggeffects)
library(kableExtra)
```

**Research Question**

Do older and younger people feel differently about their ability to participate in politics, and is this age gap influenced by how much people in their country trust politicians?

**Data Preparation**

```{r load-data}
# Example: Replace with your actual data loading method
# library(haven)
# ess <- read_sav("ESS_data.sav")

# For now, let's simulate a small example dataset
set.seed(123)
ess <- tibble(
  cntry = rep(c("DE", "PL", "SE", "ES", "FR"), each = 200),
  agea = sample(18:85, 1000, replace = TRUE),
  trstplt = round(runif(1000, 0, 10)),
  efficacy = rnorm(1000, mean = 5, sd = 2)  # Replace with real variable, e.g. "cptppol"
)
```

**Clean and Prepare Variables**

```{r prepare-vars}
# Center age
ess <- ess %>%
  mutate(age_c = agea - mean(agea, na.rm = TRUE))

# Create country-level mean trust
country_trust <- ess %>%
  group_by(cntry) %>%
  summarise(mean_trust = mean(trstplt, na.rm = TRUE))

# Join back to dataset
ess <- ess %>%
  left_join(country_trust, by = "cntry")
```

**Descriptive Statistics**

```{r descriptives}
ess %>%
  group_by(cntry) %>%
  summarise(
    Mean_Age = round(mean(agea), 1),
    Mean_Trust = round(mean(trstplt), 1),
    Mean_Efficacy = round(mean(efficacy), 2)
  ) %>%
  kbl() %>%
  kable_styling()
```

**Visual Exploration**

```{r plot-efficacy-by-age}
ggplot(ess, aes(x = agea, y = efficacy)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~cntry) +
  labs(title = "Political Efficacy by Age Across Countries",
       x = "Age", y = "Efficacy Score")
```

**Modeling**

**Basic Linear Model with Interaction**

```{r model-basic}
mod1 <- lm(efficacy ~ age_c * mean_trust, data = ess)
summary(mod1)
```

**Multilevel Model with Random Intercepts**

```{r model-mixed}
mod2 <- lmer(efficacy ~ age_c * mean_trust + (1 | cntry), data = ess)
summary(mod2)
```

#**Plotting Marginal Effects**

```{r plot-interaction}
ggeffect(mod2, terms = c("age_c", "mean_trust")) %>%
  plot() +
  labs(title = "Interaction: Age × Country-Level Trust",
       x = "Age (Centered)", y = "Predicted Political Efficacy")
```

**Interpretation**

- **Main effect of age**: Does political efficacy increase or decrease with age?
- **Interaction**: Is the age effect stronger/weaker in high-trust countries?

_Interpret results briefly in 2–3 bullet points._

**Appendix**

- Variable definitions
- Notes on missing data
- References (ESS documentation, literature)


This gives you a clean base that:
- Loads your data
- Prepares variables
- Runs both simple and multilevel models
- Visualizes the interaction

