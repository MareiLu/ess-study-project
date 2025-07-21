
# ----------- LOAD PACKAGES -----------

# install.packages("shiny")
library(shiny)
library(haven)
library(here)
library(tidyverse)
library(broom)
library(forcats)
library(interactions)
library(shinyWidgets)
library(ggcorrplot)
library(rlang)
library(skimr)       
library(psych)       
library(labelled)
library(dplyr)
library(tidyr)
library(gtools)


# ----------- LOAD DATA -----------

dt_filtered <- read_sav(here("data", "ESS_filtered.sav")) 

# ----------- GLOBAL VARIABLES -----------

# Colors and Labels for Countries 
trust_colors <- c("DE"="#003399", "PL"="#FFCC00", "SI"="#FF7300")
trust_labels <- c("DE"="Germany", "PL"="Poland", "SI"="Slovenia")

# Defining the Categories of Variables
category_vars <- list(
  "Socio-Demographics" = c("age_group", "gndr", "eduyrs_winsor"),
  "Political Attitudes" = c("stfgov", "stfedu", "stfhlth", "trstlgl", "euftf"),
  "Ideology" = c("imwbcnt", "ppltrst", "rlgatnd_rev", "polintr_rev", "lrscale")
)

# Defining the Labels attached to the variables
var_labels <- c(
  trstep        = "Trust in the EU",
  euftf         = "Attitude towards EU Unification",
  stfedu        = "Satisfaction with State of Education",
  stfgov        = "Satisfaction with National Government",
  stfhlth       = "Satisfaction with State of Health Services ",
  trstlgl       = "Trust in Legal System",
  ppltrst       = "Trust in People",
  imwbcnt       = "Attitude towards Immigrants",
  rlgatnd_rev   = "Religious Service Attendance",
  lrscale       = "Left-Right Political Placement",
  polintr_rev   = "Political Interest",
  eduyrs_winsor = "Years of Education",
  gndr          = "Gender",
  year          = "Year",
  age_group     = "Age Group"
)

label_map <- tibble::tibble(
  term = unname(var_labels),
  var = names(var_labels)
)

#Add Trust in EU separately
main_var <- setNames("trstep", "Trust in the EU")

# Grouped variable choices
grouped_choices <- lapply(category_vars, function(vars) {
  stats::setNames(vars, var_labels[vars])
})

# Combine: ungrouped main_var first, then the grouped ones
final_choices <- c(main_var, grouped_choices)

# Defining List shortcut
ul <- tags$ul
ol <- tags$ol
li <- tags$li

# ----------- GLOBAL FUNCTIONS -----------

# Set global plot theme
modern_clean_theme <- function(base_size = 14, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#dddddd", size = 0.3),
      panel.grid.minor = element_blank(),
      
      plot.title = element_text(size = base_size + 4, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = base_size + 2, hjust = 0),
      axis.title = element_text(size = base_size + 1, face = "bold"),
      axis.text = element_text(size = base_size - 1, color = "#222222"),
      
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      legend.background = element_rect(fill = "white", color = NA),
      
      strip.background = element_rect(fill = "#f0f0f0", color = NA),
      strip.text = element_text(face = "bold", size = base_size)
    )
}

# Function for age grouping
get_age_group <- function(data) {
  data %>%
    mutate(
      age_group = case_when(
        agea < 18 ~ "<18",
        agea >= 18 & agea <= 24 ~ "18–24",
        agea > 24 & agea <= 34 ~ "25–34",
        agea > 34 & agea <= 44 ~ "35–44",
        agea > 44 & agea <= 54 ~ "45–54",
        agea > 54 & agea <= 64 ~ "55–64",
        agea > 64 ~ "64<",
        TRUE ~ NA_character_
      ),
      age_group = factor(age_group, levels = c("<18", "18–24", "25–34", "35–44", "45–54", "55–64", "64<"))
    )
}

# Function for plot over time
plot_over_time <- function(var, y_label, title, year_range = NULL) {
  data <- dt_filtered
  
  if (!is.null(year_range) && length(year_range) == 2) {
    data <- data %>%
      filter(year >= year_range[1], year <= year_range[2])
  }
  
  data %>%
    filter(!is.na(.data[[var]]), !is.na(year)) %>%
    group_by(cntry, year) %>%
    summarise(mean_val = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = year, y = mean_val, color = cntry, group = cntry)) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_x_continuous(breaks = seq(min(dt_filtered$year, na.rm = TRUE),
                                    max(dt_filtered$year, na.rm = TRUE), by = 4)) +
    scale_color_manual(values = trust_colors, labels = trust_labels) +
    labs(
      title = title,
      x = "Year",
      y = y_label,
      color = "Country"
    ) +
    modern_clean_theme() +
    theme(legend.position = "bottom")
}


# ----------- UI: HEADER, LAYOUT, NAVIGATION -----------

# Defining the User Interface
ui <- fluidPage(
  
  # Tagging CSS 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css?v=2.3")
  ),
  
  # Define fixed Header
  div(class = "fixed-header",
      div(class = "header-flex",
          tags$img(src = "eu-logo.jpg", class = "eu-logo"),
          div(class = "header-text",
              h1("What Shapes Trust in the European Union in Poland, Germany, and Slovenia?"),
              h2("A Comparative Analysis of Political Attitudes, Ideology, and Socio-Demographics Over Time.")
          )
      ),
  
  # Define Tabs in Header
      div(class = "nav-wrapper", 
          tabsetPanel(id = "tabs", type = "tabs", 
                      tabPanel("About", value = "about"),
                      tabPanel("Trust in EU", value = "trust"),
                      tabPanel("Political Context", value = "context"),
                      tabPanel("Insights", value = "insights"),
                      tabPanel("Cross-Country Comparison", value = "comparison"),
                      tabPanel("Implications", value = "implications")
          )
      )
  ),
  
  # Content that is displayed depending on the tab
  div(id = "mainContent",
      uiOutput("tabContent")
  )
)
  
 
# ----------- SERVER: DEFINITION -----------


# Defining the server logic 
server <- function(input, output) {
  
  # ----------- TABLOGIC: DYNAMIC RENDERING -----------
  
  output$tabContent <- renderUI({
    switch(input$tabs,
    
    # ----------- TAB: ABOUT -----------
    
       "about" = tagList(
           div(class = "page-section",
                
                # One box
                div(class = "full-width-box",
                                      
                    fluidRow(
                    # Text left
                      column(6,
                           h2("About this Project"),
                           p("This project examines the factors influencing trust in the European Union across three distinct member states: Germany, Poland, and Slovenia. These countries represent different positions on the ranking of EU fundings: Germany as the largest net contributor, Poland as the largest net recipient, and Slovenia as a moderate, small-scale beneficiary."), 
                           p("We base the selection of these three countries on the Statista household evaluation of the year 2023 on the right. Underlying this selection is the assumption that monetary connections between countries and the institution could influence the visibility of the EU within that country and that, in turn, this visibility could influence trust in the European Parliament. This assumption, nevertheless, cannot be proven in this study and solely serves the purpose of selection of countries we look at."),
                           p("The focus lies on the investigation of how political attitudes, ideological orientations, and socio-demographic variables shape public trust in the European Parliament within each country. By comparing the three cases, we aim to uncover both country-specific and cross-national patterns."),
                           p("The project features interactive graphs and visualizations that provide a clear and comparative view of the data and a transparent picture of each country. Our findings contribute to current research on public attitudes towards the EU, offering valuable insights for policymakers that seek to strengthen pro-EU sentiments and engagement."),
                           p("On a last note: In the further Analysis of this Project, we will use trust in the European Parliament as an indicator for trust in the EU and will use the two expressions interchangeably.")
                       ),
                    
                    # PNG right
                      column(6,
                        tags$img(src = "euhouseholds.png", style = "max-width: 550px; height: auto; display: block; margin: auto;")
                      )
                    )
                ),
              
              # Two boxes 
              div(class = "flex-two-boxes",
                   
                div(class = "box-content",
                    h2("Data Source"),
                    h4("European Social Survey (ESS)"),
                    p("The data used for this project is provided by the European Social Survey, a cross-national survey that has been conducted every two years since 2002 until the newest round in  2022. It contains open access data on public attitudes, beliefs and behavior. Covering themes of social, political and economic matters, it provides rich demographic and socio-economic variables in 39 European countries. It is high-quality data with cross-sectional samples that allows for examining trends over time within and between countries. For our research purposes, we have used a personalized dataset on the three countries, with selected variables that fit our research question. We included information on all available years.")
                 ),
                
                div(class = "box-content",
                    h2("About Us"),
                    p("We are two students from the University of Cologne, studying in the B.Sc. Management, Economics and Social Sciences. As part of our final semester we developed this project study in the field of Data Science and Quantitative Methods, consisting of a statistical analysis, a data presentation on this dashboard and a final report. Our analysis has been made possible thanks to the supervision by Prof. Dr. Tom Zimmermann and Sebastian Weibels."),
                    p("For inquiries, feel free to contact us:"), 
                    p("Helena Zappe: hzappe@smail.uni-koeln.de", br(), "Marei Göbelbecker: mgoebelb@smail.uni-koeln.de")
                 )
              )
           )
       ),
       
    # ----------- TAB: TRUST EU -----------
      
      "trust" = tagList(
          div(class = "page-section",
              
              h2("How much does People's Trust in the European Parliament differ?"),
              
              p("The variable investigated in this project is individuals' trust in the European Parliament and its development over time. This variable is coded on a scale from 0 (No Trust at All) to 10 (Complete Trust).", style = "margin-bottom: 30px;"),
            
              div(class = "full-width-box",
                  plotOutput("euTrustPlot", height = "350px")
               ),
              
              p("This plot shows the average trust levels in all three countries.", strong("Overall, average trust in the EU seems to be fairly similar in all three countries."), "They are all fluctuating around a mean of 4.2. "),
              ul(
                li(strong("Germany"), "has been the most stable over the years with constant ups and downs. Its lowest point was in 2010 and its peak in 2022."),
                li(strong("Poland"), "started off as the country with the highest trust in the EU in 2002, but experienced a severe decline in trust from 2012 until 2014 when it had reached its lowest point. After that, trust rose again until 2018 after which it experienced another drop, such that at the end of 2022 it was on a level much lower than the other two countries. "),
                li(strong("Slovenia"), "shows the strongest fluctuations over time, with a sharp decline between its highest level of 5.0 in 2006 to its lowest point of 3.4 in 2014. Since then, trust rose again, such that in 2022 it was only slightly lower than the German trust level.")
              ),
              p("These differences in the evolution of average trust in the EU point towards the existence of a multitude of factors which shape trust within the different countries.", style = "margin-bottom: 30px;"), 
              
              h3("Are there Distinct Patterns of Trust Levels in the EU between Different Age Groups?"),
              selectInput("selected_year", "Select Year:", choices = c("All Years (Average)" = "all", sort(unique(dt_filtered$year)))),
              
              div(class = "full-width-box",
                  plotOutput("euTrustPlot2", height = "350px")
              ),
              
              p("Yes, there are differences of trust between the age groups, even though they are not large. On an aggregate level, all countries show a general downward trend with the youngest people <18 having the highest trust in the EU. This trend continues until the age of 55-64, from which onward people trust slightly more again."),
              p("German trust levels show a common downward trend over the years, whereas Poland does not show a distinct trend between years. Slovenia has a relatively stable trust level across all age groups up until 2014, from which onward it displays a similar trend as Germany.")
          )
       ),
      
    # ----------- TAB: POLITICAL CONTEXT -----------
      
       "context"  = tagList(
         div(class = "page-section",
             
             h2("What Is the Political Context of the EU from 2002 to 2022?"),
             p("This section provides background knowledge on political events which is absent in the ESS Data and sets an important basis for the analysis. Connections between country-specific findings and specific events are not made explicitly in this analysis, but such events are accounted for through time-fixed effects in the models, as they may influence attitudes and trust.", style = "margin-bottom: 30px;"),
             
             div(class = "full-width-box",
                 h4("Main Political Events in the EU"),
                 ul(
                   li("2002 – National currencies replaced by Euro notes and coins"),
                   li("2003 – Plans for a European constitution suffer a setback"),
                   li("2004 – The EU enlarges and a new constitution is signed"),
                   li("2007 – Treaty of Lisbon & Slovenia and Poland adopt the Euro"),
                   li("2008 – Global Financial Crisis & First Recession in Eurozone"),
                   li("2009 – Lisbon Treaty abolishes the three pillars of the European Union"),
                   li("2010 – EU Support for budget deficits of Member States"),
                   li("2015 – Migrant Crisis & Rises in Terror Attacks"),
                   li("2015 – Paris Climate Agreement"),
                   li("2016 – UK votes to leave the European Union"),
                   li("2017 – Start of Brexit"),
                   li("2019 – European Green Deal"),
                   li("2020 – Start of Covid-19 Pandemic"),
                   li("2020 – Brexit"),
                   li("2022 – Russian Invasion of Ukraine ")
                 )
             ),
             
             h3("What Distinguishes the Three Countries Politically?"),
             p(strong("EU Participation:"), "While Germany was one of the founding members of the EU and has been part of it since 1958, Poland and Slovenia joined the Institution in 2004. Therefore, the two countries have not yet been part of the EU in the beginning of the analyzed time frame."),
             p(strong("Form of Government:"), "All three countries are Parliamentary Democracies, even though of different forms."),
             p(strong("Economic Power:"), "Poland's and Slovenia's GDP per Capita are both below the EU average. Poland makes up 4.4% of the EU's total GDP, while Slovenia's part makes up 0.4%. Germany's GDP per capita is well above the EU average and constitutes the largest part of the EU's GDP with 24.2%. These numbers underline the sizes of the countries’ economic forces."),
             p(strong("Benefits from EU Funding:"), "All three countries benefit from being part of the EU by receiving funding for individual projects, as well as open borders and trade. Poland is a large net recipient of fundings, whereas Slovenia receives only small net funding. Germany is the largest net contributor to the EU household.", style = "margin-bottom: 30px;"),
             
             h3("How Does the Political Landscape Look Within the Three Countries?"),
             p("An additional baseline is provided by respondents' political self-placement and voting behavior within the ESS dataset."),
             
             radioButtons("year_select", "Filter by Year",
                                   choices = c("All Years", "2002", "2004", "2006", "2008", "2010",
                                               "2012", "2014", "2016", "2018", "2020", "2022"),
                                   selected = "All Years",
                                   inline = TRUE
                      ),
              radioButtons("age_select", "Filter by Age Group",
                                   choices = c("All Age Groups", "<18", "18–24", "25–34", "35–44", "45–54", "55–64", "64<"),
                                   selected = "All Age Groups",
                                   inline = TRUE
                      ),
             
             div(class = "full-width-box",
                 plotOutput("lrscalePlot", height = "300px"),
             ),
             div(class = "full-width-box",
                 plotOutput("votePlot", height = "300px")
             ),
             
             p(strong("Political Participation:"), "All three countries showcase high voting rates which have increased over time, with a significant growth since 2018. Across all years, the highest voter turnout is in Germany, with an exception in 2022. Comparing different age groups reveals that younger respondents vote less frequently, but participation is rising with increasing age. Older respondents are consistently more politically active."),
             p(strong("Political Orientation:"), "The political self-placement across all three countries tends to cluster around the center of the left-right scale (value 5). However, national trends are clear: respondents in Germany lean as far left, as Polish orientations lean to the right. Slovenia is less extreme but also clearly shows a left-leaning tendency."),
             ul(
               li("In", strong("Poland"), "the right-leaning share increases strongly with age when people shift from the center to the right."),
               li("In", strong("Germany"), "respondents are generally more left-leaning, especially among younger people. There's a gradual shift toward the right with increasing age, but the pattern is relatively stable over time."),
               li("In", strong("Slovenia"), "older people are more likely to identify as either left- or right-leaning, moving away from the center. Younger respondents lean left. However, there is still a balanced distribution between left- and right-leaning people across years."),
               li("A", strong("general distinction"), "by age group shows that younger people are more left-leaning across all countries, whereas older respondents tend to be more right-leaning. While there are some year-to-year fluctuations, the general ideological patterns remain stable.")
             ),
             
         )
       ),
      
    # ----------- TAB: INSIGHTS -----------
           
       "insights"  = tagList(
         div(class = "page-section",
             h2("What Shapes Trust in the EU?"),
             
             p("This section explores how trust in the EU is influenced by demographic profiles, ideological factors, and levels of political and institutional trust."),
             
             selectInput("insight_category", "Select Category:",
                         choices = names(category_vars),
                         selected = names(category_vars)[1]),
             
             uiOutput("insight_plots")
         )
       ),
      
    # ----------- TAB: CROSS-COUNTRY COMPARISON -----------
        
       "comparison"  = tagList(
         div(class = "page-section",
             h2("What Predicts Trust in the EU? And How Do These Patterns Differ by Country?"),
             p("This page presents the findings of a multiple linear regression model, which includes all predictors investigated before. It aims at estimating significance and magnitude of individual predictors and allows cross-country differences.", style = "margin-bottom: 30px;"),
             
             # Correlation Matrix
             h3("Correlation Patterns"),
             
             pickerInput(
               inputId = "cor_vars",
               label = "Select variables for correlation matrix",
               choices = final_choices,  
               selected = unlist(final_choices),
               multiple = TRUE,
               options = list(
                 `actions-box` = TRUE,
                 `live-search` = TRUE,
                 `selected-text-format` = "count > 3"
               )
             ),
             
             div(class = "full-width-box",
                 fluidRow(
                   column(4,
                          fluidRow(
                            column(4, h3("Germany")),
                            column(4, actionButton("zoom_DE", "Zoom In"))
                            ),
                          plotOutput("cor_DE")
                   ),
                   column(4,
                          fluidRow(
                            column(4, h3("Poland")),
                            column(4, actionButton("zoom_PL", "Zoom In"))
                          ),
                          plotOutput("cor_PL")
                   ),
                   column(4,
                          fluidRow(
                            column(4, h3("Slovenia")),
                            column(4, actionButton("zoom_SI", "Zoom In"))
                          ),
                          plotOutput("cor_SI")
                   )
                 )
                 
             ),
             
             p("Generally, there are", strong("moderate correlations in all three countries"), "with none being higher than 0.54, indicating a reasonable interpretation of the linear regression model. Across all three countries, the correlations range from -0.1 to 0.5."),
             p("Clusters of moderate correlations are seen between the various satisfaction variables related to institutional performance, possibly suggesting  a general sense of satisfaction with national institutions. Importantly, the correlations between the predictors and trust in the EU are relatively stronger across all three countries. This supports the relevance and empirical justification for including these variables in the regression models.", style = "margin-bottom: 30px;"),
             
             h3("Country-Specific Models"),
             
             p("These country-specific models provide insights into the key predictors of EU trust within each country. Here you can see the results of a pooled multiple linear regression model, including year fixed effects, for each country. The heat map below visualizes the regression results for Slovenia, Poland and Germany. To view detailed estimates, please click a field of your choice."),
             p("Important: Note that there is no full data coverage in 2002 and 2010. Insights into these years cannot be gained with this model. Therefore, the baseline year of estimation is 2004.", style = "margin-bottom: 30px;"),
             
             
             div(class = "full-width-box",
                 #h3("Heatmap: Regression Coefficients by Country and Variable Group"),
                 plotOutput("coef_heatmap", click = "heatmap_click", height = "750px"),
                 h4(uiOutput("selected_var_label")),
                 div(style = "text-align: center; padding-top: 10px;",
                     div(style = "display: inline-block; max-width: 600px;",
                         tableOutput("selected_variable_table"))),
                 actionButton("show_models", "Show Details", icon = icon("table"))
             ),
             
             
             h4("Main Effects in Germany"),
             p("In Germany, EU trust is most strongly associated with", strong("satisfaction with the national government, gender, and trust in the legal system.")),
             ul(
               li("Females tend to have higher EU trust."),
               li("Age appears to have a stronger effect in Germany than in the other two countries. Older people are significantly less trusting of the EU."),
               li("As expected, higher support for EU unification is positively associated with EU trust."),
               li("Temporal effects indicate that trust dropped notably in 2014 and 2016 compared to the baseline 2004.", style = "margin-bottom: 20px;")
             ),
             
             h4("Main Effects in Poland"),
             p("In Poland, trust in the EU is most closely related to", strong("support for EU unification, trust in the legal system, and political interest.")),
             ul(
               li("Interestingly, higher satisfaction with the national government is associated with lower EU trust which suggests a substitution effect where citizens may either trust the EU or their national government, but not both."),
               li("Being female, having more positive attitudes toward immigrants, and holding more left-leaning political views are also associated with higher EU trust, even though these effects are smaller."),
               li("Over time, EU trust declined significantly, especially in 2014, 2016, 2020, and 2022, even after controlling for individual attitudes and demographics."),
               li("Despite higher religious attendance in Poland compared to the other countries, it shows only a small but statistically significant negative association with EU trust.", style = "margin-bottom: 20px;")
             ),
             
             h4("Main Effects in Slovenia"),
             p("In Slovenia, the strongest associations with EU trust are found for", strong("trust in the legal system, satisfaction with the national government, EU support, gender and political interest.")),
             ul(
               li("Among these, institutional trust and political interest seem particularly influential."),
               li("EU trust declined after 2002, especially in 2018."),
               li("Unlike in Poland, religious attendance is weakly but positively associated with higher trust in the EU, even though the effect is small and less significant."),
               li("Immigration attitudes have a smaller and less significant impact compared to Poland."),
               li("Additionally, older people tend to have less trust in the EU, suggesting a decline in EU trust with increasing age.", style = "margin-bottom: 30px;")
             ),
             
             h3("Key Interaction Effects"),
             
             p("The interaction effect plots visualize how the relationship between a selected predictor and trust in the EU varies across countries. Each colored line represents one country and shows how EU trust changes as the predictor changes. Only the most meaningful and significant interactions are shown. The shaded areas around each line represent 95% confidence intervals, indicating uncertainty around the prediction. Wider bands imply less precise estimates and narrower bands indicate higher certainty. Use the dropdown menu to explore different interaction effects and to understand how specific factors shape EU trust differently across the three countries.", style = "margin-bottom: 30px;"),
             
             # Interaction Plots
             selectInput(
               inputId = "selected_var",
               label = "Choose a Variable of Interest:",
               choices = setNames(c("euftf", "stfgov", "lrscale", "trstlgl"), 
                                  var_labels[c("euftf", "stfgov", "lrscale", "trstlgl")]),
               selected = "euftf"
             ), 
             
             div(class = "full-width-box",
                 fluidRow(
                   # Graph Left
                   column(6,
                          plotOutput("interactionPlot", height = "270px")), 
                   # Text right
                   column(6,
                          output$dynamicText <- renderUI({
                            selected_var <- input$selected_var
                            explanations[[selected_var]]
                          })
                   )
                 )
             )

         )
       ),
      
    # ----------- TAB: IMPLICATIONS -----------

       "implications" = tagList(
         div(class = "page-section",
             h2("What Do the Findings Imply for the EU?"),
             
             p("Our findings reveal that trust in the EU is shaped by an interplay of political attitudes, national institutional trust, ideological factors, and demographic characteristics. While some factors, like trust in the legal system, are consistently influential, others, such as government satisfaction, vary considerably across Slovenia, Poland, and Germany."),
             
             div(class = "full-width-box",
             p("Most importantly, this analysis has shown that efforts to foster EU trust must be tailored to specific national contexts and institutional landscapes and have the most effect when tackling lacking trust in the national legal system and general public trust.")),
             
             tabsetPanel(
               type = "tabs",
               
               tabPanel("Main Findings",
                        
                        h3("What Can We Infer from the Findings of This Analysis?", style = "margin-bottom: 30px;"),
                        div(class = "flex-two-boxes",
                            div(class = "box-content",
                                p(strong(span("Institutional trust is a central driver of EU trust.", class = "highlight"))), 
                                  p("Confidence in the national legal system strongly predicts EU trust. This is particularly true in Poland and Slovenia, suggesting that perceived EU legitimacy is closely linked to national institutional performance, also including the health care system, education system and the national government.")
                            ),
                            div(class = "box-content",
                                p(strong(span("National satisfaction and EU trust can either reinforce or substitute each other in different countries.", class = "highlight"))), 
                                  p("Higher satisfaction with the national government tends to increase EU trust in Slovenia and particularly Germany. However, the opposite effect is observed in Poland, where greater satisfaction with the national government correlates with lower EU trust. This suggests a competitive dynamic between the EU and the national government, in which citizens place trust in either the national government or the EU, but not both.")
                            )
                        ),
                        
                        div(class = "flex-two-boxes",
                            div(class = "box-content",
                                p(strong(span("Political ideology influences EU trust but varies across countries.", class = "highlight"))), 
                                  p(" In Germany and Poland, individuals with right-leaning political views tend to trust the EU less, consistent with common narratives of right-wing parties. In contrast, right-leaning individuals in Slovenia tend to have more trust in the EU. This highlights how national political contexts and parties’ narratives shape perceptions of the EU.")
                            ),
                            div(class = "box-content",
                                p(strong(span("Women generally trust more than men.", class = "highlight"))), 
                                  p(" In all three countries, women tend to place higher trust in the EU, with the effect being strongest in Germany. This suggests gender-related attributes, with women perceiving the EU as a guarantor of security, rights, and stability.")
                            )
                        ),
                        
                        div(class = "flex-two-boxes",
                            div(class = "box-content",
                                p(strong(span("Age and education show varying effects.", class = "highlight"))),
                                p("Age consistently shows significant but country-specific effects. Older citizens in Poland tend to be more trusting, whereas in Germany and Slovenia, they are less trusting in the EU. This hints at generational divides in EU sentiment. Education, by contrast, is not a statistically significant predictor in any of the countries, suggesting that EU trust is influenced more by political and institutional factors than by educational attainment.")
                            ),
                            div(class = "box-content",
                                p(strong(span("Temporal dynamics reflect shared and country-specific shocks.", class = "highlight"))),
                                  p("Declines in trust in the EU between 2014 and 2016 across all countries may be linked to broader EU crises such as the migration crisis and Brexit. However, Poland’s decline in 2020 and 2022 or Slovenia’s drop in 2018 suggest that domestic political or institutional events also play a key role in shaping EU trust.")
                            )
                        ),
                        
                        div(class = "full-width-box",
                            p(strong(span("Iniciatives need to be specifically tailored to the country.", class = "highlight"))),
                            p("This analysis has shown that there are many factors are influencing Trust in the EU. Some of these factors have a stronger influence than others. Therefore, there cannot be one suggestion to raise Trust in the EU, but rather tackle specific aspects which have a strong influence and which might differ between countries. Especially the example of Poland has shown, that the way in which such issues need to be tackled, require a specific tailoring and handling.")
                            )
                        ),
               
               tabPanel("Further Questions",
                        h3("What Further Questions Can We Ask in Connection to This Analysis?", style = "margin-bottom: 30px;"),
                        div(class = "full-width-box",
                            p(strong("What Role do Macroeconomic Factors and Connected Claims to Sovereignty Play?")),
                            p("It seems that the initial country selection by net recipient or payer status turns out to be very relevant - especially in terms of the Polish \"either-or\" effect between the national and the EU level. We suggest that the intangible effects we see in this analysis are partly dependent on the economic status of a country within the EU. Further research should be done on the indicated  \"substitution relationship\" of trust in the national and international institutions and what this tells about the interplay between EU integration and national claims to sovereignty.")
                            ),
                    
                         div(class = "full-width-box",
                             p(strong("How could National Justice and Therefore Trust in the National Governments be Enhanced?")),
                             p("Trust in the legal system is a consistent factor in shaping trust in the EU. This suggests that trust in the EU is strongly dependent on institutional reliability. The EU could communicate more as a guarantor of justice and stability to strengthen trust, but it does not have the means to physically reinforce justice further than it does currently. For this reason, this suggestion cannot effectively change the operations of the EU, but it gives a starting point for analysis on how to put more pressure on the national governments in that regard. On that note, it is also important to analyse whether this would be morally feasible or not.")
                         ),
                         div(class = "full-width-box",
                             p(strong("How could Gender-Specific Differences be Either Equalized or Exploited?")),
                             p("Women consistently show more trust which could be rooted in a stronger desire for social protection, human rights or legal justice, for example. Therefore, we suggest that a more gender-sensitive EU communication could be an effective instrument for increasing female trust. On the other hand, further research should be done on the question where the male lack of trust originates, to know how to enhance their trust to female levels or higher.")
                         ),
                        
                        div(class = "full-width-box",
                            p(strong("How Stable Is Trust in the EU in Times of Crisis?")),
                            p("Our findings show that trust dropped notably during major EU-wide crises, such as the 2014 to 2016 migration and Brexit periods, and during the COVID-19 and Ukraine war in 2020 to 2022. This suggests that trust in the EU is vulnerable to external shocks but the effect size also highlights that it can be mediated by national institutions and public discourse.")
                        ),
                        
                        div(class = "full-width-box",
                            p(strong("What Does This Tell Us About EU Legitimacy?")),
                            p("If trust is strongly linked to trust in national governance and legal systems, EU legitimacy partially depends on the state of national institutions. The EU cannot build public trust in isolation from the performance and credibility of member states’ legal and political systems. For this reason, it seems very important for EU promoters to consider the narratives about the EU that national governments are spreading and which narratives they are evoking about themselves.")
                        ),
                        
               ),
               
               tabPanel("Limitations",
                        h3("What Are the Limitations of This Analysis?", style = "margin-bottom: 30px;"),
                        div(class = "full-width-box",
                        ol(
                          li("Due to", strong("data constraints"),", we are unable to place our findings in the broader context of all EU member states. The study focuses only on Germany, Poland, and Slovenia, which limits the generalizability of the results across the entire EU."),
                          li("While ", strong("other factors"), "such as income are likely to influence EU trust as well, they could not be included in the analysis due to incomplete data. As such, our analysis primarily highlights the role of socio-demographics, political attitudes and ideology, offering directional tendencies rather than a fully comprehensive explanation."),
                          li("Major ", strong("political events"), "were not directly included in the dataset. Instead, we used year fixed effects to account for changes over time. While this approach captures broader time trends, it does not allow us to link trust dynamics to specific events.")
                        )
                        )
     
               ),
             )
      
             
         )
       )
       
    )
  })

  
  # ----------- OUTPUT: TRUST TAB (PLOTS) -----------
  
  # euTrustPlot  
  output$euTrustPlot <- renderPlot({
    trust_trends <- dt_filtered %>%
      filter(cntry %in% c("DE", "PL", "SI")) %>%
      group_by(cntry, year) %>%
      summarise(
        trust_EU = mean(trstep, na.rm = TRUE),
        .groups = "drop"
      ) 
    
    ggplot(trust_trends, aes(x = year, y = trust_EU, color = cntry)) +
      scale_x_continuous(breaks = seq(min(trust_trends$year), max(trust_trends$year), by = 4)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = trust_colors, labels = trust_labels) +
      labs(
           title = "Trust in the EU over Time", 
           x = "Year", 
           y = "Average Trust in the EU", 
           color = "Country") +
      modern_clean_theme()
  })

  # euTrustPlot2
  output$euTrustPlot2 <- renderPlot({
    req(input$selected_year)
    
    dt_filtered %>%
      { if (input$selected_year != "all") filter(., year == input$selected_year) else . } %>%
      get_age_group(.) %>%   # <- das ist korrekt
      filter(!is.na(age_group)) %>%
      group_by(age_group, cntry) %>%
      summarise(trstep = mean(trstep, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = age_group, y = trstep, fill = cntry)) +
      geom_col(position = "dodge", alpha = 0.6) +
      geom_line(aes(group = cntry, color = cntry), 
                position = position_dodge(width = 0.9), size = 1) +
      geom_point(aes(color = cntry), 
                 position = position_dodge(width = 0.9), size = 2) +
      scale_fill_manual(values = trust_colors, labels = trust_labels) +
      scale_color_manual(values = trust_colors, labels = trust_labels) +
      guides(fill = "none", color = guide_legend(title = "Country")) +
      labs(
        title = if (input$selected_year == "all") {
          "Trust in the EU by Age Group and Country (Average Across Years)"
        } else {
          paste("Trust in the EU by Age Group and Country in", input$selected_year)
        },
        x = "Age Group",
        y = "Average Trust in the EU",
        fill = "Country"
      ) +
      modern_clean_theme()
  })

  # ----------- OUTPUT: CONTEXT TAB (PLOTS) -----------
  
  # lrscalePlot
  output$lrscalePlot <- renderPlot({
    dt_filtered %>%
      get_age_group(.) %>% 
      # Filter Jahr
      { if (input$year_select != "All Years") filter(., year == as.numeric(input$year_select)) else . } %>%
      # Filter Altersgruppe
      { if (input$age_select != "All Age Groups") filter(., age_group == input$age_select) else . } %>%
      filter(!is.na(lrscale)) %>%
      mutate(cntry = factor(cntry, levels = c("DE", "PL", "SI"))) %>%
      count(cntry, lrscale) %>%
      group_by(cntry) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      ggplot(aes(x = cntry, y = percentage, fill = factor(lrscale, levels = rev(0:10)))) +
      geom_col(position = "stack") +
      coord_flip() +
      scale_fill_manual(
        values = rev(c(
          "0" = "#00002E", "1" = "#002060", "2" = "#003090", "3" = "#4A90E2",
          "4" = "#8AB4F8", "5" = "#DDDDDD",
          "6" = "#FFE399", "7" = "#FFBE5C", "8" = "#FF914D", "9" = "#C23B22", "10" = "#800000"
        )),
        name = "Left-Right Scale",
        guide = guide_legend(nrow = 1, reverse = TRUE)
      ) +
      labs(
        title = "Placement on Left-Right Political Scale",
        x = NULL,
        y = "Percentage (%)"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom", legend.box = "horizontal")
  })
  
  #votePlot
  output$votePlot <- renderPlot({
    dt_filtered %>%
      { if (input$year_select != "All Years") filter(., year == as.numeric(input$year_select)) else . } %>%
      { if (input$age_select != "All Age Groups") filter(., age_group == input$age_select) else . } %>%
      filter(!is.na(vote)) %>%
      mutate(cntry = factor(cntry, levels = c("DE", "PL", "SI"))) %>%
      count(cntry, vote) %>%
      group_by(cntry) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      ggplot(aes(x = cntry, y = percentage, fill = factor(vote, levels = rev(1:3)))) +
      geom_col(position = "stack") +
      coord_flip() +
      scale_fill_manual(
        values = c(
          "1" = "#002060",
          "2" = "#4A90E2",
          "3" = "#DDDDDD"
        ),
        name = "Voted in the Last National Election",
        labels = c("3" = "Not eligible", "2" = "No", "1" = "Yes"),
        guide = guide_legend(reverse = TRUE)
      ) +
      labs(
        title = "Distribution of Voting Behavior",
        x = NULL,
        y = "Percentage (%)"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  # ----------- OUTPUT: INSIGHTS TAB (UI & PLOTS) -----------
  
  # insight_plots
  output$insight_plots <- renderUI({
    switch(input$insight_category,
           
           # IF Ideology
           "Ideology" = tagList(
             div(class = "full-width-box",
                 h3("Key Findings"),
                 ul(
                   li(strong("Immigration attitudes reveal a surprising pattern:"), "Polish respondents appear the most welcoming toward immigrants, despite their more conservative political orientation. Slovenian respondents are the least welcoming."),
                   li(strong("Trust in others is lowest in Poland:"), "Germany shows the highest interpersonal trust, while Poland’s trust dropped sharply in 2020."),
                   li(strong("Religious attendance is highest in Poland:"), "Poland has a much more religious population, while Germany reports the lowest levels of attendance. Slovenia falls in the middle with a more even distribution."),
                   li(strong("Political interest is highest in Germany:"), "Respondents in Germany show consistently higher political interest. Interest in Poland and Slovenia is on the same level, with only small fluctuations. However, a sharp rise in Poland interrupts this pattern.")
                 )
             ),
             div(class = "flex-two-boxes",
                 div(class = "box-content",  
                     p("For a better overview you can select a time range."),
                     div(style = "display: block; margin: 0 auto; width: 80%;",
                         sliderInput("year_range", "Select Range:",
                                     min = min(dt_filtered$year, na.rm = TRUE),
                                     max = max(dt_filtered$year, na.rm = TRUE),
                                     value = c(min(dt_filtered$year, na.rm = TRUE), max(dt_filtered$year, na.rm = TRUE)),
                                     step = 2,
                                     sep = ""))
                 ),
                 div(class = "box-content", plotOutput("plot_imwbcnt", height = "350px"))
             ),
             div(class = "flex-two-boxes",
                 div(class = "box-content", plotOutput("plot_ppltrst", height = "350px")),
                 div(class = "box-content", plotOutput("plot_rlgatnd_rev", height = "350px"))
             ),
             div(class = "flex-two-boxes",
                 div(class = "box-content", plotOutput("plot_polintr_rev", height = "350px")),
                 div(class = "box-content", p("")))
             ),
           
           # IF Socio-Demographics
           "Socio-Demographics" = tagList(
             div(class = "full-width-box",
                 h3("Key Findings"),
                 ul(
                   li(strong("Gender distribution is roughly balanced:"), "All three countries show almost equal gender splits. Germany has a slight male majority, while Poland and Slovenia have slightly more female respondents, with the largest gap in Slovenia."),
                   li(strong("Populations are aging across all countries:"), "The “64<” age group represents the largest share in all three countries, indicating aging demographics."),
                   li(strong("Educational attainment is highest in Germany:"), "German respondents report the most years of education. Slovenia’s values are more concentrated, with a narrower range and fewer outliers after winsorization.")
                 )
             ),
             
             div(class = "flex-two-boxes",
                 div(class = "box-content", plotOutput("plot_age_group", height = "350px")),
                 div(class = "box-content", plotOutput("plot_gndr", height = "350px")),
                 
             ),
             div(class = "full-width-box",
                 div(class = "box-content", plotOutput("plot_eduyrs_winsor", height = "350px"))
             )
           ),
           
           # IF Political Attitudes
           "Political Attitudes" = tagList(
             div(class = "full-width-box",
                 h3("Key Findings"),
                 ul(
                   li(strong("Satisfaction with national government fluctuates over time:"), "German respondents were especially dissatisfied in 2002, Polish in 2020, and Slovenians in 2012. The period between 2010–2016 showed the greatest disparity between countries."),
                   li(strong("Education satisfaction is surprisingly lowest in Germany:"), "Slovenia shows the highest and most consistent satisfaction, while Poland experienced a sharp drop in 2020."),
                   li(strong("Health system satisfaction is lowest in Poland:"), "Polish respondents consistently report the lowest satisfaction, with a dramatic drop below 3 in 2020. Germany peaked around 2016 but declined afterwards."),
                   li(strong("Trust in the legal system is highest in Germany:"), "Germany shows a steady upward trend. Slovenia shows recent improvement, while Poland saw a significant drop in 2020."),
                   li(strong("Support for further EU unification is broadly positive:"), "Respondents in all three countries generally want the EU to move forward, with Poland showing the strongest support until 2014. After 2014, all countries followed a similar upward trend, though support declined again slightly after 2020."))),
             
             div(class = "flex-two-boxes",
                 div(class = "box-content",  
                     p("For a better overview you can select a time range."),
                     div(style = "display: block; margin: 0 auto; width: 80%;",
                     sliderInput("year_range", "Select Range:",
                                 min = min(dt_filtered$year, na.rm = TRUE),
                                 max = max(dt_filtered$year, na.rm = TRUE),
                                 value = c(min(dt_filtered$year, na.rm = TRUE), max(dt_filtered$year, na.rm = TRUE)),
                                 step = 2,
                                 sep = ""))),
                 div(class = "box-content", plotOutput("plot_stfgov", height = "350px"))),
             
             div(class = "flex-two-boxes",
                 div(class = "box-content", plotOutput("plot_stfedu", height = "350px")),
                 div(class = "box-content", plotOutput("plot_stfhlth", height = "350px"))),
             
             div(class = "flex-two-boxes",
                 div(class = "box-content", plotOutput("plot_trstlgl", height = "350px")),
                 div(class = "box-content", plotOutput("plot_euftf", height = "350px")))
           )
    )
  })
  
  # Plot 
  # plot_age_group
  output$plot_age_group <- renderPlot({
    dt_filtered %>%
      get_age_group(.) %>%
      filter(!is.na(age_group)) %>%
      count(cntry, age_group) %>%
      group_by(cntry) %>%
      mutate(prop = n / sum(n) * 100) %>%
      ggplot(aes(x = age_group, y = prop, fill = cntry)) +
      geom_col(position = "dodge") +
      scale_fill_manual(
        values = trust_colors,  # Reuse your existing color scheme
        labels = trust_labels
      ) +
      labs(
        title = paste("Age Group Distribution"),
        x = "Age Group",
        y = "Proportion of Respondents (%)",
        fill = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  # plot_gndr
  output$plot_gndr <- renderPlot({
    dt_filtered %>%
      mutate(gndr = as_factor(gndr)) %>%
      filter(!is.na(gndr)) %>%
      count(cntry, gndr) %>%
      group_by(cntry) %>%
      mutate(prop = n / sum(n) * 100) %>%
      ggplot(aes(x = cntry, y = prop, fill = gndr)) +
      geom_col(position = "dodge") +
      scale_fill_manual(
        values = c("1" = "#003399", "2" = "#FFCC00"), 
        labels = c("1" = "Male", "2" = "Female")
      ) +
      labs(
        title = paste("Relative Distribution of Gender by Country"),
        x = "Country",
        y = "Proportion of Respondents (%)",
        fill = "Gender"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  # plot_eduyrs_winsor
  output$plot_eduyrs_winsor <- renderPlot({
    dt_filtered %>%
      filter(!is.na(eduyrs_winsor)) %>%
      mutate(cntry = factor(cntry, levels = c("DE", "PL", "SI"))) %>%
      ggplot(aes(x = cntry, y = eduyrs_winsor, fill = cntry)) +
      geom_boxplot() +
      scale_fill_manual(
        values = trust_colors,
        labels = trust_labels
      ) +
      labs(
        title = paste("Average Education Years by Country"),
        x = "Country",
        y = "Years of Education",
        fill = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  
  # plot_stfgov
  output$plot_stfgov <- renderPlot({
    plot_over_time(
      var = "stfgov",
      y_label = "Satisfaction (0–10 scale)",
      title = "Average Satisfaction with National Government \nby Country over Time",
      year_range = input$year_range
    )
  })
  
  # plot_stfedu
  output$plot_stfedu <- renderPlot({
    plot_over_time(
      var = "stfedu",
      y_label = "Satisfaction (0–10 scale)",
      title = "Average Satisfaction with State of Education \nby Country over Time",
      year_range = input$year_range
    )
  })
  
  # plot_stfhlth
  output$plot_stfhlth <- renderPlot({
    plot_over_time(
      var = "stfhlth",
      y_label = "Satisfaction (0–10 scale)",
      title = "Average Satisfaction with State of Health Services \nby Country over Time",
      year_range = input$year_range
    )
  })
  
  # plot_trstlgl
  output$plot_trstlgl <- renderPlot({
    plot_over_time(
      var = "trstlgl",
      y_label = "Trust (0–10 scale)",
      title = "Average Trust in the Legal System \nby Country over Time",
      year_range = input$year_range
    )
  })
  
  
  # plot_euftf
  output$plot_euftf <- renderPlot({
    plot_over_time(
      var = "euftf",
      y_label = "Attitude (0–10 scale)",
      title = "Average Support for EU Unification \nby Country over Time",
      year_range = input$year_range
    )
  })
  
  # plot_imwbcnt
  output$plot_imwbcnt <- renderPlot({
    plot_over_time(
      var = "imwbcnt",
      y_label = "Immigration Attitude (0–10 Scale)",
      title = "Average Attitude towards Immigrants \nby Country over Time",
      year_range = input$year_range
    )
  })
  
  # plot_ppltrst
  output$plot_ppltrst <- renderPlot({
    plot_over_time(
      var = "ppltrst",
      y_label = "Trust in People (0–10 Scale)",
      title = "Average Trust in People \nby Country over Time",
      year_range = input$year_range
    )
  })
  
  # plot_rlgatnd_rev
  output$plot_rlgatnd_rev <- renderPlot({
    plot_over_time(
      var = "rlgatnd_rev",
      y_label = "Attendance Frequency (0–7 scale)",
      title = "Average Attendance of Religious Services apart from \nSpecial Occasions by Country over Time",
      year_range = input$year_range
    )
  })
  

  # plot_polintr_rev
  output$plot_polintr_rev <- renderPlot({
    plot_over_time(
      var = "polintr_rev",
      y_label = "Political Interest (1–4 Scale)",
      title = "Average Political Interest \nby Country over Time",
      year_range = input$year_range
    )
  })
  
 
  
  # ----------- OUTPUT: COMPARISON TAB -----------
  
  # Correlation Matrixes
  
  # Function without labels
  render_cor_plot_nolabels <- function(cntry_var) {
    renderPlot({
      req(input$cor_vars)
      
      df <- dt_filtered %>%
        filter(cntry == cntry_var) %>%
        select(all_of(input$cor_vars)) %>%
        select(where(is.numeric))
      
      if (ncol(df) < 2) {
        plot.new()
        title("Not enough numeric variables selected.")
        return()
      }
      
      corr <- cor(df, use = "pairwise.complete.obs")
      ggcorrplot::ggcorrplot(
        corr,
        lab = FALSE,  
        type = "lower",
        colors = c("#003399", "white", "#FF7300"),
        outline.color = "grey90"
      )
    })
  }
  
  # Function with labels
  render_cor_plot_labels <- function(cntry_var) {
    renderPlot({
      req(input$cor_vars)
      
      df <- dt_filtered %>%
        filter(cntry == cntry_var) %>%
        select(all_of(input$cor_vars)) %>%
        select(where(is.numeric))
      
      if (ncol(df) < 2) {
        plot.new()
        title("Not enough numeric variables selected.")
        return()
      }
      
      corr <- cor(df, use = "pairwise.complete.obs")
      
      # Get variable labels
      var_names <- colnames(corr)
      axis_labels <- ifelse(is.na(var_labels[var_names]), var_names, var_labels[var_names])
      
      ggcorrplot::ggcorrplot(
        corr,
        lab = TRUE,
        type = "lower",
        colors = c("#003399", "white", "#FF7300"),
        outline.color = "grey90",
        tl.cex = 10,
        lab_size = 3,
        tl.srt = 45,
        tl.col = "black"
      ) +
        scale_x_discrete(labels = axis_labels) +
        scale_y_discrete(labels = axis_labels)
    })
  }
  
  # Overview (without numbers)
  output$cor_DE <- render_cor_plot_nolabels("DE")
  output$cor_PL <- render_cor_plot_nolabels("PL")
  output$cor_SI <- render_cor_plot_nolabels("SI")
  
  # Zoom (with numbers)
  output$cor_DE_zoom <- render_cor_plot_labels("DE")
  output$cor_PL_zoom <- render_cor_plot_labels("PL")
  output$cor_SI_zoom <- render_cor_plot_labels("SI")
  
  # Modals
  observeEvent(input$zoom_DE, {
    showModal(modalDialog(
      title = "Germany",
      size = "l",
      plotOutput("cor_DE_zoom", height = "600px"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$zoom_PL, {
    showModal(modalDialog(
      title = "Poland",
      size = "l",
      plotOutput("cor_PL_zoom", height = "600px"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$zoom_SI, {
    showModal(modalDialog(
      title = "Slovenia",
      size = "l",
      plotOutput("cor_SI_zoom", height = "600px"),
      easyClose = TRUE
    ))
  })

  # Linear Model Heatmap
  
  # define tidy labels for models (package function tidy didn't work correctly)
  my_tidy <- function(model) {
    coefs <- summary(model)$coefficients
    as.data.frame(coefs) %>%
      tibble::rownames_to_column(var = "term") %>%
      dplyr::rename(
        estimate = Estimate,
        std.error = `Std. Error`,
        statistic = `t value`,
        p.value = `Pr(>|t|)`
      )
  }
  # Models with tidy + term_clean
  build_tidy_model <- function(model) {
    my_tidy(model) %>%
      mutate(term_clean = case_when(
        grepl("^as\\.factor\\(year\\)", term) ~ paste0("Year: ", gsub("as.factor\\(year\\)", "", term)),
        term %in% names(var_labels) ~ var_labels[term],
        TRUE ~ term
      ))
  }
  
  # Model_DE
  modelDE <- lm(trstep ~ age_group + gndr + eduyrs_winsor + stfgov + stfedu + stfhlth + trstlgl + euftf +
                  imwbcnt + ppltrst + rlgatnd_rev + polintr_rev + lrscale + as.factor(year),
                data = filter(dt_filtered, cntry == "DE"))
  tidyDE <- build_tidy_model(modelDE)
  
  # Model_PL
  modelPL <- lm(trstep ~ age_group + gndr + eduyrs_winsor + stfgov + stfedu + stfhlth + trstlgl + euftf +
                  imwbcnt + ppltrst + rlgatnd_rev + polintr_rev + lrscale + as.factor(year),
                data = filter(dt_filtered, cntry == "PL"))
  tidyPL <- build_tidy_model(modelPL)
 
  # Model_SI
  modelSI <- lm(trstep ~ age_group + gndr + eduyrs_winsor + stfgov + stfedu + stfhlth + trstlgl + euftf +
                  imwbcnt + ppltrst + rlgatnd_rev + polintr_rev + lrscale + as.factor(year),
                data = filter(dt_filtered, cntry == "SI"))
  tidySI <- build_tidy_model(modelSI)
  
  # Alle tidy-Modelle zusammenführen
  coef_all <- bind_rows(
    tidyDE %>% mutate(country = "DE"),
    tidyPL %>% mutate(country = "PL"),
    tidySI %>% mutate(country = "SI")
  ) %>%
    filter(term != "(Intercept)")
  
  # Gruppen zuordnen
  temp_category_vars <- category_vars
  names(temp_category_vars)[names(temp_category_vars) == "Socio-Demographics"] <- "Socio-\nDemographics"
  
  group_map <- purrr::imap_dfr(temp_category_vars, ~ tibble(term = var_labels[.x], group = .y))
  
  # Jahr-Effekte ergänzen
  year_terms <- unique(coef_all$term_clean[grepl("^Year: ", coef_all$term_clean)])
  group_map <- bind_rows(
    group_map,
    tibble(term = year_terms, group = "Year Effects")
  )
  
  # Heatmap-Daten vorbereiten
  data_heatmap <- reactive({
    coef_all %>%
      filter(term_clean %in% group_map$term) %>%
      select(term = term_clean, estimate, country) %>%
      left_join(group_map, by = "term") %>%
      mutate(
        term = factor(term, levels = rev(unique(group_map$term))),
        group = factor(group, levels = c(names(temp_category_vars), "Year Effects"))
      )
  })
  
  # Plot
  output$coef_heatmap <- renderPlot({
    ggplot(data_heatmap(), aes(x = country, y = term, fill = estimate)) +
      geom_tile(color = "white") +
      facet_grid(rows = vars(group), scales = "free_y", space = "free_y") +  # Gruppierung
      scale_x_discrete(position = "top") + 
      scale_fill_gradient2(low = "#003399", mid = "white", high = "#FF7300", midpoint = 0,
                           name = "Coefficient") +
      labs(
        title = "",
        x = NULL,, y = NULL
      ) +
      theme_minimal() +
      theme(
        strip.text.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12),
        legend.position = "right"
      )
  })
  
  # Table Popup input
  observeEvent(input$show_models, {
    showModal(modalDialog(
      title = "Linear Models per Country",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Dismiss"),
      fluidRow(
        column(4,
               h4("Germany"),
               tableOutput("model_table_de")
        ),
        column(4,
               h4("Poland"),
               tableOutput("model_table_pl")
        ),
        column(4,
               h4("Slovenia"),
               tableOutput("model_table_si")
        )
      )
    ))
  })
  
  output$model_table_de <- renderTable({
    tidyDE %>%
      select(term_clean, estimate, std.error, p.value) %>%
      mutate(across(where(is.numeric), ~ round(., 3))) %>%
      rename(
        Variable = term_clean,
        Estimate = estimate,
        `Std. Error` = std.error,
        `p-value` = p.value
      )
  })
  
  output$model_table_pl <- renderTable({
    tidyPL %>%
      select(term_clean, estimate, std.error, p.value) %>%
      mutate(across(where(is.numeric), ~ round(., 3))) %>%
      rename(
        Variable = term_clean,
        Estimate = estimate,
        `Std. Error` = std.error,
        `p-value` = p.value
      )
  })
  
  output$model_table_si <- renderTable({
    tidySI %>%
      select(term_clean, estimate, std.error, p.value) %>%
      mutate(across(where(is.numeric), ~ round(., 3))) %>%
      rename(
        Variable = term_clean,
        Estimate = estimate,
        `Std. Error` = std.error,
        `p-value` = p.value
      )
  })
   
  # Interaction Plots
  
  # Basis: Model 3a
  model3a <- lm(trstep ~ (ppltrst + euftf + stfedu + stfgov + stfhlth + trstlgl + imwbcnt + rlgatnd_rev + gndr + year + agea + I(agea^2) + lrscale + polintr_rev + eduyrs_winsor) * cntry,
                data = dt_filtered)
  
  
  # Explanations Interactions
  explanations <- list(
    euftf = tagList(
      h3("Effect of EU Unification Support on EU Trust"),
      ul(
        li("In all three countries, stronger support for EU unification is clearly associated with higher trust in the EU."),
        li("The relationship is statistically significant and aligns well with theoretical expectations —", strong("higher integration support boosts EU legitimacy.")),
        li("This pattern highlights the foundational role of pro-EU sentiment in fostering trust in EU institutions.")
      )
    ),
    stfgov = tagList(
      h3("Effect of Government Satisfaction on Trust in the EU by Country"),
      ul(
        li(strong("In Poland, satisfaction with the national government negatively affects trust in the EU"), ", which is a surprising and significant result."),
        li("This suggests that Polish respondents may see the national and EU levels as competing rather than complementary."),
        li("The effect differs from Germany and Slovenia, where satisfaction with national governance is either neutral or positively related to EU trust.")
      )
    ),
    lrscale = tagList(
      h3("Effect of Political Placement on Trust in the EU by Country"),
      ul(
        li("Political ideology influences EU trust differently across countries."),
        li("In Germany, respondents on the political right express lower trust in the EU, consistent with conservative patterns."),
        li("In contrast, in Slovenia, right-leaning individuals show higher trust in the EU, suggesting", strong("national political context strongly shapes this relationship."))
      )
    ),
    trstlgl = tagList(
      h3("Effect of Trust in Legal System on Trust in the EU by Country"),
      ul(
        li(strong("Trust in the legal system significantly enhances trust in the EU in all three countries.")),
        li("The effect is especially strong in Slovenia and Poland, emphasizing the importance of institutional confidence beyond national borders."),
        li("Legal trust appears to act as a general", strong("proxy for belief in rule-based governance"), ", extending to EU institutions.")
      ))
  )
  
  
  #interactionPlot
  output$interactionPlot <- renderPlot({
    selected_var <- input$selected_var
    
    interact_plot(model3a,
                  pred = !!selected_var,
                  modx = cntry,
                  plot.points = FALSE,
                  interval = TRUE,
                  x.label = var_labels[[selected_var]],
                  y.label = "Predicted EU Trust",
                  modx.labels = trust_labels,
                  colors = trust_colors,
                  legend.main = "Country") +
      modern_clean_theme() +
      theme(legend.position = "bottom")
    
  })
} 


# ----------- START SHINY APP -----------
shinyApp(ui = ui, server = server)


