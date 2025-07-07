
# Basics:
# install.packages("shiny")
library(shiny)
library(haven)
library(tidyverse)   
library(skimr)       
library(psych)       
library(labelled)
library(dplyr)
library(here)
library(tidyr)
library(broom)
library(gtools)


# Loading the data
dt_filtered <- read_sav(here("data", "ESS_filtered.sav")) 

# Define age_group as factor
dt_filtered <- dt_filtered %>%
  mutate(age_group = factor(
    age_group,
    levels = 1:7,
    labels = c("<18", "18–24", "25–34", "35–44", "45–54", "55–64", "64<")
  ))

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

# Defining Color Scheme
trust_colors <- c(
  "DE" = "#33658a",  # Germany
  "PL" = "#f6ae2d",  # Poland
  "SI" = "#f26419"   # Slovenia
)

# Defining Country Labels
trust_labels <- c(
  "DE" = "Germany",
  "PL" = "Poland",
  "SI" = "Slovenia"
)

# Defining the Categories of Variables
category_vars <- list(
  "Socio-Demographics" = c("age_group", "gndr", "eduyrs_winsor"),
  "Political Attitudes" = c("stfgov", "stfedu", "stfhlth", "trstlgl", "euftf"),
  "Ideology" = c("imwbcnt", "ppltrst", "rlgatnd_rev", "polintr_rev")
)

var_labels <- c(
  trstep        = "Trust in the EU",
  ppltrst       = "Trust in People",
  euftf         = "Attitude to EU Unification",
  stfedu        = "Satisfaction with State of Education",
  stfgov        = "Satisfaction with National Government",
  stfhlth       = "Satisfaction with State of Health Services ",
  trstlgl       = "Trust in Legal System",
  imwbcnt       = "Attitude to Immigrants",
  rlgatnd_rev   = "Religious Service Attendance",
  gndr          = "Gender",
  year          = "Year",
  age_group     = "Age Group",
  lrscale       = "Left–Right Scale",
  polintr_rev   = "Political Interest",
  eduyrs_winsor = "Years of Education"
)

#Add Trust in EU separately
main_var <- setNames("trstep", "Trust in the EU")

# Grouped variable choices
grouped_choices <- lapply(category_vars, function(vars) {
  stats::setNames(vars, var_labels[vars])
})

# Combine: ungrouped main_var first, then the grouped ones
final_choices <- c(main_var, grouped_choices)

#named_choices <- setNames(names(var_labels), var_labels)


# Defining List shortcut
ul <- tags$ul
li <- tags$li

# Defining the User Interface
ui <- fluidPage(
  
  # Tagging CSS 
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
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
  
  # Spacing under header so that content is not covered
  div(style = "height: 190px;"),
  
  # Content that is displayed depending on the tab
  uiOutput("tabContent")
)
  
 

# Defining the server logic 
server <- function(input, output) {
  
  output$tabContent <- renderUI({
    switch(input$tabs,
           
       # About
       "about" = tagList(
           div(class = "page-section",
                
                # One box
                div(class = "full-width-box",
                                      
                    fluidRow(
                    # Text left
                      column(6,
                           h2("About this Project"),
                           p("This project examines the factors influencing trust in the European Union across three distinct member states: Germany, Poland, and Slovenia. These countries represent different positions on the ranking of EU fundings: Germany as the largest net contributor, Poland as the largest net recipient, and Slovenia as a moderate, small-scale beneficiary."), 
                           p("We base the selection of these three countires on the Statista household evaluation of the year 2023 on the right. Underlying this selection is the assumption, that monetary connections between countries and the institution could influence the visibility of the EU within that country and that, in turn, this visibility could influence Trust in the European Parliament. This assumption, nevertheless, cannot be proven in this study and solely serves the purpose of selection of countries we look at."),
                           p("The focus lies on the investigation of how political attitudes, ideological orientations, and socio-demographic variables shape public trust in the European Parliament within each country. By comparing the three cases, we aim to uncover both country-specific and cross-national patterns. In particular, attention is paid to the interplay between the selected factors and how they collectively influence trust levels."),
                           p("The project features interactive graphs and visualizations that provide a clear and comparative view of the data and a transparent picture for each country. Our findings contribute to current research on public attitudes towards the EU, offering valuable insights for policymakers that seek to strengthen pro-EU sentiments and engagement."),
                           p("On a last note: In the further Analysis of this Project, we will use Trust in the European Parliament as an indicator for Trust in the EU and will use the two expressions interchangeably.")
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
                    p("The data used for this project is provided by the European Social Survey, a cross-national survey that has been conducted every two years since 2002. It contains open access data on public attitudes, beliefs and behavior. Covering themes of social, political and economic matters, it provides rich demographic and socio-economic variables on 39 European countries. Its high-quality data with cross-sectional samples allows for examining trends over time within and between countries. For our research purposes, we have used a personalized dataset on the three countries, with selected variables that fit our research question. We included information on all available years.")
                 ),
                
                div(class = "box-content",
                    h2("About Us"),
                    p("We are two students from the University of Cologne, studying in the B.Sc. Management, Economics and Social Sciences. As part of our final semester we developed this project study in the field of Data Science and Quantitative Methods, consisting of a statistical analysis, a data presentation on this dashboard and a final report. Our analysis has been made possible thanks to the supervision by Sebastian Weibels and Prof. Dr. Tom Zimmermann."),
                    p("For inquiries, feel free to contact us:"), 
                    p("Helena Zappe: hzappe@smail.uni-koeln.de", br(), "Marei Göbelbecker: mgoebelb@smail.uni-koeln.de")
                 )
              )
           )
       ),
      
      # Trust in the EU        
      "trust" = tagList(
          div(class = "page-section",
              
              h2("How much does people's trust in the European Parliament differ?"),
              
              p("This variable is coded on a scale from 0 (No trust at all) to 10 (Complete Trust).", br(), "Please note the y-intercept of the individual plots.", style = "margin-bottom: 30px;"),
            
              div(class = "full-width-box",
                  plotOutput("euTrustPlot", height = "350px")
               ),
              
              p("The main variable that we are looking at in this analysis is individuals trust in the European Parliament and its development over time. Therefore we investigate the development of the average trust levels in all three countries. Overall, average trust in the EU seems to be fairly similar in all three countries. They are all fluctuating around a mean of 4.2. Therefore, we take a closer look at the individual evolutions of trust over the years, which show clear differences."),
              p("Germany was the most stable over the years with constant ups and downs. Its lowest point was reached in 2010 and its peak in 2022. Poland has started off as the country with the highest trust in the EU in 2002, but has experienced a severe decline in trust in 2012 until 2014 when it had reached its lowest. After that trust rose again until 2018, after which it has experienced another drop, such that at the end of 2022 it was on a level much lower than the other two countries. Slovenia shows the strongest fluctuations in mean trust in the EU over time, with a sharp decline between its highest level of 5.0 in 2006 to its lowest point of 3.4 in 2014. Since then, trust rose again, such that in 2022 it was only slightly lower than the German trust level."),
              p("These differences in the evolution of Trust in the EU points towards the existence of a multitude of factors which shape Trust. These factors are expected to influence individuals differently dependending on which country they live in. Such relationships will be analysed further in this project.", style = "margin-bottom: 30px;"),
              
              
              div(class = "full-width-box",
                  selectInput("selected_year", "Select Year:", choices = c("All Years (Average)" = "all", sort(unique(dt_filtered$year)))),
                  plotOutput("euTrustPlot2", height = "350px")
              ),
              
              p("To start our further analysis we were interested in whether there were clear patterns in trust levels when dividing by age groups. On average over the years, trust in the EU was highest among people younger than 18 in all three countries, with respondents from Germany having been the most, and respondents from Poland being the least trusting. In the higher age groups in Germany and Slovenia there was a general downward trend until the age of 65, from which onward trust levels were again slightly higher. Germany's levels are slightly higher than in Slovenia in all age groups. The trust of Polish individuals seems to be more stable in all age groups, which means it was by far the lowest in the younger age groups but highest in the ages over 45."),
              
              p("It is then interesting to look at this aspect in the different ESS rounds. Doing so this general pattern is still visible even though in different magnitudes. We will further investigate this at a later point in the analysis.")
          )
       ),
      
      # Political Context 
       "context"  = tagList(
         div(class = "page-section",
             
             h2("What is the political context of the EU from 2002 to 2022?"),
             p("In this section we want to provide some background knowledge which is not found in the ESS Data but provides an important basis for analysis. Even though we will not express the connection of individual findings to these events, they are part of why attitudes and trust can change.", style = "margin-bottom: 30px;"),
             
             div(class = "full-width-box",
                 h3("Main Political Events in the EU:"),
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
             
             h2("What distinguishes the three countries?"),
             
             p("While Germany has been one of the founding members of the EU and has therefore been part since 1958, Poland and Slovenia joined the Institution in 2004. Therefore, in the beginning of our time frame, the two countries have not even been part of the EU yet.", br(), "All three countries are Parliamentary Democracies, even though of different forms.", br(), "Poland's and Slovenia's GDP per Capita are both below the EU average. Poland makes up 4.4% of the EU's total GDP, while Slovenia's part makes up 0.4%. Germany's GDP per capita is well above the EU average and has the largest part of the EU's GDP with 24.2%. This shows the sizes of the economic force of the countries.", br(), "All three countries benefit from being part of the EU by receiving funding for individual projects and open borders and trade."),
             
             p("An additional baseline is provided by respondents' political self-placement and voting behavior within the CSS dataset. Voting behavior reveals a notable cross-country difference: respondents in Germany appear more politically engaged, with significantly higher voting participation compared to those in Poland and Slovenia. Regarding ideological orientation, political self-placement across all three countries tends to cluster around the center of the left-right scale (value 5). However, national trends are clear—respondents in Germany lean as far left, as Polish orientations lean to the right, while Slovenia is though less extreme, also clearly showing a clear leftist tendency.", style = "margin-bottom: 30px;"),
             
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
             )

         )
       ),
           
       # Insights
       "insights"  = tagList(
         div(class = "page-section",
             h2("Which general insights can be differed by looking at individual factors influencing trust in the EU?"),
             
             p("In order to get an overview of individual influences, you can investigate the variables within our categories of analysis here."),
             
             selectInput("insight_category", "Select Category:",
                         choices = names(category_vars),
                         selected = names(category_vars)[1]),
             
             uiOutput("insight_plots")
         )
       ),
        
      # Cross-Country Comparison
       "comparison"  = tagList(
         div(class = "page-section",
             h2("How big are the influences on trust in the EU and how do they differ between the countries?"),
             p("This page presents key insights into the cross-country differences, which is the core of our research questions. Interactive plots of interaction effects, alongside tables of regression outputs, highlight the most striking contrasts between Germany, Poland and Slovenia, as well as the most influential predictors of trust in the EU within each country.
We estimated a multiple linear regression model that includes all predictor variables from the “Insights” tab and interacted each with the variable “country” to allow for cross-country comparisons. In this model, Germany serves as the baseline.", style = "margin-bottom: 30px;"),
             
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
                 `selected-text-format` = "count > 3",
                 `style` = "btn-light"
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
             
             p("Across all three countries, the correlations range from -0.1 to 0.5. Moderate correlations are observed between the various satisfaction variables. These include trust in the legal system and satisfaction with the state of health services, the state of education, and the national government. The consistent clustering of these variables across countries suggest that they reflect a general sense of satisfaction with national institutions."),
             p("In most cases, correlation coefficients do not exceed 0.5, indicating moderate associations. An exception is observed in Slovenia, where satisfaction with education and healthcare services have a correlation of 0.54."),
             p("Importantly, the correlations between these predictors and trust in the EU are relatively stronger across all three countries. This supports the relevance and empirical justification for including these variables in the regression models.", style = "margin-bottom: 30px;"),

 
             h3("Key Interaction Effects"),
             
             p("The interaction effect plots visualize how the relationship between a selected predictor and trust in the EU varies across countries. Each colored line represents one country and shows how EU trust changes as the predictor changes. Only the most meaningful and significant interactions are shown.
The shaded areas around each line represent 95% confidence intervals, indicating uncertainty around the prediction. Wider bands imply less precise estimates and narrower bands indicate higher certainty.
Use the dropdown menu to explore different interaction effects and understand how specific factors shape EU trust differently across the three countries.", style = "margin-bottom: 30px;"),
             
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
                          plotOutput("interactionPlot", height = "300px")), 
                   # Text right
                   column(6,
                          output$dynamicText <- renderUI({
                            selected_var <- input$selected_var
                            explanations[[selected_var]]
                          })
                   )
                 )
             ),
             
             h3("All Interaction Effects"),
             
             p("Besides the pooled multiple linear regression model including a country interaction, we estimated the same model for each country, leaving out the interaction. The findings of these models let us gain insights into what shapes EU trust in each country, highlighting the most significant predictors. These tables represent the regression output for Slovenia, Poland and Germany.", style = "margin-bottom: 30px;"),

             # Fixed Effects Models
             div(class = "full-width-box",
                 h3("Effects of Variables in each Country"),
                 fluidRow(
                   column(4,
                          h3("Germany"),
                          div(style = "font-size: 11px;", tableOutput("Model_DE"))),
                   column(4,
                          h3("Poland"),
                          div(style = "font-size: 11px;", tableOutput("Model_PL"))),
                   column(4,
                          h3("Slovenia"),
                          div(style = "font-size: 11px;", tableOutput("Model_SI"))),
                 ),
                 plotOutput("coefPlot", height = "600px")
             ),
             
             h4("Main Effects in Germany"),
             p("In Germany, EU trust is most strongly predicted by satisfaction with the national government, gender, and trust in the legal system. Females tend to have higher EU trust. Age has a stronger effect in Germany than in the other two countries. Older people are significantly less trusting of the EU. As expected, higher support for EU unification is positively associated with EU trust. Temporal effects show that trust dropped notably in 2014 and 2016 compared to the baseline 2002."),
             
             h4("Main Effects in Poland"),
             p("In Poland, trust in the EU is most strongly shaped by support for EU unification, trust in the legal system, and political interest. Interestingly, higher satisfaction with the national government is associated with lower EU trust which suggests a substitution effect where citizens may either trust the EU or their national government, but not both. Being female, having more positive attitudes toward immigrants, and holding more left-leaning political views are also associated with higher EU trust, even though these effects are smaller. Over time, EU trust declined significantly, especially in 2014, 2016, 2020, and 2022, even after controlling for individual attitudes and demographics. Despite higher religious attendance in Poland compared to the other countries, it has only a small but statistically significant negative effect on EU trust."),
             
             h4("Main Effects in Slovenia"),
             p("In Slovenia, the strongest predictors are trust in the legal system, satisfaction with the national government, EU support, gender and political interest. Among these, institutional trust and political interest seem particularly influential. EU trust declined after 2002, especially in 2018. Unlike in Poland, religious attendance is weakly but positively associated with higher trust in the EU, even though the effect is small and less significant. Immigration attitudes have a smaller and less significant impact compared to Poland. Additionally, older people tend to have less trust in the EU, suggesting a decline in EU trust with increasing age."),
             
             
         )
       ),

       # Implications
       "implications"  = tagList(
         div(class = "page-section",
             h2("What do the findings imply for the EU?"),
             
             p("Dummy Text"),
             
             div(class = "flex-two-boxes",
                 div(class = "box-content",
                     p("dummy text")
                 ),
                 div(class = "box-content",
                     p("dummy text")
                 )
             )
             
             
         )
       )
       
    )
  })

  
# Trust in EU: 
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
      # Nur filtern, wenn ein bestimmtes Jahr ausgewählt wurde
      { if (input$selected_year != "all") filter(., year == input$selected_year) else . } %>%
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
      ) %>%
      filter(!is.na(age_group)) %>%
      group_by(age_group, cntry) %>%
      summarise(trstep = mean(trstep, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = age_group, y = trstep, fill = cntry)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = trust_colors, labels = trust_labels) + 
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
  
# Context:
  # lrscalePlot
  output$lrscalePlot <- renderPlot({
    dt_filtered %>%
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
  
# Insights:
  # insight_plots
  output$insight_plots <- renderUI({
    switch(input$insight_category,
           
           "Ideology" = tagList(
             div(class = "full-width-box",
                 h3("Key Findings"),
                 ul(
                   li(strong("Immigration attitudes reveal a surprising pattern:"), "Polish respondents appear the most welcoming toward immigrants across age groups, despite their more conservative political orientation. Slovenian respondents are the least welcoming."),
                   li(strong("Trust in others is lowest in Poland:"), "Germany shows the highest interpersonal trust, while Poland’s trust dropped sharply in 2020."),
                   li(strong("Religious attendance is highest in Poland:"), "Poland has a much more religious population, while Germany reports the lowest levels of attendance. Slovenia falls in the middle with a more even distribution."),
                   li(strong("Political interest is highest in Germany:"), "Respondents in Germany show consistently higher political interest across all age groups. Interes in Poland is slightly higher than Slovenia, particularly in younger and older age groups."),
                   li(strong("Attitudes toward LGBTQ+ rights are most liberal in Germany:"), "German respondents are more likely to agree that gay and lesbian people are free to live as they wish. Polish and Slovenian respondents are quite neutral (around 3), indicating mild or uncertain support. All countries show a positive trend across younger age groups.")
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
                 div(class = "box-content", plotOutput("plot_freehms_rev", height = "350px"))
             )
             ),
           
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
           
           "Political Attitudes" = tagList(
             div(class = "full-width-box",
                 h3("Key Findings"),
                 ul(
                   li(strong("Satisfaction with national governments fluctuates over time:"), "German respondents were especially dissatisfied in 2002, Polish in 2020, and Slovenians in 2012. The period between 2010–2016 shows the greatest disparity between countries."),
                   li(strong("Education satisfaction is surprisingly lowest in Germany:"), "Slovenia shows the highest and most consistent satisfaction, while Poland saw a sharp drop in 2020."),
                   li(strong("Health system satisfaction is lowest in Poland:"), "Polish respondents consistently report the lowest satisfaction, with a dramatic drop below 3 in 2020. Germany peaked around 2016 but declined afterwards."),
                   li(strong("Trust in the legal system is highest in Germany:"), "Germany shows a steady upward trend. Slovenia shows recent improvement, while Poland saw a significant drop in 2020."),
                   li(strong("Support for further EU unification is broadly positive:"), "Respondents in all three countries generally want the EU to move forward, with Poland showing the strongest support. After 2014, all countries followed a similar upward trend, though support declined again slightly after 2020.")
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
                 div(class = "box-content", plotOutput("plot_stfgov", height = "350px"))
             ),
             div(class = "flex-two-boxes",
                 div(class = "box-content", plotOutput("plot_stfedu", height = "350px")),
                 div(class = "box-content", plotOutput("plot_stfhlth", height = "350px"))
             ),
             div(class = "flex-two-boxes",
                 div(class = "box-content", plotOutput("plot_trstlgl", height = "350px")),
                 div(class = "box-content", plotOutput("plot_euftf", height = "350px"))
             )
           )
    )
  })
  
  
  # plot_age_group
  output$plot_age_group <- renderPlot({
    dt_filtered %>%
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
      ) %>%
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
        title = paste("Age Group Distribution in", input$selected_year),
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
        values = c("1" = "#33658a", "2" = "#f6ae2d"),
        labels = c("1" = "Male", "2" = "Female")
      ) +
      labs(
        title = paste("Relative Distribution of Gender by Country in", input$selected_year),
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
        title = paste("Average Education Years by Country in", input$selected_year),
        x = "Country",
        y = "Years of Education",
        fill = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  
  # plot_stfgov
  output$plot_stfgov <- renderPlot({
    dt_filtered %>%
      filter(!is.na(stfgov), !is.na(year)) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      group_by(cntry, year) %>%
      summarise(mean_stfgov = mean(stfgov, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_stfgov, color = cntry, group = cntry)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(dt_filtered$year, na.rm = TRUE),
                                      max(dt_filtered$year, na.rm = TRUE),
                                      by = 4)) +
      scale_color_manual(
        values = trust_colors,
        labels = trust_labels
      ) +
      labs(
        title = "Average Satisfaction with National Government \nby Country over Time",
        x = "Year",
        y = "Satisfaction (0–10 scale)",
        color = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  # plot_stfedu
  output$plot_stfedu <- renderPlot({
    dt_filtered %>%
      filter(!is.na(stfedu), !is.na(year)) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      group_by(cntry, year) %>%
      summarise(mean_stfedu = mean(stfedu, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_stfedu, color = cntry, group = cntry)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(dt_filtered$year, na.rm = TRUE),
                                      max(dt_filtered$year, na.rm = TRUE),
                                      by = 4)) +
      scale_color_manual(
        values = trust_colors,
        labels = trust_labels
      ) +
      labs(
        title = "Average Satisfaction with State of Education \nby Country over Time",
        x = "Year",
        y = "Satisfaction (0–10 scale)",
        color = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  # plot_stfhlth
  output$plot_stfhlth <- renderPlot({
    dt_filtered %>%
      filter(!is.na(stfhlth), !is.na(year)) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      group_by(cntry, year) %>%
      summarise(mean_stfhlth = mean(stfhlth, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_stfhlth, color = cntry, group = cntry)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(dt_filtered$year, na.rm = TRUE),
                                      max(dt_filtered$year, na.rm = TRUE),
                                      by = 4)) +
      scale_color_manual(
        values = trust_colors,
        labels = trust_labels
      ) +
      labs(
        title = "Average Satisfaction with State of Health Services \nby Country over Time",
        x = "Year",
        y = "Satisfaction (0–10 scale)",
        color = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  # plot_trstlgl
  output$plot_trstlgl <- renderPlot({
    dt_filtered %>%
      filter(!is.na(trstlgl), !is.na(year)) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      group_by(cntry, year) %>%
      summarise(mean_trstlgl = mean(trstlgl, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_trstlgl, color = cntry, group = cntry)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(dt_filtered$year, na.rm = TRUE),
                                      max(dt_filtered$year, na.rm = TRUE),
                                      by = 4)) +
      scale_color_manual(
        values = trust_colors,
        labels = trust_labels
      ) +
      labs(
        title = "Average Trust in the Legal System \nby Country over Time",
        x = "Year",
        y = "Trust (0–10 scale)",
        color = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  # plot_euftf
  output$plot_euftf <- renderPlot({
    dt_filtered %>%
      filter(!is.na(euftf), !is.na(year)) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      group_by(cntry, year) %>%
      summarise(mean_euftf = mean(euftf, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_euftf, color = cntry, group = cntry)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(dt_filtered$year, na.rm = TRUE),
                                      max(dt_filtered$year, na.rm = TRUE),
                                      by = 4)) +
      scale_color_manual(
        values = trust_colors,
        labels = trust_labels
      ) +
      labs(
        title = "Average Support for EU Unification \nby Country over Time",
        x = "Year",
        y = "Satisfaction (0–10 scale)",
        color = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  # plot_imwbcnt
  output$plot_imwbcnt <- renderPlot({
    dt_filtered %>%
      filter(!is.na(imwbcnt), !is.na(year)) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      group_by(cntry, year) %>%
      summarise(mean_imwbcnt = mean(imwbcnt, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_imwbcnt, color = cntry, group = cntry)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(dt_filtered$year, na.rm = TRUE),
                                      max(dt_filtered$year, na.rm = TRUE),
                                      by = 4)) +
      scale_color_manual(
        values = trust_colors,
        labels = trust_labels
      ) +
      labs(
        title = "Average Attitude towards Immigrants \nby Country over Time",
        x = "Year",
        y = "Immigration Attitude (0–10 Scale)",
        color = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  
  # plot_ppltrst
  output$plot_ppltrst <- renderPlot({
    dt_filtered %>%
      filter(!is.na(ppltrst), !is.na(year)) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      group_by(cntry, year) %>%
      summarise(mean_ppltrst = mean(ppltrst, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_ppltrst, color = cntry, group = cntry)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(dt_filtered$year, na.rm = TRUE),
                                      max(dt_filtered$year, na.rm = TRUE),
                                      by = 4)) +
      scale_color_manual(
        values = trust_colors,
        labels = trust_labels
      ) +
      labs(
        title = "Average Trust in People \nby Country over Time",
        x = "Year",
        y = "Trust in People (0–10 Scale)",
        color = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  # plot_rlgatnd_rev
  output$plot_rlgatnd_rev <- renderPlot({
    dt_filtered %>%
      filter(!is.na(rlgatnd_rev), !is.na(year)) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      group_by(cntry, year) %>%
      summarise(mean_rlgatnd_rev = mean(rlgatnd_rev, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_rlgatnd_rev, color = cntry, group = cntry)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(dt_filtered$year, na.rm = TRUE),
                                      max(dt_filtered$year, na.rm = TRUE),
                                      by = 4)) +
      scale_color_manual(
        values = trust_colors,
        labels = trust_labels
      ) +
      labs(
        title = "Average Attendance of Religious Services apart from \nSpecial Occasions by Country over Time",
        x = "Year",
        y = "Attendance Frequency (0–7 scale)",
        color = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })

  # plot_polintr_rev
  output$plot_polintr_rev <- renderPlot({
    dt_filtered %>%
      filter(!is.na(polintr_rev), !is.na(year)) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      group_by(cntry, year) %>%
      summarise(mean_polintr_rev = mean(polintr_rev, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_polintr_rev, color = cntry, group = cntry)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(dt_filtered$year, na.rm = TRUE),
                                      max(dt_filtered$year, na.rm = TRUE),
                                      by = 4)) +
      scale_color_manual(
        values = trust_colors,
        labels = trust_labels
      ) +
      labs(
        title = "Average Political Interest \nby Country over Time",
        x = "Year",
        y = "Political Interest (1-4 Scale)",
        color = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })
  
  # plot_freehms_rev
  output$plot_freehms_rev <- renderPlot({
    dt_filtered %>%
      filter(!is.na(freehms_rev), !is.na(year)) %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      group_by(cntry, year) %>%
      summarise(mean_freehms_rev = mean(freehms_rev, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_freehms_rev, color = cntry, group = cntry)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(dt_filtered$year, na.rm = TRUE),
                                      max(dt_filtered$year, na.rm = TRUE),
                                      by = 4)) +
      scale_color_manual(
        values = trust_colors,
        labels = trust_labels
      ) +
      labs(
        title = "Average Attitude towards Gays and Lesbians \nby Country over Time",
        x = "Year",
        y = "Attitudes towards Homosexuals (1-5 Scale)",
        color = "Country"
      ) +
      modern_clean_theme() +
      theme(legend.position = "bottom")
  })

# Cross-Country Comparison
  # Model_DE
  modelDE <- lm(trstep ~ ppltrst + euftf + stfedu + stfgov + stfhlth + trstlgl + imwbcnt + rlgatnd_rev + gndr + as.factor(year) + age_group + lrscale + polintr_rev + eduyrs_winsor,
                 data = subset(dt_filtered, cntry == "DE"))
  
  output$Model_DE <- renderTable({
    tidy(model_DE) %>%
      mutate(
        term = ifelse(term %in% names(var_labels), var_labels[term], term),
        estimate = round(estimate, 3),
        std.error = round(std.error, 3),
        statistic = round(statistic, 3),
        p.value = round(p.value, 3),
        signif = stars.pval(p.value)  # neue Spalte mit *, **, ***
      ) %>%
      select(
        Variable = term,
        Estimate = estimate,
        `Std. Error` = std.error,
        `t-value` = statistic,
        `p-value` = p.value,
        `Signif.` = signif
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # Model_PL
  modelPL <- lm(trstep ~ ppltrst + euftf + stfedu + stfgov + stfhlth + trstlgl + imwbcnt + rlgatnd_rev + gndr + as.factor(year) + age_group + lrscale + polintr_rev + eduyrs_winsor,
                 data = subset(dt_filtered, cntry == "PL"))
  
  
  output$Model_PL <- renderTable({
    tidy(model_PL) %>%
      mutate(
        term = ifelse(term %in% names(var_labels), var_labels[term], term),
        estimate = round(estimate, 3),
        std.error = round(std.error, 3),
        statistic = round(statistic, 3),
        p.value = round(p.value, 3),
        signif = stars.pval(p.value)  # neue Spalte mit *, **, ***
      ) %>%
      select(
        Variable = term,
        Estimate = estimate,
        `Std. Error` = std.error,
        `t-value` = statistic,
        `p-value` = p.value,
        `Signif.` = signif
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # Model_SI
  modelSI <- lm(trstep ~ ppltrst + euftf + stfedu + stfgov + stfhlth + trstlgl + imwbcnt + rlgatnd_rev + gndr + as.factor(year) + age_group + lrscale + polintr_rev + eduyrs_winsor,
                 data = subset(dt_filtered, cntry == "SI"))
  
  output$Model_SI <- renderTable({
    tidy(model_SI) %>%
      mutate(
        term = ifelse(term %in% names(var_labels), var_labels[term], term),
        estimate = round(estimate, 3),
        std.error = round(std.error, 3),
        statistic = round(statistic, 3),
        p.value = round(p.value, 3),
        signif = stars.pval(p.value)  # neue Spalte mit *, **, ***
      ) %>%
      select(
        Variable = term,
        Estimate = estimate,
        `Std. Error` = std.error,
        `t-value` = statistic,
        `p-value` = p.value,
        `Signif.` = signif
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  library(broom)
  library(dplyr)
  
  # Funktion für ein Modell + tidy output
  get_model_coef <- function(data, country_name) {
    model <- lm(trstep ~ ppltrst + euftf + stfedu + stfgov + stfhlth + trstlgl +
                  imwbcnt + rlgatnd_rev + gndr + as.factor(year) +
                  age_group + lrscale + polintr_rev + eduyrs_winsor,
                data = filter(data, cntry == country_name))
    
    tidy(model) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        country = country_name,
        term = ifelse(term %in% names(var_labels), var_labels[term], term)
      )
  }
  
  # Coef-Dataframes pro Land
  coef_DE <- get_model_coef(dt_filtered, "DE")
  coef_PL <- get_model_coef(dt_filtered, "PL")
  coef_SI <- get_model_coef(dt_filtered, "SI")
  
  # Kombiniert
  coef_all <- bind_rows(coef_DE, coef_PL, coef_SI)
  
  output$coefPlot <- renderPlot({
    ggplot(coef_all, aes(x = estimate, y = reorder(term, estimate), color = country)) +
      geom_point(position = position_dodge(width = 0.6), size = 2.5) +
      geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                         xmax = estimate + 1.96 * std.error),
                     position = position_dodge(width = 0.6),
                     height = 0.2) +
      facet_wrap(~ country) +
      labs(
        title = "Effect Sizes by Country",
        x = "Coefficient Estimate (± 95% CI)",
        y = NULL,
        color = "Country"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })
  
  # Interaction Plots
  # Model 3a
  model3a <- lm(trstep ~ (ppltrst + euftf + stfedu + stfgov + stfhlth + trstlgl + imwbcnt + rlgatnd_rev + gndr + year + agea + I(agea^2) + lrscale + polintr_rev + eduyrs_winsor) * cntry,
                data = dt_filtered)
  
  # Explanations Interactions
  explanations <- list(
    euftf = tagList(
      h3("Effect of EU Unification Support on EU Trust"),
      ul(
        li("In all three countries, stronger support for EU unification is clearly associated with higher trust in the EU."),
        li("The relationship is statistically significant and aligns well with theoretical expectations — higher integration support boosts EU legitimacy."),
        li("This pattern highlights the foundational role of pro-EU sentiment in fostering trust in EU institutions.")
      )
    ),
    stfgov = tagList(
      h3("Effect of Government Satisfaction on Trust in the EU by Country"),
      ul(
        li("In Poland, satisfaction with the national government negatively affects trust in the EU, which is a surprising and significant result."),
        li("This suggests that Polish respondents may see the national and EU levels as competing rather than complementary."),
        li("The effect differs from Germany and Slovenia, where satisfaction with national governance is either neutral or positively related to EU trust.")
      )
    ),
    lrscale = tagList(
      h3("Effect of Political Placement on Trust in the EU by Country"),
      ul(
        li("Political ideology influences EU trust differently across countries."),
        li("In Germany, respondents on the political right express lower trust in the EU, consistent with conservative patterns."),
        li("In contrast, in Slovenia, right-leaning individuals show higher trust in the EU, suggesting national political context strongly shapes this relationship.")
      )
    ),
    trstlgl = tagList(
      h3("Effect of Trust in Legal System on Trust in the EU by Country"),
      ul(
        li("Trust in the legal system significantly enhances trust in the EU in all three countries."),
        li("The effect is especially strong in Slovenia and Poland, emphasizing the importance of institutional confidence beyond national borders."),
        li("Legal trust appears to act as a general proxy for belief in rule-based governance, extending to EU institutions.")
      )
    )
  )
  
  
  #interactionPlot
  #install.packages("future", dependencies = TRUE, type = "binary")
  library(interactions)
  
  output$interactionPlot <- renderPlot({
    selected_var <- input$selected_var
    
    interact_plot(model3a,
                  pred = !!selected_var,
                  modx = cntry,
                  plot.points = FALSE,
                  interval = TRUE,
                  main.title = paste("Effect of", var_labels[[selected_var]], "on Trust in EU"),
                  x.label = var_labels[[selected_var]],
                  y.label = "Predicted EU Trust") +
      modern_clean_theme() +
      scale_color_manual(values = trust_colors, labels = trust_labels) +
      scale_fill_manual(values = trust_colors, labels = trust_labels)
      
  })
  
  # Correlation Matrixes
  
  library(shinyWidgets)
  library(ggcorrplot)
  library(rlang)
  
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
        colors = c("#33658a", "white", "#f26419"),
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
        colors = c("#33658a", "white", "#f26419"),
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
      title = "Germany – Correlation Matrix",
      size = "l",
      plotOutput("cor_DE_zoom", height = "600px"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$zoom_PL, {
    showModal(modalDialog(
      title = "Poland – Correlation Matrix",
      size = "l",
      plotOutput("cor_PL_zoom", height = "600px"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$zoom_SI, {
    showModal(modalDialog(
      title = "Slovenia – Correlation Matrix",
      size = "l",
      plotOutput("cor_SI_zoom", height = "600px"),
      easyClose = TRUE
    ))
  })

  
  
  
  
}






# Run the application 
shinyApp(ui = ui, server = server)


