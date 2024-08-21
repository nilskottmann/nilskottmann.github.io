# Libraries ####
library(shiny)
library(tidyverse)
library(gt) # better tables
library(ggtext) # better titles and captions for graphs
library(cols4all) # better color choices with accessible palettes

# Prep ####

# Theme
default_theme <-  theme_minimal() +
    theme(
        axis.text.y = element_text(size = 8),
        legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_textbox_simple(hjust = 0, padding = margin(2, 2, 10, 0)),
        plot.subtitle = element_textbox_simple(padding = margin(2, 2, 5.5, 0)),
        plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.margin = margin(20, 20, 20, 20)
    )

# Colors
names <- c("ISK","Depot")
colors <-  cols4all::c4a(palette = "met.egypt", n = length(names), type = "cat") 
account_cols <- setNames(colors, names)

rm(names,colors)

# Get the statslåneränta on Nov 30th and +1%-unit for the following year's taxrate (minimum is 1,25%)

csv_url <- "https://www.riksgalden.se/globalassets/dokument_sve/statslaneranta/slr-historisk-statslaneranta-csv.csv"

rates_hist_tmp <- readr::read_csv2(csv_url, 
                                   locale = locale(encoding = "latin1", decimal_mark = ","),
                                   col_select = c(1,2),
                                   col_types = "Dc"
                                   )

rates_hist <- rates_hist_tmp %>% 
    rename(
        date = 1,
        rates = 2
    ) %>% 
    mutate(
        year = year(date),
        date = as_date(date),
        rates = as.numeric(str_replace(rates,"\\,","."))/100
    )

rates <- rates_hist %>% 
    group_by(year) %>% 
    filter(month(date) == 11 & day(date) <= 30) %>% 
    slice_head() %>% 
    ungroup() %>% 
    mutate(
        tax_rate = case_when(rates < 0.0025 ~ 0.0125, TRUE ~ rates + 0.01) * 0.3,
        tax_year = year + 1
    ) %>% 
    select(tax_year,tax_rate)


# Define UI ####
ui <- fluidPage(
    
    # Input panel to hold all input controls
    inputPanel(
        # Input: Slider for average expected return (in %)
        sliderInput("avg_return",
                    "Expected Average Return (%)",
                    min = -5,
                    max = 20,
                    value = 5,
                    step = 0.1),
        
        # Input: Numeric input for starting amount
        numericInput("start_amount",
                     "Starting Amount (SEK)",
                     value = 100000,
                     min = 0,
                     step = 100000),
        
        # Input: Slider for starting year
        sliderInput("start_year",
                    "The first year of the investment",
                    min = 2012,
                    max = 2024,
                    value = 2020,
                    step = 1),
        
        # Input: Slider for interest rate (in %)
        sliderInput("interest_rate",
                    "Expected Future Government Borrowing Rate (%)",
                    min = 0,
                    max = 10,
                    value = 2,
                    step = 0.1),
        
        # Input: Slider for number of years to consider
        sliderInput("investment_horizon",
                    "Number of years of the investment",
                    min = 1,
                    max = 30,
                    value = 10,
                    step = 1)
        
    ),
    
    # Main panel to display results
    mainPanel(
        # Output: Table displaying the comparison
        tableOutput("results_table"),
        fluidRow(
            column(6,   # Width of the first column (out of 12)
                   plotOutput("expected_returns")),
            column(6,   # Width of the second column (out of 12)
                   plotOutput("expected_bor_rate"))
        )
    )
)


# Define server logic ####
server <- function(input, output) {
    
    # Reactive expression to calculate the results
    annual_values <- reactive({
        start_amount <- input$start_amount
        avg_return <- input$avg_return / 100
        interest_rate <- input$interest_rate / 100
        investment_horizon <- input$investment_horizon
        start_year <- input$start_year
        
        # Create a data frame with annual values
        annual_values <- data.frame(
            year = seq(start_year,start_year + investment_horizon , by = 1)
        ) %>% 
            left_join(
                rates,
                by = join_by(year == tax_year)
            ) %>% 
            mutate(tax_rate = case_when(is.na(tax_rate) ~ case_when(interest_rate < 0.0025 ~ 0.0125*0.3, TRUE ~ (interest_rate + 0.01)*0.3) , TRUE ~ tax_rate))
        
        # Update expected values each year
        annual_values <- annual_values %>%
            mutate(isk = start_amount * (1 + avg_return) ^ (year - start_year)) %>% 
            mutate(depot = isk) %>%
            mutate(
                isk_tax = case_when(year == start_year ~ isk * tax_rate, TRUE ~ (isk + lag(isk)/2) * tax_rate),
                depot_tax = case_when(year == start_year+investment_horizon ~ (depot-start_amount) * (0.3))
            )
        return(annual_values)
    })
    
    # Reactive expression to calculate the results
    results <- reactive({
        annual_data <- annual_values()
        
        # Summarize ISK values
        isk_results <- annual_data %>%
            summarize(
                start_value = first(isk),
                final_value = last(isk),
                tax = sum(isk_tax, na.rm = TRUE)
            ) %>%
            mutate(
                net_return = final_value - start_value - tax,
                net_return_rate = (final_value - start_value - tax) / start_value,
                CAGR = ((final_value - tax) / start_value) ^ (1 / input$investment_horizon) - 1,
                account = "ISK"
            )
        
        # Summarize Depot values
        depot_results <- annual_data %>%
            summarize(
                start_value = first(depot),
                final_value = last(depot),
                tax = sum(depot_tax, na.rm = TRUE)
            ) %>%
            mutate(
                net_return = final_value - start_value - tax,
                net_return_rate = (final_value - start_value - tax) / start_value,
                CAGR = ((final_value - tax) / start_value) ^ (1 / input$investment_horizon) - 1,
                account = "Depot"
            )
        
        bind_rows(isk_results, depot_results)
        
    })
    
    results_table <- reactive({
        table <- results()  
        
        table %>% 
            gt() %>%
            cols_move_to_start(columns = "account") %>%
            fmt_number(decimals = 0) %>%
            fmt_percent(columns = c("net_return_rate","CAGR"), decimals = 1) %>%
            cols_label(
                start_value = "Initial Investment",
                final_value = "Final Value",
                net_return_rate = "Net Return Rate",
                net_return = "Net Return",
                tax = "Taxes Paid",
                account = "Account"
            )
    })
    
    output$expected_returns <- renderPlot({
        exp_ret <- seq(-0.05, 0.2, by = 0.01)
        start_amount <- input$start_amount
        avg_return <- input$avg_return / 100
        interest_rate <- input$interest_rate / 100
        investment_horizon <- input$investment_horizon
        start_year <- input$start_year
        
        annual_data <- annual_values()
        
        annual_values_return <- annual_data %>%
            mutate(isk = case_when(
                year == start_year ~ start_amount,
                TRUE ~ start_amount * (1 + avg_return) ^ (year - start_year)
            )) %>%
            mutate(depot = isk) %>%
            mutate(
                isk_tax = case_when(year == start_year ~ isk * tax_rate, TRUE ~ (isk + lag(isk) /2) * tax_rate),
                depot_tax = case_when(year == start_year + investment_horizon ~ (depot - start_amount) * (0.3))
            ) %>%
            cross_join(exp_ret, copy = TRUE) %>%
            rename(exp_ret = y) %>%
            mutate(isk = start_amount * (1 + exp_ret) ^ (year - start_year)) %>%
            mutate(depot = isk) %>%
            mutate(
                isk_tax = case_when(year == start_year ~ isk * tax_rate, TRUE ~ (isk + lag(isk) /
                                                                                     2) * tax_rate),
                depot_tax = case_when(
                    year == start_year + investment_horizon ~ (depot - start_amount) * (0.3)
                )
            )
        
        return_results <- annual_values_return %>%
            group_by(exp_ret) %>%
            summarize(
                ISK = last(isk) - sum(isk_tax, na.rm = TRUE),
                Depot = last(depot) - sum(depot_tax, na.rm = TRUE),
                isk_tax = sum(isk_tax, na.rm = TRUE),
                depot_tax = sum(depot_tax, na.rm = TRUE)
            ) %>%
            mutate(isk_better = case_when(ISK > Depot ~ 1, TRUE ~ 0))
        
        isk_better_pct <- return_results %>% filter(isk_better == 1) %>% summarize(first(exp_ret)) %>% pull()
        
        return_results %>%
            pivot_longer(
                cols = c("ISK", "Depot"),
                names_to = "account",
                values_to = "net_result"
            ) %>%
            ggplot(aes(
                x = exp_ret,
                y = net_result,
                group = account,
                color = account
            )) +
            geom_vline(
                xintercept = isk_better_pct - 0.005,
                linetype = "dashed",
                color = "grey80",
                linewidth = 1
            ) +
            annotate(
                "rect",
                xmin = isk_better_pct - 0.005,
                xmax = Inf,
                ymin = -Inf,
                ymax = Inf,
                fill = "grey90",
                alpha = 0.5
            ) +
            annotate(
                "text",
                x = isk_better_pct,
                y = Inf,
                label = "ISK is beneficial\nfrom here",
                hjust = 0,
                vjust = 1,
                color = "grey40",
                size = 3.5,
                fontface = "italic"
            ) +
            # geom_area(data = . %>% filter(exp_ret >= isk_better_pct),
            # aes(fill = account), alpha = 0.2) +
            geom_line(linewidth = 1.5, alpha = 0.75) +
            geom_point(size = 3, alpha = 0.5) +
            scale_x_continuous(labels = scales::label_percent()) +
            scale_y_continuous(labels = scales::label_comma(suffix = "kr")) +
            scale_color_manual(values = account_cols) +
            ggtitle(
                paste0(
                    "An ISK will give better net results considering taxes from an average return of ",
                    isk_better_pct * 100,
                    "% or higher"
                )
            ) +
            labs(
                x = "Expected Average Returns",
                y = paste("Net Result after", investment_horizon, "years"),
                caption =  "Considering that any loss from the investment in the Depot can be used to offset capital gains taxes."
            ) +
            default_theme
        
    })
    
    output$expected_bor_rate <- renderPlot({
        exp_bor <- seq(0, 0.1, by = 0.005)
        start_amount <- input$start_amount
        avg_return <- input$avg_return / 100
        interest_rate <- input$interest_rate / 100
        investment_horizon <- input$investment_horizon
        start_year <- input$start_year
        
        annual_values_borrate <- data.frame(year = seq(start_year, start_year + investment_horizon , by = 1)) %>%
            left_join(rates, by = join_by(year == tax_year)) %>%
            cross_join(exp_bor, copy = TRUE) %>%
            rename(exp_bor = y) %>%
            mutate(tax_rate = case_when(is.na(tax_rate) ~ case_when(exp_bor < 0.0025 ~ 0.0125 * 0.3, TRUE ~ (exp_bor + 0.01) *0.3), TRUE ~ tax_rate)) %>%
            mutate(isk = start_amount * (1 + avg_return) ^ (year - start_year)) %>%
            mutate(depot = isk) %>%
            mutate(
                isk_tax = case_when(year == start_year ~ isk * tax_rate, TRUE ~ (isk + lag(isk) /2) * tax_rate),
                depot_tax = case_when(year == start_year + investment_horizon ~ (depot - start_amount) * (0.3))
            )
        
        borrowing_results <- annual_values_borrate %>%
            group_by(exp_bor) %>%
            summarize(
                ISK = last(isk) - sum(isk_tax, na.rm = TRUE),
                Depot = last(depot) - sum(depot_tax, na.rm = TRUE),
                isk_tax = sum(isk_tax, na.rm = TRUE),
                depot_tax = sum(depot_tax, na.rm = TRUE)
            ) %>%
            mutate(isk_better = case_when(ISK > Depot ~ 1, TRUE ~ 0))
        
        isk_better_pct <- borrowing_results %>% filter(isk_better == 1) %>% summarize(last(exp_bor)) %>% pull()
        
        borrowing_results %>%
            pivot_longer(
                cols = c("ISK", "Depot"),
                names_to = "account",
                values_to = "net_result"
            ) %>%
            ggplot(aes(
                x = exp_bor,
                y = net_result,
                group = account,
                color = account
            )) +
            geom_vline(
                xintercept = isk_better_pct + 0.0025,
                linetype = "dashed",
                color = "grey80",
                linewidth = 1
            ) +
            annotate(
                "rect",
                xmax = isk_better_pct + 0.0025,
                xmin = -Inf,
                ymin = -Inf,
                ymax = Inf,
                fill = "grey90",
                alpha = 0.5
            ) +
            annotate(
                "text",
                x = isk_better_pct,
                y = Inf,
                label = "ISK is beneficial\nup to here",
                hjust = 1,
                vjust = 1,
                color = "grey40",
                size = 3.5,
                fontface = "italic"
            ) +
            geom_line(linewidth = 1.5, alpha = 0.75) +
            geom_point(size = 3, alpha = 0.5) +
            scale_x_continuous(labels = scales::label_percent()) +
            scale_y_continuous(labels = scales::label_comma(suffix = "kr")) +
            scale_color_manual(values = account_cols) +
            ggtitle(paste0("An ISK will give better net results considering taxes from an average government borrowing rate up to ",isk_better_pct * 100,"% or lower"))+
            labs(
                x = "Expected Average Government Borrowing Rate",
                y = paste("Net Result after", investment_horizon, "years"),
                caption = "Only future years are adjusted with the expected government borrowing rate.\nConsidering that any loss from the investment in the Depot can be used to offset capital gains taxes."
            ) +
            default_theme
        
    })
    
    
    # Output the results in a table
    output$results_table <- render_gt({
        results_table() 
    })
    
}

# Run the application ####
shinyApp(ui = ui, server = server)

