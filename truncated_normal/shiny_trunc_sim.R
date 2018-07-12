library(shiny)
library(dplyr)
library(ggplot2)

load('~/StatisticalRethinking/truncated_normal/bayesian_trunc_sim_output/sim_output.RData')


make_data <- function(d=50, ub=600, g1=500, sd=80, n=1000) {
    data_frame(p1 = rbinom(n, 1, 0.5),
               d = d,
               data = rnorm(n, mean = g1 + p1 * d , sd=sd),
               censored = ifelse(data >= ub, 1, 0),
               cens_data = ifelse(censored == 1, ub, data),
               trunc_data = ifelse(censored == 1, NA, data))
}


make_plot <- function(data) {
    
    true_g1 <- lm(data ~ p1, data=data)$coef[[1]]
    true_diff <- lm(data ~ p1, data=data)$coef[[2]]
    
    data %>%
        select(p1, data, cens_data, trunc_data) %>%
        tidyr::gather('dataset', 'value', -p1) %>%
        filter(!is.na(value) ) %>%
        mutate(p1 = as.factor(p1)) %>%
        ggplot() + 
        geom_histogram(aes(x=value, fill=p1), bins=50) +
        geom_vline(xintercept = true_g1, colour='red', linetype='dotted') + 
        geom_vline(xintercept = true_g1 + true_diff, colour='green', linetype='dotted') + 
        facet_grid(p1~dataset, scales = 'free_y') + 
        theme_bw()
    
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Bayesian Simulation Exploration"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("selected_g1",
                        "Mean of lower group:",
                        min=200, max=600, value=500),
            sliderInput("selected_d",
                        "Difference between groups:",
                        min=10, max=100, value=50),
            sliderInput("selected_sd",
                        "Standard deviation:",
                        min=50, max=130, value=80)
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    plot_dat <- reactive({
        make_data(d=input$selected_d, ub=600, g1=input$selected_g1, sd=input$selected_sd, n=1000)
    })
    
    output$plot <- renderPlot({
        make_plot(plot_dat())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

