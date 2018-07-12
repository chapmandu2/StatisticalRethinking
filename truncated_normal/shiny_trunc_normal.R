#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

load('~/StatisticalRethinking/truncated_normal/bayesian_trunc_sim_output/sim_output.RData')

make_plot <- function(data, y_val='b_Intercept') {
    data %>%
        dplyr::filter(g1 < 600) %>%
        mutate(g1=as.factor(g1)) %>%
        ggplot(aes_(color=~g1, y=as.name(y_val), x=~g1)) + 
        geom_boxplot() + 
        facet_grid(as.factor(d)~model) +
        theme_bw()
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bayesian Simulation Results"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("selected_yval",
                     "Value to plot on y-axis:",
                     choices = c('b_Intercept', 'b_p1', 'diff_intercept', 'diff_p1'))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot"),
         tableOutput('table')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlot({
       make_plot(result_df, input$selected_yval)
   })
   
   output$table <- renderTable({
       result_df %>% group_by(param_id, d,ub,g1,sd,n) %>% 
           summarise_at(vars(b_Intercept, b_p1, diff_intercept, diff_p1), c('mean', 'sd')) %>% 
           t() %>%
           as.data.frame() %>%
           tibble::rownames_to_column('colname') %>%
           as_tibble()

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

