if (!require("plotly")) {install.packages("plotly")}
if (!require("reshape2")) {install.packages("reshape2")}
if (!require("ggplot2")) {install.packages("ggplot2")}

library(shiny)
library(plotly)
library(reshape2)
library(ggplot2)

data <- read.csv("dataset_Facebook.csv", sep=";")
data <- data[complete.cases(data),]
data$Paid <- factor(data$Paid, labels = c("No", "Yes"))
data$Post.Weekday <- factor(data$Post.Weekday, labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

multiples_df <- data[, c(4, 16:19)]
agg_df <- aggregate(. ~ Post.Month, data = multiples_df, mean)
agg_df$Post.Month <- factor(agg_df$Post.Month, labels =c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))
colnames(agg_df) <- c("Month", "Avg Comments", "Avg Likes", "Avg Shares", "Avg Interactions")
melt_df <- melt(agg_df)
melt_df$id <- 0
melt_df$id <- ifelse(melt_df$variable=="Avg Comments", 1, melt_df$id)
melt_df$id <- ifelse(melt_df$variable=="Avg Likes", 2, melt_df$id)
melt_df$id <- ifelse(melt_df$variable=="Avg Shares", 3, melt_df$id)
melt_df$id <- ifelse(melt_df$variable=="Avg Interactions", 4, melt_df$id)


ui <- fluidPage(
  titlePanel(title = 'Facebook Visualizations'),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot 1: Bubble Plot", plotlyOutput("bubble"),
               sidebarPanel(selectInput("paid", label = "Paid or Not Paid",
                                        choices = list("No" = "No", "Yes" = "Yes"), selected = "No"))),
      tabPanel("Plot 2: Small Multiples", plotlyOutput("multiples")),
      tabPanel("Plot 3: Parallel Coordinates Plot", plotlyOutput("parallel"))
    )
  )
)

server <- function(input, output) {
  output$bubble <- renderPlotly({
    data <- data[data$Paid == input$paid,]
    ggplotly(ggplot(data=data, aes(y=share, x=like,
                                text = paste("Shares: ", share, "<br>",
                                             "Comments: ", comment, "<br>",
                                             "Likes: ", like))) +
      geom_point(aes(fill=Type, size=comment), shape=21,
                 colour="black", alpha = 0.8) +
       xlab("Number of Likes") +
       ylab("Number of Shares") +
      scale_y_continuous(labels = comma) + 
      scale_size(guide = 'none') +
      scale_fill_manual(values=c("#1B9E77", "#7570B3", "#E7298A", "#E6AB02"), name="") +
      theme(panel.background = element_blank(),
                     panel.grid.major = element_line(colour = "grey"),
                     panel.grid.minor = element_line(colour = "grey"),
                     panel.border = element_rect(colour="black", fill = NA),
                     plot.margin = unit(c(1,1,1,1), "cm")),
      tooltip = c("text"))
  })

  output$multiples <- renderPlotly({
     ggplotly(melt_df %>%
      transform(id = as.integer(factor(variable))) %>%
      plot_ly(x = ~Month, y = ~value, color = ~variable, colors = c("#1B9E77", "#7570B3", "#E7298A", "#E6AB02"),
              yaxis = ~paste0("y", id)) %>%
      add_lines() %>%
      subplot(nrows = 4, shareX = TRUE))
  })
  
  output$parallel <- renderPlotly({
    ggplotly(ggparcoord(data, c(16:18), groupColumn = "Type", scale = "globalminmax", title = "") +
      ylab("Count") +
      xlab("") + 
      scale_x_discrete(labels = c("Comments", "Likes", "Shares")) +
      scale_y_continuous(labels = comma) +
      scale_colour_manual(values=c("#1B9E77", "#7570B3", "#E7298A", "#E6AB02"), name="") + 
   theme(panel.background = element_blank(),
                     panel.grid.major = element_line(colour = "grey"),
                     panel.grid.minor = element_line(colour = "grey"),
                     panel.border = element_rect(colour="black", fill = NA),
                     plot.margin = unit(c(1,1,1,1), "cm")),
   tooltip = c("text"), height = 500, width = 700)
  })

}

shinyApp(ui = ui, server = server)





