# Shiny App 
# Section 1. First install and activate all your required packages. 

library(shiny)
library(bslib)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)
library(rsconnect)

# LOAD YOUR DATA HERE
# Ensure your CSV files are saved in the exact same folder as this app.R script!
nodes_raw <- read_csv("data/ModernFamily_nodes.csv")
edges_raw <- read_csv("data/ModernFamily_edges.csv")

# The columns 'id', 'label', 'from', and 'to' are already perfectly formatted!
nodes_clean <- nodes_raw 
edges_clean <- edges_raw 


# Section 2. Design the site in the UI section. 
ui <- fluidPage(
  
  titlePanel("Modern Family: Network Analysis"),
  
  page_sidebar(
    title = "Network Controls", 
    sidebar = sidebar("Menu options"), 
    
    card(
      card_header("Project Introduction"), 
      "This app visualizes the weighted network interactions among the core cast of Modern Family across different generations."
    ),
    
    card(
      card_header("Dynamic Demo 1"), "Testing our variables",
      selectInput("select", 
                  "Select a Character Generation", 
                  choices = list("Boomer" = "Boomer", 
                                 "Gen X" = "Gen X",
                                 "Millennial" = "Millennial",
                                 "Gen Z" = "Gen Z"),
                  selected = "Gen X"), 
      textOutput("ourVariable")
    ), 
    
    
    card(
      card_header("Static Network Visualization"),
      selectInput("size",
                  "Choose a centrality measure for Node Size", 
                  choices = list("Degree Centrality" = "degree", 
                                 "Betweenness Centrality" = "betweenness"), 
                  selected = "degree"), 
      plotOutput("example_network"), height = "400px"
    ),
    
    
    card(
      card_header("An interactive network?!"), 
      "We can use the package visNetwork to explore the Modern Family network interactively.", 
      radioButtons("size_by", "Centrality Measure", 
                   choices = c("Degree Centrality" = "degree", 
                               "Betweenness Centrality" = "betweenness"), 
                   selected = "degree"),
      visNetworkOutput("int_network"), height = "600px"
    )
  )
)


# Section 3. The server section. Here's where we will put all the network analysis. 
server <- function(input, output) {
  
  # CARD 1 
  output$ourVariable <- renderText({
    paste("You have selected the", input$select, "generation.")
  })
  
  
  # CARD 2 
  network <- reactive({
    
    # Building an undirected network for family ties
    mf_net <- tbl_graph(nodes = nodes_clean, edges = edges_clean, directed = FALSE) |> 
      activate(nodes) |> 
      mutate(
        degree = centrality_degree(mode = "all"), 
        betweenness = centrality_betweenness()
      )
    
    mf_net
  })
  
  # Static Network Visualization
  output$example_network <- renderPlot({
    ex_net <- network() 
    
    p <- ggraph(ex_net, layout = "fr") +
      # Scale edge thickness based on your 'Weight' column
      geom_edge_link(aes(edge_width = Weight), alpha = 0.4, color = "grey50") + 
      # Size by centrality, color the nodes by their 'generation'
      geom_node_point(aes(size = .data[[input$size]], color = generation)) + 
      geom_node_text(aes(label = label), repel = TRUE, vjust = 1) + 
      scale_size_continuous(range = c(4, 12)) + 
      scale_edge_width_continuous(range = c(0.5, 2.5)) +
      labs(size = input$size, color = "Generation", edge_width = "Tie Weight") + 
      theme_graph()
    
    p
  })
  
  # CARD 3 
  network2 <- reactive({
    
    ex_net2 <- network() # Reuse the network
    
    nodes_df <- ex_net2 |> 
      activate(nodes) |> 
      as_tibble() |> 
      mutate(
        value = if (input$size_by == "degree") degree else betweenness,
        group = generation, # visNetwork will color the nodes by generation
        title = paste("Name:", label, "<br>Generation:", generation) # Hover-over text
      ) 
    
    edges_df <- ex_net2 |> 
      activate(edges) |> 
      as_tibble() |>
      mutate(value = Weight) # visNetwork uses 'value' to adjust edge thickness!
    
    list(nodes = nodes_df, edges = edges_df)
  })
  
  # Interactive visNetwork output
  output$int_network <- renderVisNetwork({
    net2 <- network2()
    
    visNetwork(net2$nodes, net2$edges) |> 
      
      visEdges(
        color = list(color = "darkgray", highlight = "black")
      ) |> 
      
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE), 
        nodesIdSelection = TRUE, # Dropdown to search for characters
        selectedBy = "group"     # Dropdown to highlight specific generations
      ) |>
      
      visInteraction(
        dragNodes = TRUE, 
        dragView = TRUE, 
        zoomView = TRUE
      ) |> 
      
      visPhysics(stabilization = TRUE)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)