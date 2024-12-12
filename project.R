# Dashboard Shiny App
library(shiny)
library(bslib)
library(hrbrthemes)
library(statebins)
library(forcats)
library(RColorBrewer)
library(knitr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

revenue <- readRDS("www/revenue_summary_tidied.rds")

ui <- page_sidebar(
  # main sidebar for filtering date, across all nav panels
  sidebar = card(
    card_header(
      "Date Filter",
      class = "bg-dark"
    ),
    card_body(
      # set date filter, with default being view all data
      dateInput(
        "start_date", 
        "Start Date",
        min = min(revenue$date),
        max = max(revenue$date),
        value = min(revenue$date)
      ),
      dateInput(
        "end_date", 
        "End Date",
        min = min(revenue$date),
        max = max(revenue$date), 
        value = max(revenue$date)
      ),
      # query button that updates the dashboard
      actionButton(
        "p1_generate",
        "Query"
      )
    )
  ),
  
  # main dashboard with 3 tabs
  navset_card_underline(
    # panel 1 Overall Revenue
    nav_panel(
      "Overall Revenue Over Time", 
      # main content
      layout_column_wrap(
        # column format: 3:1 for each row
        style = css(grid_template_columns = "3fr 1fr"),
        
        # main plot showing revenue over time for all datapoints
        card(
          max_height = 600,
          card_header(
            "Revenue Over Time",
            class = "bg-danger"
          ),
          card_body(
            plotOutput(outputId = "p1_revenue")
          )
        ),
        
        # summary stats that supplements the main plot
        card(
          max_height = 600,
          card_header(
            "Summary",
            class = "bg-danger"
          ),
          card_body(
            tableOutput(outputId = "p1_summary")
          )
        ),
        
        # plot for revenue over time, viewed by patter
        card(
          max_height = 600,
          card_header(
            "Revenue by Pattern",
            class = "bg-danger"
          ),
          card_body(
            plotOutput(outputId = "p1_revenue_by_pattern")
          )
        ),
        
        # bar plot shows the total revenue of each pattern
        card(
          max_height = 600,
          card_header(
            "Summary by Pattern",
            class = "bg-danger"
          ),
          card_body(
            plotOutput(outputId = "p1_pattern_bar")
          )
        )
      )
      
    ),
    
    # panel 2 Patterns Details
    nav_panel(
      "Details by Pattern", 
      
      # need a sidebar for this navpanel that allows user to filter on pattern
      layout_sidebar(
        # sidebar
        sidebar = (
          selectInput(
            inputId = "p2_pattern_selector", label = "Pattern", 
            choices = unique(revenue$pattern)
          )
        ),
        
        # same column layout as panel 1 for consistency
        layout_column_wrap(
          style = css(grid_template_columns = "3fr 1fr"),
          
          # main plot showing revenue over time for specific pattern
          # similar information as the facet plot in panel 1, putting here to
          # focus on specific pattern
          card(
            max_height = 600,
            card_header(
              textOutput("p2_dynamic_header"),
              class = "bg-danger"
            ),
            card_body(
              plotOutput(outputId = "p2_revenue")
            )
          ),
          
          # total revenue of this pattern categorized by pattern
          card(
            max_height = 600,
            card_header(
              "Revenue by Color",
              class = "bg-danger"
            ),
            card_body(
              plotOutput(outputId = "p2_color_summary")
            )
          )
        ),
        
        # new row
        # total revenue of this pattern, catogorized by size and facetted by
        # color
        card(
          max_height = 600,
          card_header(
            "Revenue by Size",
            class = "bg-danger"
          ),
          card_body(
            plotOutput(outputId = "p2_revenue_by_pattern_color")
          )
        )
      )
    ),
    
    # panel 3 Spacial Distribution
    nav_panel(
      "Details by Location", 
      
      # need a sidebar to filter by pattern
      layout_sidebar(
        sidebar = (
          selectInput(
            inputId = "p3_pattern_selector", label = "Pattern", 
            choices = c("all", unique(revenue$pattern))
          )
        ),
        
        # same column layout for consistency
        layout_column_wrap(
          style = css(grid_template_columns = "3fr 1fr"),
          
          # main plot, choropleth
          card(
            max_height = 600,
            card_header(
              "Spatial Distribution of Revenue by State",
              class = "bg-danger"
            ),
            card_body(
              plotOutput(outputId = "p3_choropleth")
            )
          ),
          
          # barplot of top 20 states as supplement to the choropleth 
          card(
            max_height = 600,
            card_header(
              "Top 20 States",
              class = "bg-danger"
            ),
            card_body(
              plotOutput(outputId = "p3_bar")
            )
          )
        )  
      )
    )
  ),

  # title of the dashboard
  title = "LUXRUG Amazon USA | Admin Dashboard"
)

server <- function(input, output) {
  # process rds file to get the needed data for plotting
  data <- eventReactive( input$p1_generate, {
    validate(
      need(
        !is.na(input$start_date) && !is.na(input$end_date), 
        "Please provide both start and end dates."
      ),
      need(
        input$start_date < input$end_date, 
        "Start date must be before end date."
      )
    )
    # filters and groups data based on the selected start and end date
    plot_data <- revenue |>
      filter(
        date >= input$start_date & date <= input$end_date
      ) 
    plot_data
  })
  
  ####### tab 1  #######
  # tab 1 main line plot
  output$p1_revenue <- renderPlot({
    plot_data <- data() |>
      group_by(date) |>
      summarise(total_revenue = sum(total_revenue), .groups = 'drop')
    
    ggplot(plot_data, aes(x = date, y = total_revenue)) +
      geom_line() +
      geom_point() +
      labs(
        x = "Date",
        y = "Total Revenue",
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12)
      )
  }) |>
    bindEvent(input$p1_generate) # render when queried
  
  # tab 1 by pattern line plot
  output$p1_revenue_by_pattern <- renderPlot({
      plot_data <- data() |>
        group_by(date, pattern) |> # need to group by pattern
        summarise(total_revenue = sum(total_revenue), .groups = 'drop')
      
      # code adapted from https://www.data-to-viz.com/caveat/spaghetti.html
      plot_data_temp <- plot_data |>
        mutate(pattern2=pattern)
      
      ggplot(plot_data_temp, aes(x=date, y=total_revenue)) +
        # show all patterns as grey
        geom_line( 
          data=plot_data_temp |> 
            dplyr::select(-pattern), 
          aes(group=pattern2), 
          color="grey", 
          linewidth=0.5, 
          alpha=0.5
        ) +
        # show specific pattern
        geom_line( aes(color=pattern), size=1.2 ) +
        scale_color_brewer(palette = "Set2") + # customized palette
        theme_ipsum() +
        theme(
          legend.position="none",
          plot.title = element_text(face = "bold", size = 14),
          axis.title = element_text(size = 12)
        ) +
        labs(
          x = "Date",
          y = "Total Revenue",
        ) +
        facet_wrap(~pattern)
  }) |>
    bindEvent(input$p1_generate) # render when queried
  
  # render tab 1 summary table
  output$p1_summary <- renderTable({
    plot_data <- data()
    # compute summary stats
    plot_data |>
      group_by(
        date
      ) |>
      summarise(
        total_revenue = sum(total_revenue)
      ) |>
      summarize(
        `Total Revenue ($)` = sum(total_revenue),
        `Daily Average ($)` = mean(total_revenue)
      )
  }) |>
    bindEvent(input$p1_generate) # render when queried
  
  # render tab 1 barplot
  output$p1_pattern_bar <- renderPlot({
    plot_data <- data() |>
      group_by(pattern) |>
      summarise(total_revenue = sum(total_revenue), .groups = 'drop')
    
    ggplot(plot_data, aes(x = pattern, y = total_revenue, fill = pattern)) +
      geom_bar(stat="identity") +
      scale_fill_brewer(palette = "Set2") + # customized palatte for consistency
      labs(
        x = "Pattern",
        y = "Total Revenue",
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12)
      )
  }) |>
    bindEvent(input$p1_generate)
  
  ####### tab 2 #######
  # dynamic title for the main plot
  output$p2_dynamic_header <- renderText({
    paste0("Revenue for Pattern ", input$p2_pattern_selector)
  })
  
  # tab 2 main plot
  output$p2_revenue <- renderPlot({
    plot_data <- data() |>
      filter(
        pattern == input$p2_pattern_selector # filter for the selected pattern
      ) |>
      group_by(date) |>
      summarise(total_revenue = sum(total_revenue), .groups = 'drop')
    
    ggplot(plot_data, aes(x = date, y = total_revenue)) +
      geom_line() +
      geom_point() +
      labs(
        x = "Date",
        y = "Total Revenue"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12)
      )
  }) |>
    # render when queried OR new pattern selected
    bindEvent(input$p1_generate, input$p2_pattern_selector)
  
  # tab 2 color bar plot
  output$p2_color_summary <- renderPlot({
    plot_data <- data() |>
      filter(
        pattern == input$p2_pattern_selector # filter the selected pattern
      ) |>
      group_by(color) |>
      summarise(total_revenue = sum(total_revenue), .groups = 'drop')
    
    ggplot(plot_data, aes(x = color, y = total_revenue)) +
      # set color to make it consistent with the overall color theme
      geom_bar(stat="identity", fill="#f68060", alpha=.6) +
      labs(
        x = "Color",
        y = "Total Revenue",
        cex = 3
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        legend.position = "none"
      )
  }) |>
    # render when queried OR new pattern selected
    bindEvent(input$p1_generate, input$p2_pattern_selector)
  
  # tab 2 by size facetted by color barplot
  output$p2_revenue_by_pattern_color <- renderPlot({
    plot_data <- data() |>
      filter(
        pattern == input$p2_pattern_selector # filter selected pattern
      ) |>
      group_by(date, pattern, color, size) |>
      summarise(total_revenue = sum(total_revenue), .groups = 'drop')
    
    ggplot(plot_data, aes(x = size, y = total_revenue, fill = size)) +
      geom_bar(stat="identity") +
      scale_fill_brewer(palette = "Reds") +
      labs(
        x = "Size",
        y = "Total Revenue",
        cex = 3
      ) +
      facet_grid( # facet
        cols = vars(color)
      ) +
      theme_linedraw() + # theme that fits the dashboard overall theme better
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        legend.position = "none"
      )
  }) |>
    # render when queried OR new pattern selected
    bindEvent(input$p1_generate, input$p2_pattern_selector)
  
  # tab 3 choropleth
  output$p3_choropleth <- renderPlot({
    plot_data <- data()
    if (input$p3_pattern_selector != "all") { # conditional filter
      plot_data <- data() |>
        filter(
          pattern == input$p3_pattern_selector
        )
    }
    plot_data <- plot_data |> 
      group_by(state.abb) |> 
      summarize(total_revenue = sum(total_revenue))
    
    #plotting choropleth
    # suppress a message on fill scale
    suppressMessages({
      # plotting statebins
      statebins(state_data = plot_data, state_col = "state.abb",
                value_col = "total_revenue") +
          theme_void() +
        scale_fill_gradient(low = "gray90", high = "red3")
    })
  }) |>
    # render when queried OR new pattern selected
    bindEvent(input$p1_generate, input$p3_pattern_selector)
  
  # tab 3 top 20 states
  output$p3_bar <- renderPlot({
    plot_data <- data()
    if (input$p3_pattern_selector != "all") { # conditional filtering
      plot_data <- data() |>
        filter(
          pattern == input$p3_pattern_selector
        )
    }
    
    # select top 20 selling states
    plot_data <- plot_data |> 
      group_by(state.abb) |> 
      summarize(total_revenue = sum(total_revenue)) |>
      slice_max(n = 20, order_by = total_revenue)
    
    ggplot(plot_data, aes(
      x = reorder(state.abb,total_revenue), # order by revenue
      y = total_revenue)
    ) +
      geom_bar(stat="identity", fill="#f68060", alpha=.6) +
      coord_flip() + # make it vertical for easier comparison
      labs(
        y = "Total Revenue",
        x = "",
        cex = 3
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        legend.position = "none"
      )
  }) |>
    # render when queried OR new pattern selected
    bindEvent(input$p1_generate, input$p3_pattern_selector)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
