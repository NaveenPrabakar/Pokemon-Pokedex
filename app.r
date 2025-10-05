library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)

# Function to fetch Pokémon base data
fetch_pokemon_stats <- function(name) {
  url <- paste0("https://pokeapi.co/api/v2/pokemon/", tolower(name))
  res <- httr::GET(url)
  if (res$status_code != 200) return(NULL)
  data <- httr::content(res, as = "text", encoding = "UTF-8")
  poke_data <- jsonlite::fromJSON(data)
  
  stats_df <- data.frame(
    stat = poke_data$stats$stat$name,
    value = poke_data$stats$base_stat,
    stringsAsFactors = FALSE
  ) %>% filter(complete.cases(.))
  
  types <- poke_data$types$type$name
  
  moves <- lapply(poke_data$moves, function(mv) {
    list(name = mv$move$name, url = mv$move$url)
  })
  
  image_url <- poke_data$sprites$front_default
  
  list(
    name = poke_data$name,
    height = poke_data$height / 10,
    weight = poke_data$weight / 10,
    stats = stats_df,
    types = types,
    moves = moves,
    image = image_url
  )
}

# Fetch move type via move URL
fetch_move_type <- function(move_url) {
  res <- httr::GET(move_url)
  if (res$status_code != 200) return(NA)
  data <- httr::content(res, as = "text", encoding = "UTF-8")
  move_data <- jsonlite::fromJSON(data)
  return(move_data$type$name)
}

# You can expand this color mapping
type_colors <- c(
  normal = "#A8A77A",
  fire = "#EE8130",
  water = "#6390F0",
  electric = "#F7D02C",
  grass = "#7AC74C",
  ice = "#96D9D6",
  fighting = "#C22E28",
  poison = "#A33EA1",
  ground = "#E2BF65",
  flying = "#A98FF3",
  psychic = "#F95587",
  bug = "#A6B91A",
  rock = "#B6A136",
  ghost = "#735797",
  dragon = "#6F35FC",
  dark = "#705746",
  steel = "#B7B7CE",
  fairy = "#D685AD"
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: #0f0f10;
        color: #e0e0e0;
        font-family: 'Arial', sans-serif;
      }
      .container-flex {
        display: flex;
        flex-wrap: wrap;
        gap: 20px;
        justify-content: center;
      }
      .pokedex-card {
        background: #1a1a1d;
        border: 2px solid #444;
        border-radius: 15px;
        padding: 15px;
        width: 350px;
      }
      .poke-header {
        text-align: center;
      }
      .poke-header img {
        width: 120px;
        margin-bottom: 10px;
      }
      .poke-details {
        font-size: 14px;
        margin-bottom: 10px;
      }
      .types span {
        display: inline-block;
        padding: 4px 8px;
        border-radius: 8px;
        margin-right: 5px;
        font-weight: bold;
        color: #222;
      }
      .stats-plot {
        margin-bottom: 10px;
      }
      .moves-list {
        max-height: 300px;
        overflow-y: auto;
        border-top: 1px solid #555;
        padding-top: 8px;
      }
      .moves-list ul {
        list-style: none;
        padding-left: 0;
      }
      .moves-list li {
        margin-bottom: 4px;
        line-height: 1.3;
      }
      .move-type {
        font-size: 0.9em;
        margin-left: 6px;
        font-weight: bold;
      }
    "))
  ),
  
  titlePanel("Futuristic Pokédex — Compare Two Pokémon"),
  
  div(class = "input-section",
      textInput("pokemon1", "", placeholder = "Enter first Pokémon"),
      textInput("pokemon2", "", placeholder = "Enter second Pokémon"),
      actionButton("go_btn", "Scan")
  ),
  
  uiOutput("error_message"),
  
  div(class = "container-flex",
      uiOutput("poke1_card"),
      uiOutput("poke2_card")
  )
)

server <- function(input, output, session) {
  
  poke_data <- eventReactive(input$go_btn, {
    p1 <- fetch_pokemon_stats(input$pokemon1)
    p2 <- fetch_pokemon_stats(input$pokemon2)
    list(poke1 = p1, poke2 = p2)
  })
  
  # For each Pokémon, get a data.frame of all moves + move types
  poke_moves_with_types <- reactive({
    data <- poke_data()
    if (is.null(data)) return(NULL)
    get_moves_with_type <- function(moves_list) {
      if (length(moves_list) == 0) return(NULL)
      # need to call fetch_move_type for each
      types_vec <- vapply(moves_list, function(mv) {
        fetch_move_type(mv$url)
      }, FUN.VALUE = character(1))
      df <- data.frame(
        move = vapply(moves_list, function(mv) mv$name, FUN.VALUE = character(1)),
        mtype = types_vec,
        stringsAsFactors = FALSE
      )
      df
    }
    list(
      poke1 = if (!is.null(data$poke1)) get_moves_with_type(data$poke1$moves) else NULL,
      poke2 = if (!is.null(data$poke2)) get_moves_with_type(data$poke2$moves) else NULL
    )
  })
  
  output$error_message <- renderUI({
    data <- poke_data()
    if (is.null(data)) return(NULL)
    if (is.null(data$poke1) && is.null(data$poke2)) {
      div(style = "color: red;", "Neither Pokémon found.")
    } else if (is.null(data$poke1)) {
      div(style = "color: red;", paste("Pokémon 1 ('", input$pokemon1, "') not found.", sep = ""))
    } else if (is.null(data$poke2)) {
      div(style = "color: red;", paste("Pokémon 2 ('", input$pokemon2, "') not found.", sep = ""))
    } else {
      NULL
    }
  })
  
  render_poke_card <- function(p, moves_df, output_plot_id) {
    tagList(
      div(class = "pokedex-card",
          div(class = "poke-header",
              img(src = p$image, alt = p$name),
              h3(toupper(p$name))
          ),
          div(class = "poke-details",
              p(sprintf("Height: %.1f m", p$height)),
              p(sprintf("Weight: %.1f kg", p$weight)),
              div(class = "types",
                  lapply(p$types, function(t) {
                    # color of type badge background
                    bgcolor <- type_colors[[t]]
                    span(
                      toupper(t),
                      style = sprintf("background: %s;", bgcolor),
                      class = "type-badge"
                    )
                  })
              )
          ),
          div(class = "stats-plot",
              plotOutput(output_plot_id, height = "230px")
          ),
          div(class = "moves-list",
              h4("All Moves:"),
              if (!is.null(moves_df) && nrow(moves_df) > 0) {
                tags$ul(
                  lapply(seq_len(nrow(moves_df)), function(i) {
                    mv <- moves_df$move[i]
                    mt <- moves_df$mtype[i]
                    # color tag for move type
                    col <- type_colors[[mt]]
                    tags$li(
                      mv,
                      span(
                        paste0("(", toupper(mt), ")"),
                        class = "move-type",
                        style = if (!is.null(col)) sprintf("color: %s;", col) else ""
                      )
                    )
                  })
                )
              } else {
                "No moves found."
              }
          )
      )
    )
  }
  
  # Pokémon 1 UI
  output$poke1_card <- renderUI({
    data <- poke_data()
    moves_data <- poke_moves_with_types()
    if (is.null(data) || is.null(data$poke1)) return(NULL)
    render_poke_card(data$poke1, moves_data$poke1, "poke1_plot")
  })
  
  # Pokémon 2 UI
  output$poke2_card <- renderUI({
    data <- poke_data()
    moves_data <- poke_moves_with_types()
    if (is.null(data) || is.null(data$poke2)) return(NULL)
    render_poke_card(data$poke2, moves_data$poke2, "poke2_plot")
  })
  
  # Plot for Pokémon 1
  output$poke1_plot <- renderPlot({
    data <- poke_data()
    if (is.null(data) || is.null(data$poke1)) return(NULL)
    p <- data$poke1
    ggplot(p$stats, aes(x = stat, y = value, fill = stat)) +
      geom_col(show.legend = FALSE) +
      labs(title = paste("Stats —", toupper(p$name)), x = NULL, y = "Base Stat") +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "#1a1a1d"),
        plot.background = element_rect(fill = "#1a1a1d"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#e0e0e0"),
        axis.text.y = element_text(color = "#e0e0e0"),
        plot.title = element_text(color = "#f0f0f0", hjust = 0.5)
      )
  })
  
  # Plot for Pokémon 2
  output$poke2_plot <- renderPlot({
    data <- poke_data()
    if (is.null(data) || is.null(data$poke2)) return(NULL)
    p <- data$poke2
    ggplot(p$stats, aes(x = stat, y = value, fill = stat)) +
      geom_col(show.legend = FALSE) +
      labs(title = paste("Stats —", toupper(p$name)), x = NULL, y = "Base Stat") +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "#1a1a1d"),
        plot.background = element_rect(fill = "#1a1a1d"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#e0e0e0"),
        axis.text.y = element_text(color = "#e0e0e0"),
        plot.title = element_text(color = "#f0f0f0", hjust = 0.5)
      )
  })
}

shinyApp(ui, server)
