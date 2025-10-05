library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)

# Function to fetch and process Pokémon data
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
  )
  stats_df <- stats_df[complete.cases(stats_df), ]
  stats_df$stat <- factor(stats_df$stat, levels = stats_df$stat)
  
  types <- poke_data$types$type$name
  moves <- poke_data$moves$move$name
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

# UI with Pokédex-style CSS
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #d32f2f, #b71c1c);
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        color: white;
        margin: 0;
        padding: 20px;
      }
      .pokedex-container {
        max-width: 900px;
        margin: 0 auto;
        background: #222;
        border-radius: 20px;
        box-shadow: 0 0 15px 5px #b71c1c;
        padding: 20px 30px;
      }
      h1, h3 {
        font-weight: 700;
        letter-spacing: 3px;
        text-align: center;
        margin-bottom: 15px;
        text-shadow: 2px 2px 5px #000;
      }
      .input-section {
        margin-bottom: 20px;
        text-align: center;
      }
      input[type='text'] {
        width: 250px;
        padding: 8px 10px;
        border: none;
        border-radius: 10px;
        font-size: 16px;
        margin-right: 10px;
        box-shadow: inset 2px 2px 5px #900000;
      }
      button {
        background: #b71c1c;
        border: none;
        color: white;
        padding: 10px 18px;
        font-weight: 700;
        font-size: 16px;
        border-radius: 12px;
        cursor: pointer;
        box-shadow: 0 3px 6px #640000;
        transition: background 0.3s ease;
      }
      button:hover {
        background: #f44336;
      }
      .poke-info {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 25px;
        margin-bottom: 25px;
      }
      .poke-info img {
        border-radius: 15px;
        box-shadow: 0 0 10px 3px #f44336;
        background: #111;
      }
      .poke-details {
        font-size: 18px;
        line-height: 1.5;
        text-shadow: 1px 1px 3px #000;
      }
      .stats-plot {
        background: #333;
        padding: 15px;
        border-radius: 15px;
        box-shadow: inset 0 0 10px #900000;
      }
      .types, .moves {
        background: #111;
        border-radius: 15px;
        padding: 15px 20px;
        margin-top: 25px;
        box-shadow: 0 0 15px 3px #b71c1c;
        max-height: 280px;
        overflow-y: auto;
      }
      .moves ul {
        padding-left: 20px;
      }
      .moves li {
        padding: 3px 0;
        border-bottom: 1px solid #900000;
      }
      .error-message {
        color: #ff8080;
        font-weight: 700;
        text-align: center;
        margin-top: 20px;
        text-shadow: 1px 1px 3px #000;
      }
    "))
  ),
  div(class = "pokedex-container",
      titlePanel("Pokémon Stats Viewer"),
      div(class = "input-section",
          textInput("pokemon_name", "", placeholder = "Enter Pokémon Name (e.g., Pikachu)"),
          actionButton("go_btn", "Scan")
      ),
      uiOutput("poke_info"),
      div(class = "stats-plot",
          plotOutput("pokemonPlot")
      ),
      uiOutput("poke_types"),
      uiOutput("poke_moves")
  )
)

# Server
server <- function(input, output, session) {
  poke_info <- eventReactive(input$go_btn, {
    fetch_pokemon_stats(input$pokemon_name)
  })
  
  output$poke_info <- renderUI({
    info <- poke_info()
    if (is.null(info)) {
      div(class = "error-message", "Pokémon not found. Please check the name.")
    } else {
      div(class = "poke-info",
          img(src = info$image, alt = info$name, width = "160px"),
          div(class = "poke-details",
              h3(toupper(info$name)),
              p(paste("Height:", info$height, "m")),
              p(paste("Weight:", info$weight, "kg"))
          )
      )
    }
  })
  
  output$pokemonPlot <- renderPlot({
    info <- poke_info()
    if (is.null(info)) return(NULL)
    
    ggplot(info$stats, aes(x = stat, y = value, fill = stat)) +
      geom_col(show.legend = FALSE) +
      labs(title = paste("Stats for", toupper(info$name)),
           x = "Stat",
           y = "Base Stat Value") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.background = element_rect(fill = "#333333", color = NA),
            panel.background = element_rect(fill = "#333333", color = NA),
            plot.title = element_text(color = "white", face = "bold"),
            axis.title = element_text(color = "white"),
            axis.text = element_text(color = "white"))
  })
  
  output$poke_types <- renderUI({
    info <- poke_info()
    if (is.null(info)) return(NULL)
    div(class = "types",
        strong("Type(s): "),
        paste(toupper(info$types), collapse = ", ")
    )
  })
  
  output$poke_moves <- renderUI({
    info <- poke_info()
    if (is.null(info)) return(NULL)
    
    div(class = "moves",
        h4("Moves"),
        tags$ul(
          lapply(info$moves, function(mv) tags$li(mv))
        )
    )
  })
}

# Run the app
shinyApp(ui, server)