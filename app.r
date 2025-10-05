library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(gridExtra)

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
  
  # Limit moves to first 20 to reduce API calls
  moves_list <- poke_data$moves
  
  # Fetch move types for these moves
  move_types <- sapply(moves_list$move$url, function(move_url) {
    move_res <- httr::GET(move_url)
    if (move_res$status_code != 200) return(NA)
    move_data <- jsonlite::fromJSON(httr::content(move_res, as = "text", encoding = "UTF-8"))
    move_data$type$name
  })
  
  moves <- data.frame(
    name = moves_list$move$name,
    type = move_types,
    stringsAsFactors = FALSE
  )
  
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

# UI with futuristic Pokédex CSS
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Orbitron:wght@700&display=swap');
      body {
        background: radial-gradient(circle at center, #0d0d0d, #000000 70%);
        font-family: 'Orbitron', monospace;
        color: #00ffea;
        margin: 0; padding: 20px;
      }
      .pokedex-container {
        max-width: 1000px;
        margin: 0 auto;
        background: #111a22;
        border-radius: 25px;
        box-shadow: 0 0 20px 5px #00ffea;
        padding: 30px 40px;
      }
      h1, h3, h4 {
        font-weight: 700;
        letter-spacing: 4px;
        text-align: center;
        margin-bottom: 20px;
        text-shadow: 0 0 10px #00ffe7, 0 0 30px #00ffe7;
      }
      .input-section {
        display: flex;
        justify-content: center;
        gap: 20px;
        margin-bottom: 30px;
      }
      input[type='text'] {
        width: 200px;
        padding: 10px 12px;
        border: none;
        border-radius: 15px;
        font-size: 18px;
        background: #002f3f;
        color: #00ffea;
        box-shadow: inset 0 0 8px #00ffe7;
        text-transform: capitalize;
      }
      input[type='text']::placeholder {
        color: #00bb9f;
        font-style: italic;
      }
      button {
        background: #00ffea;
        border: none;
        color: #003333;
        padding: 12px 25px;
        font-weight: 700;
        font-size: 18px;
        border-radius: 20px;
        cursor: pointer;
        box-shadow: 0 0 15px #00ffe7;
        transition: background 0.3s ease, color 0.3s ease;
      }
      button:hover {
        background: #00bb9f;
        color: #001a0d;
        box-shadow: 0 0 25px #00bb9f;
      }
      .poke-info-row {
        display: flex;
        justify-content: space-around;
        gap: 40px;
        flex-wrap: wrap;
      }
      .poke-info {
        background: #002f3f;
        padding: 15px 20px;
        border-radius: 20px;
        box-shadow: 0 0 10px #00ffe7;
        width: 45%;
        min-width: 280px;
        text-align: center;
      }
      .poke-info img {
        border-radius: 20px;
        box-shadow: 0 0 15px 5px #00ffe7;
        background: #000c0f;
        margin-bottom: 15px;
      }
      .poke-details {
        font-size: 20px;
        line-height: 1.5;
        text-shadow: 0 0 6px #00ffea;
      }
      .stats-plot {
        background: #001f2b;
        padding: 25px;
        border-radius: 25px;
        box-shadow: inset 0 0 15px #00ffea;
        margin-top: 30px;
      }
      .types, .moves {
        background: #002f3f;
        border-radius: 20px;
        padding: 20px 25px;
        margin-top: 30px;
        box-shadow: 0 0 20px #00ffea;
        max-height: 280px;
        overflow-y: auto;
        color: #00ffea;
        font-weight: 600;
      }
      .moves ul {
        padding-left: 25px;
      }
      .moves li {
        padding: 4px 0;
        border-bottom: 1px solid #007770;
        font-size: 16px;
      }
      .error-message {
        color: #ff4d4d;
        font-weight: 700;
        text-align: center;
        margin-top: 25px;
        text-shadow: 0 0 8px #ff4d4d;
      }
      @media(max-width: 850px) {
        .poke-info {
          width: 100%;
        }
        .poke-info-row {
          flex-direction: column;
          align-items: center;
        }
      }
    "))
  ),
  div(class = "pokedex-container",
      titlePanel("Futuristic Pokédex: Compare Pokémon"),
      div(class = "input-section",
          textInput("pokemon_name1", "", placeholder = "Enter 1st Pokémon Name"),
          textInput("pokemon_name2", "", placeholder = "Enter 2nd Pokémon Name"),
          actionButton("go_btn", "Scan")
      ),
      uiOutput("poke_compare_info"),
      div(class = "stats-plot",
          plotOutput("comparePlot")
      ),
      uiOutput("poke_compare_types"),
      uiOutput("poke_compare_moves")
  )
)

# Server
server <- function(input, output, session) {
  
  poke_info1 <- reactiveVal(NULL)
  poke_info2 <- reactiveVal(NULL)
  
  observeEvent(input$go_btn, {
    info1 <- fetch_pokemon_stats(input$pokemon_name1)
    info2 <- fetch_pokemon_stats(input$pokemon_name2)
    
    poke_info1(info1)
    poke_info2(info2)
  })
  
  output$poke_compare_info <- renderUI({
    info1 <- poke_info1()
    info2 <- poke_info2()
    
    if (is.null(info1) && is.null(info2)) {
      div(class = "error-message", "Neither Pokémon found. Please check the names.")
    } else if (is.null(info1)) {
      div(class = "error-message", paste("First Pokémon ('", input$pokemon_name1, "') not found.", sep = ""))
    } else if (is.null(info2)) {
      div(class = "error-message", paste("Second Pokémon ('", input$pokemon_name2, "') not found.", sep = ""))
    } else {
      div(class = "poke-info-row",
          # Pokémon 1 info
          div(class = "poke-info",
              img(src = info1$image, alt = info1$name, width = "160px"),
              div(class = "poke-details",
                  h3(toupper(info1$name)),
                  p(paste("Height:", info1$height, "m")),
                  p(paste("Weight:", info1$weight, "kg"))
              )
          ),
          # Pokémon 2 info
          div(class = "poke-info",
              img(src = info2$image, alt = info2$name, width = "160px"),
              div(class = "poke-details",
                  h3(toupper(info2$name)),
                  p(paste("Height:", info2$height, "m")),
                  p(paste("Weight:", info2$weight, "kg"))
              )
          )
      )
    }
  })
  
  output$comparePlot <- renderPlot({
    info1 <- poke_info1()
    info2 <- poke_info2()
    
    if (is.null(info1) || is.null(info2)) return(NULL)
    
    df1 <- info1$stats
    df1$pokemon <- toupper(info1$name)
    df2 <- info2$stats
    df2$pokemon <- toupper(info2$name)
    
    combined <- rbind(df1, df2)
    
    ggplot(combined, aes(x = stat, y = value, fill = pokemon)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      scale_fill_manual(values = c("#00ffe7", "#00bb9f")) +
      labs(title = paste("Stats Comparison:", toupper(info1$name), "vs", toupper(info2$name)),
           x = "Stat",
           y = "Base Stat Value",
           fill = "Pokémon") +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_rect(fill = "#001f2b", color = NA),
        panel.background = element_rect(fill = "#001f2b", color = NA),
        plot.title = element_text(color = "#00ffe7", face = "bold"),
        axis.title = element_text(color = "#00ffe7"),
        axis.text = element_text(color = "#00ffe7"),
        legend.title = element_text(color = "#00ffe7"),
        legend.text = element_text(color = "#00bb9f")
      )
  })
  
  output$poke_compare_types <- renderUI({
    info1 <- poke_info1()
    info2 <- poke_info2()
    
    if (is.null(info1) || is.null(info2)) return(NULL)
    
    div(class = "poke-info-row",
        div(class = "types",
            strong(toupper(info1$name), " Type(s): "),
            paste(toupper(info1$types), collapse = ", ")
        ),
        div(class = "types",
            strong(toupper(info2$name), " Type(s): "),
            paste(toupper(info2$types), collapse = ", ")
        )
    )
  })
  
  output$poke_compare_moves <- renderUI({
    info1 <- poke_info1()
    info2 <- poke_info2()
    
    if (is.null(info1) || is.null(info2)) return(NULL)
    
    # Colors for Pokémon move types
    type_colors <- c(
      normal = "#A8A77A", fire = "#EE8130", water = "#6390F0", electric = "#F7D02C",
      grass = "#7AC74C", ice = "#96D9D6", fighting = "#C22E28", poison = "#A33EA1",
      ground = "#E2BF65", flying = "#A98FF3", psychic = "#F95587", bug = "#A6B91A",
      rock = "#B6A136", ghost = "#735797", dragon = "#6F35FC", dark = "#705746",
      steel = "#B7B7CE", fairy = "#D685AD"
    )
    
    move_tag <- function(move_name, move_type) {
      color <- type_colors[move_type]
      if (is.na(color)) color <- "#FFFFFF"
      tags$li(
        style = paste0("color:", color, "; font-weight: 600;"),
        paste0(toupper(move_name), " (", toupper(move_type), ")")
      )
    }
    
    div(class = "poke-info-row",
        div(class = "moves",
            h4(paste(toupper(info1$name), "Moves")),
            tags$ul(
              lapply(seq_len(nrow(info1$moves)), function(i) {
                move_tag(info1$moves$name[i], info1$moves$type[i])
              })
            )
        ),
        div(class = "moves",
            h4(paste(toupper(info2$name), "Moves")),
            tags$ul(
              lapply(seq_len(nrow(info2$moves)), function(i) {
                move_tag(info2$moves$name[i], info2$moves$type[i])
              })
            )
        )
    )
  })
  
}

# Run the app
shinyApp(ui, server)
