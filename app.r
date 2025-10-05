# app.R
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ---------- Helper functions ----------
fetch_pokemon_stats <- function(name) {
  if (is.null(name) || name == "") return(NULL)
  url <- paste0("https://pokeapi.co/api/v2/pokemon/", tolower(name))
  res <- httr::GET(url)
  if (res$status_code != 200) return(NULL)
  poke_data <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  
  stats_df <- data.frame(
    stat = poke_data$stats$stat$name,
    value = poke_data$stats$base_stat,
    stringsAsFactors = FALSE
  ) %>% filter(complete.cases(.))
  
  types <- vapply(poke_data$types, function(x) x$type$name, FUN.VALUE = character(1))
  
  moves <- lapply(poke_data$moves, function(mv) {
    list(name = mv$move$name, url = mv$move$url)
  })
  
  # prefer official artwork, fallback to front_default
  image_url <- poke_data$sprites$other$official-artwork$front_default
  if (is.null(image_url) || image_url == "") image_url <- poke_data$sprites$front_default
  
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

fetch_move_type <- function(move_url) {
  # move_url like https://pokeapi.co/api/v2/move/{id}/
  res <- httr::GET(move_url)
  if (res$status_code != 200) return(NA_character_)
  move_data <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  return(move_data$type$name)
}

# Color mapping (neon-ish for badge text or backgrounds)
type_colors <- c(
  normal = "#A8A77A", fire = "#EE8130", water = "#6390F0", electric = "#F7D02C",
  grass = "#7AC74C", ice = "#96D9D6", fighting = "#C22E28", poison = "#A33EA1",
  ground = "#E2BF65", flying = "#A98FF3", psychic = "#F95587", bug = "#A6B91A",
  rock = "#B6A136", ghost = "#735797", dragon = "#6F35FC", dark = "#705746",
  steel = "#B7B7CE", fairy = "#D685AD"
)

# ---------- UI ----------
ui <- fluidPage(
  tags$head(
    # Google fonts, main CSS to mimic screenshot neon/panel look
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700&display=swap"),
    tags$style(HTML("
      /* Basic page */
      html, body { background: #0a0d0e; height:100%; }
      body { color: #bfeffd; font-family: 'Orbitron', Arial, sans-serif; -webkit-font-smoothing:antialiased; }
      .app-wrap { max-width: 1100px; margin: 16px auto; padding: 28px; border-radius: 24px; background: linear-gradient(180deg, rgba(0,0,0,0.35), rgba(0,0,0,0.45)); 
                  box-shadow: 0 0 36px rgba(0,255,220,0.06), inset 0 0 24px rgba(0,255,220,0.02); border: 4px solid rgba(0,255,220,0.06); }
      h1.title { color: #30fff3; font-weight:700; letter-spacing: 1px; font-size: 40px; text-shadow: 0 0 18px rgba(0,255,220,0.18); margin-bottom: 14px; }
      .top-inputs { display:flex; gap: 14px; align-items:center; margin-bottom: 18px; }
      .top-inputs .shiny-text-input { width: 260px; }
      .scan-btn { background: #ffffff; color: #0a0d0e; font-weight:700; padding: 8px 14px; border-radius: 8px; box-shadow: 0 6px 28px rgba(0,255,220,0.06); border: none; }
      
      /* two cards row */
      .cards-row { display:flex; gap: 26px; justify-content:center; margin-bottom: 22px; flex-wrap:wrap; }
      .pokedex-card {
        background: linear-gradient(180deg, rgba(14,32,36,0.75), rgba(2,12,14,0.7));
        border-radius: 22px;
        padding: 20px;
        width: 520px;
        box-shadow: 0 12px 48px rgba(0,255,220,0.04), inset 0 0 30px rgba(0,255,220,0.04);
        border: 2px solid rgba(0,255,220,0.06);
      }
      .poke-inside {
        background: linear-gradient(180deg, rgba(4,18,24,0.35), rgba(3,6,10,0.2));
        border-radius: 18px;
        padding: 18px;
        position:relative;
        box-shadow: inset 0 8px 24px rgba(0,255,220,0.03);
      }
      .sprite-box {
        width:128px; height:128px; border-radius:18px; background: #02060a; display:flex; align-items:center; justify-content:center; margin: 8px auto 10px;
        box-shadow: 0 0 18px rgba(0,255,220,0.28), inset 0 0 10px rgba(0,255,220,0.06);
        border: 6px solid rgba(0,255,220,0.04);
      }
      .sprite-box img { width: 96px; height: 96px; image-rendering: pixelated; }
      .poke-title { color: #30fff3; font-size: 30px; text-align:center; letter-spacing: 4px; margin-top: 6px; text-shadow: 0 0 12px rgba(0,255,220,0.14); }
      .poke-meta { color: #c9fff7; text-align:center; font-size: 15px; margin-top:8px; }
      .types-line { display:flex; justify-content:center; gap:8px; margin-top: 8px; }
      .type-badge { padding:8px 14px; border-radius: 25px; font-weight:700; color: #03222a; text-shadow:none; box-shadow: 0 8px 40px rgba(0,255,220,0.06); }
      
      /* big combined chart box */
      .compare-box {
        margin-top: 6px;
        background: linear-gradient(180deg, rgba(2,18,20,0.5), rgba(0,6,8,0.36));
        border-radius: 20px;
        padding: 18px;
        border: 2px solid rgba(0,255,220,0.05);
        box-shadow: 0 12px 40px rgba(0,255,220,0.02), inset 0 0 30px rgba(0,255,220,0.02);
      }
      
      /* moves columns */
      .moves-row { display:flex; gap:24px; margin-top:18px; flex-wrap:wrap; justify-content:center; }
      .moves-col {
        width: 480px;
        background: linear-gradient(180deg, rgba(3,20,24,0.35), rgba(0,8,10,0.22));
        padding: 16px; border-radius: 16px; border: 2px solid rgba(0,255,220,0.04);
        box-shadow: inset 0 10px 30px rgba(0,255,220,0.02);
        max-height: 320px; overflow-y: auto;
      }
      .moves-col h4 { color: #9ef7e7; margin-top: 0; letter-spacing: 2px; }
      .moves-col ul { list-style:none; padding-left: 0; margin:0; }
      .moves-col li { padding: 8px 6px; border-bottom: 1px solid rgba(255,255,255,0.03); display:flex; justify-content:space-between; align-items:center; color:#d7fff7; }
      .moves-col .move-type { font-weight:800; padding:3px 8px; border-radius: 12px; color: #031415; }
      
      /* small labels above moves */
      .type-label { text-align:center; padding:8px 12px; border-radius: 18px; background: rgba(0,255,220,0.03); color:#9ef7e7; font-weight:700; margin-bottom:10px; display:inline-block; }
      
      /* scroll bar aesthetics */
      .moves-col::-webkit-scrollbar { width: 10px; }
      .moves-col::-webkit-scrollbar-track { background: rgba(255,255,255,0.02); border-radius: 10px; }
      .moves-col::-webkit-scrollbar-thumb { background: rgba(0,255,220,0.12); border-radius: 10px; }
      
      /* error text */
      .err { color: #ff6b6b; font-weight:700; margin-top:6px; text-align:center; }
      
      /* responsive */
      @media (max-width: 1150px) {
        .pokedex-card, .moves-col { width: 95%; }
        .cards-row { flex-direction: column; align-items:center; }
        .moves-row { flex-direction: column; align-items:center; }
      }
    "))
  ),
  
  div(class = "app-wrap",
      div(style="display:flex; justify-content:space-between; align-items:center;",
          h1(class = "title", "Futuristic Pokédex: Compare Pokémon"),
          actionButton("go_btn_top", "Scan", class = "scan-btn")
      ),
      
      div(class = "top-inputs",
          textInput("pokemon1", NULL, placeholder = "Raichu", width = "260px"),
          textInput("pokemon2", NULL, placeholder = "Pikachu", width = "260px"),
          actionButton("go_btn", "Scan", class = "scan-btn")
      ),
      
      uiOutput("error_message"),
      
      div(class = "cards-row",
          uiOutput("poke1_card"),
          uiOutput("poke2_card")
      ),
      
      div(class = "compare-box",
          plotOutput("compare_plot", height = "360px")
      ),
      
      div(class = "moves-row",
          div(style="text-align:center; width:100%; display:flex; justify-content:center; gap:20px; margin-top:10px;",
              div(class="type-label", uiOutput("poke1_types_label")),
              div(class="type-label", uiOutput("poke2_types_label"))
          ),
          uiOutput("moves_col1"),
          uiOutput("moves_col2")
      )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  # unify both scan buttons
  observeEvent(input$go_btn_top, { session$sendInputMessage("go_btn", list()) })
  
  # reactive to fetch both pokemon on button
  both_poke <- eventReactive(input$go_btn, {
    isolate({
      p1 <- fetch_pokemon_stats(input$pokemon1)
      p2 <- fetch_pokemon_stats(input$pokemon2)
      list(p1 = p1, p2 = p2)
    })
  })
  
  # moves with types: fetch type for each move (slow for many moves)
  moves_with_types <- reactive({
    data <- both_poke()
    if (is.null(data)) return(list(poke1 = NULL, poke2 = NULL))
    fetch_for <- function(moves_list) {
      if (is.null(moves_list) || length(moves_list) == 0) return(NULL)
      # limit or keep all? We'll fetch all but this is rate heavy
      move_names <- vapply(moves_list, function(m) m$name, FUN.VALUE = character(1))
      move_urls  <- vapply(moves_list, function(m) m$url,  FUN.VALUE = character(1))
      types_vec <- vapply(move_urls, function(u) {
        # resilient call
        tryCatch(fetch_move_type(u), error = function(e) NA_character_)
      }, FUN.VALUE = character(1))
      data.frame(move = move_names, mtype = types_vec, stringsAsFactors = FALSE)
    }
    list(poke1 = fetch_for(data$p1$moves), poke2 = fetch_for(data$p2$moves))
  })
  
  output$error_message <- renderUI({
    data <- both_poke()
    if (is.null(data)) return(NULL)
    if (is.null(data$p1) && is.null(data$p2)) {
      div(class="err", "Neither Pokémon found. Try 'Raichu' and 'Pikachu'.")
    } else if (is.null(data$p1)) {
      div(class="err", paste0("Pokémon 1 ('", input$pokemon1, "') not found."))
    } else if (is.null(data$p2)) {
      div(class="err", paste0("Pokémon 2 ('", input$pokemon2, "') not found."))
    } else {
      NULL
    }
  })
  
  render_card <- function(poke, moves_df, id_prefix = "p1") {
    if (is.null(poke)) return(NULL)
    types_html <- paste0(
      sapply(poke$types, function(t) {
        bg <- if (!is.null(type_colors[[t]])) type_colors[[t]] else "#9ef7e7"
        sprintf('<span class="type-badge" style="background:%s">%s</span>', bg, toupper(t))
      }),
      collapse = " "
    )
    tagList(
      div(class = "pokedex-card",
          div(class = "poke-inside",
              div(class = "sprite-box",
                  if (!is.null(poke$image)) tags$img(src = poke$image, alt = poke$name) else tags$div("No image")
              ),
              div(class = "poke-title", toupper(poke$name)),
              div(class = "poke-meta", sprintf("Height: %.1f m | Weight: %.1f kg", poke$height, poke$weight)),
              div(class = "types-line", HTML(types_html)),
              # small stat mini-plot could go here, but main compare plot below is primary
              tags$div(style = "margin-top:8px;"),
              # placeholder for small stats
              tags$div(style="display:flex; justify-content:center; gap:8px; margin-top:8px;",
                       lapply(seq_len(nrow(poke$stats)), function(i) {
                         stat_name <- poke$stats$stat[i]
                         stat_val  <- poke$stats$value[i]
                         tags$div(style="text-align:center; width:68px;",
                                  tags$div(style="font-size:12px; color:#8bf3e7; text-transform:uppercase;", stat_name),
                                  tags$div(style="font-size:18px; color:#d5fff6; font-weight:800;", stat_val)
                         )
                       })
              )
          )
      )
    )
  }
  
  output$poke1_card <- renderUI({
    data <- both_poke()
    if (is.null(data)) return(NULL)
    render_card(data$p1, moves_with_types()$poke1, "p1")
  })
  
  output$poke2_card <- renderUI({
    data <- both_poke()
    if (is.null(data)) return(NULL)
    render_card(data$p2, moves_with_types()$poke2, "p2")
  })
  
  # types label above moves lists
  output$poke1_types_label <- renderText({
    data <- both_poke()
    if (is.null(data) || is.null(data$p1)) return("POKEMON 1 Types: -")
    paste0(toupper(data$p1$name), " Type(s): ", toupper(paste(data$p1$types, collapse = " / ")))
  })
  output$poke2_types_label <- renderText({
    data <- both_poke()
    if (is.null(data) || is.null(data$p2)) return("POKEMON 2 Types: -")
    paste0(toupper(data$p2$name), " Type(s): ", toupper(paste(data$p2$types, collapse = " / ")))
  })
  
  # moves columns
  output$moves_col1 <- renderUI({
    data <- both_poke()
    mw <- moves_with_types()
    if (is.null(data) || is.null(data$p1)) return(NULL)
    df <- mw$poke1
    div(class = "moves-col",
        h4(paste0(toupper(data$p1$name), " Moves")),
        if (is.null(df) || nrow(df) == 0) {
          "No moves found."
        } else {
          tags$ul(
            lapply(seq_len(nrow(df)), function(i) {
              mv <- df$move[i]
              mt <- df$mtype[i]
              bgcolor <- if (!is.null(type_colors[[mt]])) type_colors[[mt]] else "#bfeffd"
              tags$li(HTML(sprintf("<span style='opacity:0.9;'>%s</span>", toupper(mv))),
                      tags$span(style = sprintf("background:%s;color:#031415;", bgcolor), class = "move-type", toupper(ifelse(is.na(mt), "UNKNOWN", mt))))
            })
          )
        }
    )
  })
  
  output$moves_col2 <- renderUI({
    data <- both_poke()
    mw <- moves_with_types()
    if (is.null(data) || is.null(data$p2)) return(NULL)
    df <- mw$poke2
    div(class = "moves-col",
        h4(paste0(toupper(data$p2$name), " Moves")),
        if (is.null(df) || nrow(df) == 0) {
          "No moves found."
        } else {
          tags$ul(
            lapply(seq_len(nrow(df)), function(i) {
              mv <- df$move[i]
              mt <- df$mtype[i]
              bgcolor <- if (!is.null(type_colors[[mt]])) type_colors[[mt]] else "#bfeffd"
              tags$li(HTML(sprintf("<span style='opacity:0.9;'>%s</span>", toupper(mv))),
                      tags$span(style = sprintf("background:%s;color:#031415;", bgcolor), class = "move-type", toupper(ifelse(is.na(mt), "UNKNOWN", mt))))
            })
          )
        }
    )
  })
  
  # combined comparison plot of both pokemon stats (bar grouped)
  output$compare_plot <- renderPlot({
    data <- both_poke()
    if (is.null(data) || (is.null(data$p1) && is.null(data$p2))) return(NULL)
    # create data frame with both stats in order: hp, attack, defense, special-attack, special-defense, speed
    stat_order <- c("hp","attack","defense","special-attack","special-defense","speed")
    p1_stats <- if (!is.null(data$p1)) data.frame(stat = data$p1$stats$stat, value = data$p1$stats$value, name = data$p1$name, stringsAsFactors = FALSE) else NULL
    p2_stats <- if (!is.null(data$p2)) data.frame(stat = data$p2$stats$stat, value = data$p2$stats$value, name = data$p2$name, stringsAsFactors = FALSE) else NULL
    
    combined <- bind_rows(p1_stats, p2_stats) %>%
      mutate(stat = factor(stat, levels = stat_order))
    
    # ensure both have rows for each stat (fill zeros if missing)
    all_names <- unique(combined$name)
    grid_df <- expand.grid(stat = stat_order, name = all_names, stringsAsFactors = FALSE)
    combined_full <- left_join(grid_df, combined, by = c("stat","name")) %>%
      mutate(value = ifelse(is.na(value), 0, value),
             stat = factor(stat, levels = stat_order))
    
    # Neon palette for two series
    palette_vals <- c("#44ffd8", "#12d2b2")
    # safe mapping of name to palette
    names(palette_vals) <- all_names[1:length(palette_vals)]
    
    ggplot(combined_full, aes(x = stat, y = value, fill = name)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6, color = NA, alpha = 0.98) +
      labs(title = sprintf("Stats Comparison: %s vs %s",
                           ifelse(is.null(data$p1), "-", toupper(data$p1$name)),
                           ifelse(is.null(data$p2), "-", toupper(data$p2$name))),
           x = "Stat", y = "Base Stat Value") +
      scale_x_discrete(labels = function(x) gsub("-", "\n", x)) +
      scale_fill_manual(values = palette_vals, guide = guide_legend(title = "Pokémon")) +
      theme_minimal(base_family = "Orbitron") +
      theme(
        panel.background = element_rect(fill = "#071114", color = NA),
        plot.background  = element_rect(fill = "#071114", color = NA),
        panel.grid.major = element_line(color = alpha("white", 0.08)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "#9ef7e7"),
        axis.text = element_text(color = "#bfeffd"),
        plot.title = element_text(color = "#30fff3", size = 18, hjust = 0),
        legend.position = "right",
        legend.background = element_rect(fill = "#071114", color = NA),
        legend.key = element_rect(fill = "#071114")
      ) +
      coord_cartesian(ylim = c(0, max(100, combined_full$value, na.rm = TRUE) * 1.05))
  })
}

shinyApp(ui, server)
