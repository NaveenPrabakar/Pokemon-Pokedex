# Pokémon Stats Viewer

A Shiny web application that fetches and displays Pokémon statistics, types, and moves using the PokéAPI. The app features a Pokédex-inspired design with a sleek red and black theme.

## Features

- **Search Pokémon by Name**: Enter any Pokémon name to retrieve its data
- **Visual Stats Display**: Interactive bar chart showing base stats (HP, Attack, Defense, etc.)
- **Pokémon Information**: Displays height, weight, and sprite image
- **Type Information**: Shows all types for the selected Pokémon
- **Move List**: Comprehensive scrollable list of all moves the Pokémon can learn
- **Pokédex-Style UI**: Custom CSS styling with a red/black gradient theme

## Prerequisites

Before running the application, make sure you have R installed along with the following packages: shiny, httr, jsonlite, and ggplot2.

## Installation

1. Clone or download this repository
2. Open R or RStudio
3. Install the required packages
4. Set your working directory to the app location
5. Run the application

## Usage

1. Launch the app
2. Enter a Pokémon name in the text input field (e.g., Pikachu, Charizard, Mewtwo)
3. Click the Scan button
4. View the Pokémon's sprite image, height and weight, base stats visualization, types, and available moves

## How It Works

The application uses the PokéAPI to fetch real-time Pokémon data:

1. User inputs a Pokémon name
2. The app sends a GET request to the PokéAPI
3. JSON response is parsed and processed
4. Data is displayed in an interactive, visually appealing format

## API Reference

This app uses the free PokéAPI at https://pokeapi.co/api/v2/pokemon/{name}

No API key is required.

## Customization

You can customize the app by modifying the colors by editing the CSS in the tags$style() section, adjusting the plot theme by modifying theme_minimal() parameters in the renderPlot() function, or adding or removing data fields from the PokéAPI response.

## Error Handling

If a Pokémon name is not found or is misspelled, the app displays an error message: Pokémon not found. Please check the name.

## Credits

- PokéAPI: https://pokeapi.co/
- Pokémon: © Nintendo/Creatures Inc./GAME FREAK inc.

## License

This project is open source and available for educational and personal use.

## Note

Pokémon names are case-insensitive. The app converts all inputs to lowercase before querying the API.
