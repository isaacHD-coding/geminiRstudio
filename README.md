# rstudio-gemini-assistant
A simple Shiny add-in that connects Gemini 2.0 Flash to RStudio with editor context and multi-turn conversation.

The assistant reads the currently active R script and uses it as context to generate responses to your prompts. It supports **multi-turn conversations**, making it useful for code debugging, generation, and learning.

## Features

- Sends your active R code to Gemini as context (either the whole file or the highlighted section)
- Remembers previous prompts and replies for full conversations
- Uses Shiny Gadgets to integrate directly with the RStudio interface
- Demonstrates API calls, cloud interaction, and RStudio Add-in tooling

## How It Works

Each time you click **Generate**, the assistant:
1. Reads your currently open R script.
2. Prepends it to a running conversation history.
3. Sends the full prompt to Gemini 2.0 Flash via the Google Generative Language API.
4. Displays the assistant's response and updates the conversation.

## Requirements

- R (>= 4.0)
- RStudio
- Packages: `shiny`, `httr`, `jsonlite`, `rstudioapi`

You must also set your Gemini API key:

```r
Sys.setenv(GEMINI_API_KEY = "your_api_key_here")
```
## Installation Instructions

- Run `devtools::install_github("https://github.com/isaacHD-coding/geminiRstudio")` in your RStudio Console.
- Run `library(GeminiAssistant) to load the package.
- Run the function `gemini_assistant()` to open the assistant.
