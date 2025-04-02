library(shiny)
library(httr)
library(jsonlite)
library(rstudioapi)
library(stringr)

gemini_assistant <- function() {
  # print("Gemini Assistant V12 Started")
  
  # UI with conversation history panel and input area
  ui <- fluidPage(
    titlePanel("Gemini Assistant"),
    fluidRow(
      column(12,
             # Add a conversation history output with scrolling
             div(style = "height: 300px; overflow-y: scroll; border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                 uiOutput("conversationOutput")
             ),
             # Input area with send button
             fluidRow(
               column(10, textAreaInput("promptInput", NULL, height = "100px", placeholder = "Enter your question here...")),
               column(2, actionButton("sendButton", "Send", width = "100%", style = "margin-top: 25px;"))
             ),
             # Add a refresh context button 
             actionButton("refreshContextButton", "Refresh Code Context", style = "margin-top: 10px;"),
             # Display current context source
             textOutput("contextSourceInfo")
      )
    )
  )
  
  server <- function(input, output, session) {
    # Initialize reactive values to store conversation history and context
    values <- reactiveValues(
      conversation = list(),  # To store the conversation history
      code_context = "",      # To store the current code context
      context_source = "",    # To track where the context came from
      message_count = 0       # To keep track of message IDs
    )
    
    # Function to get active document context
    get_code_context <- function() {
      tryCatch({
        # Ensure the source editor is active so we fetch the correct document
        rstudioapi::executeCommand("activateSource")
        Sys.sleep(0.1)  # Give the UI a moment to switch focus
        
        # Use getSourceEditorContext if available (requires a newer rstudioapi)
        if ("getSourceEditorContext" %in% ls("package:rstudioapi")) {
          context <- rstudioapi::getSourceEditorContext()
        } else {
          context <- rstudioapi::getActiveDocumentContext()
        }
        
        # If there's selected text, use it; otherwise, use full document contents
        if (!is.null(context$selection) && length(context$selection) > 0 &&
            nchar(context$selection[[1]]$text) > 0) {
          values$context_source <- "Selected text in editor"
          return(paste(context$selection[[1]]$text, collapse = "\n"))
        } else {
          values$context_source <- "Full document from active editor"
          if (length(context$contents) > 0) {
            return(paste(context$contents, collapse = "\n"))
          } else {
            values$context_source <- "No document open or empty document"
            return("No code available in editor")
          }
        }
      }, error = function(e) {
        values$context_source <- paste("Error getting context:", e$message)
        return("Error retrieving code context from RStudio")
      })
    }
    
    # Get initial code context when app starts
    observe({
      values$code_context <- get_code_context()
    }, priority = 1000)
    
    # Display context source info
    output$contextSourceInfo <- renderText({
      paste("Context source:", values$context_source)
    })
    
    # Auto-refresh context before sending message
    auto_refresh_context <- function() {
      # Only refresh if we're using the full document (don't override user selections)
      if (!grepl("Selected text", values$context_source)) {
        values$code_context <- get_code_context()
      }
    }
    
    # Refresh code context when button is clicked
    observeEvent(input$refreshContextButton, {
      values$code_context <- get_code_context()
      showNotification("Code context refreshed", type = "message")
    })
    
    # Function to send message to Gemini API
    send_to_gemini <- function(prompt, conversation_history) {
      # Retrieve the API key from environment variables
      api_key <- Sys.getenv("GEMINI_API_KEY")
      if (api_key == "") {
        return("Error: GEMINI_API_KEY is not set. Use Sys.setenv(GEMINI_API_KEY = 'your_api_key') in R.")
      }
      
      # Define the Gemini model endpoint
      model_id <- "gemini-2.0-flash"
      endpoint <- sprintf("https://generativelanguage.googleapis.com/v1/models/%s:generateContent", model_id)
      
      # Create the conversation history for the API
      api_contents <- list()
      
      # Add system message with code context at the beginning
      system_prompt <- paste0(
        "You are an expert R programmer assistant. ",
        "The following is the code context from the user's RStudio environment:\n\n```r\n", 
        values$code_context, 
        "\n```\n\nPlease analyze this code when answering questions."
      )
      
      # Add system message
      api_contents[[1]] <- list(
        role = "user",
        parts = list(list(text = system_prompt))
      )
      
      # Add conversation history
      for (msg in conversation_history) {
        api_contents[[length(api_contents) + 1]] <- list(
          role = msg$role,
          parts = list(list(text = msg$content))
        )
      }
      
      # Construct the request body
      request_body <- list(
        contents = api_contents
      )
      
      # Send the POST request to the Gemini API
      res <- POST(
        url = paste0(endpoint, "?key=", api_key),
        add_headers(`Content-Type` = "application/json"),
        body = toJSON(request_body, auto_unbox = TRUE),
        encode = "json"
      )
      
      # Parse and handle the API response
      if (status_code(res) != 200) {
        error_content <- content(res, as = "parsed", type = "application/json")
        error_message <- if (!is.null(error_content$error$message)) {
          error_content$error$message
        } else {
          paste("API Error:", status_code(res))
        }
        return(error_message)
      }
      
      # Extract and return the generated content
      response_content <- content(res, as = "parsed", type = "application/json")
      if (!is.null(response_content$candidates) && length(response_content$candidates) > 0) {
        if (!is.null(response_content$candidates[[1]]$content$parts[[1]]$text)) {
          return(response_content$candidates[[1]]$content$parts[[1]]$text)
        }
      }
      return("No response generated. Please try again.")
    }
    
    # Simple function to format code blocks with pre tags
    format_code_blocks <- function(text) {
      # First wrap fenced code blocks with proper HTML
      # This regex looks for ```r or ``` followed by content and closing ```
      text <- gsub("```([a-zA-Z]*)\n(.*?)\n```", "<pre class='code-block'>\\2</pre>", text, perl = TRUE, ignore.case = TRUE)
      
      # Handle inline code with backticks
      text <- gsub("`([^`]+)`", "<code>\\1</code>", text)
      
      # Replace line breaks with <br> tags
      text <- gsub("\n", "<br>", text)
      
      return(text)
    }
    
    # Handle send button click
    observeEvent(input$sendButton, {
      req(input$promptInput)
      
      # Get user prompt and clear input field
      user_prompt <- isolate(input$promptInput)
      updateTextAreaInput(session, "promptInput", value = "")
      
      # Auto-refresh the context before sending
      auto_refresh_context()
      
      # Use str_trim to check if input is empty
      if (nchar(str_trim(user_prompt)) == 0) return()
      
      # Add user message to conversation
      values$message_count <- values$message_count + 1
      user_message <- list(
        id = values$message_count,
        role = "user",
        content = user_prompt,
        timestamp = Sys.time()
      )
      values$conversation <- c(values$conversation, list(user_message))
      
      # Prepare conversation history for API
      conv_for_api <- lapply(values$conversation, function(msg) {
        list(role = msg$role, content = msg$content)
      })
      
      # Show loading message
      values$message_count <- values$message_count + 1
      loading_message <- list(
        id = values$message_count,
        role = "model",
        content = "Thinking...",
        timestamp = Sys.time()
      )
      values$conversation <- c(values$conversation, list(loading_message))
      
      # Send to Gemini API
      response <- send_to_gemini(user_prompt, conv_for_api)
      
      # Replace loading message with actual response
      values$conversation[[length(values$conversation)]] <- list(
        id = values$message_count,
        role = "model",
        content = response,
        timestamp = Sys.time()
      )
    })
    
    # Render conversation history
    output$conversationOutput <- renderUI({
      if (length(values$conversation) == 0) {
        return(tags$div(
          tags$p("Welcome to Gemini Assistant. Start by sending a message.", style = "color: #888;"),
          tags$p(paste0("Current code context: ", if(nchar(values$code_context) > 50) paste0(substr(values$code_context, 1, 50), "...") else values$code_context), style = "color: #888; font-size: 0.8em;")
        ))
      }
      
      # Create HTML for conversation
      message_elements <- lapply(values$conversation, function(msg) {
        if (msg$role == "user") {
          tags$div(
            class = "user-message",
            style = "background-color: #e9f5ff; padding: 10px; margin: 5px 0; border-radius: 5px; text-align: right;",
            tags$strong("You: "),
            tags$span(msg$content)
          )
        } else {
          # Format the response with basic HTML instead of using markdown package
          formatted_content <- format_code_blocks(msg$content)
          
          tags$div(
            class = "assistant-message",
            style = "background-color: #f1f1f1; padding: 10px; margin: 5px 0; border-radius: 5px;",
            tags$strong("Gemini: "),
            tags$span(HTML(formatted_content)),
            tags$style(HTML("
              .code-block {
                background-color: #f8f8f8;
                border: 1px solid #ddd;
                border-radius: 3px;
                padding: 8px;
                margin: 10px 0;
                font-family: monospace;
                white-space: pre-wrap;
                display: block;
              }
              code {
                background-color: #f8f8f8;
                border: 1px solid #ddd;
                border-radius: 2px;
                padding: 1px 4px;
                font-family: monospace;
              }
            "))
          )
        }
      })
      
      # Return all messages
      do.call(tagList, message_elements)
    })
    
    # Auto-scroll to bottom when conversation updates
    observe({
      if (length(values$conversation) > 0) {
        session$sendCustomMessage("scrollToBottom", TRUE)
      }
    })
  }
  
  # Add JavaScript for auto-scrolling
  ui <- tagList(
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('scrollToBottom', function(should_scroll) {
          var conversationDiv = document.querySelector('div[style*=\"overflow-y: scroll\"]');
          if (conversationDiv) {
            conversationDiv.scrollTop = conversationDiv.scrollHeight;
          }
        });
      "))
    ),
    ui
  )
  
  # Launch the Shiny app
  viewer <- rstudioapi::viewer
  runGadget(ui, server, viewer = viewer)
}
# For direct testing
# gemini_assistant()
