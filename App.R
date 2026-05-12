library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  
  # Custom CSS for better styling
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      .title-section {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 30px;
        border-radius: 10px;
        margin-bottom: 30px;
        text-align: center;
        box-shadow: 0 4px 6px rgba(0,0,0,0.3);
      }
      .title-section h1 {
        margin: 0;
        font-size: 2.5em;
        font-weight: bold;
      }
      .subtitle {
        font-size: 1.1em;
        opacity: 0.95;
        margin-top: 10px;
      }
      .input-card {
        background: #2d2d44;
        padding: 25px;
        border-radius: 8px;
        border-left: 5px solid #667eea;
        margin-bottom: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.2);
      }
      .result-card {
        background: #2d2d44;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 15px;
        text-align: center;
        box-shadow: 0 2px 8px rgba(0,0,0,0.2);
      }
      .positive-result {
        border-top: 5px solid #4CAF50;
        background: rgba(76, 175, 80, 0.1);
      }
      .negative-result {
        border-top: 5px solid #f44336;
        background: rgba(244, 67, 54, 0.1);
      }
      .result-label {
        font-size: 0.9em;
        color: #b0b0b0;
        text-transform: uppercase;
        letter-spacing: 1px;
        margin-bottom: 10px;
      }
      .result-value {
        font-size: 2em;
        font-weight: bold;
        margin: 10px 0;
      }
      .positive-value {
        color: #4CAF50;
      }
      .negative-value {
        color: #f44336;
      }
      .confidence-badge {
        display: inline-block;
        padding: 8px 16px;
        border-radius: 20px;
        font-weight: bold;
        margin-top: 10px;
        background: #667eea;
        color: white;
      }
      .analyze-btn {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border: none;
        color: white;
        padding: 15px 40px;
        font-size: 1.1em;
        font-weight: bold;
        border-radius: 5px;
        cursor: pointer;
        width: 100%;
        margin-top: 15px;
        transition: transform 0.2s, box-shadow 0.2s;
      }
      .analyze-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
      }
      .loading-spinner {
        margin: 20px 0;
      }
    "))
  ),
  
  # Title Section
  div(class = "title-section",
    h1("🎬 Movie Review Sentiment Analysis"),
    p(class = "subtitle", "Discover whether reviews are positive or negative!")
  ),
  
  # Main Container
  fluidRow(
    column(8, offset = 2,
      # Input Section
      div(class = "input-card",
        h4("✍️ Enter Your Movie Review", style = "color: #667eea; margin-top: 0;"),
        textAreaInput(
          "review",
          label = NULL,
          placeholder = "Type your movie review here... (e.g., 'This movie was amazing! I loved every minute of it!')",
          rows = 6
        ),
        actionButton("predict", "🔍 Analyze Sentiment", class = "analyze-btn")
      ),
      
      # Results Section (hidden initially)
      uiOutput("results_container")
    )
  ),
  
  # Footer
  fluidRow(
    column(12,
      hr(style = "margin-top: 40px;"),
      p("Made with ❤️ for movie enthusiasts", 
        style = "text-align: center; color: #999; margin-top: 20px; margin-bottom: 20px;")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$predict, {
    
    if(input$review == "" || trimws(input$review) == "") {
      output$results_container <- renderUI({
        div(class = "result-card",
          p("⚠️ Please enter a movie review to analyze!", 
            style = "color: #f39c12; font-size: 1.1em;")
        )
      })
      return()
    }
    
    review <- tolower(input$review)
    
    positive_words <- c(
      "good",
      "great",
      "amazing",
      "excellent",
      "love",
      "wonderful",
      "fantastic",
      "brilliant",
      "outstanding",
      "awesome",
      "perfect",
      "beautiful",
      "superb",
      "incredible"
    )
    
    negative_words <- c(
      "bad",
      "terrible",
      "awful",
      "hate",
      "worst",
      "horrible",
      "poor",
      "disappointing",
      "disgusting",
      "waste",
      "boring",
      "dull",
      "unwatchable"
    )
    
    score <- 0
    
    for(word in positive_words){
      if(grepl(word, review)){
        score <- score + 1
      }
    }
    
    for(word in negative_words){
      if(grepl(word, review)){
        score <- score - 1
      }
    }
    
    # Calculate confidence based on absolute score
    confidence_value <- min(100, 65 + abs(score) * 5)
    confidence <- paste0(round(confidence_value), "%")
    
    if(score >= 0){
      prediction <- "👍 POSITIVE"
      emoji <- "😊"
      result_class <- "positive-result"
      value_class <- "positive-value"
    } else {
      prediction <- "👎 NEGATIVE"
      emoji <- "😞"
      result_class <- "negative-result"
      value_class <- "negative-value"
    }
    
    output$results_container <- renderUI({
      div(
        div(class = paste("result-card", result_class),
          div(class = "result-label", "Sentiment Analysis Result"),
          div(class = paste("result-value", value_class), emoji, " ", prediction),
          div(class = "confidence-badge", paste("Confidence:", confidence))
        ),
        div(class = "result-card",
          p(paste("Based on", abs(score), "key words found in your review."),
            style = "color: #b0b0b0; font-size: 0.95em; margin: 0;")
        )
      )
    })
  })
}

shinyApp(ui = ui, server = server)