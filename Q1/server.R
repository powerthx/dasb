function(input, output, session) {


  # Define a reactive expression for the document term matrix
  terms <- reactive({

    freq_values <- reactiveVal(input$freq)
    observe({
      if(input$checkbox){
        freq_values(isolate(input$freq))
        updateSliderInput(session, "freq", max=10)
      }else{
        updateSliderInput(session, "freq", max=300)
      }
    })
    # Change when the "update" button is pressed...
    #input$update
    # ...but not for anything else
    #isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection, input$year, input$freq)
     # })
    })
  })

  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)

  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(words = v$words, freq= v$freq, scale=c(3.5,0.25),
                    max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  }, height = 600, width = 800 )
}
