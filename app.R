shinyApp(
  ui = tagList(
    navbarPage(
      theme = shinythemes::shinytheme("yeti"),
      "Internet for Prosperity",
      tabPanel("best-connected/worst-connected countries",
        #sidebarPanel(
        # fileInput("file", "File input:"),
        #  textInput("txt", "Text input:", "general"),
        #  sliderInput("slider", "Slider input:", 1, 100, 30),
        #  tags$h5("Default actionButton:"),
        #  actionButton("action", "Search"),
        # tags$h5("actionButton with CSS class:"),
        #  actionButton("action2", "Action button", class = "btn-primary")
        #),
        #HTML('<style>background-image: url("https://www.onewayticket.ml/s/header.png");</style>'),
        HTML('<style>.navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>li:hover, .navbar-default .navbar-nav>.active>a:focus {    color: #ffffff;    background-color: orange;    font-weight: 600;} .container-fluid {    padding-left: 0px;    padding-right: -15px;} .navbar-default {padding-left: 5px} .navbar-static-top { margin-bottom: 0px; }</style>'),
        HTML('<iframe src="/s/Q1/" style="border: 1px solid #AAA; width: 100%; height: 800px"></iframe>
                                  <div class="caption"> </div>')
      ),
      tabPanel("internet connectivity over the past decade", HTML('<iframe src="/s/Q2/" style="border: 1px solid #AAA; width: 100%; height: 600px"></iframe>
                                  <div class="caption"> </div>')),
      tabPanel("FSI", HTML('<iframe src="/s/Q3/" style="border: 1px solid #AAA; width: 100%; height: 660px"></iframe>
                                  <div class="caption"> </div>')),
      tabPanel("increase/decrease in internet connectivity", HTML('<iframe src="/s/Q5/" style="border: 1px solid #AAA; width: 100%; height: 660px"></iframe>
                                  <div class="caption"> </div>')),
      tabPanel("Hypothesis", HTML('<iframe src="/s/Q4/" style="border: 1px solid #AAA; width: 100%; height: 660px"></iframe>
                                  <div class="caption"> </div>'))
    )
  ),
  server = function(input, output) {
    output$txtout <- renderText({
      paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
      head(cars, 4)
    })
  }
)
