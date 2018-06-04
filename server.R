server <- shinyServer(function(input, output, session) {
  xmlFunctionServer(input = input, output = output, session = session)
})
