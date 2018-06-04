xmlFunctionUI <- function(id){
  tabItem(
    tabName = "relatieFamilie",
    bootstrapPage(
      fileInput("File", "Incarca XML"),
      fileInput("FileXSL", "Incarca XSL"),
      tableOutput("Data"),
      column(
        width = 2,
        radioButtons(inputId = "tip",
                     label =  "XML",
                     choices = c("XML" = "XML",
                                 "XSL" = "XSL"),
                     selected = "XSL")),
        column(
          width = 4,
        tabPanel("Result", 
                 uiOutput("text")))
      
      
      )
  )
}

xmlFunctionServer <- function(input = input, output = output, session = session){


  Data <- eventReactive(input$File, {
    
    # Se atribuie xml-ul introdus unei variabile de tip xml
    doc <- htmlTreeParse(file = paste0(input$File$datapath),useInternalNodes = TRUE)
    
    # Se ia fiecare tip din XML
      tip = xpathApply(doc, "//tip")

      parinti <- tip

      # Se initializeaza cu un vector null pentru a ii putea popula
      codParinte <- c()
      numeParinte <- c()
      manoperaParinte <- c()
      dataParinte <- c()
      oraParinte <- c()
      categorieParinte <- c()

      
      # Pentru fiecare parinte de tip tip se iau valorile si se salveaza in vectorii creati anterior
      for(i in 1:length(parinti)){
        codParinte[i] <- xmlValue(xmlChildren(parinti[[i]])[[1]])
        numeParinte[i] <- xmlValue(xmlChildren(parinti[[i]])[[2]])
        manoperaParinte[i] <- xmlValue(xmlChildren(parinti[[i]])[[3]])
        dataParinte[i] <- xmlValue(xmlChildren(parinti[[i]])[[4]])
        oraParinte[i] <- xmlValue(xmlChildren(parinti[[i]])[[5]])
        categorieParinte[i] <- xmlValue(xmlParent(xmlParent(parinti[[i]]))[[1]])

      }
      
      # Se convertesc la dataframe pentru a putea fi alipiti
      codParinte <- as.data.frame(codParinte)
      numeParinte <- as.data.frame(numeParinte)
      manoperaParinte <- as.data.frame(manoperaParinte)
      dataParinte <- as.data.frame(dataParinte)
      oraParinte <- as.data.frame(oraParinte)
      categorieParinte <- as.data.frame(categorieParinte)

      # Se unesc toate valorile aflate anterior
      dataFinal <- cbind(categorieParinte, codParinte, numeParinte, manoperaParinte, dataParinte, oraParinte)
      
      # Se schimba numele coloanelor pentru a fi mai explicite
      colnames(dataFinal) <- c("Categorie", "Cod Categorie", "Programare La", "Angajat",
                               "Data Programare", "Ora Programare")
      dataFinal
    
    
  })
  
  output$Data <- renderTable({
    if(input$tip == "XML"){
      Data()
    }

  })

  output$text<-renderUI({
    inFile <- input$FileXSL
      if (is.null(inFile)){
        return(NULL)
      }else{
        if(input$tip == "XSL" & !is.null(input$File) & !is.null(input$FileXSL)){
          doc<-read_xml(file(paste0(input$File$datapath)))
          style <- read_xml(file(paste0(input$FileXSL$datapath)))
          html <- xml_xslt(doc, style)
          
          # Se scrie html-ul in PC 
          write_html(html,file = "../XML-Tema/dsadada.html")
          HTML(readLines("../XML-Tema/dsadada.html"))
        }
        
        
      }

    })
  

}


