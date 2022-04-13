#' @export
DCEtool <- function(){
  

  requireNamespace('shiny')
  requireNamespace('rlist')
  requireNamespace('shinythemes')
  requireNamespace('shinyWidgets')
  requireNamespace('mvtnorm')
  requireNamespace('DT')
  requireNamespace('shinycssloaders')
  requireNamespace('writexl')
  requireNamespace('readxl')
  requireNamespace('rlist')
  requireNamespace('idefix')
  requireNamespace('shinyBS')
  requireNamespace('survival')
  requireNamespace('tidyr')
  requireNamespace('mlogit')
  requireNamespace('ggplot2')
  requireNamespace('bslib')
  requireNamespace('dfidx')
  
  
#' @importFrom shinyBS bsModal
#' @importFrom graphics barplot
#' @importFron usethis use_pipe
#' @importFrom magrittr %>%
#' @importFrom survival strata
#' @importFrom survival coxph
#' @importFrom survival clogit
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar 
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom shiny actionButton
#' @importFrom shiny checkboxGroupInput
#' @importFrom shiny shinyApp
#' @importFrom rlist list.match
  
  #external variables
  savechoices <- c()
  resultados <- c()
  num <- c()
  y <- c()
  se <- c()

  ui <- shiny::navbarPage(
    "DCEtool", id = 'inTabset',   
    shiny::tabPanel("Home", value = "start",
             shiny::fluidRow(
               shiny::column(12,
                 align = "center",
                 htmltools::img(src = "assets/granada.png", width = "50%"),  # prefix + filename
                 htmltools::p("Welcome to DCEtool. ",style="text-align:justify;color:black;background-color:LightGrey;padding:15px;border-radius:10px"),
                 shiny::HTML("<p>Find more <a href = 'https://danielpereztr.github.io/DCEtool/'>here</a>.</p>"),
                 shiny::HTML("<p>Please cite as P&eacute;rez-Troncoso (2022). Efficient and Accessible Discrete Choice Experiments: DCEtool (Version 1.0.0). danielpereztr.github.io/DCEtool</p>")
               )
             )
             ), #Home tab
    shiny::tabPanel("Design settings", value = "params", #Design settings tab
      shiny::sidebarLayout(
        shiny::sidebarPanel( #Inputs sidebar panel
          shiny::numericInput('ats', "Number of attributes", 0),
          shiny::uiOutput('dynamic'),
          shiny::actionButton('loadats','Save attributes'),
          htmltools::hr(),
          shiny::uiOutput('altbut'),
          shiny::uiOutput('csetbut'),
          shiny::uiOutput('optcheck'),
          shiny::uiOutput('bayescheck'),
          shiny::uiOutput('savebut'),
          htmltools::hr(),
          shiny::uiOutput('priorsbut'),
          shiny::uiOutput('seedbut'),
          shiny::uiOutput('saveopt')
        ),
        shiny::mainPanel(
          shiny::verbatimTextOutput('levelsnumber'),
          shiny::verbatimTextOutput('errorscheck'),
          shiny::uiOutput('gobut')
        )
      )
    ),
    shiny::tabPanel(title = "Design matrix", value = "desmattab", #Design matrix tab
      shiny::sidebarLayout(
        shiny::sidebarPanel( # Design matrix left tab
          shiny::actionButton("gendesign", "Generate design"),
          htmltools::br(),
          htmltools::br(),
          shiny::downloadButton("downloaddesing","Save design", icon = shiny::icon("download")), #save design
          shiny::fileInput(inputId = "loaddesign",label = "", multiple = FALSE), #load design
          shiny::actionButton("showdesdetails", "Show design details", icon = shiny::icon("eye")),
          htmltools::br(),
          htmltools::br(),
          shiny::actionButton("writeatnames", "Name the attributes", icon = shiny::icon("braille")),
          htmltools::br(),
          htmltools::br(),
          shiny::uiOutput("atnames"),
          htmltools::br(),
          shiny::uiOutput('levdropdown'),
          shiny::uiOutput('levtext')
        ),
        shiny::mainPanel(    # Design matrix main panel
          shiny::uiOutput("spinner"),
          shiny::verbatimTextOutput("desdetails"),
          shiny::verbatimTextOutput("printlevnames"),
          shiny::uiOutput('modmatrix')
        )
      )
    ),
    shiny::tabPanel(title = "Create a survey", value = "createsurv", # Crear la encuesta
      shiny::sidebarLayout(
        shiny::sidebarPanel( # Sidebar Panel for survey creator
          htmltools::p("Intro and end text: "),
          shiny::textAreaInput("intro", "Introductory text in Markdown", rows = 3),
          shiny::textAreaInput("outro", "End text in Markdown", rows = 3),
          htmltools::hr(),
          htmltools::p("Label each alternative: "),
          shiny::uiOutput("labels"),
          htmltools::hr()
        ),
        shiny::mainPanel(
          htmltools::p("Survey preview"),
          htmltools::hr(),
          shiny::uiOutput("dispintrotext"),
          htmltools::br(),
          htmltools::hr(),
          shiny::uiOutput('cstext'),
          shiny::tableOutput("designcoded"),
          shiny::uiOutput('radiodummy', width = '100%'),
          htmltools::br(),
          htmltools::hr(),
          shiny::uiOutput("dispoutrotext"),
        )
      )
    ),
    shiny::tabPanel(title = "Survey",
             shiny::fluidRow(
               shiny::column(12,
                      align = "center",
                      shiny::uiOutput("serialui")
               )
             ),
             shiny::fluidRow(
               shiny::column(12,
                      align = "center",
                      shiny::actionButton(inputId = "popbut", "Launch survey"),
                      htmltools::br(),
                      htmltools::br(),
                      shiny::uiOutput("resultsbut")
                      )
             ),
          shinyBS::bsModal("modalExample", "", "popbut", size = "large",
              shiny::fluidRow(
                shiny::column(12,
                   align = "left",
                   shiny::uiOutput("texts"),
                ),
                shiny::column(12,
                   align = "center",
                   shiny::tableOutput("css"),
                   shiny::uiOutput("cssbut")  
                ),
                
                shiny::column(12,
                   align = "right",
                   shiny::uiOutput('contsurv1')
                 )
                )
               )
    ),
    shiny::tabPanel(title = "Results", value = "results",
             shiny::sidebarLayout(
               shiny::sidebarPanel(
                 shiny::tabsetPanel(id = "restabs",
                    shiny::tabPanel("Data",
                          htmltools::br(),
                             shiny::downloadButton("downloadresults","Save results", icon = shiny::icon("download")),
                             shiny::fileInput(inputId = "loadresults",label = "", multiple = FALSE), #load results
                             shiny::selectInput(
                               inputId = "pricevarcoding",
                               label = "Price variable coding",
                               choices = c("Dummy coding" = "dumcod",
                                           #"There is already a continuous price variable" = "already",
                                           "Code price as continuous variable" = "cont")
                             ),
                             shiny::uiOutput("atpriceselect"),
                             shiny::uiOutput("savethem"),
                             shiny::uiOutput("pricelevbr")
                             ),
                    shiny::tabPanel("Estimation",
                            htmltools::br(),
                             shiny::selectInput("modelname", "Choose estimate", 
                                         choices = c("Select a model" == "nullselect",
                                                     "Conditional logit" = "clogit",
                                                     "Mixed logit" = "mixlogit",
                                                     "Willingness to pay" = "wtp",
                                                     "Figures" = "figures")
                                         ),
                             shiny::uiOutput("modopt"),
                             htmltools::hr(),
                             shiny::uiOutput("modopt2")
                             )
                 )
               ),
               shiny::mainPanel(
                 DT::DTOutput("restable"),
                 shiny::verbatimTextOutput("clog"),
                 shiny::plotOutput("figure")
               )
             )
             ),
    shiny::tabPanel(title = "About", 
               shiny::fluidRow(
                 shiny::column(12,
                        align = "Left",
                        htmltools::h3("About"),
                        shiny::HTML("<p>This app was created by Daniel P&eacute;rez-Troncoso in 2021. Version 1.0.0 was released in April 2022.</p>"),
                        shiny::HTML("<p>Please, if you use this app in your research, reference it as 'P&eacute;rez-Troncoso, D. (2022). DCEtool (1.0.0) [Software]. https://cran.r-project.org/package=DCEtool ' </p>"),
                        shiny::HTML("<p>Find a guide in <a href = 'https://danielpereztr.github.io/posts/DCEtool/'>this link</a></p>"),
                        htmltools::h3("Downloads"),
                        shiny::plotOutput("downloads")
                 )
               )
             )
  )
  
  
  server <- function(input, output, session){
    
    # Check if there is a newer version in CRAN
    shiny::observeEvent(input$inTabset == "start", {
      db <- utils::available.packages(filters = "duplicates")
      cranvers <- db[db[,1] == "DCEtool"][2]
      thisvers <- utils::packageVersion("DCEtool")
      if (cranvers == thisvers){
        
      } else if (cranvers != thisvers) {
        shiny::showNotification("There is a newer version of DCEtool on CRAN. Run install.packages('DCEtool') to update DCEtool.")
      }
    })
    
    #Close shiny 
    session$onSessionEnded(function() {
      shiny::stopApp()
    })
    
    #change tab
    shiny::observeEvent(input$create_DCE, {
      shiny::updateTabsetPanel(session, 'inTabset', selected = "params")
    })
    
    # Values across functions
    values <- shiny::reactiveValues() 
    
    
    # Download flag for later
    values$downloadflag <- 0
    # Choices storage for the survey
    values$choices <- as.data.frame(matrix(ncol = 2, nrow = 0))
    # Designstorage
    values$designstor <- data.frame() 
    
    # Create as many inputs as attributes are
    output$dynamic <- shiny::renderUI({
      tags <- htmltools::tagList()
      for (i in seq_len(input$ats)) {
        tags[[i]] <- shiny::numericInput(paste0('at', i), 
                                  paste0('Levels in attribute ', i),
                                  0)
      }
      tags
    })
    
    
    
    # Create the vector 'levels' with the attributes' levels
    shiny::observeEvent(input$loadats, {
      nats <- input$ats
      levels <- c()
      for (i in seq_len(nats)){
        levels <- append(levels, eval(parse(text = paste0("input$at",i))))
      }
      values$levels <- levels
      values$nats <- nats
      
      # Print the number of attributes and levels
      output$levelsnumber <- shiny::renderPrint({
        cat("Design settings \nNumber of attributes: ", values$nats, "\nLevels per attribute: ", values$levels)
      })
      
      #Ask the alternatives and csets after sending attributes
      output$altbut <- shiny::renderUI({
        shiny::numericInput("nalt", "Alternatives per choice set", value = 2, max = 5, min = 2)
      })
      
      #Ask the number of choice sets
      output$csetbut <- shiny::renderUI({
        shiny::numericInput("nset", "Number of choice sets", value = 12, min = 4)
      })
      
      #Checkbox if opt-out alternative
      output$optcheck <- shiny::renderUI({
        shinyWidgets::materialSwitch(inputId = "optout", label = "Opt-out alternative", value = FALSE, status = "danger")
      })
      
      # Checkboxes to select the optimization procedure
      output$bayescheck <- shiny::renderUI({
        shinyWidgets::materialSwitch(inputId = "bayesian", label = "Bayesian design", status = "danger", value = FALSE)
      })
      
      #Print the button to save the settings
      output$savebut <- shiny::renderUI({
        shiny::actionButton("saveset", "Save settings")
      })
    
      
    })
    
    
    
    # Save and print settings
    shiny::observeEvent(input$saveset,{
      values$alts <- input$nalt
      values$sets <- input$nset
      values$optout <- input$optout
      values$bayesian <- input$bayesian
      output$levelsnumber <- shiny::renderPrint({
        cat("Design settings \nNumber of attributes: ", values$nats, "\nLevels per attribute: ",
            values$levels, "\nAlternatives per choice set: ", values$alts,
            "\nNumber of sets: ", values$sets,
            "\nNull alternative: ", values$optout, "\nBayesian priors: ",
            values$bayesian)
      })
      
      # Show button for priors 
      output$priorsbut <- shiny::renderUI({
        optout <- values$optout
        levels <- values$levels
        nats <- values$nats
        null <- ifelse(optout == TRUE, 1, 0)
        npar <- sum(levels)-nats+null
        priors <- c()
        for (x in seq_len(npar)){
          priors <- append(priors, 0)
        }
        values$npar <- npar
        shiny::textInput("priorsinput", "Prior coefficients", value = paste(shQuote(priors, type="cmd2"), collapse=", "))
      })
      
      # show button for random seed
      output$seedbut <- shiny::renderUI({
        shiny::numericInput("randomseed", "Random seed", value = 9999)
      })
      
      # save options button
      output$saveopt <- shiny::renderUI({
        shiny::actionButton("finalsave", "Save options")
      })
    })
    
    # Print the final saved options
    shiny::observeEvent(input$finalsave,{
      values$priors <- as.numeric(strsplit(as.character(input$priorsinput), ",")[[1]])
      values$seed <- input$randomseed
      output$levelsnumber <- shiny::renderPrint({
        cat("Design settings \nNumber of attributes: ", values$nats, "\nLevels per attribute: ",
            values$levels, "\nAlternatives per choice set: ", values$alts,
            "\nNumber of sets: ", values$sets,
            "\nNull alternative: ", values$optout, "\nBayesian priors: ",
            values$bayesian, "\nPriors: ", values$priors, "\nSeed: ", values$seed) 
      })
      
      #Check errors 
      output$errorscheck <- shiny::renderPrint({
        cat("Error checker results:  \n")
        if (length(values$levels) != values$nats){
          cat("Some error occurred. Please restart DCEtool.\n")
        } else if (values$sets < (sum(values$levels)-1)){
          cat("The number of sets is not enough. Try with ", sum(values$levels)-1, " sets or more.\n")
        } else if (length(values$priors) != values$npar){
          cat("The number of priors does not correspond with the number of parameters. Try using ", values$npar, " prior parameters.")
        } else {
          cat("No errors found.")
        }
        
      })
      
      # Button to create the design matrix
      output$gobut <- shiny::renderUI({
        shiny::actionButton("go", "Happy with the settings? Go to next step")
      })
    })
    
    
    # Move to the next tab
    shiny::observeEvent(input$go,{
      #Move to the Design matrix tab
      shiny::updateTabsetPanel(session, 'inTabset', selected = "desmattab")
    })
    
    
    shiny::observeEvent(input$gendesign,{
      # Loader
      output$spinner <- shiny::renderUI({
        shinycssloaders::withSpinner(DT::dataTableOutput("design"))
      })
      
      # Render the design table
      output$design <- DT::renderDT({
        
        # Decide if bayesian
        if (values$bayesian == TRUE){
          alg = "cea"
        } else {
          alg = "fedorov"
        }
        
        values$alg <- alg
        
        # Priors
        if (values$bayesian == TRUE){
          values$bpriors <- mvtnorm::rmvnorm(100, values$priors, diag(length(values$priors)))
        } else {
          values$bpriors <- values$priors
        }
        
        
        # Generate
        design <- dce_toolbox(attributes = values$levels, csets = values$sets,
                              alts = values$alts, nochoice = values$optout,
                              priors = values$bpriors, alg = alg)
        
        values$design <- design$design
        values$`D-error` <- design$`D-error`
        values$details <- design$details
        as.data.frame(design$design)
      })
    })
    
    # Download button
    
    output$downloaddesing <- shiny::downloadHandler(
      filename = function() {"design.xlsx"},
      content = function(file) {values$downloadflag <- values$downloadflag+1
                                writexl::write_xlsx(values$deslist, file)}
    )
    
    # Load saved design
    shiny::observeEvent(input$loaddesign, {
      #Spinner loader
      output$spinner <- shiny::renderUI({
        shinycssloaders::withSpinner(DT::dataTableOutput("design"))
      })
        # Design loader
        output$design <- DT::renderDT({
        if (!is.null(input$loaddesign)){
          values$`D-error` <- as.numeric(unlist(readxl::read_excel(input$loaddesign$datapath, sheet = "Derror"))) #Update
          values$details <- as.vector(unlist(readxl::read_excel(input$loaddesign$datapath, sheet = "details"))) #Update
          values$nats <- as.numeric(unlist(readxl::read_excel(input$loaddesign$datapath, sheet = "nats"))) #Update
          values$levels <- as.vector(unlist(readxl::read_excel(input$loaddesign$datapath, sheet = "levels"))) #Update
          values$sets <- as.numeric(unlist(readxl::read_excel(input$loaddesign$datapath, sheet = "sets"))) #Update
          values$optout <- as.logical(unlist(readxl::read_excel(input$loaddesign$datapath, sheet = "optout"))) #Update
          values$priors <- as.vector(unlist(readxl::read_excel(input$loaddesign$datapath, sheet = "priors"))) #Update
          values$alts <- as.numeric(unlist(readxl::read_excel(input$loaddesign$datapath, sheet = "alts"))) #Update
          values$bayesian <- as.logical(unlist(readxl::read_excel(input$loaddesign$datapath, sheet = "bayesian")))
          values$atnames <- as.vector(unlist(readxl::read_excel(input$loaddesign$datapath, sheet = "atnames")))
          values$levnames <- as.list(unlist(readxl::read_excel(input$loaddesign$datapath, sheet = "levnames")))
          values$design <- readxl::read_excel(input$loaddesign$datapath) # Print this one
        }
         
      })
        
    })
    
    #Show design details
    shiny::observeEvent(input$showdesdetails, {
      output$desdetails <- shiny::renderPrint(
        cat("Number of attributes: ", values$nats, "\nLevels per attribute: ",
            values$levels, "\nAlternatives per choice set: ", values$alts,
            "\nNumber of sets: ", values$sets,
            "\nNull alternative: ", values$optout, "\nBayesian priors: ",
            values$bayesian, "\nPriors: ", values$priors, "\nAttribute names: ", values$atnames,
            "\nLevel names: ", unlist(values$levnames), "\nDetails:", values$details)
      )
    })
    
    # Create inputs for attribute names
    shiny::observeEvent(input$writeatnames, { 
      output$atnames <- shiny::renderUI({
        tags <- htmltools::tagList()
        for (i in seq_len(values$nats)) {
          tags[[i]] <- shiny::textInput(paste0('atn', i), 
                                    paste0('Name of attribute ', i),
                                    value = paste0("atrib",i))
        }
        tags[[i+1]] <- shiny::actionButton("saveatnames", "Save names")
        tags
      })
    })
    
    # Save the attribute names
    shiny::observeEvent(input$saveatnames, {
      atnames <- c()
      for (i in seq_len(values$nats)){
        atnames <- append(atnames, eval(parse(text = paste0("input$atn",i))))
      }
      values$atnames <- atnames
      output$desdetails <- shiny::renderPrint(
        cat("Number of attributes: ", values$nats, "\nLevels per attribute: ",
            values$levels, "\nAlternatives per choice set: ", values$alts,
            "\nNumber of sets: ", values$sets,
            "\nNull alternative: ", values$optout, "\nBayesian priors: ",
            values$bayesian, "\nPriors: ", values$priors, "\nAttribute names: ", values$atnames,
            "\nLevel names: ", unlist(values$levnames), "\nDetails:", values$details)
      )
      # Create buttons with the attribute names
      shiny::observeEvent(input$saveatnames, {
        output$levdropdown <- shiny::renderUI({
          shiny::selectInput("levdrop", "Change level names", as.list(atnames))
        })
        values$levnames <- list()
      })
      
      # Level names
      shiny::observeEvent(input$levdrop,{
        pos <- match(input$levdrop, values$atnames)
        output$levtext <- shiny::renderUI({
          tags <- htmltools::tagList()
          for (i in seq_len(values$levels[pos])){
            tags[[i]] <- shiny::textInput(paste0("at",pos,"lev",i), paste0("Name level",i),
                                   value = paste0("at",pos,"lev",i))
            tags[[i+1]] <- shiny::actionButton(paste0("savelevs",pos), paste0("Save level ",pos))
          }
          tags
        })
        
        #save level names
        levnames <- list()
        shiny::observeEvent(eval(parse(text = paste0("input$savelevs",pos))),{
          acum <- c()
          for (i in seq_len(values$levels[pos])){
            acum <- append(acum, eval(parse(text = paste0("input$at",pos,"lev",i))))
          }
          values$levnames[[paste0(values$atnames[pos])]] <- acum
          output$printlevnames <- shiny::renderPrint({
            values$levnames
          })
          
        # If all levels have a name, option to paste in the design matrix
          if (length(values$levnames) == length(values$levels)){
            output$modmatrix <- shiny::renderUI({
              shiny::actionButton("modmatbut", "Change names in design matrix")
            })
          }
          
        })
        
      })
      
    })
    
    #Put names in the design matrix
    shiny::observeEvent(input$modmatbut, {
      levvector <- c()
      for (i in 1:length(values$levnames)){
        levvector <- append(levvector, rlist::list.match(values$levnames, values$atnames[i]))
      }
      if (values$optout == FALSE){
      namvect <- append(c("task","alt"), unlist(lapply(levvector, function(x) x[-1])))
      } else {
      namvect <- append(c("task","alt", "optout"), unlist(lapply(levvector, function(x) x[-1])))  
      }
      print(namvect)
      colnames(values$design) <- namvect
      
      #Print the table
      output$design <- DT::renderDT({
        values$design <- as.data.frame(values$design)
      })
      
    })
    
  
    # Save design and other design info (if design changes)
    shiny::observeEvent(values$design, {
      deslist <- list("design" = as.data.frame(values$design), # Save in a list to download in the excel
                      "priors" = as.data.frame(values$priors),
                      "alts" = as.data.frame(values$alts),
                      "optout" = as.data.frame(values$optout),
                      "Derror" = as.data.frame(values$`D-error`),
                      "nats" = as.data.frame(values$nats),
                      "sets" = as.data.frame(values$sets),
                      "details" = as.data.frame(values$details),
                      "levels" = as.data.frame(values$levels),
                      "bayesian" = as.data.frame(values$bayesian),
                      "atnames" = as.data.frame(values$atnames),
                      "levnames" = as.data.frame(unlist(values$levnames)))
      values$deslist <- deslist
      atnames <- values$atnames
      levnames <- values$levnames
      #list the levnames correctly
      if (length(values$levnames)>0 && length(levnames) > length(atnames)){
        levnames2 <- list()
        levnames <- as.vector(unlist(values$levnames))
        for (i in 1:length(values$atnames)){
          levnames2[[i]] <- levnames[1:values$levels[[i]]]
          levnames <- levnames[-c(1:values$levels[i])]
        } 
        values$levnames2 <- levnames2
        
      } else {
        values$levnames2 <- levnames
      }
      
      # Generate another variable with +1 if null
      if (values$optout == TRUE){
        values$ncalts <- values$alts + 1
      } else {
        values$ncalts <- values$alts
      }
      
      #Generate the vector indicating the null alternative
      if (values$optout == TRUE){
        values$nullvect <- c()
        for (i in seq_len(values$alts)){
          values$nullvect <- c(values$nullvect, 0)
        }
        values$nullvect <- c(values$nullvect, 1)
      }
      
      #Generate a no.choice integer to indicate the position
      if (values$optout == TRUE){
        values$no.choice <- values$ncalts
      } else {
        values$no.choice <- NULL
      }
      
    })
    
    
    # survey creator
    output$labels <- shiny::renderUI({
      tags <- htmltools::tagList()
      for (i in 1:values$ncalts) {
        tags[[i]] <- shiny::textInput(paste0('alt_label_', i), 
                                  paste0('Alternative ', i),
                                  paste0('Alternative ',i))
      }
      tags[[i+1]] <- shiny::actionButton("savelabelsbut", "Save labels")
      tags
    })
    
    #Observe labels and save them
    shiny::observeEvent(input$savelabelsbut, {
    labels <- c()
    a <- 0
      for (i in seq_len(values$ncalts)){
        a <- eval(parse(text = paste0("input$alt_label_",i)))
        labels <- append(labels, a)
      }
    values$labels <- labels
    })
    
    # Observe and print intro text
    shiny::observeEvent(input$intro, {
      output$dispintrotext <- shiny::renderUI({
        shiny::markdown(input$intro, .noWS = TRUE)
        })
      values$intro <- input$intro
    })
    
    #Observe and print end text
    shiny::observeEvent(input$outro, {
      output$dispoutrotext <- shiny::renderUI({
        shiny::markdown(input$outro, .noWS = TRUE)
      })
      values$outro <- input$outro
    })
    
    #Render the choice sets
    shiny::observeEvent(input$savelabelsbut,{
      output$designcoded <- shiny::renderTable({
        #Create the coding vector
        coding <- c()
        for (i in seq_len(values$nats)){
          coding <- append(coding, "D")
        }
        levnames2 <- values$levnames2
        values$coding <- coding
        print(coding)
        decodes <- idefix::Decode(as.matrix(values$design[,3:ncol(values$design)]), values$ncalts, values$levnames2,
                          coding = coding, no.choice = values$no.choice, alt.cte = values$nullvect)
        decodes <- decodes$design
        decodes <- cbind("set" = rep(1:values$sets, each = values$ncalts), decodes)
        decodes <- decodes[decodes$set == 1,]
        decodes <- decodes[,2:ncol(decodes)]
        decodes <- t(decodes)
        colnames(decodes) <- values$labels
        decodes <- cbind("Attributes" = values$atnames,decodes)
        decodes
      })
      
      #show serial mode
      
      if (length(values$choices) == 2){
        output$serialui <- shiny::renderUI({
          shiny::radioButtons("serialmode", label = "Serial mode" ,choices = c("No" = "no",
                                                                        "Bliemer & Rose (each respondent)" = "pure",
                                                                        "Each 5 respondents" = "five"),
                       inline = TRUE) 
        })
      }
      
      
    })
    
    # Radio dummies
    shiny::observeEvent(input$savelabelsbut, {
      output$radiodummy <- shiny::renderUI(
        shiny::radioButtons("nullinp", label = "", choices = values$labels, inline = TRUE)
      )
    })
    
    #Show cs text
    shiny::observeEvent(input$savelabelsbut, {
      output$cstext <- shiny::renderUI({
        htmltools::h5("First choice set")
      })
    })
    
   
    
    # Decide when showing the Next > button
    output$contsurv1 <- shiny::renderUI({
      if (values$survclick != values$sets+1){ 
        shiny::actionButton("contsurv", "Next >")
      } else if (values$survclick == values$sets+1){
        shiny::actionButton("nextresp", "Next respondent >")
      }
    })
  
    
    
    # Survey in popup
    shiny::observeEvent(input$popbut, once = TRUE, {
      values$survclick <- 0
  
      
      shiny::observeEvent(input$contsurv, {
        values$survclick <- values$survclick + 1
      })
      
      shiny::observeEvent(values$survclick,{
        if (values$survclick == 0){
          output$css <- NULL
          output$cssbut <- NULL
          output$texts <- shiny::renderUI({
            shiny::markdown(values$intro, .noWS = TRUE)
          })
        } else if (values$survclick > 0 && values$survclick <= values$sets){
          output$texts <- NULL
          output$css <- shiny::renderTable({
            ### Load the table
            decodes <- idefix::Decode(as.matrix(values$design[,3:ncol(values$design)]), values$ncalts, values$levnames2,
                              coding = values$coding, no.choice = values$no.choice, alt.cte = values$nullvect)
            decodes <- decodes$design
            decodes <- cbind("set" = rep(1:values$sets, each = values$ncalts), decodes)
            decodes <- decodes[decodes$set == values$survclick,]
            decodes <- decodes[,2:ncol(decodes)]
            decodes <- t(decodes)
            colnames(decodes) <- values$labels
            decodes <- cbind("Attributes" = values$atnames,decodes)
            decodes
          })
          output$cssbut <- shiny::renderUI({
            shiny::radioButtons("choices", label = "", choices = values$labels, inline = TRUE,
                         selected = "0")
          })
        } else if (values$survclick == values$sets+1){
          output$css <- NULL
          output$cssbut <- NULL
          output$texts <- shiny::renderUI({
            shiny::markdown(values$outro, .noWS = TRUE)
          })
        }
        
        # Save responses to the survey
        shiny::observeEvent(values$survclick, once = TRUE,{
          if (values$survclick > 1 && (values$survclick < values$sets+2)){
            current <- c("cs" = values$survclick-1, "choice" = input$choices)
            values$choices <- rbind(values$choices, current)
            current <- NULL
          }
          names(values$choices) <- c("task", "choice")
          savechoices <<- values$choices
        })
        
        if (length(values$choices > 0)){
          output$resultsbut <- shiny::renderUI({
            shiny::actionButton("resultsgo", "Analyze the results")
          })
        }
        
      }) # End of the function to show the choice sets
      
      # Count the number of respondents
      shiny::observeEvent(input$contsurv, {
        values$respondents <- floor(nrow(values$choices) / values$sets)
        
        # Add the design to the accumulated frame
        if (values$respondents == 0){
          values$designstor <- values$design
        }
        
      })
      
      #Load old or new design depending on the mode
      shiny::observeEvent(input$nextresp, {
        if (input$serialmode == "no"){
          values$survclick <- 0
        } else if (input$serialmode == "pure"){
          larray <- merge(cbind("num" = seq(len = nrow(savechoices)),savechoices), cbind("choice" = values$labels, "userselect" = seq(len = length(values$labels))), by = c("choice"), all =  TRUE)
          larray <- larray %>% tidyr::drop_na(num)
          larray <- larray[order(larray$num),]
          larray$pid <- rep(1:(nrow(larray)/values$sets), len = nrow(savechoices), each = values$sets)
          designstor <- values$designstor
          designstor$pid <- rep(1:(nrow(designstor)/(values$ncalts*values$sets)), each = (values$sets*values$ncalts))
          larray <- merge(designstor, larray, by = c("pid","task"), order = TRUE)
          larray <- larray[order(larray$pid,larray$num),]
          larray$choice <- NULL
          larray$num <- NULL
          larray$choice <- ifelse(larray$userselect == larray$alt, 1, 0)
          larray$userselect <- NULL
          cdesmat <- larray
          #extract the regressors' name to input it in the model
          regressors <- names(cdesmat[,4:(ncol(cdesmat)-1)])
          regressors <- paste0('`',regressors, '`')
          clog <- survival::clogit(stats::as.formula(paste0("choice ~ ",do.call(paste, c(as.list(regressors), sep = " + ")), " + strata(task)")),
                         data = as.data.frame(cdesmat))
          clog <- summary(clog)
          clog$coefficients <- as.data.frame(clog$coefficients)
          clog <- cbind(clog$coefficients[,1],clog$coefficients[,5])
          clog <- cbind(clog,ifelse(clog[,2] <= 0.05, clog[,1], 0))
          if(sum(clog[,3]) != 0){
            for (i in 1:length(clog[,3])){
              if (clog[,3][i] != 0){
                values$priors[i] <- clog[,3][i]
              }
            }
            
            # Decide if bayesian
            if (values$bayesian == TRUE){
              alg = "cea"
            } else {
              alg = "fedorov"
            }
            
            values$alg <- alg
            
            # Priors
            if (values$bayesian == TRUE){
              values$bpriors <- mvtnorm::rmvnorm(100, values$priors, diag(length(values$priors)))
            } else {
              values$bpriors <- values$priors
            }
            
            # Generate
            design <- dce_toolbox(attributes = values$levels, csets = values$sets,
                                  alts = values$alts, nochoice = values$optout,
                                  priors = values$bpriors, alg = alg)
            
            design <- design$design
            
            #Replace the global design
            tempnam <- colnames(values$design)
            colnames(design) <- tempnam
  
            values$design <- design
          }
        } else if (input$serialmode == "five"){
          if ((values$respondents+1) %% 5 == 0){
            larray <- merge(cbind("num" = seq(len = nrow(savechoices)),savechoices), cbind("choice" = values$labels, "userselect" = seq(len = length(values$labels))), by = c("choice"), all =  TRUE)
            larray <- larray %>% tidyr::drop_na(num)
            larray <- larray[order(larray$num),]
            larray$pid <- rep(1:(nrow(larray)/values$sets), len = nrow(savechoices), each = values$sets)
            designstor <- values$designstor
            designstor$pid <- rep(1:(nrow(designstor)/(values$ncalts*values$sets)), each = (values$sets*values$ncalts))
            larray <- merge(designstor, larray, by = c("pid","task"), order = TRUE)
            larray <- larray[order(larray$pid,larray$num),]
            larray$choice <- NULL
            larray$num <- NULL
            larray$choice <- ifelse(larray$userselect == larray$alt, 1, 0)
            larray$userselect <- NULL
            cdesmat <- larray
            #extract the regressors' name to input it in the model
            regressors <- names(cdesmat[,4:(ncol(cdesmat)-1)])
            regressors <- paste0('`',regressors, '`')
            clog <- survival::clogit(stats::as.formula(paste0("choice ~ ",do.call(paste, c(as.list(regressors), sep = " + ")), " + strata(task)")),
                           data = as.data.frame(cdesmat))
            clog <- summary(clog)
            clog$coefficients <- as.data.frame(clog$coefficients)
            clog <- cbind(clog$coefficients[,1],clog$coefficients[,5])
            clog <- cbind(clog,ifelse(clog[,2] <= 0.05, clog[,1], 0))
            if(sum(clog[,3]) != 0){
              for (i in 1:length(clog[,3])){
                if (clog[,3][i] != 0){
                  values$priors[i] <- clog[,3][i]
                }
              }
              
              # Decide if bayesian
              if (values$bayesian == TRUE){
                alg = "cea"
              } else {
                alg = "fedorov"
              }
              
              values$alg <- alg
              
              # Priors
              if (values$bayesian == TRUE){
                values$bpriors <- mvtnorm::rmvnorm(100, values$priors, diag(length(values$priors)))
              } else {
                values$bpriors <- values$priors
              }
              
              
              # Generate
              design <- dce_toolbox(attributes = values$levels, csets = values$sets,
                                    alts = values$alts, nochoice = values$optout,
                                    priors = values$bpriors, alg = alg)
              
              design <- design$design
              
              #Replace the global design
              tempnam <- colnames(values$design)
              colnames(design) <- tempnam
              
              values$design <- design
              }
            } else {
            values$survclick <- 0
            }
          
        }
        #Reload the choice sets
        values$designstor <- rbind(values$designstor, values$design)
        designstor <- values$designstor
        values$survclick <- 0
      })
      
  
    }) # End of the popup
    
    # Move to results when click on analyze the survey
    shiny::observeEvent(input$resultsgo, {
      shiny::updateTabsetPanel(session, 'inTabset', selected = "results")
      
      #detect if the number of responses correspond to the designs stored
      storedchoices <- nrow(savechoices)*values$ncalts
      #if they are the same then reshape the dataset
      if (storedchoices == nrow(values$designstor)){
        #Reshape and present 
        larray <- merge(cbind("num" = seq(len = nrow(savechoices)),savechoices), cbind("choice" = values$labels, "userselect" = seq(len = length(values$labels))), by = c("choice"), all =  TRUE)
        larray <- larray[order(larray$num),]
        larray$pid <- rep(1:(nrow(larray)/values$sets), len = nrow(savechoices), each = values$sets)
        designstor <- values$designstor
        designstor$pid <- rep(1:(nrow(designstor)/(values$ncalts*values$sets)), each = (values$sets*values$ncalts))
        larray <- merge(designstor, larray, by = c("pid","task"), order = TRUE)
        larray <- larray[order(larray$pid,larray$num),]
        larray$choice <- NULL
        larray$num <- NULL
        larray$choice <- ifelse(larray$userselect == larray$alt, 1, 0)
        larray$userselect <- NULL
        cdesmat <- larray
        resultados <<- cdesmat
        cdesmat$gid <- rep(1:(nrow(cdesmat)/values$ncalts), each = values$ncalts)
        values$cdesmat <- cdesmat
        # Render the table with the results
        output$restable <- DT::renderDT({
          cdesmat
        })
        
      } else if (storedchoices != nrow(values$designstor)){
        #Cut the design storage if it's bigger than the response
        values$designstor <- values$designstor[1:storedchoices,]
        
        #Now reshape the database
        larray <- merge(cbind("num" = seq(len = nrow(savechoices)),savechoices), cbind("choice" = values$labels, "userselect" = seq(len = length(values$labels))), by = c("choice"), all =  TRUE)
        larray <- larray[order(larray$num),]
        larray$pid <- rep(1:(nrow(larray)/values$sets), len = nrow(savechoices), each = values$sets)
        designstor <- values$designstor
        designstor$pid <- rep(1:(nrow(designstor)/(values$ncalts*values$sets)), each = (values$sets*values$ncalts))
        larray <- merge(designstor, larray, by = c("pid","task"), order = TRUE)
        larray <- larray[order(larray$pid,larray$num),]
        larray$choice <- NULL
        larray$num <- NULL
        larray$choice <- ifelse(larray$userselect == larray$alt, 1, 0)
        larray$userselect <- NULL
        cdesmat <- larray
        resultados <<- cdesmat
        cdesmat$gid <- rep(1:(nrow(cdesmat)/values$ncalts), each = values$ncalts)
        values$cdesmat <- cdesmat
        # Render the table with the results
        output$restable <- DT::renderDT({
          cdesmat
        })
        
      }
      
    })
    
    # Download the coded design
    output$downloadresults <- shiny::downloadHandler(
      filename = function() {"results.xlsx"},
      content = function(file) {
      writexl::write_xlsx(values$cdesmat, file)}
    )
    
    #Load a results
    shiny::observeEvent(input$loadresults, {
      # Design loader
      output$restable <- DT::renderDT({
          values$cdesmat <- readxl::read_excel(input$loadresults$datapath) #Update
      })
    })
    
    # Estimate a conditional logit
    shiny::observeEvent(input$modelname, {
      cdesmat <- values$cdesmat
      output$modopt <- NULL
      if (input$modelname == "clogit"){
        output$modopt <- shiny::renderUI({
          options <- htmltools::tagList(
            shinyWidgets::pickerInput("dep", "Dependent variable", choices = colnames(cdesmat), multiple = FALSE),
            shinyWidgets::pickerInput("ind", "Independent variables", choices = colnames(cdesmat), multiple = TRUE),
            shinyWidgets::pickerInput("gid", "Group variable", choices = colnames(cdesmat), multiple = FALSE),
            shiny::actionButton("goest", "Estimate")
          )
          options
        })
      } else if (input$modelname == "nullselect"){
        output$modopt <- NULL
      } else if (input$modelname == "mixlogit"){
        output$modopt <- shiny::renderUI({
          options <- htmltools::tagList(
            shinyWidgets::pickerInput("dep", "Dependent variable", choices = colnames(cdesmat), multiple = FALSE),
            shinyWidgets::pickerInput("ind", "Independent variables", choices = colnames(cdesmat), multiple = TRUE),
            shinyWidgets::pickerInput("gid", "Group variable", choices = colnames(cdesmat), multiple = FALSE),
            shinyWidgets::pickerInput("pid", "Individual identificator", choices = colnames(cdesmat), multiple = FALSE),
            shinyWidgets::pickerInput("alt", "Alternative identificator", choices = colnames(cdesmat), multiple = FALSE),
            shiny::actionButton("goest", "Estimate")
          )
          options
        })
      } else if (input$modelname == "wtp"){
        output$modopt <- shiny::renderUI({
          options <- htmltools::tagList(
            shinyWidgets::pickerInput("pr", "Continuous 'price' variable", choices = rownames(values$clog$coefficients), multiple = FALSE),
            shinyWidgets::pickerInput("rl", "Rest of levels", choices = rownames(values$clog$coefficients), multiple = TRUE),
            shiny::actionButton("goest", "Estimate")
          )
          options
        })
      } else if (input$modelname == "figures"){
        ui <- htmltools::tagList(
          shinyWidgets::pickerInput("coefs", "Select levels to plot", choices = rownames(values$clog$coefficients), multiple = TRUE),
        shiny::actionButton("plotit", "Create figure")
        #numericInput("ngrup", "Number of groups (attributes)", value = 0)
        )
        output$modopt <- shiny::renderUI({
          ui
        })
      }
    })
    
    # Future implementation
    # observeEvent(input$ngrup,{
    #   output$modopt2 <- renderUI({
    #     if (input$ngrup > 0) {
    #       tags <- tagList()
    #       for (i in seq_len(input$ngrup)) {
    #         tags[[i]] <- textInput(paste0('gr', i),
    #                                   paste0('Name group ', i))
    #       }
    #       tags[[i+1]] <- actionButton("plotit", "Create figure")
    #       tags
    #     } else {
    #       actionButton("plotit", "Create figure")
    #     }
    #   })
    # })
    
    # Estimations
    shiny::observeEvent(input$goest, {
      cdesmat <- values$cdesmat
      if (input$modelname == "clogit"){
        ind <- input$ind
        ind <- paste0("`",ind, "`")
        dep <- input$dep
        dep <- paste0("`",dep, "`")
        clog <- survival::clogit(stats::as.formula(paste0(paste0(dep," ~"),do.call(paste, c(as.list(ind), sep = " + ")), " + strata(",paste0(input$gid) ,")")),
                       data = as.data.frame(cdesmat))
        clog <- summary(clog)
        #Render the results
        output$clog <- shiny::renderPrint({
          clog
        })
        clog <<- clog
        values$clog <- clog
        ### Mixlogit
      } else if (input$modelname == "mixlogit"){
        dep <- input$dep
        dep <- paste0("`",dep, "`")
        gid <- input$gid
        alt <- input$alt
        pid <- input$pid
        ind <- input$ind
        ind <- paste0("`",ind, "`")
        rpar <- c()
        for (i in 1:length(ind)){
          rpar <- append(rpar, "n")
        }
        names(rpar) <- ind
        ind <- input$ind
        ind <- paste0("`",ind, "`")
        cdesmat$choice <- as.logical(cdesmat$choice)
        disMIX <- dfidx::dfidx(cdesmat, choice="choice", idx = list(c(paste0(gid), paste0(pid)), paste0(alt)), idnames= c("cs", "alt"))
        modeloMIX <- try(mlogit::mlogit(stats::as.formula(paste0(paste0(dep," ~"),do.call(paste, c(as.list(ind), sep = " + ")), " | 0")), disMIX,
                            rpar = rpar, R = 100, halton = NA))
        clog <- summary(modeloMIX)
        if (typeof(clog) == "character"){
          ind <- input$ind
          rpar <- c()
          for (i in 1:length(ind)){
            rpar <- append(rpar, "n")
          }
          names(rpar) <- ind
          ind <- input$ind
          ind <- paste0("`",ind, "`")
          cdesmat$choice <- as.logical(cdesmat$choice)
          disMIX <- dfidx::dfidx(cdesmat, choice="choice", idx = list(c(paste0(gid), paste0(pid)), paste0(alt)), idnames= c("cs", "alt"))
          modeloMIX <- try(mlogit::mlogit(stats::as.formula(paste0(paste0(dep," ~"),do.call(paste, c(as.list(ind), sep = " + ")), " | 0")), disMIX,
                                  rpar = rpar, R = 100, halton = NA))
          clog <- summary(modeloMIX)
          if (typeof(clog) == "character"){
            clog <- "There is a problem related with the variable names. More likely, you combined level names with and without numbers and blank spaces. Save the design and modify the excel file by deleting all numbers and blanks from the variable names."
          }
        }
        #Render the results
        output$clog <- shiny::renderPrint({
          clog
        })
        
      } else if (input$modelname == "wtp") {
        if (is.null(values$clog)){
          output$clog <- shiny::renderPrint({
            "First estimate the conditional logit"
          })
        } else {
          reswtp <- as.data.frame(values$clog$coefficients)
          results <- -reswtp[input$rl,]/as.numeric(reswtp[input$pr,][1])
          output$clog <- shiny::renderPrint({
            results
          })
        }
      } 
    })
    
    # If click on figures, render the graph based on the coefficients of the clogit and the user selection
    shiny::observeEvent(input$modelname,{
        if (input$modelname == "figures"){
          shiny::observeEvent(input$plotit, {
  
              if (!is.null(values$clog)){
                df <- data.frame(names = input$coefs, y = values$clog$coefficients[,1][input$coefs], se = values$clog$coefficients[,3][input$coefs])
                values$df <- df
                output$clog <- NULL
                output$figure <- shiny::renderPlot(
                  ggplot2::ggplot(df, aes(x=names, y=y)) + geom_bar(stat = "identity", fill="skyblue", alpha=0.7) + 
                    geom_errorbar( ggplot2::aes(x=names, ymin=y-se, ymax=y+se), width=0.4, colour="orange", alpha=0.9, size=1.3)
                )
              } else {
                output$clog <- shiny::renderPrint(
                  "First, estimate a conditional logit model."
                )
              }  
            
          }) 
        }
    })
    
    # Change level names and axis names in the graph
    shiny::observeEvent(input$plotit, {
      tl <- htmltools::tagList()
      for (i in 1:length(input$coefs)){
        tl[[i]] <- shiny::textInput(inputId = paste0("coefplot", i), label = paste0(input$coefs[i]), value = paste0(input$coefs[i]))
      }
      tl[[i+1]] <- shiny::textInput(inputId = "xlabnew", "X axis")
      tl[[i+2]] <- shiny::textInput(inputId = "ylabnew", "Y axis")
      tl[[i+3]] <- actionButton(inputId = "changenames", label = "Change names")
      output$modopt2 <- shiny::renderUI({
        tl
      })
    })
    
    shiny::observeEvent(input$changenames, {
      newnames <- c()
      for (i in 1:length(input$coefs)){
        newnames <- append(newnames, eval(parse(text = paste0("input$coefplot", i))))
      }
      output$figure <- shiny::renderPlot(
        ggplot2::ggplot(values$df, aes(x=names, y=y)) + geom_bar(stat = "identity", fill="skyblue", alpha=0.7) + 
          geom_errorbar( aes(x=names, ymin=y-se, ymax=y+se), width=0.4, colour="orange", alpha=0.9, size=2) +
          scale_x_discrete(labels = newnames) + xlab(input$xlabnew) + ylab(input$ylabnew)
      )
    })
    
  
    
    shiny::observeEvent(input$pricevarcoding, {
      if (input$pricevarcoding == "cont"){
        cdesmat <- values$cdesmat
        regressors <- names(cdesmat[,4:(ncol(cdesmat)-1)])
        output$atpriceselect <- shiny::renderUI({
          checkboxGroupInput(
            inputId = "pricelevs",
            label = "Select price levels",
            choices = names(cdesmat)
          )
        })
        output$savethem <- shiny::renderUI({
          actionButton("savepricelevs", "Save price levels")
        })
      } else if (input$pricevarcoding == "already"){
        cdesmat <- values$cdesmat
        output$atpriceselect <- shiny::renderUI({
          checkboxGroupInput(
            inputId = "selectedcontvar",
            label = "Select the continuous price variable",
            choices = names(cdesmat)
          )
        })
      }
    })
    
    shiny::observeEvent(input$pricevarcoding, {
      if (!is.null(input$selectedcontvar)){
        cdesmat <- values$cdesmat
        cdesmat$price <- eval(parse(text = paste0("cdesmat$selectedcontvar")))
      }
    })
    
    shiny::observeEvent(input$savepricelevs, {
      pricevect <- input$pricelevs
      output$pricelevbr <- shiny::renderUI({
        tags <- htmltools::tagList()
        for (i in 1:length(input$pricelevs)) {
          tags[[i]] <- shiny::numericInput(paste0('pricelev', i), 
                                    paste0(input$pricelevs[i]),
                                    0)
        }
        omittedlev <- shiny::numericInput("omlev", "Omitted level (In monetary units)", value = 0)
        tags <- htmltools::tagList(omittedlev, tags, actionButton("sprc", "Add variable to the data frame"))
        tags
      })
    })
    
    shiny::observeEvent(input$sprc, {
      cdesmat <- values$cdesmat
      if (!is.null(cdesmat$cont_price)){
        cdesmat$cont_price <- NULL
      } else if (is.null(cdesmat$cont_price)){
        pricelevs <- input$pricelevs
        cdesmat$cont_price <- ifelse(rowSums(cdesmat[colnames(cdesmat) == pricelevs]) == 0, input$omlev, NA)
        for (i in 1:length(pricelevs)){
          inppric <- pricelevs[i]
          cdesmat$cont_price <- ifelse(eval(parse(text = paste0("cdesmat$`", inppric,"`")))== 1, eval(parse(text = paste0("input$pricelev", i))) , cdesmat$cont_price)
        }
        cdesmat <- cbind(cdesmat[,1:(ncol(cdesmat)-2)], "cont_price" = cdesmat[,ncol(cdesmat):ncol(cdesmat)], "gid" = cdesmat[,(ncol(cdesmat)-1):(ncol(cdesmat)-1)])
        values$cdesmat <- cdesmat
      }
      output$restable <- DT::renderDT({
        values$cdesmat
      })
      output$pricelevbr <- NULL
    })
    
    shiny::observe({
      if (shiny::req(input$inTabset == "About")){
        x <- adjustedcranlogs::adj_cran_downloads("DCEtool",when="last-month")[,c(1,3)]
      }
      output$downloads <- shiny::renderPlot({
        if (!is.null(x)) {
          barplot(count ~ date, data = x)
        }
      })
        
    })
    
    
  }
  
  shinyApp(ui, server)
}