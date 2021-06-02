#' @export
DCEtool <- function(){
  
#Packages
requireNamespace('shiny')
requireNamespace('shinythemes')
requireNamespace('survival')
requireNamespace('DT')
requireNamespace('idefix')
requireNamespace('writexl')
requireNamespace('shinyjs')
requireNamespace('MASS')
requireNamespace('stats')
requireNamespace('shinycssloaders')
requireNamespace('shinyglide')
requireNamespace('htmltools')
requireNamespace('stringr')
requireNamespace('bslib')
requireNamespace('mlogit')


# Imports
#' @importFrom shiny fluidPage
#' @importFrom shiny navbarPage
#' @importFrom shiny fluidRow
#' @importFrom shiny column
#' @importFrom shiny p
#' @importFrom shiny HTML
#' @importFrom shiny sidebarLayout
#' @importFrom shiny sidebarPanel
#' @importFrom shiny textInput
#' @importFrom shiny numericInput
#' @importFrom shiny actionButton
#' @importFrom shiny icon
#' @importFrom shiny checkboxInput
#' @importFrom shiny mainPanel
#' @importFrom DT dataTableOutput
#' @importFrom shiny downloadButton
#' @importFrom shiny textAreaInput
#' @importFrom shiny tags
#' @importFrom shiny textOutput
#' @importFrom shinythemes shinytheme
#' @importFrom shiny tabPanel
#' @importFrom shiny hr
#' @importFrom shiny h4
#' @importFrom shiny tableOutput
#' @importFrom shiny selectInput
#' @importFrom shiny conditionalPanel
#' @importFrom shinyjs useShinyjs
#' @importFrom shiny verbatimTextOutput
#' @importFrom shiny reactiveValues
#' @importFrom shiny uiOutput
#' @importFrom shiny observeEvent
#' @importFrom utils head
#' @importFrom idefix CEA
#' @importFrom shiny downloadHandler
#' @importFrom writexl write_xlsx
#' @importFrom idefix Decode
#' @importFrom shiny renderTable
#' @importFrom shiny radioButtons
#' @importFrom shiny renderText
#' @importFrom survival clogit
#' @importFrom shiny renderUI
#' @importFrom stats as.formula
#' @importFrom survival strata
#' @importFrom survival coxph
#' @importFrom survival Surv
#' @importFrom shiny shinyApp
#' @importFrom shiny renderPrint
#' @importFrom DT renderDataTable
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyglide glide
#' @importFrom shinyglide screen
#' @importFrom shinyglide screenOutput
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_sub
#' @importFrom shiny markdown
#' @importFrom bslib bs_theme
#' @importFrom bslib bs_themer
#' @importFrom mlogit mlogit.data
#' @importFrom mlogit mlogit



#### Initialisation ####
# Data frame for attributes and levels #
data <- data.frame(
  name = character(),
  lev = as.numeric(),
  label = character()
  )

# Variables initialisation #
Dataset <- data.frame()
data.0 <- data
niveles <- 0
nula <- TRUE
a <- 0
s <- 0
sn <- 0
resp  <- vector("character")
sdata <- vector(mode = "list")
fulldes <- matrix()
sncum <- 0
nsn <- 0

#### User interface ####
ui <- fluidPage(theme=bs_theme(version = 4, bootswatch = "simplex"),
      navbarPage(title=tags$div(tags$img(src="http://danielpereztr.es/wp-content/uploads/2021/06/logo.png", height = '37.5px', width = '150px')),
                  tabPanel("Parameters", #Parameters input
                           sidebarLayout(
                             sidebarPanel(
                               h4("Attributes and levels"),
                               textInput("name", "Name of the attribute", ""),
                               numericInput("lev", "Number of levels", ""),
                               textInput("label", "Levels' name (in order and separated by commas)", ""),
                               actionButton(inputId = "add.button", label = "Add", icon = icon("plus")),
                               actionButton(inputId = "delete.button", label = "Delete", icon = icon("minus")),
                               hr(),
                               actionButton(inputId = "example.button", label = "Load example data", icon = icon("upload")),
                               hr(),
                               h4("Other parameters"),
                               numericInput("a", "No. of alternatives per choice set", ""),
                               numericInput("s", "No. of choice sets per respondent", ""),
                               checkboxInput("nula", "Opt-out alternative", FALSE),
                               selectInput("priorstype", "Type of priors", c("Zero (pilot design)"="zero", "Personalized priors"="personalized")),
                               conditionalPanel(
                                   condition = "input.priorstype == 'personalized'",
                                   textInput('userpriors', 'Prior coefficients*',""),
                                   t("*Enter the prior coefficients separated by commas. The number of coefficients must be equal to: l-k (+1 if an opt-out alternative is included). l: total number of levels, k: total number of attributes. ")),
                               hr(),
                               actionButton(inputId = "gen.button", label = "Save inputs", icon = icon("window-restore"))
                             ),
                             mainPanel(
                               dataTableOutput('table')
                             )
                           )
                        ),

                  tabPanel("Design matrix", #Design output
                           sidebarLayout(
                             sidebarPanel(
                               downloadButton("downloadData", "Download"),
                               hr(),
                               actionButton("decode", "Decode the design matrix")
                             ),
                             mainPanel(
                               shinycssloaders::withSpinner(dataTableOutput('table2'), type = 6, color="#D5220E"),
                               verbatimTextOutput('decoded')
                             )
                           )
                  ),
                 
                 tabPanel("Survey wizard", #instructions
                          p("If the content is not displayed properly, resize the window. "),
                          hr(),
                          glide(
                            screen(
                              p("In this section you can set up a static survey.
                                You can use this static survey to record responses
                                to a discrete choice experiment on a single computer.
                                This means that all respondents will have to answer
                                the survey on this computer."),
                              hr(),
                              HTML("<p>Throughout the wizard you will be offered several configuration options.
                                You can change the name of the alternatives, add an introductory text, 
                                add a final text, and decide whether or not to 
                                use the serial method for calculating the priors 
                                of Bliemer & Rose (2010) <a href='https://doi.org/10.1108/9781849507738-006'>https://doi.org/10.1108/9781849507738-006</a></p>")
                            ),
                            
                            screen(
                              p("Enter the name of the alternatives separated by commas. For example, for choice sets with
                                two alternatives, you could type 'Alternative 1, Alternative 2' (with no quotes)."),
                              textOutput("alternatives"),
                              hr(),
                              textInput("altnames", "Alternative' names (in order and separated by commas)", "", width = '80%')
                            ),
                            screen(
                            HTML("<p>Please write an introductory text for the survey. Use Markdown language, see guide <a href='https://www.markdownguide.org/cheat-sheet/'>here</a></p>"),
                            tags$textarea(
                              id = "intro.text", 
                              rows = "8",
                              cols = "100",
                              "### Survey title \n \n This is a markdown paragraph. \n \n  The following is an unordered list \n \n * a bullet \n \n * another \n \n[Links](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a) work; so does *emphasis*.\n" 
                            ),
                            p('Preview:'),
                            column(12, uiOutput('markdown'))
                            ),
                            screen(
                              HTML("<p>Please write an introductory end for the survey. Use Markdown language, see guide <a href='https://www.markdownguide.org/cheat-sheet/'>here</a></p>"),
                              tags$textarea(
                                id = "end.text", 
                                rows = "8",
                                cols = "100",
                                "End of the survey. Thank you. "
                              ),
                              p('Preview:'),
                              column(12, uiOutput('markdownend'))
                            ),
                            screen(
                              HTML("<p>Use Bliemer and Rose serial approach <a href='https://doi.org/10.1108/9781849507738-006'>https://doi.org/10.1108/9781849507738-006</a></p>"),
                              checkboxInput("serial", "Use serial approach", FALSE)),
                            screenOutput("check_screen"),
                            screen(
                              p("Push the button and move to the 'Survey' tab."),
                              actionButton(inputId = "survey.button", label = " Create the survey", icon = icon("plus-circle"))
                            )
                          )
                 ),

                  tabPanel("Survey", #Survey phase
                           mainPanel(
                             tags$style(type="text/css", #Hiding some annoying errors
                                        ".shiny-output-error { visibility: hidden; }",
                                        ".shiny-output-error:before { visibility: hidden; }"
                             ),
                             column(12, align = 'center', textOutput("set.nr")),
                             column(12, align = 'center', tableOutput("choice.set")),
                             column(12, align = 'center', uiOutput('buttons')),
                             column(12, uiOutput('intro.text')),
                             column(12, uiOutput('end')),
                             column(12, align = "center", actionButton("OK", "OK")),
                             useShinyjs(),
                             column(12, align = "center", actionButton("nextbutton", "Next respondent")),
                             hr()

                           )
                  ),

                  tabPanel("Results", #Results page
                      sidebarLayout(
                        sidebarPanel(
                          downloadButton("downloadResults", "Download"),
                          hr(),
                          checkboxInput("linearpricecheck", "Price as linear variable", FALSE),
                          conditionalPanel(
                            condition = "input.linearpricecheck ==1",
                            uiOutput('pricevars'),
                            hr(),
                            uiOutput("remprice"),
                            hr(),
                            actionButton(inputId = "goprec", label = "Code the price attribute"),
                            hr(),
                            actionButton(inputId = "linpriceclogit", label = "Conditional logit with linear price")
                          ),
                          hr(),
                          actionButton(inputId = "estimate", label = "Estimate a conditional logit"),
                          hr(),
                          actionButton(inputId = "mixed", label = "Estimate a mixed (random parameters) logit")
                        ),
                        mainPanel(
                          dataTableOutput('results'),
                          verbatimTextOutput('model')
                        )
                      )
                  ),
                 
                 tabPanel("Instructions", #Instructions
                          fluidRow(column(width=2),
                                   column(
                                     p("Welcome to DCEtool, an app to create, respond and analyse DCEs",style="color:black;text-align:center"),
                                     width=8,style="background-color:papayawhip;border-radius: 10px")),
                          hr(),
                          HTML('<p align="center"><iframe width="840" height="472.5" src="https://www.youtube.com/embed/PmngsAHCOIY" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></p>'),
                          h4("About"),
                          HTML("<p>This app was built on top of the <a href='http://dx.doi.org/10.18637/jss.v096.i03'>-idefix-</a> package by Traets et al. (2020) to simplify the task of creating DCEs and feasibly implement <a href='https://doi.org/10.1108/9781849507738-006'>Bliemer and Rose's sequential design approach</a>. </p>"),
                          hr(),
                          HTML("<p>The app was developed in R by <a href='http://danielpereztr.es/'>Daniel Perez Troncoso</a> at the Department of Applied Economics at the University of Granada.
           Contact: <a href = 'mailto:danielperez@ugr.es'>danielperez@ugr.es</a></p>"),
                          hr(),
                          HTML("<p>Please, cite as Daniel Perez Troncoso (2021) DCE tool. <a href='https://github.com/danielpereztr/DCEtool'>https://github.com/danielpereztr/DCEtool</a>"),
                          hr(),
                          HTML("<p><a href='http://danielpereztr.es/dcetool-una-herramienta-visual-para-disenar-encuestar-y-analizar-experimentos-de-eleccion-discreta'>Guide in Spanish</a></p>"),
                          HTML("<p><b>Changelog</b><p>"),
                          HTML("<ul>
                                <li>20/05/2021 (V 0.3.0): Price can be coded as a linear variable and a mixed logit can be estimated (unstable). Bugs fixed</li>
                                <li>20/05/2021 (V 0.2.2): Priors selection improved, variable names added to the tables and model, survey wizard, serial designs improved</li>
                                <li>07/05/2021 (V 0.2.1): Now priors can be specified</li>
                                <li>16/04/2021 (V 0.0.1): Release</li>
                                </ul>")
                 )
                 
                 
    ) #end of navpage
    ) #end of ui

server = function(input, output) {

  # Storage of changing variables #
  values <- reactiveValues()
  values$df <- data

  # Table input for attributes and levels #
  observeEvent(input$add.button,{
    print(input$name)
    print(input$lev)
    print(input$label)
    newRow <- data.frame(input$name, input$lev, input$label)
    colnames(newRow)<-c("name", "lev", "label")
    values$df <- rbind(values$df,newRow)
    data.0 <- rbind(data.0,newRow)
    values$data0 <- data.0
    print(nrow(values$df))
  })

  # Delete last input in the table #
  observeEvent(input$delete.button,{
      values$df <- head(values$df,-1)
    }
  )

  # Input an example data set #
  observeEvent(input$example.button,{
    exampledata <- data.frame(name=c("Effectiveness", "Doses", "Adverse events", "Price"), lev=c(3, 2, 3, 3), label=c("70%,80%,90%","1 dose,2 doses","0.1%,0.5%,1%","50,100,150"))
    values$df <- rbind(values$df, exampledata)
    data.0 <- rbind(data.0, exampledata)
  })

  # Change the column names to the table #
  output$table = renderDataTable({
    tablex=as.data.frame(values$df)
    colnames(tablex) <- c("Attribute", "No. of levels", "Levels' name")
    values$tablex <- tablex
    tablex
  })

  # Save inputs in changing variables #
  observeEvent(input$gen.button,{
    values$a <- input$a
    values$atext <- input$a
    values$s <- input$s
    values$nula <- input$nula
    values$niveles <- unlist(c(values$df[2]))
    values$atnames <- unlist(c(values$df[1]))
    values$levnames <- unlist(c(values$df[3]))
    values$userpriors <- input$userpriors
    values$userpriors <- unlist(strsplit(values$userpriors, split=","))
    values$userpriors <- as.numeric(values$userpriors)
    values$nsn <- 1
  })

  output$alternatives <- renderPrint({
     if (values$nula==FALSE){
    cat("Since you are using ",values$atext, " alternatives, you need ", values$atext, " strings separated by ", values$atext-1, "comma(s).")
     } else {
       cat("Since you are using ",values$atext+1, " alternatives, you need ", values$atext+1, " strings separated by ", values$atext, "comma(s).")
     }
  })
  
  # Markdown intro text #
  output$markdown <- renderUI({
    markdown(input$intro.text, .noWS = TRUE)
  })
  
  # Markdown end text #
  output$markdownend <- renderUI({
    markdown(input$end.text, .noWS = TRUE)
  })
  
  output$check_screen <- renderUI({
    if(!input$serial) return(NULL)
    numericInput("serialno", "Start serial strategy after X responses:", "20")
  })
  
  observeEvent(input$serialno,{
    values$serialno <- input$serialno
  })

  
  # DCE creator function  #
  creator <- function (niveles, nula, a, s, priors) {
    x <- 0
    codif <- c()
    while (x<length(niveles)){
      codif <- append(codif, "D")
      x <- x+1
    }
    l <- sum(niveles)
    k <- length(niveles)
    if (nula==TRUE){
      pr <- (l-k+1)
    } else {
      pr <- (l-k)
    }
    I <- diag(length(priors))
    sim <- MASS::mvrnorm(n=100, mu=priors, Sigma=I)
    if (nula==TRUE){
      a <- a+1
      sim <- list(sim[,1:1], sim[, 2:(length(priors))])
      t <- 0
      u <- a-1
      const <- c()
      while(t<=u){
        if (t<u){
          const <- append(const,0)
        } else {
          const <- append(const,1)
        }
        t <- t+1
      }
      dis <- CEA(lvls=niveles, coding=codif, n.alts=a, n.sets=s, alt.cte=const,
                 par.draws=sim, no.choice=TRUE, best=TRUE)
    }
    if (nula==FALSE){
      dis <- CEA(lvls=niveles, coding=codif, n.alts=a, n.sets=s,
                 par.draws=sim, no.choice=FALSE, best=TRUE)
    }
    if (nula==TRUE){
    return(list(dis, codif, const, codif))
    } else {
    return(list(dis, codif, NULL, codif))
    }
  } #End of the creator

  output$table2 <- renderTable({
    niveles <- values$niveles
    nula <- values$nula
    a <- values$a
    s <- values$s
    up <- values$userpriors
    if(!exists("up")){
      priors <- values$userpriors
    } else {
      # zero priors
      l <- sum(niveles)
      k <- length(niveles)
      if (nula==TRUE){
        pr <- (l-k+1)
      } else {
        pr <- (l-k)
      }
      y <- 0
      priors <- c()
      while (y<pr){
        priors <- append(priors,0)
        y <- y+1
      }
    }
    dis <- creator(niveles, nula, a, s, priors)
    values$dis <- dis
    values$alt.cte <- dis[[3]]
    values$codif <- dis[[4]]
    final <- dis[[1]]$design
    values$des <- final
    if (nula==TRUE){
      a <- a+1
      values$a <- a
    }
    
    output$table2 = renderDataTable({
      input$go.button
      tablex <- values$tablex
      d <- nrow(tablex)
      i <- 1
      titles <- c()
      d <- nrow(tablex)
      i <- 1
      titles <- c()
      while(i<d+1){
        current <- tablex[3][i,]
        current <- unlist(strsplit(current, ","))
        current <- current[2:length(current)]
        titles <- append(titles, current)
        i <- i+1
      }
      if(nula==TRUE){
        titles <- append("opt-out",titles)
      }
      titles <- gsub(" ", "-", titles)
      colnames(final) <- titles
      values$final1 <- final
      final
    })
    
    values$downloadata <- as.data.frame(values$des) #formatting the data to be downloaded
    values$downloadata <- cbind(rownames(values$des),values$des)
  })
  
  observeEvent(input$decode,{
    tablex <- values$tablex
    n <- nrow(tablex)
    c <- 0
    nivn <- tablex[,3]
    lvls <- list()
    while (c<n){
      c <- c + 1
      z <- strsplit(nivn[c], ",")[[1]]
      lvls <- append(lvls,list(z))
    }
    dis <- values$dis
    nula <- values$nula
    a <- values$a
    cte <- values$alt.cte
    cod <- values$codif
    if (nula==TRUE){
    decoded <- Decode(des = dis[[1]]$design, n.alts = a ,lvl.names = lvls, alt.cte = c(0,0,1), coding = cod, no.choice = 3)
    } else {
    decoded <- Decode(des = dis[[1]]$design, n.alts = a ,lvl.names = lvls, coding = cod)
    }
    colnames(decoded$design) <- tablex[,1]
    values$decoded <- decoded$design
  })
  
  output$decoded <-  renderPrint({
    decoded <- values$decoded
    design <- decoded
    a <- values$a
    x <- 1
    z <- 1
    y <- nrow(design)
    while (x < y){
      s <- x+a-1
      current <- design[x:s,]
      print(paste("Choice set", z))
      current <- t(current)
      b <- 1
      altnames <- c()
      while(b < (a+1)){
        an <- paste("Alternative ", b)
        altnames <- append(altnames, an)
        b <- b+1
      }
      colnames(current)<-altnames
      prcurr <- print(current)
      assign(paste("cs",z,sep=""), current)
      x <- x+a
      z <- z+1
    }
  })

  # Download button #
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(as.data.frame(values$downloadata), path = file)
    }
  )

  # Create the survey when survey.button is clicked #
  observeEvent(input$survey.button,{
    values$respcount <- 1
    algorithm = "CEA"
    values$altnames <- input$altnames
    values$altnames <- strsplit(values$altnames, split=",")
    sdata <- vector(mode = "list")
    surveyData <- vector(mode = "list")
    y.bin <- vector("numeric")
    resp  <- vector("character")
    n.atts <- length(unlist(c(values$df[1])))
    n.alts <- values$a
    atts <- values$atnames
    n.levels <- length(unlist(c(values$df[2])))
    choice.sets <- matrix(data = NA, nrow = s * a, ncol = n.atts)
    values$choice.sets <- choice.sets
    buttons <- NULL
    nula <- values$nula
    values$intro.text <- input$intro
    values$end.text <- input$end
    values$sn <- 0
    if (nula==FALSE){
      no.choice=NULL
    } else {
      no.choice <- length(values$alt.cte)
    }
    des <- values$des
    n.init <- nrow(des) / n.alts
    alt.cte <- values$alt.cte
    if (is.null(alt.cte) || all(alt.cte == 0)) {
      alt.cte <- rep(0, n.alts)
      n.cte <- 0
      cte.des <- NULL
    }
    #by now
    data.dir = NULL
    #end
    n.total <- s
    lower = c(-Inf, -Inf, -Inf, 0, 0, -Inf)
    bs <- seq(1, (nrow(des) - n.alts + 1), n.alts)
    es <- c((bs - 1), nrow(des))[-1]

    # added from the idefix package #
    Rcnames <- function(n.sets, n.alts, alt.cte, no.choice) {
      r.s <- rep(1:n.sets, each = n.alts)
      r.a <- rep(1:n.alts, n.sets)
      r.names <- paste(paste("set", r.s, sep = ""), paste("alt", r.a, sep = ""), sep = ".")
      if(no.choice){
        ncsek <- seq(n.alts, (n.sets * n.alts), n.alts)
        r.names[ncsek] <- "no.choice"
      }
    }
    rowcol <- Rcnames(n.sets = n.init, n.alts = n.alts, alt.cte = alt.cte, no.choice = FALSE)
    rownames(des) <- rowcol[[1]]
    fulldes <- des
    
    if(sn==0){
    shinyjs::hide("nextbutton")
    }
  })

  
    observeEvent(input$survey, {
      shinyjs::click("OK")
    })
    
    observeEvent(input$OK,{
      n.total <- values$s
      values$nsn <- values$nsn+1
      if (values$nsn>n.total+2){
        values$nsn <- 0
      }
    })
    
   
    
    # When the OK button is clicked #
    observeEvent(input$OK, {
      values$bnum <- values$bnum+1
      n.atts <- length(unlist(c(values$df[1])))
      atts <- values$df[,1]
      alts <- unlist(values$altnames)
      n.alts <- values$a
      des <- values$des
      n.init <- nrow(des) / n.alts
      n.total <- values$s
      values$OK <- input$OK
      values$sn <- sn
      if (values$sn > n.total+1){
      values$sn <- 0
      } else {
      values$sn <- sn
      }
      sn <<- sn + 1
      values$sn <<- values$sn + 1
      n.total <- values$s
      choice.sets <- values$choice.sets
      bs <- seq(1, (nrow(des) - n.alts + 1), n.alts)
      es <- c((bs - 1), nrow(des))[-1]
      Select <- function() {
        if (sn <= n.total) {
          if (sn <= n.init) {
            set <- des[bs[sn]:es[sn], ]
            le <- length(unlist(c(values$df[1])))
            nam <- unlist(values$df[3])
            labels <- vector(mode = "list", le)
            m <- 0
            while (m<le){
              m <- m+1
              labels[m] <- strsplit(nam[m], split=",")
            }
            if (values$nula==TRUE){
              nu <- n.alts
            } else {
              nu <- NULL
            }
            choice.set <- Decode(des = set, n.alts = n.alts, lvl.names = labels, coding = values$codif,
                                 alt.cte = values$alt.cte, c.lvls = NULL, no.choice = nu)[[1]]
            colnames(choice.set) <- atts
            rownames(choice.set) <-  unlist(values$altnames)
            choice.set <- t(choice.set[ , 1:n.atts])
          }
        }
      }
      if (sn <= n.total ) {
        output$choice.set <-  renderTable(Select(), rownames = TRUE) # Plot new choice set
      }
      if (sn > 1 && sn <= (n.total + 1)) {
        resp  <<- c(resp, input$survey) # Store responses and design
        
        # function from the idefix package #
        Charbin <- function (resp, alts, n.alts, no.choice = FALSE) {
          map <- match(resp, alts)
          l <- list()
          for(i in 1:length(map)){
            l[[i]] <- rep(0, n.alts)
            if (no.choice) {
              l[[i]][map[i] - 1] <- 1
            } else {
              l[[i]][map[i]] <- 1
            }
          }
          v <- unlist(l)
          return(v)
        }

      y.bin <<- Charbin(resp = resp, alts = alts, n.alts = n.alts)
      sdata[["bin.responses"]] <- y.bin
      sdata[["responses"]] <- resp
      sdata[["desing"]] <- fulldes
      sdata[["survey"]] <- choice.sets
      sdata[["gid"]] <- rep(c(1:n.init), each=n.alts)
      surveyData <<- sdata
      values$surveyData <- surveyData
      }
       if (sn > n.total) {
         output$choice.set <-  renderTable(NULL)
       }
    })

    # Show options after first OK #
    output$buttons <- renderUI({
    n.atts <- length(unlist(c(values$df[1])))
    n.alts <- values$a
    values$n.alts <- n.alts
    des <- values$des
    n.total <- values$s
    n.init <- nrow(des) / n.alts
    values$n.init <- n.init
    alts <- unlist(values$altnames)
    buttons.text <- "Select your preferred option"
    if (values$sn >= 1 && values$sn <= n.total) {
      
      return(list(radioButtons("survey", buttons.text, alts , inline = TRUE, selected = "None", width = "100%")))
    }
    })

    # Set the choice set number #
    observeEvent(input$OK, {
    n.total <- values$s
    if (sn <= n.total) {
      output$set.nr <- renderText(paste(c("choice set:", sn, "/", n.total)))
      shinyjs::hide("nextbutton")
    } else {
      output$set.nr <- renderText(NULL)
    }
    })
    

    # Intro text #
    output$intro.text <- renderUI(markdown(input$intro.text, .noWS = TRUE))
     observeEvent(input$OK, {
       n.total <- values$s
       cat("\n nsn: ", values$nsn, "\n")
       cat("\n n.total: ", n.total, "\n")
       if (sn!=sn+2){
       output$intro.text <- renderUI(NULL)
       shinyjs::hide("OK")
       } 
       if (values$nsn>n.total+1){
         values$nsn <<- 0 
         sn <<- 0 
         output$intro.text <- renderUI(markdown(input$intro.text, .noWS = TRUE))
         shinyjs::show("OK")
         if(!is.null(input$serial)){
         }
       }
     })

     
     
    # End of the survey #
    observeEvent(input$OK, {
      
    if (values$respcount == 1){
      r <- 1
    } 
    if (values$respcount > 1){
      r <- 2
    }
      
    n.total <- values$s
    vsn <- values$sn
    if ((input$OK)==((values$s+1)+(values$s+2)*(values$respcount-1))) { # Display end text
        values$alldes <- rbind(values$alldes, values$des)
        output$end <- renderUI(markdown(input$end.text, .noWS = TRUE))
        shinyjs::show("nextbutton")
        if(!is.null(input$serial)){
         shinyjs::hide("OK")
        }
    } else {
        output$end <- renderUI(NULL)
        shinyjs::hide("nextbutton")
    }

    })
    


    # Show the results #
    output$results = renderDataTable({
      savedData <- data.frame()
      x <- nrow(values$alldes)/values$a
      gid <- rep(1:x, each=values$a)
      alt <- rep(c(1:values$n.alts), values$n.init)
      results <- cbind(values$alldes,as.data.frame(values$surveyData[1]), as.data.frame(gid), as.data.frame(alt))
      print(results)
      filas <- nrow(results)
      pid <- rep(1:(filas/nrow(values$des)), each = nrow(values$des))
      results <- cbind(results, pid)
      print(results)
      results <- as.data.frame(results)
      vars <- results[ , -((ncol(results) - 3):ncol(results))]
      vars <- colnames(vars)
      vars <- paste(vars, collapse ="+")
      values$vars <- vars
      tablex <- values$tablex
      d <- nrow(tablex)
      i <- 1
      titles <- c()
      while(i<d+1){
        current <- tablex[3][i,]
        current <- unlist(strsplit(current, ","))
        current <- current[2:length(current)]
        titles <- append(titles, current)
        i <- i+1
      }
      if(values$nula==TRUE){
        titles <- append("opt-out",titles)
      }
      titles <- str_replace_all(titles, "[^[:alnum:]]", " ")
      titles <- append(titles, c("bin.responses", "gid", "alt", "pid"))
      colnames(results) <- titles 
      values$titles <- titles
      savedData <<- results
      values$results <- results
    })

    # estimate the clogit #
    observeEvent(input$estimate, {
      results <- values$results
      titles <- values$titles
      titles <- titles[1:(length(titles)-4)]
      titles <- paste0('`',titles, '`')
      titles <- paste(titles, collapse ="+")
      model <- clogit(as.formula(paste("bin.responses~",titles)),strata(gid),data=results)
      values$model <- summary(model)
    })
    
    # estimate the mixlogit #
    observeEvent(input$mixed, {
      results <- values$results
      titles <- values$titles
      titles <- titles[1:(length(titles)-4)]
      titles <- paste0('`',titles, '`')
      x <- length(titles)
      y <- 0
      eqnormal <- c()
      while(y<x){
        eqnormal <- cbind(eqnormal, "n")
      }
      names(eqnormal) <- titles
      titles <- paste(titles, collapse ="+")
      DatosLX <- mlogit.data(results, shape = "long", alt.var = "alt", chid.var = "gid", id.var = "pid")
      ResultadosLX <- mlogit(as.formula(paste("bin.responses~",titles, " | 0")), data=DatosLX, rpar=eqnormal, R=100, halton=NA, panel=TRUE)
      values$model <- summary(ResultadosLX)
    })
    

    

    # display the results #
    output$model <- renderPrint({
      values$model
    })

    # download the data #
    output$downloadResults <- downloadHandler(
    filename = function() {
      paste("results-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(as.data.frame(values$results), path = file)
    })
    
    
    # responses counter #
    observeEvent(input$OK,{
    if (sn > values$s){
      sncum <<- sncum+1
    }
      values$sncum <- sncum
    })
    
    observeEvent(input$linearprice, {
      tablex <- values$tablex
      tablex <- as.vector(unlist(tablex[1]))
    })
    
    output$pricevars <- renderUI({
      results <- values$results
      results <- as.data.frame(results)
      final1 <- as.data.frame(values$final1)
      selectInput('selectprice', 'Select price variables', names(final1), multiple = TRUE)
    })
    
    output$remprice <- renderUI({
      numericInput("otherprice", "Omitted price", value=0)
    })
    
    observeEvent(input$goprec,{
      results <- values$results
      pricevars <- input$selectprice
      otherprice <- input$otherprice
      results$pricevars <- 0
      x <- length(pricevars)
      i <- 1
      while (i<=x){
        current <- pricevars[i]
        current <- results[,current]
        current <- current*as.numeric(pricevars[i])
        results$pricevars <- results$pricevars+current
        i <- i+1
      }
      if(is.null(results$`opt out`)){
        results$pricevars[results$pricevars == 0] <- otherprice
      } else {
        results$pricevars[results$pricevars == 0] <- otherprice
        results$pricevars[results$`opt out` == 1] <- 0
      } 
      values$results <- results
      output$results = renderDataTable({
        values$results
      })
      values$pricevars <- pricevars
    })
    
    observeEvent(input$linpriceclogit, {
      results <- values$results
      titles <- values$titles
      pricevars <- values$pricevars
      titles2 <- titles[1:(length(titles)-4)]
      titles2 <- grep(paste0(pricevars, collapse = "|"), titles2, invert = TRUE, value = TRUE)
      titles2 <- paste0('`',titles2, '`')
      titles2 <- paste(titles2, collapse ="+")
      model <- clogit(as.formula(paste("bin.responses~",titles2,"+pricevars")),strata(gid),data=results)
      values$model <- summary(model)
    })
    

    observeEvent(input$nextbutton, {
      values$sn <- 1
      values$respcount <- values$respcount+1
      if(!is.null(values$serialno)){
      if(values$serialno <= values$sncum){
        
        # importing parameters #
        x <- nrow(values$alldes)/values$a
        gid <- rep(1:x, each=values$a)
        alt <- rep(c(1:values$n.alts), values$n.init)
        results <- cbind(values$alldes,as.data.frame(values$surveyData[1]), as.data.frame(gid), as.data.frame(alt))
        filas <- nrow(results)
        pid <- rep(1:(filas/nrow(values$des)), each = nrow(values$des))
        results <- cbind(results, pid)
        results <- as.data.frame(results)
        vars <- results[ , -((ncol(results) - 3):ncol(results))]
        vars <- colnames(vars)
        vars <- paste(vars, collapse ="+")
        values$vars <- vars
        values$results <- results
        results <- values$results
        model <- clogit(as.formula(paste("bin.responses~",values$vars,"+strata(gid)")),data=results, method="efron")
        coef <- summary(model)$coefficients[,1]
        coef <- as.data.frame((cbind(coef, summary(model)$coefficients[,4])))
        coef <- cbind(coef,ifelse(abs(coef[,2]) >= 1.96,1,0))
        coef <- cbind(coef, coef[,1]*coef[,3])
        priors <- coef[,4]
        # end of importing parameters #
        
        # new creator function adapted for the sequential design #
        creator2 <- function (niveles, nula, a, s, priors) {
          x <- 0
          codif <- c()
          while (x<length(niveles)){
            codif <- append(codif, "D")
            x <- x+1
          }
          l <- sum(niveles)
          k <- length(niveles)
          if (nula==TRUE){
            pr <- (l-k+1)
          } else {
            pr <- (l-k)
          }
          y <- 0
          I <- diag(length(priors))
          sim <- MASS::mvrnorm(n=100, mu=priors, Sigma=I)
          if (nula==TRUE){
            a <- a
            sim <- list(sim[,1:1], sim[, 2:(length(priors))])
            t <- 0
            u <- a-1
            const <- c()
            while(t<=u){
              if (t<u){
                const <- append(const,0)
              } else {
                const <- append(const,1)
              }
              t <- t+1
            }
            dis <- CEA(lvls=niveles, coding=codif, n.alts=a, n.sets=s, alt.cte=const,
                       par.draws=sim, no.choice=TRUE, best=TRUE)
          }
          if (nula==FALSE){
            dis <- CEA(lvls=niveles, coding=codif, n.alts=a, n.sets=s,
                       par.draws=sim, no.choice=FALSE, best=TRUE)
          }
          if (nula==TRUE){
            return(list(dis, codif, const, codif))
          } else {
            return(list(dis, codif, NULL, codif))
          }
        } #end of the new creation function
        
        
        niveles <- values$niveles
        nula <- values$nula
        a <- values$a
        s <- values$s
        ndes <- creator2(niveles, nula, a, s, priors)
        ndes <- ndes[[1]]$design
        values$des <- ndes
        values$priors <- priors
      }}
      
       if(is.null(values$serial)){
         shinyjs::click("OK")
       }
      
    })

}

shinyApp(ui,server)
}
