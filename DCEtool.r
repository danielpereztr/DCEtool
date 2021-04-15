############## DCEtool created by Daniel Perez Troncoso ########################
############################ danielperez@ugr.es ################################
############################### release 0.0.1 ##################################

#### Required libraries ####
library(shiny)
library(shinythemes)
library(survival)
library(purrr)
library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggrepel)
library(digest)
library(idefix)
library(writexl)
library(shinyjs)

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

#### User interface ####
ui <- fluidPage(theme=shinytheme("flatly"),
      navbarPage("DCE tool",
                 tabPanel("Instructions", #Instructions
                          fluidRow(column(width=2),
                                   column(
                                     p("Welcome to DCEtool, an app to create, respond and analyse DCEs",style="color:black;text-align:center"),
                                     width=8,style="background-color:papayawhip;border-radius: 10px")),
                          hr(),
                          h4("The app"),
                          p("This app is intended to be a complete package for creating, surveying, and analysing discrete choice experiments. Although all these functionalities are available, the app can also be used only to obtain the design of a discrete choice experiment."),
                          h4("How does it work?"),
                          HTML("<p>Introduce the parameters of the DCE in the 'Parameters' tab. Suppose you want to enter the first attribute in Table 1 of this <a href='https://doi.org/10.1016/j.jval.2016.04.004'>article</a>. To do this, enter 'Efficacy' under 'Name of the attribute', 
                               '3' under 'Number of levels' and 'L1,L2,L3' under 'Levels' name'. Once you click on 'Add,' you will see the attribute appear on the right table. Try including more attributes. Once you have more then two attributes, enter the number of alternatives per choice 
                               set, the number of choice sets per respondent, choose if you want a null choice, and click on 'Save inputs'.</p>"),
                          HTML("<p>Once the inputs are saved, you can go to 'Design matrix' and click on 'Print the design matrix'. This procedure could take some seconds/minutes, depending on the design size and the computer's hardware.  The design that will be printed is a pilot design, 
                               a D-efficient design estimated with the <a href='http://dx.doi.org/10.18637/jss.v096.i03'>-idefix-</a> package (Traets et al. 2020) with priors coefficients equal to zero. If the DCE is to be implemented outside the app, the design can be downloaded by clicking on the 'Download' button. The output is using 'Dummy coding', 
                               and the attributes and levels codification can be interpreted as in Table 3 of this <a href='https://www.researchgate.net/publication/344360005_A_step-by-step_guide_to_design_implement_and_analyze_a_discrete_choice_experiment'>paper</a>. To implement the DCE in the app, you will need to give a name to the alternatives, an intro text, and an end text. Then you can proceed by clicking on the 'Create the survey' button.</p>"),
                          HTML("<p>In the next tab, 'Survey', a respondent can respond to the DCE by clicking on 'OK'. Once all the choice sets are completed, the end text will appear, and a new survey can be responded by clicking on 'OK'. If the researcher wants to use a sequential design, see 
                               the original proposal by <a href='https://doi.org/10.1108/9781849507738-006'>Bliemer and Rose (2010)</a>, she can click on 'Next sequential design', and the information of the recorded responses will be used for increasing the efficiency of the design. Once the new priors appear under 
                               the survey, a new respondent can complete the survey by clicking on 'OK'. If all priors are equal to zero, the model coefficients are not significant at 5% yet (probably due to a small number of responses). Adding more responses should lead to significant coefficients. Every time a survey is finished, you can move to the 'Results' tab. </p>"),
                          HTML("<p>In the 'Results' tab, you can download the data set, which is already coded to estimate a conditional logit model, or you can estimate the conditional logit model directly by clicking on 'Estimate a clogit'.</p>"),
                          hr(),
                          h4("About"),
                          HTML("<p>This app was built on top of the <a href='http://dx.doi.org/10.18637/jss.v096.i03'>-idefix-</a> package by Traets et al. (2020) to simplify the task of creating DCEs and feasibly implement <a href='https://doi.org/10.1108/9781849507738-006'>Bliemer and Rose's sequential design approach</a>. </p>"),
                          hr(),
                          HTML("<p>The app was developed in R by <a href='http://danielpereztr.es/'>Daniel Pérez Troncoso</a> at the Department of Applied Economics at the University of Granada.
           Contact: <a href = 'mailto:danielperez@ugr.es'>danielperez@ugr.es</a></p>"),
                          hr(),
                          HTML("<p>Please, cite as Daniel Pérez Troncoso (2021) DCE tool. <a href='link'>link</a>"),
                          hr(),
                          HTML("<p><b>Changelog</b><p>"),
                          HTML("<ul>
                                <li>20/03/2020 (V 0.0): Release</li>
                                </ul>")
                          ),
                 
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
                               h4("Other parameters"),
                               numericInput("a", "No. of alternatives per choice set", ""),
                               numericInput("s", "No. of choice sets per respondent", ""),
                               checkboxInput("nula", "Opt-out alternative", FALSE),
                               actionButton(inputId = "gen.button", label = "Save inputs", icon = icon("window-restore")),
                               hr(),
                               actionButton(inputId = "example.button", label = "Load example data", icon = icon("upload"))
                             ),
                             mainPanel(
                               dataTableOutput('table')
                             )
                           )
                        ),
                 
                  tabPanel("Design matrix", #Design output
                           sidebarLayout(
                             sidebarPanel(
                               h4("Design matrix"),
                               actionButton(inputId = "go.button", label = "Print the design matrix", icon = icon("calculator")),
                               hr(),
                               downloadButton("downloadData", "Download"),
                               hr(),
                               h4("Survey settings"),
                               textInput("altnames", "Alternative' names (in order and separated by commas)", ""),
                               hr(),
                               textAreaInput("intro", "Intro text", "Intro", height = "200px"),
                               hr(),
                               textAreaInput("end", "End text", "End", height = "200px"),
                               hr(),
                               actionButton(inputId = "survey.button", label = " Create the survey", icon = icon("plus-circle"))
                             ),
                             mainPanel(
                              dataTableOutput('table2')
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
                             column(12, align = 'center', textOutput('intro')),
                             column(12, align = "center", actionButton("OK", "OK")),
                             hr(),
                             hr(),
                             column(12, align = 'center', textOutput('end')),
                             hr(),
                             hr(),
                             useShinyjs(),
                             column(12, align = "center", actionButton("nextseq", "Next sequential design")),
                             hr(),
                             hr(),
                             useShinyjs(),
                             verbatimTextOutput("priors")
                             
                           )
                  ),
                 
                  tabPanel("Results", #Results page
                      sidebarLayout(
                        sidebarPanel(
                          downloadButton("downloadResults", "Download"),
                          hr(),
                          actionButton(inputId = "estimate", label = "Estimate a clogit", icon = icon('equals'))
                        ),
                        mainPanel(
                          dataTableOutput('results'),
                          verbatimTextOutput('model')
                        )
                      )
                  )       
    ) #End of navpage
    ) #End of UI

server = function(input, output) {
  
  # Storage of changing variables #
  values <- reactiveValues()
  values$df <- data
  
  # Table input for attributes and levels #
  observeEvent(input$add.button,{
    cat("add Entry\n")
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
    cat("deleteEntry\n")
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
    tablex
  })
  
  # Save inputs in changing variables #
  observeEvent(input$gen.button,{
    values$a <- input$a
    values$s <- input$s
    values$nula <- input$nula
    values$niveles <- unlist(c(values$df[2]))
    values$atnames <- unlist(c(values$df[1]))
    values$levnames <- unlist(c(values$df[3]))
  })
  
  
  # DCE creator function (when priors are = 0) #
  creator <- function (niveles, nula, a, s) {
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
    priors <- c()
    while (y<pr){
      priors <- append(priors,0)
      y <- y+1
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
  
  # Creator function execution #
  observeEvent(input$go.button,{
    niveles <- values$niveles
    nula <- values$nula
    a <- values$a
    s <- values$s
    dis <- creator(niveles, nula, a, s)
    values$alt.cte <- dis[[3]]
    values$codif <- dis[[4]]
    final <- dis[[1]]$design
    values$des <- final
    if (nula==TRUE){
      a <- a+1
      values$a <- a
    } 
    output$table2 = renderDataTable({
      final
    })
    values$downloadata <- as.data.frame(values$des) #formatting the data to be downloaded
    values$downloadata <- cbind(rownames(values$des),values$des)
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
      if (sn > n.total){
        sn <<- 0
      }
      sn <<- sn + 1
      values$sn <- sn
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
    if (values$sn >= 0 && sn <= n.total) {
      return(list(radioButtons("survey", buttons.text, alts , inline = TRUE, selected = "None")))
    } 
    })
    
    # Set the choice set number #
    observeEvent(input$OK, {
    n.total <- values$s
    sn <- values$sn
    if (sn <= n.total) {
      output$set.nr <- renderText(paste(c("choice set:", sn, "/", n.total)))
    } else {
      output$set.nr <- renderText(NULL)
    }
    })
    
    # Intro text #
    output$intro <- renderText(values$intro.text)
    observeEvent(input$OK, {
      output$intro <- renderText(NULL)
    })
    
    # End of the survey #
    observeEvent(input$OK, {
    n.total <- values$s
    if (values$sn > n.total) { # Display end text
        values$alldes <- rbind(values$alldes, values$des)
        shinyjs::show("nextseq")
        output$end <- renderText(values$end.text)
    } else {
        output$end <- renderText(NULL)
        shinyjs::hide("nextseq")
    }
    
    # Quit application # 
    if (input$OK > (n.total)) {
    values$sn <- 0
    }
    })
    
    # Show the results #
    output$results = renderDataTable({
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
    })
    
    # estimate the clogit #
    observeEvent(input$estimate, {
      results <- values$results
      model <- clogit(as.formula(paste("bin.responses~",values$vars)),strata(gid),data=results)
      values$model <- summary(model)
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
    
    # create the next sequential design #
    observeEvent(input$nextseq,{
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
      model <- clogit(as.formula(paste("bin.responses~",values$vars)),strata(gid),data=results)
      coef <- summary(model)$coefficients[,1]
      coef <- as.data.frame((cbind(coef, summary(model)$coefficients[,4])))
      coef <- cbind(coef,ifelse(abs(coef[,2]) >= 1.96,1,0))
      coef <- cbind(coef, coef[,1]*coef[,3])
      priors <- coef[,4]
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
    })
    
    output$priors = renderText({
      values$priors
    })
     
}

shinyApp(ui,server)