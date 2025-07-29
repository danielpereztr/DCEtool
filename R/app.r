#' @export
DCEtool <- function(){
  

  requireNamespace('shiny')
  requireNamespace('rlist')
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
  requireNamespace('dfidx')
  requireNamespace('remotes')
  requireNamespace('shinyhelper')
  requireNamespace("httr")
  requireNamespace("markdown")
  requireNamespace("shinyjs")
  requireNamespace("ggplot2")
  requireNamespace("htmltools")

#' @importFrom shinyBS bsModal
#' @importFrom graphics barplot
#' @importFrom usethis use_pipe
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
#' @importFrom remotes install_github
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyhelper helper
#' @importFrom ggplot2 ggplot
#' @importFrom shiny tagList tags renderUI renderPlot reactive observe observeEvent reactiveVal icon hr uiOutput
#' @importFrom htmltools br
#' @importFrom shiny invalidateLater
#' @importFrom ggplot2 geom_col labs theme_minimal theme element_text margin element_line element_blank

  #external variables
  savechoices <- c()
  resultados <- c()
  num <- c()
  y <- c()
  se <- c()
  notifications_url <- "https://raw.githubusercontent.com/danielpereztr/dcetool_notifications/main/notifications.md"
  
  #external packages
  
  ui <- shiny::navbarPage(
    title = htmltools::div(
      htmltools::img(src = "assets/logo.png", height = "35px", style = "display:inline; margin-right:10px; vertical-align:middle;"),
      htmltools::span("DCEtool", style = "display:inline; vertical-align:middle; font-size:20px; font-weight:bold; color:white;"),
      htmltools::div(
        style = "position:relative; display:inline-block;",
        actionButton(
          inputId = "notif_bell",
          label = NULL,
          icon = shiny::icon("bell"),
          style = "background:transparent; border:none; color:white; font-size:15px; margin-right:12px;",
          title = "Show notifications"
        ),
        htmltools::tags$span(
          id = "notif_badge",
          class = "badge badge-danger",
          style = "position:absolute;top:-8px;right:-10px;display:none;z-index:10;",
          "0"
        )
      ),
      shinyjs::useShinyjs(),
      htmltools::tags$script(htmltools::HTML("
      $(document).ready(function() {
        // Funcion para alternar notificaciones con animacion
        function toggleNotifications() {
          var dropdown = $('#notif_dropdown');
          if(dropdown.hasClass('showing')) {
            dropdown.removeClass('showing');
            setTimeout(function() { dropdown.hide(); }, 300);
          } else {
            dropdown.show();
            setTimeout(function() { dropdown.addClass('showing'); }, 10);
          }
        }
        
        // Cerrar notificaciones al hacer clic fuera
        $(document).on('click', function(e) {
          if (!$(e.target).closest('#notif_dropdown, #notif_bell').length) {
            var dropdown = $('#notif_dropdown');
            if(dropdown.hasClass('showing')) {
              dropdown.removeClass('showing');
              setTimeout(function() { dropdown.hide(); }, 300);
            }
          }
        });
        
        // Alternar visibilidad al hacer clic en la campana
        $('#notif_bell').on('click', function(e) {
          e.stopPropagation();
          toggleNotifications();
        });
        
        // Animacion para nuevas notificaciones
        Shiny.addCustomMessageHandler('newNotification', function(n) {
          if(n > 0) {
            $('#notif_badge').text(n).addClass('new-notification').show();
            setTimeout(function() {
              $('#notif_badge').removeClass('new-notification');
            }, 1000);
          }
        });
      });
      
      Shiny.addCustomMessageHandler('updateNotifBadge', function(n) {
        if(n > 0) {
          $('#notif_badge').text(n).show();
        } else {
          $('#notif_badge').hide();
        }
      });
    ")),
      
      shiny::absolutePanel(
        id = "notif_dropdown",
        top = 60, right = 30, width = 360, draggable = FALSE,
        style = "display:block; z-index:1001; background:white; box-shadow:0 5px 20px rgba(0,0,0,0.15); border-radius:10px; padding:0; border:1px solid #eee;",
        htmltools::div(
          style = "border-bottom:1px solid #eee; padding:15px 20px; display:flex; align-items:center; font-weight:bold;",
          shiny::icon("bell", style = "margin-right:10px; color:#666;"),
          "Notifications"
        ),
        htmltools::div(
          style = "max-height:370px; overflow-y:auto; padding:15px 20px;",
          shiny::htmlOutput("notificationsContent")
        )
      )
    ),
    id = 'inTabset',
    header = tagList(
      shiny::includeCSS(system.file("www/custom.css", package = "DCEtool"))
    ),
    shiny::tabPanel(
      "Home", value = "start",
      shiny::fluidRow(
        shiny::column(
          width = 8, offset = 2,
          align = "center",
          htmltools::div(
            style = "background:#f4f8fd; padding:28px 32px 18px 32px; border-radius:20px; margin-bottom:28px; box-shadow:0 2px 12px #0001;",
            htmltools::img(src = "assets/logo.png", height = "48px", style = "margin-bottom:14px; border-radius:9px;"),
            htmltools::h2("Welcome to DCEtool", style = "font-weight:bold; color:#2c3e50; margin-bottom:14px;"),
            htmltools::p(
              style="font-size:17px; color:#444; margin-bottom:18px;",
              htmltools::HTML("<b>DCEtool</b> is a graphical interface in R for designing and analyzing <b>Discrete Choice Experiments (DCEs)</b>. Easily generate efficient designs, preview and deploy surveys, and analyze results - all without coding.")
            ),
            htmltools::tags$ul(
              style = "text-align:left; font-size:15px; color:#333; margin-left:2em; margin-bottom:18px;",
              htmltools::tags$li(tags$b("No coding required:"), " Intuitive, guided workflow for all steps."),
              htmltools::tags$li(tags$b("Efficient designs:"), " Supports Bayesian and classical algorithms."),
              htmltools::tags$li(tags$b("Survey preview:"), " Instantly preview and test your questionnaire."),
              htmltools::tags$li(tags$b("Built-in analysis:"), " Estimate models and visualize results directly."),
              htmltools::tags$li(tags$b("Export:"), " Download designs and results as Excel files.")
            )
            ,
            htmltools::div(
              style = "display: flex; justify-content: center; gap: 20px; margin-bottom:12px;",
              shiny::actionButton("create_new_dce", "Create a new DCE", style = "padding:8px 20px; font-weight:600; border-radius:8px; font-size:16px; background:#1676d2; color:white; border:none;"),
              shiny::actionButton("load_data", "Load your data", style = "padding:8px 20px; font-weight:600; border-radius:8px; font-size:16px; background:#ededed; color:#333; border:none;")
            ),
            htmltools::div(
              style="margin-top:10px; font-size:15px; color:#666;",
              "Find more information in the ",
              htmltools::tags$a("user guide", href = "https://danielpereztr.github.io/posts/DCEtool/", target = "_blank"),
              " or visit the ",
              htmltools::tags$a("About", href = "#", onclick = "Shiny.setInputValue('inTabset', 'About', {priority:'event'});"),
              " tab."
            ),
            htmltools::div(
              style="margin-top:10px; font-size:14px; color:#888;",
              "Please cite as P\u00e9rez-Troncoso (2022). Efficient and Accessible Discrete Choice Experiments: DCEtool (Version 1.2.0). ",
              htmltools::tags$a(href = "https://danielpereztr.github.io/posts/DCEtool", target = "_blank", "danielpereztr.github.io/posts/DCEtool")
            )
          ),
          # Caja tipo promo ChoiceLab (opcional, solo si quieres tambien en home)
          htmltools::div(
            style = "background:#e7f3fe; padding:18px 22px; border-radius:14px; margin-top:16px; border-left:6px solid #1676d2; display: inline-block; text-align: left;",
            htmltools::tags$b("Need an online DCE platform? "),
            htmltools::span("Try "),
            htmltools::tags$a("ChoiceLab", href="https://cognitur.net/cl", target="_blank"),
            htmltools::span(" by Cognitur to create, share, and analyze DCEs online. "),
            htmltools::tags$a("Learn more", href="https://cognitur.net", target="_blank")
          )
        )
      )
    )
    , #Home tab
    shiny::tabPanel("Design settings", value = "params", #Design settings tab
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          htmltools::div(
            style = "background:#f8f9fa; padding:22px 20px 12px 20px; border-radius:18px; margin-bottom:28px; box-shadow:0 2px 10px #0001;",
            shinyhelper::helper(
              shiny::numericInput("ats", "Number of attributes", value = 0),
              type = "inline",
              content = "The number of attributes determines..."
            ),
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
          )
        ),
        shiny::mainPanel(
          htmltools::div(
            style = "background:#f4f8fd; padding:26px 28px 18px 28px; border-radius:20px; margin-bottom:28px; box-shadow:0 2px 12px #0001;",
            shiny::verbatimTextOutput('levelsnumber'),
            shiny::verbatimTextOutput('errorscheck'),
            shiny::uiOutput('gobut')
          )
        )
      )
    ),
    shiny::tabPanel(title = "Design matrix", value = "desmattab", #Design matrix tab
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          htmltools::div(
            style = "background:#f8f9fa; padding:22px 20px 12px 20px; border-radius:18px; margin-bottom:28px; box-shadow:0 2px 10px #0001;",
            uiOutput("desmatrix_controls")
          )
        ),
        shiny::mainPanel(
          htmltools::div(
            style = "background:#f4f8fd; padding:26px 28px 18px 28px; border-radius:20px; margin-bottom:28px; box-shadow:0 2px 12px #0001;",
            shiny::uiOutput("spinner"),
            shiny::verbatimTextOutput("desdetails"),
            shiny::verbatimTextOutput("printlevnames"),
            shiny::uiOutput('modmatrix'),
            hr(),
            shiny::verbatimTextOutput("decoded_main"),
            shiny::uiOutput("after_decoded"),
            shinyBS::bsModal("modaldecode", "", "decode", size = "large",
                             shiny::fluidRow(
                               shiny::verbatimTextOutput("decoded")
                             )
            )
          )
        )
        
      )
    ),
    shiny::tabPanel(title = "Create a survey", value = "createsurv", # Crear la encuesta
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          htmltools::div(
            style = "background:#f8f9fa; padding:22px 20px 12px 20px; border-radius:18px; margin-bottom:28px; box-shadow:0 2px 10px #0001;",
            htmltools::p("Intro and end text: "),
            shiny::textAreaInput("intro", "Introductory text in Markdown", rows = 3),
            shiny::textAreaInput("outro", "End text in Markdown", rows = 3),
            htmltools::hr(),
            htmltools::p("Label each alternative: "),
            shiny::uiOutput("labels"),
            htmltools::hr()
          )
        ),
        shiny::mainPanel(
          htmltools::div(
            style = "background:#f4f8fd; padding:26px 28px 18px 28px; border-radius:20px; margin-bottom:28px; box-shadow:0 2px 12px #0001;",
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
            shiny::uiOutput("dispoutrotext")
          )
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
                 htmltools::div(
                   style = "background:#f8f9fa; padding:22px 20px 12px 20px; border-radius:18px; margin-bottom:28px; box-shadow:0 2px 10px #0001;",
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
                 )
               ),
               shiny::mainPanel(
                 htmltools::div(
                   style = "background:#f4f8fd; padding:26px 28px 18px 28px; border-radius:20px; margin-bottom:28px; box-shadow:0 2px 12px #0001;",
                   DT::DTOutput("restable"),
                   shiny::verbatimTextOutput("clog"),
                   shiny::plotOutput("figure")
                 )
               )
               
             )
             ),
    shiny::tabPanel(
      title = "About", 
      shiny::fluidRow(
        shiny::column(
          width = 8, offset = 2,
          align = "left",
          htmltools::div(
            style = "background:#f8f9fa;padding:24px 24px 8px 24px;border-radius:16px;margin-bottom:30px;box-shadow:0 2px 12px #0001;",
            htmltools::h2("About DCEtool", style = "font-weight:bold;"),
            htmltools::div(
              style="display:flex;gap:12px;align-items:center;margin-bottom:12px;",
              htmltools::img(src="assets/logo.png", height="40px", style="border-radius:6px;"),
              htmltools::div(
                htmltools::tags$b("Developed by Daniel P\u00e9rez-Troncoso"),
                htmltools::tags$span(" - Version 1.2.0 (July 2025)", style="color:#888;font-size:15px;")
              )
            ),
            htmltools::p(
              "DCEtool is an R Shiny application for designing, running, and analyzing Discrete Choice Experiments (DCEs). Designed for maximum usability and efficiency, DCEtool is open, free, and aimed at both beginners and experts."
            ),
            htmltools::p(
              "For documentation and guides, visit the ",
              htmltools::tags$a("online user guide", href="https://danielpereztr.github.io/posts/DCEtool/", target="_blank"), "."
            ),
            htmltools::p(
              htmltools::tags$b("How to cite DCEtool: "),
              "P\u00e9rez-Troncoso, D. (2022). DCEtool (v1.2.0) [Software]. https://cran.r-project.org/package=DCEtool"
            )
          ),
          htmltools::div(
            style = "background:#e7f3fe;padding:20px;border-radius:14px;margin-bottom:20px;border-left:6px solid #1676d2;",
            htmltools::tags$b("Looking for an online DCE platform?"),
            htmltools::p(
              "Try ",
              htmltools::tags$a("ChoiceLab", href="https://cognitur.net/cl", target="_blank"),
              " by Cognitur for easy survey deployment, online data collection, and advanced analytics. Ideal for professional projects, teaching, and consulting."
            ),
            htmltools::div(
              style="font-size:14px;",
              "More at ",
              htmltools::tags$a("cognitur.net", href="https://cognitur.net", target="_blank")
            )
          ),
          htmltools::h4("Downloads in the last month"),
          shiny::plotOutput("downloads", height = "280px")
        )
      )
    )
    ,
    footer = tagList(
    htmltools::tags$script(htmltools::HTML("
      Shiny.addCustomMessageHandler('toggleNotifDropdown', function(message) {
        var dropdown = document.getElementById('notif_dropdown');
        if(dropdown.style.display === 'none' || dropdown.style.display === ''){
          dropdown.style.display = 'block';
        } else {
          dropdown.style.display = 'none';
        }
      });
    
      // Cierra el dropdown al hacer clic fuera
      document.addEventListener('click', function(event) {
        var dropdown = document.getElementById('notif_dropdown');
        var bell = document.getElementById('notif_bell');
        if (dropdown && bell) {
          if (!dropdown.contains(event.target) && !bell.contains(event.target)) {
            dropdown.style.display = 'none';
          }
        }
      });
    ")))
  )
  
  server <- function(input, output, session){
    # Contador de notificaciones
    notif_count <- reactiveVal(0)
    
    # Actualizar badge al inicio y periodicamente
    observe({
      invalidateLater(300000)  # Actualizar cada 5 minutos
      
      tryCatch({
        response <- httr::GET(notifications_url)
        if (httr::http_status(response)$category == "Success") {
          md_txt <- httr::content(response, as = "text", encoding = "UTF-8")
          # Solo contar los titulos que son [NEW]
          count <- length(grep("^###?\\s*\\[NEW\\]", strsplit(md_txt, "\n")[[1]]))
          notif_count(count)
        } else {
          notif_count(0)
        }
      }, error = function(e) {
        notif_count(0)
      })
    })
    
    # Actualizar el badge
    observe({
      session$sendCustomMessage("updateNotifBadge", notif_count())
    })
    
    
    
    # Renderizar contenido de notificaciones
    output$notificationsContent <- renderUI({
      tryCatch({
        if (httr::http_error(notifications_url)) {
          htmltools::div(
            style = "color:#999;text-align:center;padding:30px 0;",
            icon("exclamation-circle", style = "font-size:2em;margin-bottom:10px;color:#ccc;"),
            br(),
            "Could not load notifications"
          )
        } else {
          md_txt <- httr::content(httr::GET(notifications_url), as = "text", encoding = "UTF-8")
          if (nchar(md_txt) == 0) {
            htmltools::div(
              style = "color:#999;text-align:center;padding:30px 0;",
              icon("check-circle", style = "font-size:2em;margin-bottom:10px;color:#4CAF50;"),
              br(),
              "No new notifications"
            )
          } else {
            # Convertir Markdown a HTML
            html_content <- markdown::markdownToHTML(text = md_txt, fragment.only = TRUE)
            
            # Mejorar estilos
            styled_content <- htmltools::HTML(gsub("<ul>", "<ul style='padding-left:20px;margin-bottom:15px;'>", html_content))
            styled_content <- gsub("<li>", "<li style='margin-bottom:8px;'>", styled_content)
            styled_content <- gsub("<h1>|<h2>|<h3>|<h4>", "<h4 style='margin-top:1.2em;margin-bottom:0.8em;color:#2c3e50;border-bottom:1px solid #eee;padding-bottom:5px;'>", styled_content)
            
            htmltools::div(
              style = "color:#333;font-size:14px;line-height:1.6;",
              htmltools::HTML(styled_content)
            )
          }
        }
      }, error = function(e) {
        htmltools::div(
          style = "color:#e74c3c;padding:20px;text-align:center;",
          icon("exclamation-triangle"),
          br(),
          paste("Error:", e$message)
        )
      })
    })
    
    
    shinyhelper::observe_helpers()
    
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
    
    shiny::observeEvent(input$create_new_dce, {
      shiny::updateNavbarPage(session, "inTabset", selected = "params")
    })
    
    shiny::observeEvent(input$load_data, {
      shiny::updateNavbarPage(session, "inTabset", selected = "desmattab")
    })
    
    # Values across functions
    values <- shiny::reactiveValues() 
    
    #Check design
    has_design <- reactive({
      !is.null(values$design) && nrow(as.data.frame(values$design)) > 0
    })
    
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
        tags[[i]] <- shinyhelper::helper(
          shiny::numericInput(
            paste0('at', i), 
            paste0('Levels in attribute ', i),
            0
          ),
          type = "inline",
          content = "Specify how many levels this attribute should have. For example, 'Price' might have 4 levels (e.g., \u20ac5, \u20ac10, \u20ac15, \u20ac20), while 'Brand' might have 3 levels (e.g., Brand A, Brand B, Brand C). Each attribute should have at least 2 levels to allow for meaningful variation in your experiment."
        )
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
        shinyhelper::helper(
          shiny::numericInput(
            "nalt", 
            "Alternatives per choice set", 
            value = 2, 
            min = 2, 
            max = 5
          ),
          type = "inline",
          content = "Specify how many alternatives (options) should appear in each choice set. For example, if you enter 2, each question will present two alternatives to the respondent. Typically, two or three alternatives are common in discrete choice experiments."
        )
      })
      
      
      #Ask the number of choice sets
      output$csetbut <- shiny::renderUI({
        shinyhelper::helper(
          shiny::numericInput(
            "nset", 
            "Number of choice sets", 
            value = 12, 
            min = 4
          ),
          type = "inline",
          content = "Set the total number of choice sets (questions) each respondent will answer. More choice sets increase statistical power but can also make the survey longer and potentially more demanding for respondents. Typical DCEs use between 8 and 16 choice sets per participant."
        )
      })
      
      #Checkbox if opt-out alternative
      output$optcheck <- shiny::renderUI({
        shinyhelper::helper(
          shinyWidgets::materialSwitch(
            inputId = "optout",
            label = "Opt-out alternative", 
            value = FALSE, 
            status = "danger"
          ),
          type = "inline",
          content = "Enable this option if you want to allow respondents to choose 'none of the above' in each choice set. Including an opt-out alternative can make the experiment more realistic, especially when participants might reject all options in real-world decisions."
        )
      })
      
      # Checkboxes to select the optimization procedure
      output$bayescheck <- shiny::renderUI({
        shinyhelper::helper(
          shinyWidgets::materialSwitch(
            inputId = "bayesian",
            label = "Bayesian design", 
            status = "danger", 
            value = FALSE
          ),
          type = "inline",
          content = "Activate this option to generate a Bayesian efficient design. Bayesian designs use prior information about the likely values of parameters to optimize the experiment. This can improve statistical efficiency, especially when you have some knowledge or assumptions about attribute importance."
        )
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
        npar <- sum(levels) - nats + null
        priors <- c()
        for (x in seq_len(npar)){
          priors <- append(priors, 0)
        }
        values$npar <- npar
        
        shinyhelper::helper(
          shiny::textInput(
            "priorsinput", 
            "Prior coefficients", 
            value = paste(shQuote(priors, type="cmd2"), collapse=", ")
          ),
          type = "inline",
          content = paste("Enter the prior coefficients to be used in generating the Bayesian design. These values represent initial estimates of the attribute-level coefficients based on previous knowledge or preliminary research. Each coefficient corresponds to a parameter in the utility function of your discrete choice model. Note that for each attribute, the first level is always omitted as a reference category (baseline), so coefficients are provided for all attribute levels except these reference levels.\n\n",
                          "The coefficients must be entered as comma-separated values in the exact order of appearance in your design (attributes ordered sequentially, and within each attribute, levels excluding the baseline). For instance, if you have two attributes, 'Price' (3 levels) and 'Brand' (3 levels), the input might look like this:\n\n",
                          "`-0.1, -0.2, 0.3, 0.5`\n\n",
                          "Here, 'Price' level 1 and 'Brand' level 1 are omitted as baselines, and the provided coefficients are for 'Price' levels 2 and 3, and 'Brand' levels 2 and 3, respectively.\n\n",
                          "By default, the app provides zeros (e.g., `0, 0, 0,...`) indicating no prior preference or information. Modify these only if you have prior evidence or estimates from previous studies. If unsure, leave the default zeros.")
        )
      })
      
      
      # show button for random seed
      output$seedbut <- shiny::renderUI({
        shinyhelper::helper(
          shiny::numericInput(
            "randomseed", 
            "Random seed", 
            value = 9999
          ),
          type = "inline",
          content = "Setting a random seed ensures that the random components of the design generation process can be reproduced. Use any integer value. This is especially useful if you want others to be able to recreate your exact design."
        )
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
    
    output$desmatrix_controls <- renderUI({
      if (!has_design()) {
        tagList(
          shinyhelper::helper(
            shiny::actionButton("gendesign", "Generate design"),
            type = "inline",
            content = "Click here to generate the experimental design using the parameters you've defined. This will create a new design matrix based on your settings."
          ),
          shinyhelper::helper(
            shiny::fileInput(inputId = "loaddesign", label = ""),
            type = "inline",
            content = "Upload a previously saved Excel file to reload your design and continue working from where you left off."
          )
        )
      } else {
        tagList(
          shinyhelper::helper(
            shiny::actionButton("gendesign", "Generate design"),
            type = "inline",
            content = "Click here to generate the experimental design using the parameters you've defined. This will overwrite any previous design."
          ),
          htmltools::br(),
          htmltools::br(),
          shinyhelper::helper(
            shiny::downloadButton("downloaddesing", "Save design", icon = shiny::icon("download")),
            type = "inline",
            content = "Click here to download your current design as an Excel file. Useful for saving or sharing your work."
          ),
          shinyhelper::helper(
            shiny::fileInput(inputId = "loaddesign", label = ""),
            type = "inline",
            content = "Upload a previously saved Excel file to reload your design. This is helpful if you want to edit or continue a previous experiment."
          ),
          shinyhelper::helper(
            shiny::actionButton("showdesdetails", "Show design details", icon = shiny::icon("eye")),
            type = "inline",
            content = "Click to display detailed information about your current design, including number of attributes, alternatives, and more."
          ),
          htmltools::br(),
          htmltools::br(),
          shinyhelper::helper(
            shiny::actionButton("writeatnames", "Name the attributes", icon = shiny::icon("braille")),
            type = "inline",
            content = "Click here to assign custom names to each attribute in your experiment for better interpretation and reporting."
          ),
          htmltools::br(),
          htmltools::br(),
          shiny::uiOutput("atnames"),
          htmltools::br(),
          shiny::uiOutput('levdropdown'),
          shiny::uiOutput('levtext'),
          htmltools::hr(),
          if(!is.null(input$modmatbut)){
            if(input$modmatbut == TRUE){
              shinyhelper::helper(
                shiny::actionButton("decode", "Decode the design matrix"),
                type = "inline",
                content = "Click to decode your design matrix. This will display the experimental conditions with actual attribute and level names."
              )
            }
          }
        )
      }
    })
    
    observeEvent(input$gotosurvprev, {
      shiny::updateTabsetPanel(session, "inTabset", selected = "createsurv")
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
        design <<- design
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
    
    # Decoding
    shiny::observeEvent(input$decode, {
      dec <- function(x){
        coding <- c()
        for (i in seq_len(values$nats)){
          coding <- append(coding, "D")
        }
        decodes <- idefix::Decode(as.matrix(values$design[,3:ncol(values$design)]), values$ncalts, values$levnames2,
                                  coding = coding, no.choice = values$no.choice, alt.cte = values$nullvect)
        decodes <- decodes$design
        decodes <- cbind("set" = rep(1:values$sets, each = values$ncalts), decodes)
        decodes <- decodes[decodes$set == x,]
        decodes <- decodes[,2:ncol(decodes)]
        decodes <- t(decodes)
        decodes <- as.data.frame(decodes)
        decodes <- cbind(values$atnames,decodes)
        row.names(decodes) <- NULL
        colnames(decodes) <- c("", 1:(ncol(decodes)-1))
        cat("\n")
        cat("=== Choice set", x, "===\n")
        print(decodes)
      }
      
      
      # Render en el mainPanel
      output$decoded_main <- shiny::renderPrint({
        for (i in 1:values$sets){
          dec(i)
        }
      })
      
      output$after_decoded <- shiny::renderUI({
        tags$div(
          style = "margin-top: 2em; text-align: center;",
          tags$p(
            style = "font-size: 1.15em; margin-bottom: 1.2em;",
            "From this point, you can save this design and create your own DCE in an external online survey platform. For a quick and easy way to generate and customize your choice sets, try ",
            tags$a(
              href = "https://cognitur.net/playground/csmaker.html",
              target = "_blank",
              "ChoiceSetMaker by Cognitur",
              style = "font-weight: bold; color: #4F46E5; text-decoration: underline;"
            ),
            "."
          ),
          tags$a(
            href = "https://cognitur.net/playground/csmaker.html",
            target = "_blank",
            class = "btn btn-primary btn-lg",
            style = "background: linear-gradient(90deg, #6366F1 0%, #2563EB 100%); border: none; font-weight: 500;",
            "Go to ChoiceSetMaker"
          )
        )
      })
      
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
      output$downloads <- renderPlot({
        x <- adjustedcranlogs::adj_cran_downloads("DCEtool", when = "last-month")[, c("date", "count")]
        if (!is.null(x) && nrow(x) > 0) {
          ggplot2::ggplot(x, aes(x = as.Date(date), y = count)) +
            geom_col(fill="#1676d2", alpha=0.85, width=0.8) +
            labs(title="CRAN Downloads (last month)", x="Date", y="Downloads") +
            theme_minimal(base_size=14) +
            theme(
              plot.title=element_text(face="bold", size=16, margin=margin(b=10)),
              axis.title=element_text(face="bold"),
              axis.text=element_text(color="#333"),
              panel.grid.major.y=element_line(color="#eee"),
              panel.grid.minor=element_blank()
            )
        }
      })
      
        
    })
    
    
  }
  
  shinyApp(ui, server)
}