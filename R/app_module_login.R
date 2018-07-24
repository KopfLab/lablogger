
loginServer <- function(input, output, session, app_pwd, group, timezone) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    menu = NULL,
    logged_in = FALSE
  )

  # output
  output$group <- renderText(str_c("Group: ", group))
  output$tz <- renderText(str_c("Timezone: ", timezone))

  # always jump back to login
  observeEvent(input$login, login(input$password))
  observeEvent(input$auto_login_trigger, { if (is.null(app_pwd)) login(NULL) })

  login <- function(pwd) {
    log_in = FALSE
    if (is.null(app_pwd)) {
      message("INFO: No app_pwd required, logged in automatically")
      log_in = TRUE
    } else {
      glue("INFO: checking app_pwd '{pwd}'... ") %>% message(appendLF = FALSE)
      if (!is.null(pwd) && app_pwd == pwd) {
        message("correct.")
        log_in = TRUE
      } else {
        message("incorrect.")
        showModal(modalDialog(h2(str_c("Sorry, password not recognized.")), easyClose = TRUE, fade = FALSE))
      }
    }

    if (log_in) {
      hide("login-panel")
      show("welcome-panel")
      values$logged_in <- TRUE
    }
  }

  list(
    is_logged_in = reactive({ values$logged_in })
  )

}

loginUI <- function(id, title) {
  ns <- NS(id)

  tagList(
    div(id = ns("login-panel"),
        column(width = 12,
               h2(textOutput(ns("group"))),
               h3(textOutput(ns("tz"))),
               passwordInput(ns("password"), NULL, placeholder = "Please enter your password."), br(),
               selectInput(ns("auto_login_trigger"), NULL, choices = "1", selected = "1") %>% hidden(),
               actionButton(ns("login"), "Login")
        )),
    div(id = ns("welcome-panel"), column(width = 12, h2("Welcome to ", title, ". You have been succesfully logged on."))) %>% hidden()
  )
}
