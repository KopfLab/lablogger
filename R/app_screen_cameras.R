# RASPI CAMS ====
# NOTE: needs proper implementation
# output$raspicams <- renderUI({
#
#   # refresh if refresh is pushed
#   input$refresh_cams
#
#   # retrieve raspi camp ip addresses
#   boxes <- map2(values$cameras$label, values$cameras$ip,
#                 function(label, ip) {
#                   box(title = a(glue("{label} camera"), href = glue("http://{ip}"), target = "_new"), collapsible = TRUE, collapsed = FALSE,
#                       width = 12, status = "success", solidHeader = TRUE,
#                       tags$iframe(src=glue("http://{ip}"), width = "640px", height = "480px", frameborder="0")) %>%
#                     column(width = 12)
#                 })
#   return(tagAppendChildren(fluidRow(), list = boxes))
# })
