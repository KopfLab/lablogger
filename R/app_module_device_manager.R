deviceManagerServer <- function(input, output, session, dm_devices, dm_cloudinfo, dm_datalogs, access_token) {

  # namespace
  ns <- session$ns

  # add/register new device ===

  observeEvent(input$device_add, {
    module_message(ns, "debug", "adding new devices is not implemented yet")
    # # FIXME implement
    # # generate modal dialog with radio button selection of the device ypes
    # # make sure both devices and cloud info are up to date
    # dm_devices$refresh_devices()
    # dm_cloudinfo$refresh_cloud_info()
    # all_devices <- dm_cloudinfo$get_all_devices_cloud_info()
    # print(all_devices) # filter by !registered
  })

  observeEvent(input$device_inuse, {
    # FIXME implement
    module_message(ns, "debug", "deactivating devices is not implemented yet")
  })

  # devices selector ===

  devices <- callModule(
    deviceSelectorServer, "devices",
    get_devices = dm_devices$get_devices,
    get_selected_devices = dm_devices$get_selected_devices,
    refresh_devices = dm_devices$refresh_devices,
    select_devices = dm_devices$select_devices,
    access_token = access_token
  )

  # devices info ===

  deviceInfoServer <- callModule(
    deviceInfoServer, "devices_info",
    get_cloud_state = dm_cloudinfo$get_devices_cloud_state,
    refresh_cloud_state = dm_cloudinfo$refresh_cloud_state,
    get_cloud_data = dm_cloudinfo$get_devices_cloud_data,
    refresh_cloud_data = dm_cloudinfo$refresh_cloud_data,
    refresh_experiment_device_links = dm_devices$refresh_devices_experiments_links,
    get_cloud_info = dm_cloudinfo$get_devices_cloud_info,
    refresh_cloud_info = dm_cloudinfo$refresh_cloud_info,
    get_devices = dm_devices$get_selected_devices,
    get_state_logs = dm_datalogs$get_devices_state_logs,
    refresh_state_logs = dm_datalogs$refresh_state_logs
  )

}



deviceManagerUI <- function(id, width = 12) {

  ns <- NS(id)

  tagList(
    deviceSelectorUI(ns("devices"), width = width, selector_height = 200,
      add_footer = tagList(
        spaces(1),
        tooltipInput(actionButton, ns("device_add"), label = "Add device", icon = icon("plus-circle"), tooltip = "Register new device. NOT IMPLEMENETED YET"),
        spaces(1),
        tooltipInput(actionButton, ns("device_unuse"), label = "Deactivate", icon = icon("eye-slash"),
                     tooltip = "Deactivate the selected device(s). Inactive devices do not record any data but are otherwise fully functional and can be reactivated using -Add device-. NOT IMPLEMENTED YET.")
      )
    ),
    tabsetPanel(
      type = "tabs", # selected = "data",
      tabPanel(
        value = "live",
        "Live Info", br(),
        # fetch all is a bit confusing...
        deviceDataUI(ns("devices_info"), selected_options = c("r_exps"), include_fetch_all = FALSE),
        deviceStateUI(ns("devices_info"), include_fetch_all = FALSE),
        deviceInfoUI(ns("devices_info"), include_fetch_all = FALSE)
      ),
      tabPanel(
        value = "logs",
        "Logs", br(),
        deviceLogsUI(ns("devices_info"), include_fetch_all = FALSE)
      )
    )
  )
}
