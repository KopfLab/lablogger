# setup
# setup <- data.frame(
#   pos = paste0("p", 1:9),
#   field = c("DegC", "DegC", "DegC", "ug/LO2", "pH", "pH", "nA", "mV ORP", "mV ORP"),
#   probe = c("O2", "pH", "pH", "O2", "pH", "pH", "O2", "pH", "pH"),
#   vessel = paste("Vessel", c(2, 2, 3, 2, 2, 3, 2, 2, 3))
# )

# setup <- data.frame(
#   pos = paste0("p", 1:9),
#   field = c("pH", "mV ORP", "DegC", "ug/LO2", "pH", "pH", "nA", "mV ORP", "mV ORP"),
#   probe = c("pH", "pH", "pH", "O2", "pH", "pH", "O2", "pH", "pH"),
#   vessel = paste("Vessel", c(1, 1, 1, 1, 2, 3, 1, 2, 3))
# )

setup <- data.frame(
  pos = paste0("p", 1:9),
  field = c("pH", "mV ORP", "DegC", "mV", "pH", "pH", "DLI(d)", "mV ORP", "mV ORP"),
  probe = c("pH", "pH", "pH", "O2", "pH", "pH", "O2", "pH", "pH"),
  vessel = paste("Vessel", c(1, 1, 1, 1, 2, 3, 1, 2, 3))
)

data_types <- data.frame(
  field = c("DegC", "pH", "ug/LO2", "nA", "mV ORP"),
  data_type = c("Temp", "pH", "[O2]", "raw", "ORP"),
  units = c("[C]", "", "[ug/L]", "[nA]", "[mV]")
)

probes <- data.frame(
  probe = c("pH", "O2"),
  technical = c("InPro 3250iSG", "Inpro 6960i"),
  measures = c("pH, redox, temp", "O2, temp")
)

# generate field info
setup_info <- merge(setup, probes) %>% merge(data_types) 
setup_info <- setup_info[order(setup_info$pos),] %>%
  transform(trace = paste(probe, data_type, units))