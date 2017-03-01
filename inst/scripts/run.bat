:: This is the launch script for the GUI on Windows
:: Make sure to input the correct path to your R installation

Echo Launch dir: "%~dp0"
Echo Current dir: "%CD%"
"C:\Program Files\R\R-3.3.3\bin\R.exe" -e "labwareC3::run_gui (launch.browser = TRUE)"
