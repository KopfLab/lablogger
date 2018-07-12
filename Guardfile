# Ruby guard file (need ruby >2.2 and bundler installed: 'gem install bundler')
# To make sure all the gems are installed, run 'bundle install' once in terminal
# Then you can use the Makefile target 'make gui_dev' to start the GUI in development mode
# For browser livereload to work, need the browser extension: http://livereload.com/extensions/#installing-sections
# If the delay is too short for relaunching the app, increase the grace_period

port = 5000

guard 'process', name: 'Shiny', command: ['R', '-e', " \
source('db/connection.R'); \
message('INFO: Connection to database... ', appendLF = FALSE); \
pool <- pool::dbPool(drv = RPostgreSQL::PostgreSQL(), host = db_host, dbname = db_name, user = db_user, password = db_pwd); \
message('successful.'); \
shiny::onStop(function() { pool::poolClose(pool) }); \
devtools::load_all('.'); \
labwareC3:::turn_debug_on(); \
labwareC3::run(group_id = group_id, access_token = access_token, pool = pool, app_pwd = NULL, port = #{port})"] do
  watch(%r{NAMESPACE})
  watch(%r{R/.+\.R$})
end

guard 'livereload', grace_period: 3 do
  watch(%r{NAMESPACE})
  watch(%r{R/.+\.R$})
end
