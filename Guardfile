# Ruby guard file (need ruby >2.2 and bundler installed: 'gem install bundler')
# To make sure all the gems are installed, run 'bundle install' once in terminal
# Then you can use the Makefile target 'make gui_dev' to start the GUI in development mode
# For browser livereload to work, need the browser extension: http://livereload.com/extensions/#installing-sections
# If the delay is too short for relaunching the app, increase the grace_period

port = 5000

guard 'process', name: 'Shiny', command: ['R', '-e', "source('db/connection.R'); devtools::load_all('.'); labwareC3::run(db_host, db_name, db_user, db_pwd, port = #{port})"] do
  watch(%r{NAMESPACE})
  watch(%r{R/.+\.R$})
end

guard 'livereload', grace_period: 3 do
  watch(%r{NAMESPACE})
  watch(%r{R/.+\.R$})
end
