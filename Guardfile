# Ruby guard file (need ruby >2.2 and bundler installed: 'gem install bundler')
# To make sure all the gems are installed, run 'bundle install' once in your browser
# Then you can use the Makefile target 'make gui_dev' to start the GUI in development mode
# For browser livereload to work, need the browser extension: http://livereload.com/extensions/#installing-sections
# If the delay is too short for relaunching the app, increase the grace_period

dir = 'inst/gui'
data = 'inst/extdata'
port = 5000

guard 'process', name: 'Shiny', command: ['R', '-e', "labwareC3:::run_gui_dev(base_dir = '#{data}', app_dir = '#{dir}', port = #{port})"] do
  watch(%r{#{dir}/.+\.R$})
end

guard 'livereload', grace_period: 4 do
  watch(%r{#{dir}/.+\.R$})
end
