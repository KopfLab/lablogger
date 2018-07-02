# simplifying make commands
SHELL := /bin/bash

# install the virtual environments and all dependencies
install: bin bin/lambda lib/python2.7/site-packages/pg8000

bin:
	virtualenv . --python=python2.7

bin/lambda:
	source bin/activate && pip install python-lambda

lib/python2.7/site-packages/pg8000:
	source bin/activate && pip install pg8000

# initialize project (only needed once)
init: install config.yaml webhooks service.py

service.py:
	source bin/activate && lambda init

# cleanup the virtual environment installation
clean:
	rm -R -f dist
	rm -R -f lib
	rm -R -f include
	rm -R -f bin

# invoke the lambda function for a state log
invoke_state_log: install
	source bin/activate && lambda invoke -v --event-file event_state_log.json

# invoke the lambda function for a data log
invoke_data_log: install
	source bin/activate && lambda invoke -v --event-file event_data_log.json

# configuration for deployment - generate from template file (automatically git-ignored)
config.yaml: config_template.yaml
	cp config_template.yaml config.yaml

# deploy the lambda function (requires config.yaml to exist and have the necessary credentials)
deploy: install
	source bin/activate && lambda deploy

# webhook files - generate webhook files from template (automatically git-ignored)
webhooks: webhook_data_log.json webhook_state_log.json
webhook_data_log.json: webhook_data_log_template.json
	cp webhook_data_log_template.json webhook_data_log.json
webhook_state_log.json: webhook_state_log_template.json
	cp webhook_state_log_template.json webhook_state_log.json
