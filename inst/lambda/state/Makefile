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
init: install service.py

service.py:
	source bin/activate && lambda init

# cleanup the virtual environment installation
clean:
	rm -R -f dist
	rm -R -f lib
	rm -R -f include
	rm -R -f bin

# invoke the lambda function
invoke: install
	source bin/activate && lambda invoke -v

# deploy the lambda function (requires config.yaml to exist and have the necessary credentials)
deploy: install config.yaml
	source bin/activate && lambda deploy
