# Lambda function development

## Installation

### python / pip

- easiest is just to install anaconda: https://www.anaconda.com/what-is-anaconda/
- make sure python and pip exist (`python -V` and `pip -V`)

### virtual environments

- install virtualenv `pip install virtualenv` (virtual environment to develop lambda functions in, [doc](http://docs.python-guide.org/en/latest/dev/virtualenvs/#lower-level-virtualenv))
  - test that it was installed properly `virtualenv --version`
  - if it does give a version number, it might not be in the path (possibly already installed), easiest solution: `pip uninstall virtualenv`, then re-installed
- Optional: for general work with virtualenvs, use the wrapper: `pip install virtualenvwrapper` ([doc](http://docs.python-guide.org/en/latest/dev/virtualenvs/#virtualenvwrapper))

### AWS CLI

- install AWS command line interface: `conda install -c conda-forge awscli` (via anaconda, easier than with pip and then defining the env. variables)
  - to check: `aws --version`
  - to configure: `aws configure` ([doc](http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-getting-started.html))
  - test: `aws rds describe-db-instances`

## Setup

- create a new project folder (best in atom terminal - extension `platformio-ide-terminal` to develop python there):
- run `make install`, which does the following:
  - creates virtual environment: `virtualenv .`
  - activates virtual environment: `source bin/activate`
    - Note: from the make file it does not translate into the shell but if run manually, terminal should show `(folder name)` at the beginning of the console line
  - installs python lambda for the virtual environment: `pip install python-lambda` ([doc](https://github.com/nficano/python-lambda))
  - optional (if needed): installs postgresql db driver in the virtual env.: `pip install pg8000` ([doc](https://github.com/mfenniak/pg8000))
- run `make init`:
  - bootstraps lambda project if it does not exist yet: `lambda init`

## Development

- develop the lambda function in the `service.py` file and connected scipts
- run `make invoke` to run lambda function with the information in `event.json`
  - uses `lambda invoke -v` in the virtual environment

## Deployment

 - run `make deploy` to deploy the lambda function:
  - uses `lambda deploy` in the virtual environment (requires config.yaml to exist and be fully defined - see `config_template.yaml` for details)
 - Note: for large project dependencies, consider using S3 bucket for the labmda function, see [docs](https://github.com/nficano/python-lambda#uploading-to-s3) for details
 - Note: for the lambda function to work properly, it needs to be well integrated into other services (see below for details)

## Integration

For the lambda function to be useful, it needs to be setup with the right IAM role and usually some sort of API gateway. This happens separately from the lambda function development and deployment, for a good intro see https://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-lambda-non-proxy-integration.html#getting-started-new-lambda

- IAM administration: https://console.aws.amazon.com/iam
  - It is useful to create an IAM role for the lambda function, a useful default policy is `AWSLambdaVPCAccessExecutionRole` (see [setup docs](https://docs.aws.amazon.com/lambda/latest/dg/vpc-rds-create-iam-role.html) for details)
- API gateway administration: https://us-west-2.console.aws.amazon.com/apigateway/
- Lambda functions administration: https://us-west-2.console.aws.amazon.com/lambda (this is where the deployed functions should appear)
- Additionally, to get access to the an RDS database running in a Virtual Private Container (VPC), the lambda function needs to be added to the VPC the database is in, this can be configured after the function is created:
  - in `Network` settings for the lambda function, select the proper VPC
  - select all the subnets
  - select the default security group (or even better, make a simple custom one for the lambda function)
- make sure to add the assigned security group to the allowed **Inbound** traffic rules of the RDS security group (enter the lambda security group ID under **Source**)
