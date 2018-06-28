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
    - Note: to make successful database connections, make sure to have permissions set up for your IP to connect to the database
- run `make init`, which does the following:
  - bootstraps lambda project if it does not exist yet via `lambda init`
  - makes local copies of the config and webhook template files (automatically git-ignored)

## Development

- develop the lambda function in the `service.py` file and connected scipts
- run `make invoke` to run lambda function with the information in `event.json`
  - uses `lambda invoke -v` in the virtual environment

## Deployment

 - if no `config.yaml` exists yet (should have been created during `make init`), run `make config.yaml`
 - fill out the settings in `config.yaml`, see **Integration** section for details (no worries if not everything correct right away, deploy will throw errors)
 - run `make deploy` to deploy the lambda function:
   - uses `lambda deploy` in the virtual environment
 - Note: for large project dependencies, consider using S3 bucket for the labmda function, see [docs](https://github.com/nficano/python-lambda#uploading-to-s3) for details
 - Note: for the lambda function to work properly, it needs to be well integrated into other services (see below for details)

## Integration

Once the Lambda function is deployed it can be viewed and tested in the Lambda functions console https://console.aws.amazon.com/lambda. Detailed log messages and errors are available at the Cloudwatch console https://console.aws.amazon.com/cloudwatch. For the lambda function to be useful though, it usually needs to be setup with the right IAM role, VPC access and usually some sort of API gateway.

### IAM (database permissions)

- IAM administration: https://console.aws.amazon.com/iam
  - It is useful to create an IAM role for the lambda function, a useful default policy is `AWSLambdaVPCAccessExecutionRole` (see [setup docs](https://docs.aws.amazon.com/lambda/latest/dg/vpc-rds-create-iam-role.html) for details)
  - Add the name of the role in the `role` setting of the `config.yaml`
- Additionally, to get access to the an RDS database running in a Virtual Private Container (VPC), the lambda function needs to be added to the VPC the database is in:
  - Subnets:
    - find the subnet IDs for your primary VPC in the **Subnets** menu at https://console.aws.amazon.com/vpc
    - add at least one (might as well add all though) in the `subnet_ids` setting in `config.yaml` - format: `[subnet-1,subnet-2,...]`
  - Security group(s):
    - if none exists yet, make a simple security group for lambda functions in the **Security Groups** menu at https://console.aws.amazon.com/vpc (doesn't need any custom inbound or outbound definitions) and note the Group ID
    - add at least one (usually one is enough) in the `security_group_ids` setting in `config.yaml` - format: `[sg-1,sg-2,...]`
    - make sure to add the assigned security group to the allowed **Inbound** traffic rules of the RDS databases' security group (enter the lambda security group ID under **Source**)
  - redeploy the lambda function (`make deploy`), these settings should now all be listed in the details for the lambda function at https://console.aws.amazon.com/lambda

### API gateway

For the lambda function to be easily accessible via web API (be it http or something else), an API gateway needs to be set up, see [this link for a good example](https://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-lambda-non-proxy-integration.html#getting-started-new-lambda) and the API gateway administration: https://console.aws.amazon.com/apigateway

Notes:
- API gateway configuration is _completely_ independent of the lambda function deployment and must be configured separately (i.e. no setting in the `config.yaml` that plays a role here)
- if you want to make the API secure, good to include an API key requirement which requires creating an API key and setting up a usage plan that allows the API key for a specific API gateway and deployed stage (this stage's method must have the `API Key Required` setting in the `Method->Method Request` set to `true`).
- API development can be very tricky, [POSTMAN](https://www.getpostman.com/) helps with testing APIs:

### Webhooks

Lastly, the particle photons need to actually send data to the API, this is done via [Webhooks](https://docs.particle.io/reference/webhooks/).

- if no `webhook_data_log.json` and `webhook_state_log.json` files exist yet (should have been created during `make init`), run `make webhooks`
- replace the placeholders in both webhook files:
  - `<<ACCESS_POINT>>`, `<<STAGE>>` and `<<RESOURCE>>` are parameters from the API gateway
  - `<<API-KEY>>` is the access key from the API gateway (if configured to need one, see details above)
  - `<<OWNER>>` is the name/id of the owner record in the database (depends on what is set up in your database)
- create the webhooks via `particle webhook create webhook_data_log.json` and `particle webhook create webhook_state_log.json` (need to be logged into the particle account via `particle login`)
- note that creating the webhooks multiple times will NOT throw an error but lead to multiple trigger events, make sure to remove old webhooks via `particle webhook list` and `particle webhook delete` before re-creating updated versions
