# Lambda function development

## Python installation

- install https://www.anaconda.com/what-is-anaconda/ (check version with `conda -V`, update with `conda update conda` if lower than 4.6)
- install `virtualenv` with `python3 -m pip install --user virtualenv` (https://packaging.python.org/guides/installing-using-pip-and-virtual-environments/) - Note that conda can also manage virtual environments very nicely but it is easier to use `virtualenv` directly to make smaller lambda function environments

## AWS CLI installation

- install AWS command line interface: https://docs.aws.amazon.com/cli/latest/userguide/install-cliv2.html
  - to check: `aws --version`
  - to configure: `aws configure` ([doc](http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-getting-started.html))
  - test: `aws rds describe-db-instances`

## Setup

- run `make clean install`, which does the following:
  - creates virtual environment in the project folder (cleans up first there was anything)
  - activates virtual environment: `source bin/activate`
    - Note: from the make file it does not translate into the shell but if run manually, terminal should show `(folder name)` at the beginning of the console line
  - installs the dependencies for the lambda function defined in `requirements.txt`, here python lambda ([doc](https://github.com/nficano/python-lambda)) and postgresql db drivers ([doc](https://github.com/mfenniak/pg8000))
    - Note: to make successful database connections, make sure to have permissions set up for your IP to connect to the database
- run `make init`, which does the following:
  - bootstraps lambda project if it does not exist yet via `lambda init`
  - makes local copies of the config and webhook template files (automatically git-ignored) if they don't exist yet

## Development

- develop the lambda function in the `service.py` file and connected scripts
- run `make invoke` to run lambda function with the information in some event file `event.json`
  - uses `lambda invoke -v --event_file event.json` in the virtual environment

## Deployment

 - if no `config.yaml` exists yet (should have been created during `make init`), run `make config.yaml`
 - fill out the settings in `config.yaml`, see **Integration** section for details (no worries if not everything correct right away, deploy will throw errors)
 - run `make deploy` to deploy the lambda function:
   - uses `lambda deploy` in the virtual environment
 - Note: for large project dependencies, consider using S3 bucket for the lambda function, see [docs](https://github.com/nficano/python-lambda#uploading-to-s3) for details or a layer
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
  - troubleshooting: if the labmda function times out with a test JSON, most likely the security settings aren't quite right, sometimes the security group and subnets don't get saved properly by the `lambda deploy`, simply add them manually in the console at https://console.aws.amazon.com/lambda for the new lambda function (under the **Network** section)

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

## Alternative (cut?)

[serverless](https://www.serverless.com/dashboard/) provides a potential deployment alternative compatible with multiple providers, not just AWS but it's a bit different setup (and not currently what we use). Below are a few notes on the approach.

### serverless setup

 - install node js and npm (e.g. with `brew install node`, manual installation of https://github.com/nvm-sh/nvm and then `nvm install node`) and then the serverless framework with `npm install -g serverless` (see https://www.serverless.com/framework/docs/providers/aws/guide/quick-start/ for details)
 - create an AWS access key for serverless: https://www.serverless.com/framework/docs/providers/aws/guide/credentials#creating-aws-access-keys
 - set up AWS credentials for serverless with `serverless config credentials --provider aws --key AKIAIOSFODNN7EXAMPLE --secret wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY` (and the correct key and secret for the new user created above)
 - create a serverless labmda function project with: `serverless create \
  --template aws-python3 \
  --name lablogger_lambda \
  --path lablogger_lambda` (example: https://www.serverless.com/blog/serverless-python-packaging)

### python setup

 - go into the `lablogger_lamda` folder (`cd lablogger_lambda`)
 - create an empty virtual environment in this folder with `python3 -m venv env` (note that using conda for this makes later steps more complicated and the lambda function zip bigger than it needs to be)
 - activate the virtual environment with `source venv/bin/activate`
 - install packages as needed with `pip install ...` but keep in mind that lambda functions have a size limit so ideally restrict to only absolutely necessary packages
 - write the python function(s) in a `.py` file
 - create a virtual environment `requirements.txt` file with `pip freeze > requirements.txt`
 - exit the virtual environment with `deactivate` when done

### serverless deployment

 - setup a `serverless.yml` deployment file and `package.json` file for node dependencies as described here: https://www.serverless.com/blog/serverless-python-packaging#deploying-your-service
 - deploy using `serverless deploy` (needs docker installed)
