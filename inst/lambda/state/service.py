# -*- coding: utf-8 -*-

import logging

# logger
logging.basicConfig()
logger = logging.getLogger()
logger.setLevel(logging.INFO)

# handler
def handler(event, context):
    # Your code goes here!
    logger.info(event.get("type"));
    logger.info(event['data']);
    logger.info(event['device_id']);    
    return("Success!")
