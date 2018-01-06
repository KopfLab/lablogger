# -*- coding: utf-8 -*-

import os
import sys
import logging
import pg8000

# logger
logging.basicConfig()
logger = logging.getLogger()
logger.setLevel(logging.INFO)

# db connection
try:
    conn = pg8000.connect(user=os.environ['db_user'], host=os.environ['db_host'], database=os.environ['db_name'], password=os.environ['db_pwd'])
except:
    logger.error("unexpected error - could not connect to database.")
    sys.exit()
logger.info("connection to database succeeded")

# parameter checks
def get_parameter(event, param):
    param = event.get(param)
    if param is None:
        logger.error("no {} provided in event {}".format(param, event))
        sys.exit()
    return(param)


# handler
def handler(event, context):
    # safety checks
    device_id = get_parameter(event, 'device_id')
    published_at = get_parameter(event, 'published_at')
    data = get_parameter(event, 'data')
    logger.info("processing event for device_id '{}': {}".format(device_id, event));

    # get in use
    cur = conn.cursor()
    cur.execute("SELECT in_use FROM devices WHERE device_id = (%s)", (device_id,))
    device_in_use = cur.fetchone()

    # check if need to create new device entry
    undefined_device_type_id = "undefined"
    if device_in_use is None:
        logger.info("creating new devices entry")
        cur.execute("INSERT INTO devices (device_id, device_type_id, in_use) VALUES (%s, %s, %s) RETURNING in_use",  (device_id, undefined_device_type_id, True))
        device_in_use = cur.fetchone()
        conn.commit()

    # proceed according to device in use
    if device_in_use == True:
        device_state_log_ids = []
        for entry in data.get('data'):
            cur.execute("INSERT INTO device_state_logs (device_id, log_datetime, log_type, log_message, state_key, state_value, state_units, notes) VALUES (%s, %s, %s, %s, %s, %s, %s, %s) RETURNING device_state_log_id",
                (device_id, published_at, data.get('type'), data.get('msg'), entry.get('k'), entry.get('v'), entry.get('u'), data.get('notes')))
            device_state_log_ids = device_state_log_ids + cur.fetchone()
        conn.commit()
        logger.info("device in use, created {} log entries (IDs: {})".format(len(device_state_log_ids), ', '.join(map(str, device_state_log_ids))))
        return("Device in use ({} log entries created).".format(len(device_state_log_ids)))
    else:
        logger.info("device not in use, discarding log entries")
        return("Device not in use.")
