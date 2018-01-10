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
def get_parameter(event, param_name):
    param = event.get(param_name)
    if param is None:
        logger.error("no '{}' provided in event {}".format(param_name, event))
        sys.exit()
    return(param)

# log environment
def log_event(event):
    # event raw log
    cur = conn.cursor()
    cur.execute("INSERT INTO device_raw_logs (created_datetime, raw_data) VALUES (current_timestamp, %s) RETURNING device_raw_log_id",  ('{}'.format(event),));
    device_raw_log_id = cur.fetchone()[0]
    conn.commit()
    return(device_raw_log_id)

# check device, return device_in_use
def check_device(device_particle_id):
    # get in use
    cur = conn.cursor()
    cur.execute("SELECT device_id, in_use FROM devices WHERE device_particle_id = (%s)", (device_particle_id,))
    device = cur.fetchone()

    # check if need to create new device entry
    undefined_device_type_id = "undefined"
    if device is None:
        logger.info("creating new devices entry")
        cur.execute("INSERT INTO devices (device_particle_id, device_type_id, in_use) VALUES (%s, %s, %s) RETURNING device_id, in_use",
            (device_particle_id, undefined_device_type_id, True, ))
        device = cur.fetchone()
        conn.commit()

    return(device)

# handler
def handler(event, context):
    # event log
    device_raw_log_id = log_event(event)

    # safety checks
    device_particle_id = get_parameter(event, 'device_id')
    published_at = get_parameter(event, 'published_at')
    payload = get_parameter(event, 'payload')
    logger.info("processing data for device_particle_id '{}' (device_raw_log_id={}): {}".format(device_particle_id, device_raw_log_id, event));

    # check device
    [device_id, device_in_use] = check_device(device_particle_id)

    # proceed according to device in use
    if device_in_use == True:
        device_state_log_ids = []
        cur = conn.cursor()
        for data in payload.get('data'):
            cur.execute("INSERT INTO device_state_logs (device_raw_log_id, device_id, log_datetime, log_type, log_message, state_key, state_value, state_units, notes) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s) RETURNING device_state_log_id",
                (device_raw_log_id, device_id, published_at, payload.get('type'), payload.get('msg'), data.get('k'), data.get('v'), data.get('u'), payload.get('notes')))
            device_state_log_ids = device_state_log_ids + cur.fetchone()
        conn.commit()
        logger.info("device in use, created {} log entries (IDs: {})".format(len(device_state_log_ids), ', '.join(map(str, device_state_log_ids))))
        return("Device in use ({} log entries created).".format(len(device_state_log_ids)))
    else:
        logger.info("device not in use, discarding log entries")
        return("Device not in use.")
