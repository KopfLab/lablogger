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

# create raw log
def log_event(event):
    # event raw log
    cur = conn.cursor()
    cur.execute("INSERT INTO device_raw_logs (created_datetime, raw_data) VALUES (current_timestamp, %s) RETURNING device_raw_log_id",  ('{}'.format(event),));
    device_raw_log_id = cur.fetchone()[0]
    conn.commit()
    return(device_raw_log_id)

# handler
def handler(event, context):
    # event log
    device_raw_log_id = log_event(event)

    # safety checks
    group_id = get_parameter(event, 'group_id')
    particle_id = get_parameter(event, 'particle_id')
    published_at = get_parameter(event, 'published_at')
    payload = get_parameter(event, 'payload')
    device_name = get_parameter(payload, 'id')
    log_type = get_parameter(event, 'log')

    logger.info("processing '{}' log for device '{}' (group_id={}, particle_id={}, device_raw_log_id={}): {}".format(
        log_type, device_name, group_id, particle_id, device_raw_log_id, event))

    # valid log_type?
    if log_type != "state" and log_type != "data":
        logger.info("invalid log type");
        return("Log type not supported.");

    # get device
    cur = conn.cursor()
    cur.execute("SELECT device_id, particle_id, in_use FROM devices WHERE device_name = (%s) AND group_id = (%s)", (device_name, group_id,))
    device = cur.fetchone()

    # no device
    if device is None:
        logger.info("device/group pair not listed in database")
        return("Device does not exist for group.")

    [device_id, known_particle_id, in_use] = device
    # device in use?
    if in_use != True:
        logger.info("device not in use, discarding log entries")
        return("Device not in use.")

    # update particle_id?
    if particle_id != known_particle_id:
        logger.info("updating device particle_id from {} to {}".format(known_particle_id, particle_id))
        cur.execute("UPDATE devices SET particle_id = %s WHERE device_id = %s",
            (particle_id, device_id, ))
        conn.commit()

    # process state logs
    if log_type == "state":
        device_state_log_ids = []
        cur = conn.cursor()
        for state in payload.get('s'):
            cur.execute("INSERT INTO device_state_logs (device_raw_log_id, device_id, log_datetime, log_type, log_message, state_key, state_value, state_units, notes) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s) RETURNING device_state_log_id",
                (device_raw_log_id, device_id, published_at, payload.get('t'), payload.get('m'), state.get('k'), state.get('v'), state.get('u'), payload.get('n')))
            device_state_log_ids = device_state_log_ids + cur.fetchone()
        conn.commit()
        logger.info("device in use, created {} log entries (IDs: {})".format(len(device_state_log_ids), ', '.join(map(str, device_state_log_ids))))
        return("Device in use ({} state log entries created).".format(len(device_state_log_ids)))
    elif log_type == "data":
        return("FIXME: data log")
