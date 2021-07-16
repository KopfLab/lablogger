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
def get_parameter(event, param_name, require = True):
    param = event.get(param_name)
    if require and param is None:
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
    date_time = get_parameter(payload, 'dt', False)
    if date_time is None:
        date_time = published_at

    logger.info("processing '{}' log triggered at '{}' for device '{}' (group_id={}, particle_id={}, device_raw_log_id={}): {}".format(
        log_type, date_time, device_name, group_id, particle_id, device_raw_log_id, event))

    # valid log_type?
    if log_type != "state" and log_type != "data":
        logger.info("invalid log type");
        return("Log type not supported.");

    # session
    cur = conn.cursor()

    # get device
    cur.execute("SELECT device_id, particle_id, in_use FROM devices WHERE device_name = (%s) AND group_id = (%s)", (device_name, group_id,))
    device = cur.fetchone()

    # no device
    if device is None:
        logger.info("device/group pair not listed in base")
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
        for state in payload.get('s'):
            cur.execute("INSERT INTO device_state_logs (device_raw_log_id, device_id, log_datetime, log_type, log_message, state_key, state_value, state_units, notes) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s) RETURNING device_state_log_id",
                (device_raw_log_id, device_id, date_time, payload.get('t'), payload.get('m'), state.get('k'), state.get('v'), state.get('u'), payload.get('n')))
            device_state_log_ids = device_state_log_ids + cur.fetchone()
        conn.commit()
        logger.info("device in use, created {} log entries (IDs: {})".format(len(device_state_log_ids), ', '.join(map(str, device_state_log_ids))))
        return("Device in use ({} state log entries with date time '{}' created).".format(len(device_state_log_ids), date_time))

    # process data logs
    elif log_type == "data":

        # get device data logs
        cur.execute("SELECT data_idx, exp_device_data_id, experiments.exp_id FROM experiments, experiment_device_data WHERE experiments.exp_id = experiment_device_data.exp_id AND device_id = (%s) AND group_id = (%s) AND recording = true AND active = true",
            (device_id, group_id,))
        exp_device_idxs = cur.fetchall()

        # loop through data and create log entries as needed
        device_data_log_ids = []
        exp_ids = []
        global_to = payload.get('to')
        for data in payload.get('d'):
            if(not(data.get('v') is None)):
                for exp_device_idx in exp_device_idxs:
                    if exp_device_idx[0] == data.get('i'):
                        exp_ids.append(exp_device_idx[2])
                        # figure out global or local time offset
                        local_to = data.get('to')
                        if (local_to is None):
                            local_to = global_to
                        # generate a record for the exp_device_data_id
                        cur.execute("INSERT INTO device_data_logs (device_raw_log_id, device_id, exp_device_data_id, log_datetime, log_time_offset, data_idx, data_key, data_value, data_sd, data_units, data_n) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s) RETURNING device_data_log_id",
                                    (device_raw_log_id, device_id, exp_device_idx[1], date_time, local_to/1000., data.get('i'), data.get('k'), data.get('v'), data.get('s'), data.get('u'), data.get('n')))
                        device_data_log_ids.append(cur.fetchone())
        conn.commit()
        exp_ids = list(set(exp_ids))
        logger.info("device in use, created {} log entries (IDs: {}) for {} experiments (IDs: {})".format(
            len(device_data_log_ids), ', '.join(map(str, device_data_log_ids)), len(exp_ids), ', '.join(map(str, exp_ids))))
        if (len(device_data_log_ids) > 0):
            return("Device in use ({} log entries with date time '{}' created for {} experiments).".format(len(device_data_log_ids), date_time, len(exp_ids)))
        else:
            return("Device in use but no log entries created (no active experiments).")
