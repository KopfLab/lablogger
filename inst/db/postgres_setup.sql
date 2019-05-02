# Data base setup

-- Database: chemostat-db1
-- Note: all timestamps must be WITH timezone, otherwise R has trouble

-- DROP DATABASE "chemostat-db1";

CREATE DATABASE "chemostat-db1"
    WITH
    OWNER = root
    ENCODING = 'UTF8'
    LC_COLLATE = 'en_US.UTF-8'
    LC_CTYPE = 'en_US.UTF-8'
    TABLESPACE = pg_default
    CONNECTION LIMIT = -1;

-- Table: public.camera

DROP TABLE IF EXISTS cameras;

CREATE TABLE cameras
(
    name character varying(30) PRIMARY KEY,
    label character varying(100) NULL,
    address character varying(20) NOT NULL,
    active boolean DEFAULT true
);

INSERT INTO cameras(name, label, address, active)
	VALUES ('Cam #1', 'Test Camera', '128.138.152.159:8081', true);

SELECT * FROM cameras;

-- Table: public.settings

DROP TABLE IF EXISTS settings;

CREATE TABLE settings
(
    setting_id character varying(50) PRIMARY KEY,
    setting_value text
);

INSERT INTO settings(setting_id, setting_value)
  VALUES('particle_access_token', '');

SELECT * FROM settings;

-- Table: device_types

DROP TABLE IF EXISTS device_types;

CREATE TABLE device_types
(
    device_type_id character varying(20) PRIMARY KEY,
    device_type_desc character varying(255) NULL
);

INSERT INTO device_types(device_type_id, device_type_desc)
	VALUES ('undefined', 'undefined device types'),
         ('M800', 'Mettler Toledo multi-channel amplifier'),
         ('scale', 'Scale'),
         ('mfc', 'Mass Flow Controller'),
         ('pump', 'Peristaltic Pump');

-- Table: groups

DROP TABLE IF EXISTS groups;

CREATE TABLE groups
(
    group_id character varying(20) PRIMARY KEY,
    group_desc character varying(255) NULL
);

INSERT INTO groups(group_id, group_desc)
	VALUES ('testing', 'testing account');

-- Table: devices

DROP TABLE IF EXISTS devices CASCADE;

CREATE TABLE devices
(
    device_id SERIAL PRIMARY KEY,
    device_name character varying(20) NOT NULL,
    particle_id character varying(50) NULL,
    group_id character varying(20) NOT NULL references groups(group_id),
    device_type_id character varying(20) NOT NULL references device_types(device_type_id),
    device_desc character varying(255) NULL,
    in_use boolean NOT NULL DEFAULT true,
    unique (group_id, device_name)
);

INSERT INTO devices(device_name, group_id, device_type_id, device_desc)
	VALUES ('testing', 'testing', 'undefined', 'testing device');

-- Table: experiments

DROP TABLE IF EXISTS experiments;

CREATE TABLE experiments
(
  exp_id character varying(20) PRIMARY KEY,
  group_id character varying(20) NOT NULL references groups(group_id),
  exp_desc character varying(255) NULL,
  recording boolean NOT NULL DEFAULT false,
  last_recording_change timestamp with time zone default NOW()
);

INSERT INTO experiments(exp_id, group_id, exp_desc, recording)
  VALUES('TEST', 'testing', 'Test Exp', true);
INSERT INTO experiments(exp_id, group_id, exp_desc, recording)
  VALUES('TEST2', 'testing', 'Test Exp', true);
INSERT INTO experiments(exp_id, group_id, exp_desc, recording)
  VALUES('TEST3', 'testing', 'Inactive Test Exp', false);

-- Table: experiment_device_data

DROP TABLE IF EXISTS experiment_device_data;

CREATE TABLE experiment_device_data (
  exp_device_data_id SERIAL PRIMARY KEY,
  exp_id character varying(20) NOT NULL references experiments(exp_id) ON UPDATE CASCADE,
  -- only used for mapping during log processing
  device_id integer NOT NULL references devices(device_id),
  data_idx integer NOT NULL,
  -- additional information for data
  data_group character varying (50) NULL,
  -- whether this is an active experiment device
  active boolean NOT NULL DEFAULT True,
  -- constraint
  unique (exp_id, device_id, data_idx)
);

INSERT INTO experiment_device_data(exp_id, device_id, data_idx)
  SELECT 'TEST', d.device_id, 1 FROM devices d WHERE d.device_name = 'testing' AND d.group_id = 'testing';
INSERT INTO experiment_device_data(exp_id, device_id, data_idx)
  SELECT 'TEST', d.device_id, 2 FROM devices d WHERE d.device_name = 'testing' AND d.group_id = 'testing';
INSERT INTO experiment_device_data(exp_id, device_id, data_idx)
  SELECT 'TEST2', d.device_id, 1 FROM devices d WHERE d.device_name = 'testing' AND d.group_id = 'testing';
INSERT INTO experiment_device_data(exp_id, device_id, data_idx)
  SELECT 'TEST3', d.device_id, 1 FROM devices d WHERE d.device_name = 'testing' AND d.group_id = 'testing';


-- Table: device_raw_logs

DROP TABLE IF EXISTS device_raw_logs CASCADE;

CREATE TABLE device_raw_logs(
  device_raw_log_id SERIAL PRIMARY KEY,
  created_datetime timestamp with time zone NOT NULL,
  raw_data text NULL
);

-- Table: device_state_logs

DROP TABLE IF EXISTS device_state_logs;

CREATE TABLE device_state_logs (
  device_state_log_id SERIAL PRIMARY KEY,
  device_raw_log_id integer NOT NULL references device_raw_logs(device_raw_log_id),
  device_id integer NOT NULL references devices(device_id),
  log_datetime timestamp with time zone,
  log_type character varying(50) NOT NULL,
  log_message character varying(255) NULL,
  state_key character varying(50) NULL,
  state_value character varying(100) NULL,
  state_units character varying(20) NULL,
  notes character varying(255) NULL
);

-- Table: device_data_logs

DROP TABLE IF EXISTS device_data_logs;

CREATE TABLE device_data_logs (
  device_data_log_id SERIAL PRIMARY KEY,
  device_raw_log_id integer NOT NULL references device_raw_logs(device_raw_log_id),
  device_id integer NOT NULL references devices(device_id),
  exp_device_data_id integer NOT NULL references experiment_device_data(exp_device_data_id),
  log_datetime timestamp with time zone,
  log_time_offset real default 0.0,
  data_idx integer NOT NULL,
  data_key character varying(50) NOT NULL,
  data_value double precision NULL,
  data_sd double precision NULL,
  data_units character varying(20) NULL,
  data_n integer NULL
);
