-- create table upgrade flags to store the last upgrade status
CREATE TABLE IF NOT EXISTS upgrade_flags (
    id                         text PRIMARY KEY,
    upgrade_flag               text NOT NULL UNIQUE,
    upgrade_value              boolean NOT NULL
);

INSERT INTO UPGRADE_FLAGS (id,upgrade_flag,upgrade_value) VALUES (1,'day_latest',true);

INSERT INTO UPGRADE_FLAGS (id,upgrade_flag,upgrade_value) VALUES (2,'control_index',true);
