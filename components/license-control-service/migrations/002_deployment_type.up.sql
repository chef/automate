BEGIN;

-- create table deployment_type
CREATE TABLE IF NOT EXISTS deployment_type (
    id  INTEGER PRIMARY KEY,
    type TEXT NOT NULL
);

INSERT INTO deployment_type(id, type) values (0, 'Standalone');
INSERT INTO deployment_type(id, type) values (1, 'HA');
INSERT INTO deployment_type(id, type) values (2, 'SAAS');

ALTER TABLE IF EXISTS deployment
    ADD COLUMN IF NOT EXISTS type_id INTEGER DEFAULT (0)
        CONSTRAINT FK_DEPLOYMENT_TYPE REFERENCES deployment_type(id);

COMMIT;