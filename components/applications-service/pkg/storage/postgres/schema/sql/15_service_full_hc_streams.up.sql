ALTER TABLE service_full DROP COLUMN health_check_message;

ALTER TABLE service_full ADD COLUMN health_check_stdout TEXT NOT NULL DEFAULT '';
ALTER TABLE service_full ADD COLUMN health_check_stderr TEXT NOT NULL DEFAULT '';
-- Windows exit codes are 32-bit, as is the exit_status field in the hab event protobuf.
ALTER TABLE service_full ADD COLUMN health_check_exit_status integer NOT NULL DEFAULT 0;
