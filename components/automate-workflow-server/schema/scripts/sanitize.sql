-- These take up a ton of space, and aren't needed for pgTAP testing
-- currently.
UPDATE phase_runs SET run_log = 'log';
UPDATE phase_runs SET run_status = 'status';

-- This is populated by sqitch, so we can nuke it from this dump
DELETE FROM stage_ordering;

-- Obfuscate SSH keys; our pgtap tests don't currently do anything
-- with SSH keys *as* SSH keys.
UPDATE users
SET ssh_pub_key = 'sshsshsshsshsshsshssh' -- haha
WHERE ssh_pub_key IS NOT NULL;

-- Get rid of passwords and tokens
TRUNCATE user_passwords;
TRUNCATE user_tokens;
