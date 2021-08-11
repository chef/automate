CREATE TABLE blacklisted_id_tokens (
    token TEXT
);
CREATE INDEX blacklist_session_idx ON blacklisted_id_tokens (token);