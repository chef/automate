CREATE TABLE blacklisted_id_tokens (
    token TEXT,
    inserted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
CREATE INDEX blacklist_session_idx ON blacklisted_id_tokens (token);