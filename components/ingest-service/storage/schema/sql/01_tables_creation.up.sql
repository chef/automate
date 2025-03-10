CREATE TABLE IF NOT EXISTS reindex_requests (
    request_id INT PRIMARY KEY,
    status VARCHAR(50) CHECK (status IN ('running', 'failed', 'completed')),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS reindex_request_detailed (
    id INT PRIMARY KEY,
    request_id INT NOT NULL,
    index TEXT NOT NULL,
    from_version TEXT NOT NULL,
    to_version TEXT NOT NULL,
    stage JSONB NOT NULL,
    os_task_id TEXT,
    heartbeat TIMESTAMP,
    having_alias BOOLEAN,
    alias_list TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (request_id) REFERENCES reindex_requests(request_id) ON DELETE CASCADE
);

