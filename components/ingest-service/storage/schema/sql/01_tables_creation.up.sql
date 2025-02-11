CREATE TABLE reindex_requests (
    request_id INT PRIMARY KEY,
    status VARCHAR(50) CHECK (status IN ('running', 'failed', 'completed')),
    created_at TIMESTAMP,
    last_updated TIMESTAMP
);
 
CREATE TABLE reindex_request_detailed (
    id INT PRIMARY KEY,
    request_id INT,
    index TEXT NOT NULL,
    from_version TEXT NOT NULL,
    to_version TEXT NOT NULL,
    stage VARCHAR(50) CHECK (stage IN ('running', 'failed', 'completed')),
    os_task_id TEXT,
    heartbeat TIMESTAMP,
    having_alias BOOLEAN,
    alias_list TEXT,
    created_at TIMESTAMP,
    updated_at TIMESTAMP,
    FOREIGN KEY (request_id) REFERENCES reindex_requests(request_id)
);
