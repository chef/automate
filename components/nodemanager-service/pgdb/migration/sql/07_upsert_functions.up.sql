CREATE OR REPLACE FUNCTION upsert_by_source_id_run_data(_id text, _name text, _platform text, _platform_version text, _source_state text,
		_last_contact timestamp, _source_id text, _source_region text, _source_account_id text, _last_run json, _projects_data json, _manager text)
RETURNS TEXT
LANGUAGE plpgsql
AS $$
BEGIN
    UPDATE nodes SET name = name, platform = _platform, platform_version = _platform_version, source_state = _source_state,
        last_contact = _last_contact,  source_id = _source_id, source_region = _source_region, source_account_id = _source_account_id, last_run = _last_run, projects_data = _projects_data
        WHERE id = _id;
    IF found THEN
        RETURN _id;
    END IF;

    BEGIN
        INSERT INTO nodes
            (id, name, platform, platform_version, source_state,
                last_contact, source_id, source_region, source_account_id, last_run, projects_data, manager)
        VALUES (_id, _name, _platform, _platform_version, _source_state, _last_contact, _source_id, _source_region, _source_account_id, _last_run, _projects_data, _manager)
        ON CONFLICT (source_id, source_region, source_account_id)
        DO UPDATE
            SET name = _name, platform = _platform, platform_version = _platform_version, source_state = _source_state,
            last_contact = _last_contact,  source_id = _source_id, source_region = _source_region, source_account_id = _source_account_id, last_run = _last_run, projects_data = _projects_data
            WHERE nodes.source_state != 'TERMINATED';
            RETURN _id;
    END;
END;
$$;

CREATE OR REPLACE FUNCTION upsert_by_source_id_scan_data(_id text, _name text, _platform text, _platform_version text, _source_state text,
		_last_contact timestamp, _source_id text, _source_region text, _source_account_id text, _last_job text, _last_scan json, _projects_data json, _manager text)
RETURNS TEXT
LANGUAGE plpgsql
AS $$
BEGIN
    UPDATE nodes SET name = name, platform = _platform, platform_version = _platform_version, source_state = _source_state,
        last_contact = _last_contact,  source_id = _source_id, source_region = _source_region, source_account_id = _source_account_id, 
        last_job = _last_job, last_scan = _last_scan, projects_data = _projects_data
        WHERE id = _id;
    IF found THEN
        RETURN _id;
    END IF;

    BEGIN
        INSERT INTO nodes
            (id, name, platform, platform_version, source_state,
                last_contact, source_id, source_region, source_account_id, last_job, last_scan, projects_data, manager)
        VALUES (_id, _name, _platform, _platform_version, _source_state, _last_contact, _source_id, _source_region, _source_account_id, _last_job, _last_scan, _projects_data, _manager)
        ON CONFLICT (source_id, source_region, source_account_id)
        DO UPDATE
            SET name = _name, platform = _platform, platform_version = _platform_version, source_state = _source_state,
                last_contact = _last_contact,  source_id = _source_id, source_region = _source_region, 
                source_account_id = _source_account_id, last_job = _last_job, last_scan = _last_scan, projects_data = _projects_data
            WHERE nodes.source_state != 'TERMINATED';
            RETURN _id;
    END;
END;
$$;
