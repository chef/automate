--
-- PostgreSQL database cluster dump
--

SET default_transaction_read_only = off;

SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;

--
-- Drop databases (except postgres and template1)
--

DROP DATABASE chef_applications_service;
DROP DATABASE chef_authn_service;
DROP DATABASE chef_authz_service;
DROP DATABASE chef_cereal_service;
DROP DATABASE chef_compliance_service;
DROP DATABASE chef_config_mgmt_service;
DROP DATABASE chef_infra_proxy;
DROP DATABASE chef_ingest_service;
DROP DATABASE chef_license_control_service;
DROP DATABASE chef_session_service;
DROP DATABASE chef_teams_service;
DROP DATABASE chef_user_settings_service;
DROP DATABASE data_feed_service;
DROP DATABASE dex;
DROP DATABASE nodemanager_service;
DROP DATABASE notifications_service;
DROP DATABASE secrets_service;




--
-- Drop roles
--

DROP ROLE postgres;


--
-- Roles
--

CREATE ROLE postgres;
ALTER ROLE postgres WITH SUPERUSER INHERIT CREATEROLE CREATEDB LOGIN REPLICATION BYPASSRLS PASSWORD 'md53175bce1d3201d16594cebf9d7eb3f9d';






--
-- Databases
--

--
-- Database "template1" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

UPDATE pg_catalog.pg_database SET datistemplate = false WHERE datname = 'template1';
DROP DATABASE template1;
--
-- Name: template1; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE template1 WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE template1 OWNER TO postgres;

\connect template1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: DATABASE template1; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON DATABASE template1 IS 'default template for new databases';


--
-- Name: template1; Type: DATABASE PROPERTIES; Schema: -; Owner: postgres
--

ALTER DATABASE template1 IS_TEMPLATE = true;


\connect template1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: DATABASE template1; Type: ACL; Schema: -; Owner: postgres
--

REVOKE CONNECT,TEMPORARY ON DATABASE template1 FROM PUBLIC;
GRANT CONNECT ON DATABASE template1 TO PUBLIC;


--
-- PostgreSQL database dump complete
--

--
-- Database "chef_applications_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: chef_applications_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE chef_applications_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE chef_applications_service OWNER TO postgres;

\connect chef_applications_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: save_previous_health_on_state_change(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.save_previous_health_on_state_change() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF NEW.health != OLD.health THEN
    NEW.previous_health := OLD.health;
    NEW.health_updated_at := NOW();
    RETURN NEW;
  END IF;
  RETURN NEW;
END;
$$;


ALTER FUNCTION public.save_previous_health_on_state_change() OWNER TO postgres;

--
-- Name: update_timestamp_updated_at_column(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.update_timestamp_updated_at_column() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  NEW.updated_at = now();
  RETURN NEW;
END;
$$;


ALTER FUNCTION public.update_timestamp_updated_at_column() OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: service_full; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.service_full (
    id bigint NOT NULL,
    origin text NOT NULL,
    name text NOT NULL,
    version text NOT NULL,
    release text NOT NULL,
    package_ident text NOT NULL,
    health text NOT NULL,
    channel text NOT NULL,
    update_strategy text NOT NULL,
    supervisor_id text NOT NULL,
    fqdn text DEFAULT ''::text NOT NULL,
    site text DEFAULT ''::text NOT NULL,
    service_group_name text NOT NULL,
    service_group_name_suffix text NOT NULL,
    application text DEFAULT ''::text NOT NULL,
    environment text DEFAULT ''::text NOT NULL,
    previous_health text DEFAULT 'NONE'::text NOT NULL,
    health_updated_at timestamp without time zone DEFAULT now() NOT NULL,
    last_event_occurred_at timestamp without time zone DEFAULT now() NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_at timestamp without time zone DEFAULT now() NOT NULL,
    service_group_id text,
    disconnected boolean DEFAULT false NOT NULL,
    health_check_stdout text DEFAULT ''::text NOT NULL,
    health_check_stderr text DEFAULT ''::text NOT NULL,
    health_check_exit_status integer DEFAULT 0 NOT NULL
);


ALTER TABLE public.service_full OWNER TO postgres;

--
-- Name: service_full_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.service_full_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.service_full_id_seq OWNER TO postgres;

--
-- Name: service_full_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.service_full_id_seq OWNED BY public.service_full.id;


--
-- Name: telemetry; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.telemetry (
    id text NOT NULL,
    last_telemetry_reported_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone NOT NULL
);


ALTER TABLE public.telemetry OWNER TO postgres;

--
-- Name: service_full id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.service_full ALTER COLUMN id SET DEFAULT nextval('public.service_full_id_seq'::regclass);


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version, dirty) FROM stdin;
18	f
\.


--
-- Data for Name: service_full; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.service_full (id, origin, name, version, release, package_ident, health, channel, update_strategy, supervisor_id, fqdn, site, service_group_name, service_group_name_suffix, application, environment, previous_health, health_updated_at, last_event_occurred_at, created_at, updated_at, service_group_id, disconnected, health_check_stdout, health_check_stderr, health_check_exit_status) FROM stdin;
\.


--
-- Data for Name: telemetry; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.telemetry (id, last_telemetry_reported_at, created_at) FROM stdin;
dcead35d-ebf4-495c-9420-a5cc7999c8ff	2022-06-03 10:04:17.11+00	2022-06-03 10:04:17.54184+00
\.


--
-- Name: service_full_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.service_full_id_seq', 1, false);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: service_full service_full_name_supervisor_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.service_full
    ADD CONSTRAINT service_full_name_supervisor_id_key UNIQUE (name, supervisor_id);


--
-- Name: service_full service_full_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.service_full
    ADD CONSTRAINT service_full_pkey PRIMARY KEY (id);


--
-- Name: telemetry telemetry_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.telemetry
    ADD CONSTRAINT telemetry_pkey PRIMARY KEY (id);


--
-- Name: service_full_application_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX service_full_application_idx ON public.service_full USING btree (application);


--
-- Name: service_full_channel_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX service_full_channel_idx ON public.service_full USING btree (channel);


--
-- Name: service_full_disconnected_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX service_full_disconnected_idx ON public.service_full USING btree (disconnected);


--
-- Name: service_full_environment_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX service_full_environment_idx ON public.service_full USING btree (environment);


--
-- Name: service_full_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX service_full_name_idx ON public.service_full USING btree (name);


--
-- Name: service_full_origin_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX service_full_origin_idx ON public.service_full USING btree (origin);


--
-- Name: service_full_release_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX service_full_release_idx ON public.service_full USING btree (release);


--
-- Name: service_full_service_group_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX service_full_service_group_id ON public.service_full USING btree (service_group_id);


--
-- Name: service_full_service_group_name_suffix_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX service_full_service_group_name_suffix_idx ON public.service_full USING btree (service_group_name_suffix);


--
-- Name: service_full_site_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX service_full_site_idx ON public.service_full USING btree (site);


--
-- Name: service_full_version_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX service_full_version_idx ON public.service_full USING btree (version);


--
-- Name: service_full service_full_save_previous_health_on_state_change; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER service_full_save_previous_health_on_state_change BEFORE UPDATE ON public.service_full FOR EACH ROW EXECUTE FUNCTION public.save_previous_health_on_state_change();


--
-- Name: service_full update_service_full_updated_at; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER update_service_full_updated_at BEFORE UPDATE ON public.service_full FOR EACH ROW EXECUTE FUNCTION public.update_timestamp_updated_at_column();


--
-- PostgreSQL database dump complete
--

--
-- Database "chef_authn_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: chef_authn_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE chef_authn_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE chef_authn_service OWNER TO postgres;

\connect chef_authn_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: move_if_exists(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.move_if_exists() RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF EXISTS(SELECT * FROM information_schema.tables
              WHERE table_schema = current_schema()
              AND table_name = 'migrations') THEN
     INSERT INTO schema_migrations (
       SELECT MAX(num) AS version, FALSE AS dirty FROM migrations
     );
  END IF;
END;
$$;


ALTER FUNCTION public.move_if_exists() OWNER TO postgres;

--
-- Name: projects_match(text[], text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.projects_match(_token_projects text[], _projects_filter text[]) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
     BEGIN
       RETURN (
         -- no projects filter requested (length 0) will be the case for v1.0 or v2.1 ["*"]
         array_length(_projects_filter, 1) IS NULL
         -- projects filter intersects with projects for row
         OR _token_projects && _projects_filter
         -- projects for row is an empty array, check if (unassigned) in project filter
         OR (array_length(_token_projects, 1) IS NULL AND '{(unassigned)}' && _projects_filter)
       );
     END
$$;


ALTER FUNCTION public.projects_match(_token_projects text[], _projects_filter text[]) OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: chef_authn_tokens; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.chef_authn_tokens (
    id text NOT NULL,
    value text NOT NULL,
    created timestamp with time zone,
    updated timestamp with time zone,
    active boolean DEFAULT true NOT NULL,
    name text NOT NULL,
    project_ids text[] DEFAULT '{}'::text[] NOT NULL,
    db_id integer NOT NULL,
    CONSTRAINT chef_authn_clients_description_check CHECK ((name <> ''::text))
);


ALTER TABLE public.chef_authn_tokens OWNER TO postgres;

--
-- Name: chef_authn_tokens_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.chef_authn_tokens_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.chef_authn_tokens_db_id_seq OWNER TO postgres;

--
-- Name: chef_authn_tokens_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.chef_authn_tokens_db_id_seq OWNED BY public.chef_authn_tokens.db_id;


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: chef_authn_tokens db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.chef_authn_tokens ALTER COLUMN db_id SET DEFAULT nextval('public.chef_authn_tokens_db_id_seq'::regclass);


--
-- Data for Name: chef_authn_tokens; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.chef_authn_tokens (id, value, created, updated, active, name, project_ids, db_id) FROM stdin;
\.


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version, dirty) FROM stdin;
15	f
\.


--
-- Name: chef_authn_tokens_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.chef_authn_tokens_db_id_seq', 1, false);


--
-- Name: chef_authn_tokens chef_authn_tokens_id_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.chef_authn_tokens
    ADD CONSTRAINT chef_authn_tokens_id_unique UNIQUE (id);


--
-- Name: chef_authn_tokens chef_authn_tokens_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.chef_authn_tokens
    ADD CONSTRAINT chef_authn_tokens_pkey PRIMARY KEY (db_id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- PostgreSQL database dump complete
--

--
-- Database "chef_authz_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: chef_authz_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE chef_authz_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE chef_authz_service OWNER TO postgres;

\connect chef_authz_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: iam_effect; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.iam_effect AS ENUM (
    'allow',
    'deny'
);


ALTER TYPE public.iam_effect OWNER TO postgres;

--
-- Name: iam_policy_type; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.iam_policy_type AS ENUM (
    'custom',
    'chef-managed'
);


ALTER TYPE public.iam_policy_type OWNER TO postgres;

--
-- Name: migration_state; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.migration_state AS ENUM (
    'init',
    'in-progress',
    'successful',
    'failed',
    'successful-beta1'
);


ALTER TYPE public.migration_state OWNER TO postgres;

--
-- Name: cannot_delete_policy_error(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cannot_delete_policy_error() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    RAISE EXCEPTION 'You cannot delete policy with id % as it is not marked as deletable.', OLD.id USING
        ERRCODE='DRPPC';
END$$;


ALTER FUNCTION public.cannot_delete_policy_error() OWNER TO postgres;

--
-- Name: fn_deleted_rule_check(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.fn_deleted_rule_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN 
    RAISE EXCEPTION 'rule with id % has been deleted', NEW.id USING
        ERRCODE='RDLTD'; 
END$$;


ALTER FUNCTION public.fn_deleted_rule_check() OWNER TO postgres;

--
-- Name: insert_iam_policy(text, text, public.iam_policy_type); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.insert_iam_policy(_id text, _name text, _type public.iam_policy_type) RETURNS void
    LANGUAGE sql
    AS $$

    INSERT INTO iam_policies (id, name, type)
      VALUES (_id, _name, _type);

$$;


ALTER FUNCTION public.insert_iam_policy(_id text, _name text, _type public.iam_policy_type) OWNER TO postgres;

--
-- Name: insert_iam_stanza_into_policy(uuid, uuid, public.iam_effect, text[], text[], text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.insert_iam_stanza_into_policy(_policy_id uuid, _stanza_id uuid, _stanza_effect public.iam_effect, _stanza_actions text[], _stanza_resources text[], _stanza_role text, _stanza_scope text) RETURNS void
    LANGUAGE sql
    AS $$

    INSERT INTO iam_policy_stanzas (policy_id, stanza_id)
      VALUES (_policy_id, _stanza_id);

    INSERT INTO iam_stanzas (id, effect, actions, resources, role, scope)
      VALUES (_stanza_id, _stanza_effect, _stanza_actions, _stanza_resources, _stanza_role, _stanza_scope);

$$;


ALTER FUNCTION public.insert_iam_stanza_into_policy(_policy_id uuid, _stanza_id uuid, _stanza_effect public.iam_effect, _stanza_actions text[], _stanza_resources text[], _stanza_role text, _stanza_scope text) OWNER TO postgres;

--
-- Name: insert_iam_statement_into_policy(text, public.iam_effect, text[], text[], text, text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.insert_iam_statement_into_policy(_policy_id text, _statement_effect public.iam_effect, _statement_actions text[], _statement_resources text[], _statement_role text, _statement_projects text[]) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
  statement_db_id INTEGER;
BEGIN
    -- if NULL or an empty string was passed for the role, we shouldn't try to insert a role.
    IF _statement_role IS NULL OR _statement_role=''
    THEN
        INSERT INTO iam_statements (policy_id, effect, actions, resources)
        VALUES (policy_db_id(_policy_id), _statement_effect, _statement_actions, _statement_resources)
        RETURNING db_id INTO statement_db_id;
    ELSE
        INSERT INTO iam_statements (policy_id, effect, actions, resources, role_id)
        VALUES (policy_db_id(_policy_id), _statement_effect, _statement_actions, _statement_resources, role_db_id(_statement_role))
        RETURNING db_id INTO statement_db_id;
    END IF;
    INSERT INTO iam_statement_projects (statement_id, project_id)
      SELECT statement_db_id, project_db_id(p_id)
      FROM UNNEST(_statement_projects) projects(p_id)
    ON CONFLICT DO NOTHING;
    RETURN;
END;
$$;


ALTER FUNCTION public.insert_iam_statement_into_policy(_policy_id text, _statement_effect public.iam_effect, _statement_actions text[], _statement_resources text[], _statement_role text, _statement_projects text[]) OWNER TO postgres;

--
-- Name: member_db_id(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.member_db_id(_name text, OUT _db_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
BEGIN
    SELECT
        db_id INTO STRICT _db_id
    FROM
        iam_members
    WHERE
        name = _name;
END;
$$;


ALTER FUNCTION public.member_db_id(_name text, OUT _db_id integer) OWNER TO postgres;

--
-- Name: member_db_id(uuid); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.member_db_id(_id uuid) RETURNS integer
    LANGUAGE sql
    AS $$
    SELECT
        db_id
    FROM
        iam_members
    WHERE
        id = _id;
$$;


ALTER FUNCTION public.member_db_id(_id uuid) OWNER TO postgres;

--
-- Name: notify_policy_change(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.notify_policy_change() RETURNS void
    LANGUAGE sql
    AS $$

    UPDATE policy_change_tracker SET policy_change_id = uuid_generate_v4();

    NOTIFY policychange;
$$;


ALTER FUNCTION public.notify_policy_change() OWNER TO postgres;

--
-- Name: policy_db_id(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.policy_db_id(_id text, OUT _db_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
BEGIN
    SELECT
        db_id INTO STRICT _db_id
    FROM
        iam_policies
    WHERE
        id = _id;
END;
$$;


ALTER FUNCTION public.policy_db_id(_id text, OUT _db_id integer) OWNER TO postgres;

--
-- Name: policy_projects(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.policy_projects(_policy_id text, OUT _project_ids text[]) RETURNS text[]
    LANGUAGE plpgsql
    AS $$
DECLARE
    policy_db_id INTEGER;
BEGIN
    SELECT
        db_id INTO STRICT policy_db_id
    FROM
        iam_policies AS pol
    WHERE
        pol.id = _policy_id;
    SELECT
        array_agg(p.id) INTO _project_ids
    FROM
        iam_policy_projects AS pp
        JOIN iam_projects AS p ON pp.project_id = p.db_id
    WHERE
        pp.policy_id = policy_db_id;
END;
$$;


ALTER FUNCTION public.policy_projects(_policy_id text, OUT _project_ids text[]) OWNER TO postgres;

--
-- Name: project_db_id(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.project_db_id(_id text, OUT _db_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
BEGIN
    SELECT
        db_id INTO _db_id
    FROM
        iam_projects
    WHERE
        id = _id;
    IF NOT FOUND THEN
        RAISE foreign_key_violation
        USING MESSAGE = 'project not found: ' || _id;
    END IF;
END;
$$;


ALTER FUNCTION public.project_db_id(_id text, OUT _db_id integer) OWNER TO postgres;

--
-- Name: project_id(integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.project_id(_db_id integer, OUT _id text) RETURNS text
    LANGUAGE plpgsql
    AS $$
BEGIN
    SELECT
        id INTO STRICT _id
    FROM
        iam_projects
    WHERE
        db_id = _db_id;
END;
$$;


ALTER FUNCTION public.project_id(_db_id integer, OUT _id text) OWNER TO postgres;

--
-- Name: projects_match(text[], text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.projects_match(_projects text[], _projects_filter text[]) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
    BEGIN
      RETURN (
        -- no projects filter requested (length 0) will be the case for v2.0 or v2.1 ["*"]
        array_length(_projects_filter, 1) IS NULL
        -- projects filter intersects with projects for row
        OR _projects && _projects_filter
        -- projects for row is an empty array, check if (unassigned) in project filter
        OR (array_length(_projects, 1) IS NULL AND '{(unassigned)}' && _projects_filter)
      );
    END
$$;


ALTER FUNCTION public.projects_match(_projects text[], _projects_filter text[]) OWNER TO postgres;

--
-- Name: projects_match_for_rule(integer, text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.projects_match_for_rule(_project_db_id integer, _projects_filter text[]) RETURNS boolean
    LANGUAGE sql
    AS $$
    SELECT array_length(_projects_filter, 1) IS NULL
    OR EXISTS (SELECT 1 FROM iam_projects WHERE db_id=_project_db_id AND id=ANY(_projects_filter))
$$;


ALTER FUNCTION public.projects_match_for_rule(_project_db_id integer, _projects_filter text[]) OWNER TO postgres;

--
-- Name: projects_match_for_rule(text, text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.projects_match_for_rule(_project_id text, _projects_filter text[]) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
    BEGIN
      RETURN (
        array_length(_projects_filter, 1) IS NULL
        OR _project_id = ANY (_projects_filter)
      );
    END
$$;


ALTER FUNCTION public.projects_match_for_rule(_project_id text, _projects_filter text[]) OWNER TO postgres;

--
-- Name: purge_statements_missing_both_actions_and_role(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.purge_statements_missing_both_actions_and_role() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    -- for statements that will still have actions, simply remove the role since those
    -- statements are still valid
    UPDATE iam_statements SET role_id=NULL WHERE role_id=OLD.db_id AND actions != '{}';

    -- for statements that become invalid (no role or actions), delete them
    DELETE FROM iam_statements WHERE role_id=OLD.db_id AND actions = '{}';

    -- if as a result, a policy now has no statements, remove the policy unless it's
    -- chef-managed (chef-managed case should never happen since chef-managed roles can't be
    -- deleted, but just to be safe adding it in)
    DELETE FROM iam_policies p
        WHERE NOT EXISTS (SELECT 1 FROM iam_statements s WHERE p.db_id = s.policy_id)
        AND p.type != 'chef-managed';

    PERFORM notify_policy_change();

    RETURN NULL;
  END
$$;


ALTER FUNCTION public.purge_statements_missing_both_actions_and_role() OWNER TO postgres;

--
-- Name: purge_statements_with_no_actions_or_role(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.purge_statements_with_no_actions_or_role() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    -- for statements that will still have actions, simply remove the role since those
    -- statements are still valid
    UPDATE iam_statements SET role_id=NULL WHERE role_id=OLD.db_id AND actions != '{}';

    -- for statements that become invalid (no role or actions), delete them
    DELETE FROM iam_statements WHERE role_id=OLD.db_id AND actions = '{}';

    -- if as a result, a policy now has no statements, remove the policy unless it's
    -- chef-managed (chef-managed case should never happen since chef-managed roles can't be
    -- deleted, but just to be safe adding it in)
    DELETE FROM iam_policies p
        WHERE NOT EXISTS (SELECT 1 FROM iam_statements s WHERE p.db_id = s.policy_id)
        AND p.type != 'chef-managed';

    RETURN NULL;
  END
$$;


ALTER FUNCTION public.purge_statements_with_no_actions_or_role() OWNER TO postgres;

--
-- Name: purge_statements_with_no_projects(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.purge_statements_with_no_projects() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    -- delete all statements that now have no projects
    DELETE FROM iam_statements s
      WHERE NOT EXISTS (SELECT 1 FROM iam_statement_projects p WHERE s.db_id = p.statement_id AND p.project_id IS NOT NULL);

    -- clean up any policies that don't have statements as a result
    DELETE FROM iam_policies p
        WHERE NOT EXISTS (SELECT 1 FROM iam_statements s WHERE p.db_id = s.policy_id)
        AND p.type != 'chef-managed';

    PERFORM notify_policy_change();

    RETURN NULL;
  END
$$;


ALTER FUNCTION public.purge_statements_with_no_projects() OWNER TO postgres;

--
-- Name: query_policies(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_policies(_projects_filter text[]) RETURNS SETOF json
    LANGUAGE sql
    AS $$
    WITH temp AS (
        SELECT
            pol.db_id,
            pol.id,
            pol.name,
            pol.type,
            ( WITH statement_rows AS (
                    SELECT
                        stmt.db_id,
                        stmt.effect,
                        stmt.actions,
                        stmt.resources,
                        (
                          SELECT COALESCE((SELECT id FROM iam_roles WHERE db_id=stmt.role_id), '') AS role
                        ),
                        (
                            SELECT
                                COALESCE(json_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL),
                                    '[]')
                                FROM iam_statement_projects AS stmt_projs
                            LEFT JOIN iam_projects AS proj ON stmt_projs.statement_id = stmt.db_id WHERE stmt_projs.project_id = proj.db_id) AS projects FROM iam_statements stmt
                    JOIN iam_policies ON stmt.policy_id = pol.db_id
                GROUP BY
                    stmt.db_id,
                    stmt.effect,
                    stmt.actions,
                    stmt.resources,
                    stmt.role_id
)
            SELECT array_agg(statement_rows) FROM statement_rows) AS statements,
            ( SELECT
                array_agg(mem)
              FROM iam_policy_members AS pol_mems
              LEFT JOIN iam_members AS mem ON pol_mems.member_id = mem.db_id WHERE pol_mems.policy_id = pol.db_id
            ) AS members,
    ( SELECT
        array_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL)
        FROM iam_policy_projects AS pol_projs
        LEFT JOIN iam_projects AS proj ON pol_projs.project_id = proj.db_id WHERE pol_projs.policy_id = pol.db_id
    ) AS projects FROM iam_policies AS pol
GROUP BY
    pol.db_id,
    pol.id,
    pol.name,
    pol.type
)
SELECT
    json_build_object('id', temp.id, 'name', temp.name, 'type', temp.type, 'statements', COALESCE(temp.statements, '{}'), 'members', COALESCE(temp.members, '{}'), 'projects', COALESCE(temp.projects, '{}')) AS POLICY
FROM
    temp
WHERE
    projects_match (temp.projects::TEXT[], _projects_filter);
$$;


ALTER FUNCTION public.query_policies(_projects_filter text[]) OWNER TO postgres;

--
-- Name: query_policy(text, text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_policy(_policy_id text, _projects_filter text[]) RETURNS json
    LANGUAGE sql
    AS $$
    WITH temp AS (
        SELECT
            pol.db_id,
            pol.id,
            pol.name,
            pol.type,
            -- get policy's statements using temporary table
            ( WITH statement_rows AS (
                    SELECT
                        stmt.db_id,
                        stmt.effect,
                        stmt.actions,
                        stmt.resources,
                        (
                          SELECT COALESCE((SELECT id FROM iam_roles WHERE db_id=stmt.role_id), '') AS role
                        ),
                        (
                            SELECT
                                COALESCE(json_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL),
                                    '[]')
                                FROM iam_statement_projects AS stmt_projs
                            LEFT JOIN iam_projects AS proj
                            ON stmt_projs.statement_id = stmt.db_id
                            WHERE stmt_projs.project_id = proj.db_id
                        ) AS projects
                    FROM iam_statements stmt
                    JOIN iam_policies ON stmt.policy_id = pol.db_id
                GROUP BY
                    stmt.db_id,
                    stmt.effect,
                    stmt.actions,
                    stmt.resources,
                    stmt.role_id)
            SELECT
                array_agg(statement_rows)
            FROM statement_rows) AS statements,
            -- get policy members
            (
            SELECT
                array_agg(mem)
            FROM iam_policy_members AS pol_mems
            LEFT JOIN iam_members AS mem ON pol_mems.member_id = mem.db_id WHERE pol_mems.policy_id = pol.db_id) AS members,
            -- get projects
            (
            SELECT
                array_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL)
            FROM iam_policy_projects AS pol_projs
            LEFT JOIN iam_projects AS proj
            ON pol_projs.project_id = proj.db_id
            WHERE pol_projs.policy_id = pol.db_id
        ) AS projects
    FROM iam_policies AS pol
    WHERE pol.id = _policy_id
        GROUP BY
            pol.db_id,
            pol.id,
            pol.name,
            pol.type
)
SELECT
    json_build_object('id', temp.id, 'name', temp.name, 'type', temp.type, 'statements', COALESCE(temp.statements, '{}'), 'members', COALESCE(temp.members, '{}'), 'projects', COALESCE(temp.projects, '{}')) AS POLICY
FROM
    temp
WHERE
    projects_match (temp.projects::TEXT[], _projects_filter);
$$;


ALTER FUNCTION public.query_policy(_policy_id text, _projects_filter text[]) OWNER TO postgres;

--
-- Name: query_project(text, text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_project(_project_id text, _projects_filter text[]) RETURNS json
    LANGUAGE plpgsql
    AS $$
declare
  project_json json;

BEGIN
  WITH staged AS (
    SELECT r.project_id
      FROM iam_staged_project_rules AS r
      WHERE project_db_id(_project_id) = r.project_id
      GROUP BY r.project_id
  ),
  applied AS (
    SELECT  r.project_id
      FROM iam_project_rules AS r
      WHERE project_db_id(_project_id) = r.project_id
    GROUP BY r.project_id
  )
  SELECT
  json_build_object(
    'id', p.id,
    'name', p.name,
    'type', p.type,
    'status',
    CASE WHEN staged.project_id IS NULL AND applied.project_id IS NULL THEN 'no-rules'
      WHEN staged.project_id IS NULL THEN 'applied'
      ELSE 'edits-pending'
    END
  ) INTO STRICT project_json
  FROM iam_projects AS p
  LEFT JOIN applied
    ON applied.project_id = p.db_id
  LEFT JOIN staged
    ON staged.project_id = p.db_id
  WHERE p.id=_project_id AND (array_length(_projects_filter, 1) IS NULL OR p.id=ANY(_projects_filter));

  RETURN project_json;
END;
$$;


ALTER FUNCTION public.query_project(_project_id text, _projects_filter text[]) OWNER TO postgres;

--
-- Name: query_projects(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_projects(_projects_filter text[]) RETURNS SETOF json
    LANGUAGE sql
    AS $$

WITH
staged AS (
  SELECT r.project_id
  FROM iam_staged_project_rules AS r
  WHERE projects_match_for_rule(r.project_id, _projects_filter)
  GROUP BY r.project_id
),
applied AS (
  SELECT r.project_id
  FROM iam_project_rules AS r
  WHERE projects_match_for_rule(r.project_id, _projects_filter)
  GROUP BY r.project_id
)
SELECT json_build_object(
  'id', p.id,
  'name', p.name,
  'type', p.type,
  'status',
  CASE WHEN staged.project_id IS NULL AND applied.project_id IS NULL THEN 'no-rules'
       WHEN staged.project_id IS NULL THEN 'applied'
       ELSE 'edits-pending'
  END
)
FROM iam_projects AS p
LEFT JOIN applied
  ON applied.project_id = p.db_id
LEFT JOIN staged
  ON staged.project_id = p.db_id
WHERE projects_match_for_rule(p.id, _projects_filter)

$$;


ALTER FUNCTION public.query_projects(_projects_filter text[]) OWNER TO postgres;

--
-- Name: query_role(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_role(_role_id text) RETURNS json
    LANGUAGE sql
    AS $$
    SELECT
        json_build_object('id', r.id, 'name', r.name, 'type', r.type, 'actions', r.actions, 'projects', COALESCE(role_projects (r.id), '{}')) AS ROLE
    FROM
        iam_roles AS r
    WHERE
        r.id = _role_id
$$;


ALTER FUNCTION public.query_role(_role_id text) OWNER TO postgres;

--
-- Name: query_roles(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_roles(_projects_filter text[]) RETURNS SETOF json
    LANGUAGE sql
    AS $$
    SELECT
        json_build_object('id', r.id, 'name', r.name, 'type', r.type, 'actions', r.actions, 'projects', COALESCE(role_projects (r.id), '{}')) AS ROLE
    FROM
        iam_roles AS r
    WHERE
        projects_match (COALESCE(role_projects (r.id), '{(unassigned)}'), _projects_filter);
$$;


ALTER FUNCTION public.query_roles(_projects_filter text[]) OWNER TO postgres;

--
-- Name: query_rule(text, text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_rule(_rule_db_id text, _project_filter text[]) RETURNS json
    LANGUAGE sql
    AS $$

  WITH t AS (
    SELECT
      r.id,
      r.project_id,
      r.name,
      r.type,
      -- A rule can't exist without conditions so we don't need to worry
      -- about null case here.
      json_agg(rc) AS conditions
    FROM iam_project_rules AS r
    LEFT OUTER JOIN iam_rule_conditions
    AS rc ON rc.rule_db_id=r.db_id
    WHERE id=_rule_db_id AND projects_match_for_rule(project_id, _project_filter)
    GROUP BY r.id, r.project_id, r.name, r.type
  )
  SELECT row_to_json(t) AS rule FROM t;

$$;


ALTER FUNCTION public.query_rule(_rule_db_id text, _project_filter text[]) OWNER TO postgres;

--
-- Name: query_rule_table_associations(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_rule_table_associations(_id text) RETURNS text[]
    LANGUAGE sql
    AS $$

  SELECT ARRAY(
    SELECT 'applied' as TableName from iam_project_rules a where a.id=_id
    UNION
    SELECT 'staged' as TableName from iam_staged_project_rules s where s.id=_id
    );

$$;


ALTER FUNCTION public.query_rule_table_associations(_id text) OWNER TO postgres;

--
-- Name: query_rule_table_associations(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_rule_table_associations(_id text, _project_id text) RETURNS text[]
    LANGUAGE sql
    AS $$

  -- confirm project exists
  SELECT project_db_id(_project_id);

  SELECT ARRAY(
    SELECT 'applied' AS TableName FROM iam_project_rules a WHERE a.id=_id
    UNION
    SELECT 'staged' AS TableName FROM iam_staged_project_rules s WHERE s.id=_id
    );

$$;


ALTER FUNCTION public.query_rule_table_associations(_id text, _project_id text) OWNER TO postgres;

--
-- Name: query_rules(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_rules(_project_filter text[]) RETURNS SETOF json
    LANGUAGE sql
    AS $$

  SELECT json_build_object(
    'id', r.id,
    'project_id', p.id,
    'name', r.name,
    'type', r.type,
    'status', 'applied',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rule
  FROM iam_project_rules AS r
  INNER JOIN iam_rule_conditions AS rc
  ON rc.rule_db_id=r.db_id
  INNER JOIN iam_projects AS p
  ON r.project_id=p.db_id
  WHERE projects_match_for_rule(p.id, _project_filter)
  GROUP BY r.id, p.id, r.name, r.type;

$$;


ALTER FUNCTION public.query_rules(_project_filter text[]) OWNER TO postgres;

--
-- Name: query_rules_for_project(text, text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_rules_for_project(_project_id text, _project_filter text[]) RETURNS SETOF json
    LANGUAGE sql
    AS $$

  SELECT project_db_id(_project_id);

  -- fetch staged rules
  SELECT json_build_object(
    'id', r.id,
    'project_id', _project_id,
    'name', r.name,
    'type', r.type,
    'status', 'staged',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rule
  FROM iam_staged_project_rules AS r
  INNER JOIN iam_staged_rule_conditions AS rc 
  ON rc.rule_db_id=r.db_id
  WHERE r.project_id=project_db_id(_project_id) AND 
        projects_match_for_rule(_project_id, _project_filter) AND
        r.deleted=false
  GROUP BY r.id, r.name, r.type

  UNION ALL

  -- fetch applied rules
  SELECT json_build_object(
    'id', r.id,
    'project_id', _project_id,
    'name', r.name,
    'type', r.type,
    'status', 'applied',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rule
  FROM iam_project_rules AS r
  INNER JOIN iam_rule_conditions AS rc
  ON rc.rule_db_id=r.db_id
  WHERE r.project_id=project_db_id(_project_id) AND projects_match_for_rule(_project_id, _project_filter)
    -- return applied rule only if no staged changes exist
    AND NOT EXISTS (SELECT 1 FROM iam_staged_project_rules WHERE id=r.id)
  GROUP BY r.id, r.name, r.type

 $$;


ALTER FUNCTION public.query_rules_for_project(_project_id text, _project_filter text[]) OWNER TO postgres;

--
-- Name: query_staged_and_applied_rules(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_staged_and_applied_rules(_project_filter text[]) RETURNS SETOF json
    LANGUAGE sql
    AS $$

  -- fetch staged rules
  SELECT json_build_object(
    'id', r.id,
    'project_id', p.id,
    'name', r.name,
    'type', r.type,
    'deleted', r.deleted,
    'status', 'staged',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rules
  FROM iam_staged_project_rules AS r
  INNER JOIN iam_staged_rule_conditions AS rc
  ON rc.rule_db_id=r.db_id
  INNER JOIN iam_projects AS p
  ON r.project_id=p.db_id
  WHERE projects_match_for_rule(p.id, _project_filter)
  GROUP BY r.id, p.id, r.name, r.type, r.deleted

  UNION ALL

  -- fetch applied rules
  SELECT json_build_object(
    'id', r.id,
    'project_id', p.id,
    'name', r.name,
    'type', r.type,
    'deleted', false,
    'status', 'applied',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rules
  FROM iam_project_rules AS r
  INNER JOIN iam_rule_conditions AS rc
  ON rc.rule_db_id=r.db_id
  INNER JOIN iam_projects AS p
  ON r.project_id=p.db_id
  WHERE projects_match_for_rule(p.id, _project_filter)
  GROUP BY r.id, p.id, r.name, r.type;

$$;


ALTER FUNCTION public.query_staged_and_applied_rules(_project_filter text[]) OWNER TO postgres;

--
-- Name: query_staged_or_applied_rule(text, text, text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.query_staged_or_applied_rule(_rule_id text, _project_id text, _project_filter text[]) RETURNS json
    LANGUAGE sql
    AS $$

  SELECT project_db_id(_project_id);

  -- check for applied rule
  SELECT json_build_object(
    'id', r.id,
    'project_id', _project_id,
    'name', r.name,
    'type', r.type,
    'deleted', r.deleted,
    'status', 'staged',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rule
  FROM iam_staged_project_rules AS r
  INNER JOIN iam_staged_rule_conditions AS rc
  ON rc.rule_db_id=r.db_id
  WHERE r.project_id=project_db_id(_project_id) AND
        r.id=_rule_id AND
        projects_match_for_rule(r.project_id, _project_filter)
  GROUP BY r.id, r.name, r.type, r.deleted

  UNION ALL

  -- check for applied rule
  SELECT json_build_object(
    'id', r.id,
    'project_id', _project_id,
    'name', r.name,
    'type', r.type,
    'deleted', false,
    'status', 'applied',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rule
  FROM iam_project_rules AS r
  INNER JOIN iam_rule_conditions AS rc
   ON rc.rule_db_id=r.db_id
  WHERE r.project_id=project_db_id(_project_id) AND
        r.id=_rule_id AND
        projects_match_for_rule(r.project_id, _project_filter)
  GROUP BY r.id, r.name, r.type;

$$;


ALTER FUNCTION public.query_staged_or_applied_rule(_rule_id text, _project_id text, _project_filter text[]) OWNER TO postgres;

--
-- Name: replace_array_elements(jsonb, text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.replace_array_elements(arr jsonb, old text, new text) RETURNS jsonb
    LANGUAGE sql
    AS $$
    SELECT jsonb_agg(replace(e, old, new))
    FROM jsonb_array_elements_text(arr) e(e)
$$;


ALTER FUNCTION public.replace_array_elements(arr jsonb, old text, new text) OWNER TO postgres;

--
-- Name: role_db_id(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.role_db_id(_id text, OUT _db_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
BEGIN
    SELECT
        db_id INTO _db_id
    FROM
        iam_roles
    WHERE
        id = _id;
    IF NOT FOUND THEN
        RAISE foreign_key_violation
        USING MESSAGE = 'role not found: ' || _id;
    END IF;
END;
$$;


ALTER FUNCTION public.role_db_id(_id text, OUT _db_id integer) OWNER TO postgres;

--
-- Name: role_projects(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.role_projects(_role_id text, OUT _project_ids text[]) RETURNS text[]
    LANGUAGE plpgsql
    AS $$
DECLARE
    role_db_id INTEGER;
BEGIN
    SELECT
        db_id INTO STRICT role_db_id
    FROM
        iam_roles AS r
    WHERE
        r.id = _role_id;
    SELECT
        array_agg(p.id) INTO _project_ids
    FROM
        iam_role_projects AS rp
        JOIN iam_projects AS p ON rp.project_id = p.db_id
    WHERE
        rp.role_id = role_db_id;
END;
$$;


ALTER FUNCTION public.role_projects(_role_id text, OUT _project_ids text[]) OWNER TO postgres;

--
-- Name: update_rule(text, text, text, text, text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.update_rule(_in_rule_id text, _in_project_id text, _in_name text, _in_type text, projects_filter text[]) RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
  rule_db_id INTEGER;
BEGIN
    IF (NOT EXISTS (SELECT 1 FROM iam_project_rules WHERE id=_in_rule_id AND projects_match_for_rule(project_id, projects_filter))) AND
        (NOT EXISTS (SELECT 1 FROM iam_staged_project_rules WHERE id=_in_rule_id AND projects_match_for_rule(project_id, projects_filter))) THEN
            RAISE EXCEPTION 'not found %', _in_rule_id
            USING ERRCODE='case_not_found';
    ELSIF (NOT EXISTS (SELECT 1 FROM iam_project_rules WHERE id=_in_rule_id AND project_id=project_db_id(_in_project_id))) AND
        (NOT EXISTS (SELECT 1 FROM iam_staged_project_rules WHERE id=_in_rule_id AND project_id=project_db_id(_in_project_id))) THEN
            RAISE EXCEPTION 'incoming project does not match rule project %', _in_rule_id
            USING ERRCODE='PRJTR';
    ELSIF (NOT EXISTS (SELECT 1 FROM iam_project_rules WHERE id=_in_rule_id AND type=_in_type)) AND
        (NOT EXISTS (SELECT 1 FROM iam_staged_project_rules WHERE id=_in_rule_id AND type=_in_type)) THEN
            RAISE EXCEPTION 'incoming type does not match rule type %', _in_rule_id
            USING ERRCODE='RLTYP';
    ELSE
        INSERT INTO iam_staged_project_rules as ispr (id, project_id, name, type, deleted)
        VALUES (_in_rule_id, project_db_id(_in_project_id), _in_name, _in_type, false)
        ON CONFLICT (id) DO -- applied rule has already been updated, so update the staged version of it
            UPDATE SET
                id          = _in_rule_id,
                project_id  = project_db_id(_in_project_id),
                name        = _in_name,
                type        = _in_type
            WHERE ispr.id = _in_rule_id AND projects_match_for_rule(ispr.project_id, projects_filter)
        RETURNING ispr.db_id INTO rule_db_id;
    RETURN rule_db_id;
    END IF;
END;
$$;


ALTER FUNCTION public.update_rule(_in_rule_id text, _in_project_id text, _in_name text, _in_type text, projects_filter text[]) OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: data_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.data_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.data_migrations OWNER TO postgres;

--
-- Name: iam_members; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_members (
    name text NOT NULL,
    db_id integer NOT NULL
);


ALTER TABLE public.iam_members OWNER TO postgres;

--
-- Name: iam_members_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.iam_members_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.iam_members_db_id_seq OWNER TO postgres;

--
-- Name: iam_members_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.iam_members_db_id_seq OWNED BY public.iam_members.db_id;


--
-- Name: iam_policies; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_policies (
    id text NOT NULL,
    name text NOT NULL,
    type public.iam_policy_type DEFAULT 'custom'::public.iam_policy_type NOT NULL,
    db_id integer NOT NULL
);


ALTER TABLE public.iam_policies OWNER TO postgres;

--
-- Name: iam_policies_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.iam_policies_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.iam_policies_db_id_seq OWNER TO postgres;

--
-- Name: iam_policies_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.iam_policies_db_id_seq OWNED BY public.iam_policies.db_id;


--
-- Name: iam_policy_members; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_policy_members (
    member_id integer NOT NULL,
    policy_id integer NOT NULL
);


ALTER TABLE public.iam_policy_members OWNER TO postgres;

--
-- Name: iam_policy_projects; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_policy_projects (
    policy_id integer NOT NULL,
    project_id integer NOT NULL
);


ALTER TABLE public.iam_policy_projects OWNER TO postgres;

--
-- Name: iam_policy_projects_project_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.iam_policy_projects_project_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.iam_policy_projects_project_id_seq OWNER TO postgres;

--
-- Name: iam_policy_projects_project_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.iam_policy_projects_project_id_seq OWNED BY public.iam_policy_projects.project_id;


--
-- Name: iam_project_rules; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_project_rules (
    db_id integer NOT NULL,
    id text NOT NULL,
    name text NOT NULL,
    type text NOT NULL,
    project_id integer NOT NULL
);


ALTER TABLE public.iam_project_rules OWNER TO postgres;

--
-- Name: iam_project_rules_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.iam_project_rules_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.iam_project_rules_db_id_seq OWNER TO postgres;

--
-- Name: iam_project_rules_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.iam_project_rules_db_id_seq OWNED BY public.iam_project_rules.db_id;


--
-- Name: iam_projects; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_projects (
    db_id integer NOT NULL,
    id text NOT NULL,
    name text NOT NULL,
    type public.iam_policy_type DEFAULT 'custom'::public.iam_policy_type NOT NULL
);


ALTER TABLE public.iam_projects OWNER TO postgres;

--
-- Name: iam_projects_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.iam_projects_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.iam_projects_db_id_seq OWNER TO postgres;

--
-- Name: iam_projects_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.iam_projects_db_id_seq OWNED BY public.iam_projects.db_id;


--
-- Name: iam_projects_graveyard; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_projects_graveyard (
    db_id integer NOT NULL,
    id text NOT NULL
);


ALTER TABLE public.iam_projects_graveyard OWNER TO postgres;

--
-- Name: iam_projects_graveyard_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.iam_projects_graveyard_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.iam_projects_graveyard_db_id_seq OWNER TO postgres;

--
-- Name: iam_projects_graveyard_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.iam_projects_graveyard_db_id_seq OWNED BY public.iam_projects_graveyard.db_id;


--
-- Name: iam_role_projects; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_role_projects (
    role_id integer NOT NULL,
    project_id integer NOT NULL
);


ALTER TABLE public.iam_role_projects OWNER TO postgres;

--
-- Name: iam_roles; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_roles (
    db_id integer NOT NULL,
    id text NOT NULL,
    name text NOT NULL,
    type public.iam_policy_type DEFAULT 'custom'::public.iam_policy_type NOT NULL,
    actions text[] NOT NULL
);


ALTER TABLE public.iam_roles OWNER TO postgres;

--
-- Name: iam_roles_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.iam_roles_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.iam_roles_db_id_seq OWNER TO postgres;

--
-- Name: iam_roles_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.iam_roles_db_id_seq OWNED BY public.iam_roles.db_id;


--
-- Name: iam_rule_conditions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_rule_conditions (
    db_id integer NOT NULL,
    rule_db_id integer NOT NULL,
    value text[] NOT NULL,
    attribute text NOT NULL,
    operator text NOT NULL
);


ALTER TABLE public.iam_rule_conditions OWNER TO postgres;

--
-- Name: iam_rule_conditions_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.iam_rule_conditions_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.iam_rule_conditions_db_id_seq OWNER TO postgres;

--
-- Name: iam_rule_conditions_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.iam_rule_conditions_db_id_seq OWNED BY public.iam_rule_conditions.db_id;


--
-- Name: iam_staged_project_rules; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_staged_project_rules (
    db_id integer NOT NULL,
    id text NOT NULL,
    name text NOT NULL,
    type text NOT NULL,
    deleted boolean DEFAULT false NOT NULL,
    project_id integer NOT NULL
);


ALTER TABLE public.iam_staged_project_rules OWNER TO postgres;

--
-- Name: iam_staged_project_rules_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.iam_staged_project_rules_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.iam_staged_project_rules_db_id_seq OWNER TO postgres;

--
-- Name: iam_staged_project_rules_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.iam_staged_project_rules_db_id_seq OWNED BY public.iam_staged_project_rules.db_id;


--
-- Name: iam_staged_rule_conditions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_staged_rule_conditions (
    db_id integer NOT NULL,
    rule_db_id integer NOT NULL,
    value text[] NOT NULL,
    attribute text NOT NULL,
    operator text NOT NULL
);


ALTER TABLE public.iam_staged_rule_conditions OWNER TO postgres;

--
-- Name: iam_staged_rule_conditions_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.iam_staged_rule_conditions_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.iam_staged_rule_conditions_db_id_seq OWNER TO postgres;

--
-- Name: iam_staged_rule_conditions_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.iam_staged_rule_conditions_db_id_seq OWNED BY public.iam_staged_rule_conditions.db_id;


--
-- Name: iam_statement_projects; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_statement_projects (
    project_id integer NOT NULL,
    statement_id integer NOT NULL
);


ALTER TABLE public.iam_statement_projects OWNER TO postgres;

--
-- Name: iam_statements; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.iam_statements (
    effect public.iam_effect NOT NULL,
    actions text[] NOT NULL,
    resources text[] NOT NULL,
    db_id integer NOT NULL,
    policy_id integer NOT NULL,
    role_id integer
);


ALTER TABLE public.iam_statements OWNER TO postgres;

--
-- Name: iam_statements_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.iam_statements_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.iam_statements_db_id_seq OWNER TO postgres;

--
-- Name: iam_statements_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.iam_statements_db_id_seq OWNED BY public.iam_statements.db_id;


--
-- Name: migration_status; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.migration_status (
    only_one_row boolean DEFAULT true NOT NULL,
    state public.migration_state NOT NULL,
    CONSTRAINT only_one_row_unique CHECK (only_one_row)
);


ALTER TABLE public.migration_status OWNER TO postgres;

--
-- Name: policies; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.policies (
    id uuid NOT NULL,
    policy_data jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    version integer NOT NULL,
    deletable boolean DEFAULT true NOT NULL,
    updated_at timestamp with time zone
);


ALTER TABLE public.policies OWNER TO postgres;

--
-- Name: policy_change_tracker; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.policy_change_tracker (
    policy_change_id uuid DEFAULT public.uuid_generate_v4() NOT NULL
);


ALTER TABLE public.policy_change_tracker OWNER TO postgres;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: iam_members db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_members ALTER COLUMN db_id SET DEFAULT nextval('public.iam_members_db_id_seq'::regclass);


--
-- Name: iam_policies db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_policies ALTER COLUMN db_id SET DEFAULT nextval('public.iam_policies_db_id_seq'::regclass);


--
-- Name: iam_policy_projects project_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_policy_projects ALTER COLUMN project_id SET DEFAULT nextval('public.iam_policy_projects_project_id_seq'::regclass);


--
-- Name: iam_project_rules db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_project_rules ALTER COLUMN db_id SET DEFAULT nextval('public.iam_project_rules_db_id_seq'::regclass);


--
-- Name: iam_projects db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_projects ALTER COLUMN db_id SET DEFAULT nextval('public.iam_projects_db_id_seq'::regclass);


--
-- Name: iam_projects_graveyard db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_projects_graveyard ALTER COLUMN db_id SET DEFAULT nextval('public.iam_projects_graveyard_db_id_seq'::regclass);


--
-- Name: iam_roles db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_roles ALTER COLUMN db_id SET DEFAULT nextval('public.iam_roles_db_id_seq'::regclass);


--
-- Name: iam_rule_conditions db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_rule_conditions ALTER COLUMN db_id SET DEFAULT nextval('public.iam_rule_conditions_db_id_seq'::regclass);


--
-- Name: iam_staged_project_rules db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_staged_project_rules ALTER COLUMN db_id SET DEFAULT nextval('public.iam_staged_project_rules_db_id_seq'::regclass);


--
-- Name: iam_staged_rule_conditions db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_staged_rule_conditions ALTER COLUMN db_id SET DEFAULT nextval('public.iam_staged_rule_conditions_db_id_seq'::regclass);


--
-- Name: iam_statements db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_statements ALTER COLUMN db_id SET DEFAULT nextval('public.iam_statements_db_id_seq'::regclass);


--
-- Data for Name: data_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.data_migrations (version, dirty) FROM stdin;
11	f
\.


--
-- Data for Name: iam_members; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_members (name, db_id) FROM stdin;
team:local:admins	1
team:local:editors	2
team:local:viewers	3
\.


--
-- Data for Name: iam_policies; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_policies (id, name, type, db_id) FROM stdin;
administrator-access	Administrator	chef-managed	1
editor-access	Editors	chef-managed	2
viewer-access	Viewers	chef-managed	3
ingest-access	Ingest	chef-managed	4
compliance-viewer-access	Compliance Viewers	custom	5
compliance-editor-access	Compliance Editors	custom	6
\.


--
-- Data for Name: iam_policy_members; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_policy_members (member_id, policy_id) FROM stdin;
1	1
2	2
3	3
\.


--
-- Data for Name: iam_policy_projects; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_policy_projects (policy_id, project_id) FROM stdin;
\.


--
-- Data for Name: iam_project_rules; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_project_rules (db_id, id, name, type, project_id) FROM stdin;
\.


--
-- Data for Name: iam_projects; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_projects (db_id, id, name, type) FROM stdin;
2	~~ALL-PROJECTS~~	All Projects	chef-managed
1	(unassigned)	(unassigned)	chef-managed
\.


--
-- Data for Name: iam_projects_graveyard; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_projects_graveyard (db_id, id) FROM stdin;
\.


--
-- Data for Name: iam_role_projects; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_role_projects (role_id, project_id) FROM stdin;
\.


--
-- Data for Name: iam_roles; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_roles (db_id, id, name, type, actions) FROM stdin;
1	owner	Owner	chef-managed	{*}
4	ingest	Ingest	chef-managed	{ingest:*,compliance:profiles:get,compliance:profiles:list}
5	project-owner	Project Owner	chef-managed	{infra:nodes:*,infra:nodeManagers:*,compliance:*,event:*,ingest:*,secrets:*,iam:projects:list,iam:projects:get,iam:projects:assign,iam:policies:list,iam:policies:get,iam:policyMembers:*,iam:teams:list,iam:teams:get,iam:teamUsers:*,iam:users:get,iam:users:list,applications:*}
7	compliance-viewer	Compliance Viewer	custom	{compliance:*:get,compliance:*:list}
8	compliance-editor	Compliance Editor	custom	{compliance:*}
2	editor	Editor	chef-managed	{infra:infraServers:list,infra:infraServers:get,infra:nodes:*,infra:nodeManagers:*,compliance:*,event:*,ingest:*,secrets:*,iam:projects:list,iam:projects:get,iam:projects:assign,applications:*}
3	viewer	Viewer	chef-managed	{infra:infraServers:list,infra:infraServers:get,secrets:*:get,secrets:*:list,infra:nodes:get,infra:nodes:list,infra:nodeManagers:get,infra:nodeManagers:list,compliance:*:get,compliance:*:list,event:*:get,event:*:list,ingest:*:get,ingest:*:list,iam:projects:list,iam:projects:get,applications:*:get,applications:*:list}
\.


--
-- Data for Name: iam_rule_conditions; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_rule_conditions (db_id, rule_db_id, value, attribute, operator) FROM stdin;
\.


--
-- Data for Name: iam_staged_project_rules; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_staged_project_rules (db_id, id, name, type, deleted, project_id) FROM stdin;
\.


--
-- Data for Name: iam_staged_rule_conditions; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_staged_rule_conditions (db_id, rule_db_id, value, attribute, operator) FROM stdin;
\.


--
-- Data for Name: iam_statement_projects; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_statement_projects (project_id, statement_id) FROM stdin;
2	1
2	2
2	3
2	4
2	5
2	6
2	7
\.


--
-- Data for Name: iam_statements; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.iam_statements (effect, actions, resources, db_id, policy_id, role_id) FROM stdin;
deny	{iam:policies:delete,iam:policies:update}	{iam:policies:administrator-access}	2	1	\N
allow	{}	{*}	3	2	2
allow	{}	{*}	4	3	3
allow	{}	{*}	5	4	4
allow	{}	{*}	1	1	1
allow	{}	{*}	6	5	7
allow	{}	{*}	7	6	8
\.


--
-- Data for Name: migration_status; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.migration_status (only_one_row, state) FROM stdin;
t	successful
\.


--
-- Data for Name: policies; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.policies (id, policy_data, created_at, version, deletable, updated_at) FROM stdin;
f9eb8c5a-3b8b-4695-ae39-ca434237f69b	{"action": "read", "effect": "allow", "resource": "auth:users:${a2:username}", "subjects": ["user:local:*"]}	2022-06-03 09:19:25.928059+00	1	t	\N
b4b00330-22a4-4cd2-ac2a-820983c1c3b0	{"action": "*", "effect": "allow", "resource": "*", "subjects": ["team:local:admins"]}	2022-06-03 09:19:25.810291+00	1	f	\N
0632b04d-9453-402e-9ebe-12d8e44711ac	{"action": "*", "effect": "allow", "resource": "auth_introspection:*", "subjects": ["user:*"]}	2022-06-03 09:19:25.810291+00	1	f	\N
e9b37b6a-cf30-4167-bfcb-dfd50e45926c	{"action": "*", "effect": "allow", "resource": "service_info:*", "subjects": ["user:*"]}	2022-06-03 09:19:25.810291+00	1	t	\N
3b8e626f-a46e-4f1c-8e84-d026111cd566	{"action": "*", "effect": "allow", "resource": "events", "subjects": ["user:*"]}	2022-06-03 09:19:25.810291+00	1	t	\N
fb1bf117-1941-4208-9771-b45bf288ff28	{"action": "*", "effect": "allow", "resource": "events:*", "subjects": ["user:*"]}	2022-06-03 09:19:25.810291+00	1	t	\N
d4167a66-823d-4cf5-a7bb-4702094ec38c	{"action": "*", "effect": "allow", "resource": "ingest:*", "subjects": ["token:*"]}	2022-06-03 09:19:25.810291+00	1	t	\N
3222e45f-7b05-4b49-9bf3-c4357d6c032f	{"action": "*", "effect": "allow", "resource": "nodes", "subjects": ["user:*"]}	2022-06-03 09:19:25.810291+00	1	t	\N
254fdd71-276e-4a21-a43a-5f59e2b09d6e	{"action": "*", "effect": "allow", "resource": "nodes:*", "subjects": ["user:*"]}	2022-06-03 09:19:25.810291+00	1	t	\N
ecf04384-05d6-41b4-9544-9d39a6d2892f	{"action": "*", "effect": "allow", "resource": "nodemanagers", "subjects": ["user:*"]}	2022-06-03 09:19:25.810291+00	1	t	\N
167115ac-361b-448f-b28e-8ab5b16f3a67	{"action": "*", "effect": "allow", "resource": "nodemanagers:*", "subjects": ["user:*"]}	2022-06-03 09:19:25.810291+00	1	t	\N
a6a0031a-409a-40b8-83f5-cd3a92e4c527	{"action": "*", "effect": "allow", "resource": "secrets", "subjects": ["user:*"]}	2022-06-03 09:19:25.810291+00	1	t	\N
febfe2ed-be49-47d7-83f3-95b18a48328d	{"action": "*", "effect": "allow", "resource": "secrets:*", "subjects": ["user:*"]}	2022-06-03 09:19:25.810291+00	1	t	\N
6ac3f062-0a84-437d-9f67-3d343fd8b0b8	{"action": "*", "effect": "allow", "resource": "users:${a2:username}", "subjects": ["user:local:*"]}	2022-06-03 09:19:25.978591+00	1	t	\N
9fb73dc9-ef08-498a-af64-a8a17176f18f	{"action": "search", "effect": "allow", "resource": "compliance:profiles", "subjects": ["token:*"]}	2022-06-03 09:19:25.853626+00	1	t	\N
f8d97cb1-a40e-46e4-bba8-a4ae8be51371	{"action": "read", "effect": "allow", "resource": "telemetry:config", "subjects": ["user:*"]}	2022-06-03 09:19:25.874858+00	1	f	\N
ab2c89a9-a584-4501-b394-ec13d9991d61	{"action": "read", "effect": "allow", "resource": "license:status", "subjects": ["user:*"]}	2022-06-03 09:19:25.901906+00	1	f	\N
3db8c368-9cb2-4839-9de4-57ded0de9a55	{"action": "read", "effect": "allow", "resource": "compliance:profiles:*", "subjects": ["token:*"]}	2022-06-03 09:19:25.853626+00	1	t	\N
0d16e400-e6db-4694-a39c-a4d61f161bae	{"action": "upload", "effect": "allow", "resource": "compliance:profiles:*", "subjects": ["token:*"]}	2022-06-03 09:19:25.853626+00	1	t	\N
8df0a55f-2a93-4119-b84f-f382d5019271	{"action": "read", "effect": "allow", "resource": "ingest:status", "subjects": ["tls:service:automate-cs-oc-erchef:*"]}	2022-06-03 09:19:25.939309+00	1	f	\N
9d67b657-2d37-40f6-a11a-0b6c86aac1d9	{"action": "create", "effect": "allow", "resource": "ingest:unified_events", "subjects": ["tls:service:automate-cs-oc-erchef:*"]}	2022-06-03 09:19:25.939309+00	1	f	\N
9ed85167-a659-4415-b984-fde88d68c6f0	{"action": "read", "effect": "allow", "resource": "compliance:profiles:storage:*", "subjects": ["tls:service:automate-cs-nginx:*"]}	2022-06-03 09:19:25.957193+00	1	f	\N
6e792df9-e51f-4474-9539-40ca2a2b308c	{"action": "create", "effect": "allow", "resource": "ingest:unified_events", "subjects": ["tls:service:automate-cs-nginx:*"]}	2022-06-03 09:19:25.957193+00	1	f	\N
73eb6ea3-55af-4eae-b7e9-4d89334087ed	{"action": "*", "effect": "allow", "resource": "compliance:*", "subjects": ["user:*"]}	2022-06-03 09:19:26.040786+00	1	t	\N
42ca2668-511a-48e2-b5e2-553b26d34698	{"action": "*", "effect": "allow", "resource": "cfgmgmt:stats:*", "subjects": ["user:*"]}	2022-06-03 09:19:26.128971+00	1	t	\N
76a38aae-a15e-463b-8c2b-4a09c1d62ef6	{"action": "*", "effect": "allow", "resource": "cfgmgmt:nodes:*", "subjects": ["user:*"]}	2022-06-03 09:19:26.128971+00	1	t	\N
ce23fd69-c4bd-4407-961b-60f9b26c4dfa	{"action": "*", "effect": "allow", "resource": "cfgmgmt:nodes", "subjects": ["user:*"]}	2022-06-03 09:19:26.128971+00	1	t	\N
7c0f72b0-d706-424e-b6e8-22abaa37d5cc	{"action": "read", "effect": "allow", "resource": "auth:policies", "subjects": ["user:*"]}	2022-06-03 09:19:26.50151+00	1	f	\N
aee14d59-da0b-4974-ba6d-1a018b024874	{"action": "*", "effect": "allow", "resource": "service_groups", "subjects": ["user:*"]}	2022-06-03 09:19:26.844339+00	1	t	\N
\.


--
-- Data for Name: policy_change_tracker; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.policy_change_tracker (policy_change_id) FROM stdin;
77db4e1b-dc31-48a6-bbaf-5d6c769e2c2c
\.


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version, dirty) FROM stdin;
77	f
\.


--
-- Name: iam_members_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.iam_members_db_id_seq', 3, true);


--
-- Name: iam_policies_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.iam_policies_db_id_seq', 6, true);


--
-- Name: iam_policy_projects_project_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.iam_policy_projects_project_id_seq', 1, false);


--
-- Name: iam_project_rules_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.iam_project_rules_db_id_seq', 1, false);


--
-- Name: iam_projects_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.iam_projects_db_id_seq', 2, true);


--
-- Name: iam_projects_graveyard_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.iam_projects_graveyard_db_id_seq', 1, false);


--
-- Name: iam_roles_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.iam_roles_db_id_seq', 8, true);


--
-- Name: iam_rule_conditions_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.iam_rule_conditions_db_id_seq', 1, false);


--
-- Name: iam_staged_project_rules_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.iam_staged_project_rules_db_id_seq', 1, false);


--
-- Name: iam_staged_rule_conditions_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.iam_staged_rule_conditions_db_id_seq', 1, false);


--
-- Name: iam_statements_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.iam_statements_db_id_seq', 7, true);


--
-- Name: data_migrations data_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.data_migrations
    ADD CONSTRAINT data_migrations_pkey PRIMARY KEY (version);


--
-- Name: iam_members iam_members_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_members
    ADD CONSTRAINT iam_members_name_key UNIQUE (name);


--
-- Name: iam_members iam_members_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_members
    ADD CONSTRAINT iam_members_pkey PRIMARY KEY (db_id);


--
-- Name: iam_policies iam_policies_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_policies
    ADD CONSTRAINT iam_policies_id_key UNIQUE (id);


--
-- Name: iam_policies iam_policies_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_policies
    ADD CONSTRAINT iam_policies_pkey PRIMARY KEY (db_id);


--
-- Name: iam_policy_members iam_policy_members_member_id_policy_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_policy_members
    ADD CONSTRAINT iam_policy_members_member_id_policy_id_key UNIQUE (member_id, policy_id);


--
-- Name: iam_policy_projects iam_policy_projects_policy_id_project_id_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_policy_projects
    ADD CONSTRAINT iam_policy_projects_policy_id_project_id_unique UNIQUE (policy_id, project_id);


--
-- Name: iam_project_rules iam_project_rules_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_project_rules
    ADD CONSTRAINT iam_project_rules_id_key UNIQUE (id);


--
-- Name: iam_project_rules iam_project_rules_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_project_rules
    ADD CONSTRAINT iam_project_rules_pkey PRIMARY KEY (db_id);


--
-- Name: iam_projects_graveyard iam_projects_graveyard_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_projects_graveyard
    ADD CONSTRAINT iam_projects_graveyard_id_key UNIQUE (id);


--
-- Name: iam_projects_graveyard iam_projects_graveyard_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_projects_graveyard
    ADD CONSTRAINT iam_projects_graveyard_pkey PRIMARY KEY (db_id);


--
-- Name: iam_projects iam_projects_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_projects
    ADD CONSTRAINT iam_projects_id_key UNIQUE (id);


--
-- Name: iam_projects iam_projects_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_projects
    ADD CONSTRAINT iam_projects_pkey PRIMARY KEY (db_id);


--
-- Name: iam_role_projects iam_role_projects_role_id_project_id_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_role_projects
    ADD CONSTRAINT iam_role_projects_role_id_project_id_unique UNIQUE (role_id, project_id);


--
-- Name: iam_roles iam_roles_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_roles
    ADD CONSTRAINT iam_roles_id_key UNIQUE (id);


--
-- Name: iam_roles iam_roles_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_roles
    ADD CONSTRAINT iam_roles_pkey PRIMARY KEY (db_id);


--
-- Name: iam_rule_conditions iam_rule_conditions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_rule_conditions
    ADD CONSTRAINT iam_rule_conditions_pkey PRIMARY KEY (db_id);


--
-- Name: iam_staged_project_rules iam_staged_project_rules_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_staged_project_rules
    ADD CONSTRAINT iam_staged_project_rules_id_key UNIQUE (id);


--
-- Name: iam_staged_project_rules iam_staged_project_rules_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_staged_project_rules
    ADD CONSTRAINT iam_staged_project_rules_pkey PRIMARY KEY (db_id);


--
-- Name: iam_staged_rule_conditions iam_staged_rule_conditions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_staged_rule_conditions
    ADD CONSTRAINT iam_staged_rule_conditions_pkey PRIMARY KEY (db_id);


--
-- Name: iam_statement_projects iam_statement_projects_statement_id_project_id_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_statement_projects
    ADD CONSTRAINT iam_statement_projects_statement_id_project_id_unique UNIQUE (statement_id, project_id);


--
-- Name: iam_statements iam_statements_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_statements
    ADD CONSTRAINT iam_statements_pkey PRIMARY KEY (db_id);


--
-- Name: migration_status migration_status_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.migration_status
    ADD CONSTRAINT migration_status_pkey PRIMARY KEY (only_one_row);


--
-- Name: policies policies_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.policies
    ADD CONSTRAINT policies_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: iam_projects on_project_deletion; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER on_project_deletion AFTER DELETE ON public.iam_projects FOR EACH ROW EXECUTE FUNCTION public.purge_statements_with_no_projects();


--
-- Name: iam_roles on_role_deletion; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER on_role_deletion AFTER DELETE ON public.iam_roles FOR EACH ROW EXECUTE FUNCTION public.purge_statements_with_no_actions_or_role();


--
-- Name: policies only_allow_delete_on_deletable_policies; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER only_allow_delete_on_deletable_policies BEFORE DELETE ON public.policies FOR EACH ROW WHEN ((old.deletable = false)) EXECUTE FUNCTION public.cannot_delete_policy_error();


--
-- Name: iam_staged_project_rules verify_not_deleted; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER verify_not_deleted BEFORE UPDATE ON public.iam_staged_project_rules FOR EACH ROW WHEN ((old.deleted = true)) EXECUTE FUNCTION public.fn_deleted_rule_check();


--
-- Name: iam_policy_members iam_policy_members_member_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_policy_members
    ADD CONSTRAINT iam_policy_members_member_id_fkey FOREIGN KEY (member_id) REFERENCES public.iam_members(db_id) ON DELETE CASCADE;


--
-- Name: iam_policy_members iam_policy_members_policy_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_policy_members
    ADD CONSTRAINT iam_policy_members_policy_id_fkey FOREIGN KEY (policy_id) REFERENCES public.iam_policies(db_id) ON DELETE CASCADE DEFERRABLE;


--
-- Name: iam_policy_projects iam_policy_projects_policy_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_policy_projects
    ADD CONSTRAINT iam_policy_projects_policy_id_fkey FOREIGN KEY (policy_id) REFERENCES public.iam_policies(db_id) ON DELETE CASCADE;


--
-- Name: iam_policy_projects iam_policy_projects_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_policy_projects
    ADD CONSTRAINT iam_policy_projects_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.iam_projects(db_id) ON DELETE CASCADE;


--
-- Name: iam_project_rules iam_projects_db_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_project_rules
    ADD CONSTRAINT iam_projects_db_id_fkey FOREIGN KEY (project_id) REFERENCES public.iam_projects(db_id) ON DELETE CASCADE;


--
-- Name: iam_role_projects iam_role_projects_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_role_projects
    ADD CONSTRAINT iam_role_projects_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.iam_projects(db_id) ON DELETE CASCADE;


--
-- Name: iam_role_projects iam_role_projects_role_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_role_projects
    ADD CONSTRAINT iam_role_projects_role_id_fkey FOREIGN KEY (role_id) REFERENCES public.iam_roles(db_id) ON DELETE CASCADE;


--
-- Name: iam_rule_conditions iam_rule_conditions_rule_db_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_rule_conditions
    ADD CONSTRAINT iam_rule_conditions_rule_db_id_fkey FOREIGN KEY (rule_db_id) REFERENCES public.iam_project_rules(db_id) ON DELETE CASCADE;


--
-- Name: iam_staged_project_rules iam_staged_project_rules_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_staged_project_rules
    ADD CONSTRAINT iam_staged_project_rules_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.iam_projects(db_id) ON DELETE CASCADE;


--
-- Name: iam_staged_rule_conditions iam_staged_rule_conditions_rule_db_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_staged_rule_conditions
    ADD CONSTRAINT iam_staged_rule_conditions_rule_db_id_fkey FOREIGN KEY (rule_db_id) REFERENCES public.iam_staged_project_rules(db_id) ON DELETE CASCADE;


--
-- Name: iam_statement_projects iam_statement_projects_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_statement_projects
    ADD CONSTRAINT iam_statement_projects_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.iam_projects(db_id) ON DELETE CASCADE;


--
-- Name: iam_statement_projects iam_statement_projects_statement_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_statement_projects
    ADD CONSTRAINT iam_statement_projects_statement_id_fkey FOREIGN KEY (statement_id) REFERENCES public.iam_statements(db_id) ON DELETE CASCADE;


--
-- Name: iam_statements iam_statements_policy_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_statements
    ADD CONSTRAINT iam_statements_policy_id_fkey FOREIGN KEY (policy_id) REFERENCES public.iam_policies(db_id) ON DELETE CASCADE DEFERRABLE;


--
-- Name: iam_statements iam_statements_role_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.iam_statements
    ADD CONSTRAINT iam_statements_role_id_fkey FOREIGN KEY (role_id) REFERENCES public.iam_roles(db_id) DEFERRABLE INITIALLY DEFERRED;


--
-- PostgreSQL database dump complete
--

--
-- Database "chef_cereal_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: chef_cereal_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE chef_cereal_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE chef_cereal_service OWNER TO postgres;

\connect chef_cereal_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: cereal_task_state; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.cereal_task_state AS ENUM (
    'queued',
    'running'
);


ALTER TYPE public.cereal_task_state OWNER TO postgres;

--
-- Name: cereal_task_status; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.cereal_task_status AS ENUM (
    'success',
    'failed',
    'lost'
);


ALTER TYPE public.cereal_task_status OWNER TO postgres;

--
-- Name: cereal_workflow_event_type; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.cereal_workflow_event_type AS ENUM (
    'start',
    'task_complete',
    'cancel'
);


ALTER TYPE public.cereal_workflow_event_type OWNER TO postgres;

--
-- Name: cereal_workflow_instance_status; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.cereal_workflow_instance_status AS ENUM (
    'starting',
    'running',
    'completed'
);


ALTER TYPE public.cereal_workflow_instance_status OWNER TO postgres;

--
-- Name: cereal_cancel_workflow(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_cancel_workflow(_instance_name text, _workflow_name text) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO cereal_workflow_events(event_type, workflow_instance_id)
        (SELECT 'cancel', id FROM cereal_workflow_instances WHERE instance_name = _instance_name
            AND workflow_name = _workflow_name);
    RETURN FOUND;
END
$$;


ALTER FUNCTION public.cereal_cancel_workflow(_instance_name text, _workflow_name text) OWNER TO postgres;

--
-- Name: cereal_complete_task(bigint, public.cereal_task_status, text, bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_complete_task(_tid bigint, _status public.cereal_task_status, _error text, _result bytea) RETURNS SETOF bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
    t cereal_tasks%rowtype;
    task_results_id BIGINT;
BEGIN
    FOR t IN
         SELECT * FROM cereal_tasks WHERE id = _tid FOR UPDATE
    LOOP
        INSERT INTO cereal_task_results(task_id, workflow_instance_id, parameters, task_name, enqueued_at, status, error, result)
        VALUES(t.id, t.workflow_instance_id, t.parameters, t.task_name, t.enqueued_at, _status, _error, _result)
        RETURNING id INTO task_results_id;

        INSERT INTO cereal_workflow_events(event_type, task_result_id, workflow_instance_id)
        VALUES('task_complete', task_results_id, t.workflow_instance_id);

        DELETE FROM cereal_tasks WHERE id = t.id;

        RETURN NEXT t.id;
    END LOOP;
    IF NOT FOUND THEN
        RAISE check_violation USING MESSAGE = 'Failed to update task: no such task_id';
    END IF;
END
$$;


ALTER FUNCTION public.cereal_complete_task(_tid bigint, _status public.cereal_task_status, _error text, _result bytea) OWNER TO postgres;

--
-- Name: cereal_complete_workflow(bigint, bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_complete_workflow(_wid bigint, _result bytea) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO cereal_workflow_results(instance_name, workflow_name, parameters, start_at, result)
        (SELECT instance_name, workflow_name, parameters, start_at, _result FROM cereal_workflow_instances WHERE id = _wid);

    DELETE FROM cereal_tasks WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_task_results WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_workflow_events WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_workflow_instances WHERE id = _wid;
END
$$;


ALTER FUNCTION public.cereal_complete_workflow(_wid bigint, _result bytea) OWNER TO postgres;

--
-- Name: cereal_continue_workflow(bigint, bigint, bytea, integer, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_continue_workflow(wid bigint, _eid bigint, _payload bytea, _enqueued_tasks integer, _completed_tasks integer) RETURNS void
    LANGUAGE sql
    AS $$
    UPDATE cereal_workflow_instances SET updated_at = NOW(), payload = _payload, status = 'running',
        enqueued_tasks = _enqueued_tasks, completed_tasks = _completed_tasks WHERE id = wid;
    -- We've decided there is more to do but are done processing this event.
    DELETE FROM cereal_workflow_events WHERE id = _eid
$$;


ALTER FUNCTION public.cereal_continue_workflow(wid bigint, _eid bigint, _payload bytea, _enqueued_tasks integer, _completed_tasks integer) OWNER TO postgres;

--
-- Name: cereal_dequeue_task(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_dequeue_task(_task_name text) RETURNS TABLE(id bigint, parameters bytea)
    LANGUAGE plpgsql
    AS $$
DECLARE
    r cereal_tasks%rowtype;
BEGIN
    FOR r IN
        SELECT * FROM cereal_tasks
        WHERE task_name = _task_name AND task_state = 'queued' AND start_after < NOW()
        ORDER BY enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    LOOP
        UPDATE cereal_tasks SET task_state = 'running', updated_at = NOW() WHERE cereal_tasks.id = r.id;

        id := r.id;
        parameters := r.parameters;
        RETURN NEXT;
    END LOOP;
END
$$;


ALTER FUNCTION public.cereal_dequeue_task(_task_name text) OWNER TO postgres;

--
-- Name: cereal_dequeue_task_v2(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_dequeue_task_v2(_task_name text) RETURNS TABLE(id bigint, parameters bytea, enqueued_at timestamp with time zone)
    LANGUAGE plpgsql
    AS $$
DECLARE
    r cereal_tasks%rowtype;
BEGIN
    FOR r IN
        SELECT * FROM cereal_tasks
        WHERE task_name = _task_name AND task_state = 'queued' AND start_after < NOW()
        ORDER BY enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    LOOP
        UPDATE cereal_tasks SET task_state = 'running', updated_at = NOW() WHERE cereal_tasks.id = r.id;

        id := r.id;
        parameters := r.parameters;
        enqueued_at := r.enqueued_at;
        RETURN NEXT;
    END LOOP;
END
$$;


ALTER FUNCTION public.cereal_dequeue_task_v2(_task_name text) OWNER TO postgres;

--
-- Name: cereal_dequeue_workflow(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_dequeue_workflow(VARIADIC _workflow_names text[]) RETURNS TABLE(workflow_instance_id bigint, instance_name text, workflow_name text, status public.cereal_workflow_instance_status, parameters bytea, payload bytea, event_id bigint, event_type public.cereal_workflow_event_type, task_result_id bigint, enqueued_tasks integer, completed_tasks integer)
    LANGUAGE sql
    AS $$
    WITH nextwinst AS (
        SELECT
            a.id id,
            a.instance_name instance_name,
            a.workflow_name workflow_name,
            a.status status,
            a.parameters parameters,
            a.payload payload,
            b.id event_id,
            b.event_type event_type,
            b.task_result_id task_result_id,
            a.enqueued_tasks,
            a.completed_tasks
        FROM cereal_workflow_instances a
        INNER JOIN cereal_workflow_events b ON a.id = b.workflow_instance_id
        WHERE a.workflow_name = ANY(_workflow_names)
        ORDER BY b.enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    ),
    updated AS (
        UPDATE cereal_workflow_instances w1 SET updated_at = NOW()
        WHERE w1.id = (
            SELECT id FROM nextwinst
        )
    )
    SELECT * from nextwinst
$$;


ALTER FUNCTION public.cereal_dequeue_workflow(VARIADIC _workflow_names text[]) OWNER TO postgres;

--
-- Name: cereal_dequeue_workflow_v2(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_dequeue_workflow_v2(VARIADIC _workflow_names text[]) RETURNS TABLE(workflow_instance_id bigint, instance_name text, workflow_name text, status public.cereal_workflow_instance_status, parameters bytea, payload bytea, event_id bigint, event_type public.cereal_workflow_event_type, task_result_id bigint, enqueued_tasks integer, completed_tasks integer, enqueued_at timestamp with time zone)
    LANGUAGE sql
    AS $$
    WITH nextwinst AS (
        SELECT
            a.id id,
            a.instance_name instance_name,
            a.workflow_name workflow_name,
            a.status status,
            a.parameters parameters,
            a.payload payload,
            b.id event_id,
            b.event_type event_type,
            b.task_result_id task_result_id,
            a.enqueued_tasks,
            a.completed_tasks,
            b.enqueued_at
        FROM cereal_workflow_instances a
        INNER JOIN cereal_workflow_events b ON a.id = b.workflow_instance_id
        WHERE a.workflow_name = ANY(_workflow_names)
        ORDER BY b.enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    ),
    updated AS (
        UPDATE cereal_workflow_instances w1 SET updated_at = NOW()
        WHERE w1.id = (
            SELECT id FROM nextwinst
        )
    )
    SELECT * from nextwinst
$$;


ALTER FUNCTION public.cereal_dequeue_workflow_v2(VARIADIC _workflow_names text[]) OWNER TO postgres;

--
-- Name: cereal_enqueue_task(bigint, timestamp with time zone, text, bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_enqueue_task(_workflow_instance_id bigint, _start_after timestamp with time zone, _task_name text, _parameters bytea) RETURNS void
    LANGUAGE sql
    AS $$
    INSERT INTO cereal_tasks(workflow_instance_id, start_after, task_name, parameters)
        VALUES(_workflow_instance_id, _start_after, _task_name, _parameters);
$$;


ALTER FUNCTION public.cereal_enqueue_task(_workflow_instance_id bigint, _start_after timestamp with time zone, _task_name text, _parameters bytea) OWNER TO postgres;

--
-- Name: cereal_enqueue_workflow(text, text, bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_enqueue_workflow(_instance_name text, _workflow_name text, _parameters bytea) RETURNS integer
    LANGUAGE sql
    AS $$
    WITH winst AS (
        INSERT INTO cereal_workflow_instances(instance_name, workflow_name, parameters)
            VALUES(_instance_name, _workflow_name, _parameters)
            ON CONFLICT DO NOTHING
            RETURNING id
        )
    INSERT INTO cereal_workflow_events(event_type, workflow_instance_id)
    (SELECT 'start', id FROM winst WHERE id IS NOT NULL)
    RETURNING 1
$$;


ALTER FUNCTION public.cereal_enqueue_workflow(_instance_name text, _workflow_name text, _parameters bytea) OWNER TO postgres;

--
-- Name: cereal_expire_tasks(bigint); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_expire_tasks(_task_timeout_seconds bigint) RETURNS TABLE(tid bigint, workflow_instance_id bigint)
    LANGUAGE plpgsql
    AS $$
DECLARE
    t cereal_tasks%rowtype;
    task_results_id BIGINT;
BEGIN
    FOR t IN
        SELECT *
        FROM cereal_tasks
        WHERE
            task_state = 'running' AND
            updated_at < NOW() - (_task_timeout_seconds || ' seconds')::interval
        FOR UPDATE SKIP LOCKED
    LOOP
        DELETE FROM cereal_tasks WHERE id = t.id;

        INSERT INTO cereal_task_results(task_id, workflow_instance_id, parameters, task_name, enqueued_at, status)
            VALUES(t.id, t.workflow_instance_id, t.parameters, t.task_name, t.enqueued_at, 'lost')
            RETURNING id INTO task_results_id;

        INSERT INTO cereal_workflow_events(event_type, task_result_id, workflow_instance_id)
            VALUES('task_complete', task_results_id, t.workflow_instance_id);

        tid := t.id;
        workflow_instance_id := t.workflow_instance_id;

        RETURN NEXT;
    END LOOP;
END
$$;


ALTER FUNCTION public.cereal_expire_tasks(_task_timeout_seconds bigint) OWNER TO postgres;

--
-- Name: cereal_fail_workflow(bigint, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_fail_workflow(_wid bigint, _error text) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO cereal_workflow_results(instance_name, workflow_name, parameters, start_at, error)
        (SELECT instance_name, workflow_name, parameters, start_at, _error FROM cereal_workflow_instances WHERE id = _wid);

    DELETE FROM cereal_tasks WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_task_results WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_workflow_events WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_workflow_instances WHERE id = _wid;
END
$$;


ALTER FUNCTION public.cereal_fail_workflow(_wid bigint, _error text) OWNER TO postgres;

--
-- Name: cereal_kill_workflow(text, text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_kill_workflow(_instance_name text, _workflow_name text, _error text) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
DECLARE
    w cereal_workflow_instances%rowtype;
BEGIN
    FOR w IN
        SELECT id FROM cereal_workflow_instances WHERE instance_name = _instance_name AND workflow_name = _workflow_name FOR UPDATE
    LOOP
        PERFORM cereal_fail_workflow(w.id, _error);
    END LOOP;
    RETURN FOUND;
END
$$;


ALTER FUNCTION public.cereal_kill_workflow(_instance_name text, _workflow_name text, _error text) OWNER TO postgres;

--
-- Name: cereal_ping_task(bigint); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_ping_task(_task_id bigint) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    UPDATE cereal_tasks SET updated_at = NOW() WHERE id = _task_id;
    IF NOT FOUND THEN
        RAISE check_violation USING MESSAGE = 'Failed to update task: no such task_id';
    END IF;
END
$$;


ALTER FUNCTION public.cereal_ping_task(_task_id bigint) OWNER TO postgres;

--
-- Name: cereal_update_workflow_schedule_enabled(bigint, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_update_workflow_schedule_enabled(_id bigint, _enabled boolean) RETURNS void
    LANGUAGE sql
    AS $$
    WITH sched AS (
        SELECT * FROM cereal_workflow_schedules WHERE id = _id FOR UPDATE
    )
    UPDATE cereal_workflow_schedules SET enabled = _enabled WHERE id = _id;
$$;


ALTER FUNCTION public.cereal_update_workflow_schedule_enabled(_id bigint, _enabled boolean) OWNER TO postgres;

--
-- Name: cereal_update_workflow_schedule_parameters(bigint, bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_update_workflow_schedule_parameters(_id bigint, _parameters bytea) RETURNS void
    LANGUAGE sql
    AS $$
    WITH sched AS (
        SELECT * FROM cereal_workflow_schedules WHERE id = _id FOR UPDATE
    )
    UPDATE cereal_workflow_schedules SET parameters = _parameters WHERE id = _id;
$$;


ALTER FUNCTION public.cereal_update_workflow_schedule_parameters(_id bigint, _parameters bytea) OWNER TO postgres;

--
-- Name: cereal_update_workflow_schedule_recurrence(bigint, text, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_update_workflow_schedule_recurrence(_id bigint, _recurrence text, _next_run_at timestamp with time zone) RETURNS void
    LANGUAGE sql
    AS $$
    WITH sched AS (
        SELECT * FROM cereal_workflow_schedules WHERE id = _id FOR UPDATE
    )
    UPDATE cereal_workflow_schedules
    SET
        recurrence = _recurrence,
        next_run_at = _next_run_at WHERE id = _id;
$$;


ALTER FUNCTION public.cereal_update_workflow_schedule_recurrence(_id bigint, _recurrence text, _next_run_at timestamp with time zone) OWNER TO postgres;

--
-- Name: cereal_workflow_clean_workflow_results(bigint, bigint); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_workflow_clean_workflow_results(_max_size bigint, _margin bigint) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
    count BIGINT;
BEGIN
SELECT COUNT(*) FROM cereal_workflow_results INTO count;
IF count > _max_size THEN
    DELETE FROM cereal_workflow_results
    WHERE id IN (
        SELECT id FROM cereal_workflow_results
        ORDER BY end_at DESC OFFSET _max_size - _margin
    );
    GET DIAGNOSTICS count = ROW_COUNT;
    RETURN count;
ELSE
    RETURN 0;
END IF;

END
$$;


ALTER FUNCTION public.cereal_workflow_clean_workflow_results(_max_size bigint, _margin bigint) OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: cereal_schema_version; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_schema_version (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.cereal_schema_version OWNER TO postgres;

--
-- Name: cereal_task_results; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_task_results (
    id bigint NOT NULL,
    task_id bigint NOT NULL,
    workflow_instance_id bigint NOT NULL,
    parameters bytea,
    task_name text NOT NULL,
    enqueued_at timestamp with time zone NOT NULL,
    completed_at timestamp with time zone DEFAULT now() NOT NULL,
    status public.cereal_task_status,
    error text DEFAULT ''::text,
    result bytea
);


ALTER TABLE public.cereal_task_results OWNER TO postgres;

--
-- Name: cereal_task_results_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cereal_task_results_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cereal_task_results_id_seq OWNER TO postgres;

--
-- Name: cereal_task_results_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cereal_task_results_id_seq OWNED BY public.cereal_task_results.id;


--
-- Name: cereal_tasks; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_tasks (
    id bigint NOT NULL,
    workflow_instance_id bigint NOT NULL,
    enqueued_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    start_after timestamp with time zone DEFAULT now() NOT NULL,
    task_name text NOT NULL,
    parameters bytea,
    task_state public.cereal_task_state DEFAULT 'queued'::public.cereal_task_state NOT NULL
);


ALTER TABLE public.cereal_tasks OWNER TO postgres;

--
-- Name: cereal_tasks_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cereal_tasks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cereal_tasks_id_seq OWNER TO postgres;

--
-- Name: cereal_tasks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cereal_tasks_id_seq OWNED BY public.cereal_tasks.id;


--
-- Name: cereal_workflow_events; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_workflow_events (
    id bigint NOT NULL,
    event_type public.cereal_workflow_event_type NOT NULL,
    workflow_instance_id bigint,
    enqueued_at timestamp with time zone DEFAULT now() NOT NULL,
    task_result_id bigint
);


ALTER TABLE public.cereal_workflow_events OWNER TO postgres;

--
-- Name: cereal_workflow_events_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cereal_workflow_events_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cereal_workflow_events_id_seq OWNER TO postgres;

--
-- Name: cereal_workflow_events_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cereal_workflow_events_id_seq OWNED BY public.cereal_workflow_events.id;


--
-- Name: cereal_workflow_instances; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_workflow_instances (
    id bigint NOT NULL,
    instance_name text NOT NULL,
    workflow_name text NOT NULL,
    parameters bytea,
    payload bytea,
    start_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    enqueued_tasks integer DEFAULT 0 NOT NULL,
    completed_tasks integer DEFAULT 0 NOT NULL,
    status public.cereal_workflow_instance_status DEFAULT 'starting'::public.cereal_workflow_instance_status NOT NULL
);


ALTER TABLE public.cereal_workflow_instances OWNER TO postgres;

--
-- Name: cereal_workflow_instances_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cereal_workflow_instances_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cereal_workflow_instances_id_seq OWNER TO postgres;

--
-- Name: cereal_workflow_instances_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cereal_workflow_instances_id_seq OWNED BY public.cereal_workflow_instances.id;


--
-- Name: cereal_workflow_results; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_workflow_results (
    id bigint NOT NULL,
    instance_name text NOT NULL,
    workflow_name text NOT NULL,
    parameters bytea,
    start_at timestamp with time zone NOT NULL,
    end_at timestamp with time zone DEFAULT now() NOT NULL,
    error text,
    result bytea
);


ALTER TABLE public.cereal_workflow_results OWNER TO postgres;

--
-- Name: cereal_workflow_results_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cereal_workflow_results_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cereal_workflow_results_id_seq OWNER TO postgres;

--
-- Name: cereal_workflow_results_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cereal_workflow_results_id_seq OWNED BY public.cereal_workflow_results.id;


--
-- Name: cereal_workflow_schedules; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_workflow_schedules (
    id bigint NOT NULL,
    instance_name text NOT NULL,
    workflow_name text NOT NULL,
    parameters bytea,
    recurrence text,
    enabled boolean,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    last_enqueued_at timestamp with time zone,
    next_run_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.cereal_workflow_schedules OWNER TO postgres;

--
-- Name: cereal_workflow_schedules_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cereal_workflow_schedules_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cereal_workflow_schedules_id_seq OWNER TO postgres;

--
-- Name: cereal_workflow_schedules_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cereal_workflow_schedules_id_seq OWNED BY public.cereal_workflow_schedules.id;


--
-- Name: cereal_task_results id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_task_results ALTER COLUMN id SET DEFAULT nextval('public.cereal_task_results_id_seq'::regclass);


--
-- Name: cereal_tasks id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_tasks ALTER COLUMN id SET DEFAULT nextval('public.cereal_tasks_id_seq'::regclass);


--
-- Name: cereal_workflow_events id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_events ALTER COLUMN id SET DEFAULT nextval('public.cereal_workflow_events_id_seq'::regclass);


--
-- Name: cereal_workflow_instances id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_instances ALTER COLUMN id SET DEFAULT nextval('public.cereal_workflow_instances_id_seq'::regclass);


--
-- Name: cereal_workflow_results id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_results ALTER COLUMN id SET DEFAULT nextval('public.cereal_workflow_results_id_seq'::regclass);


--
-- Name: cereal_workflow_schedules id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_schedules ALTER COLUMN id SET DEFAULT nextval('public.cereal_workflow_schedules_id_seq'::regclass);


--
-- Data for Name: cereal_schema_version; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_schema_version (version, dirty) FROM stdin;
3	f
\.


--
-- Data for Name: cereal_task_results; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_task_results (id, task_id, workflow_instance_id, parameters, task_name, enqueued_at, completed_at, status, error, result) FROM stdin;
\.


--
-- Data for Name: cereal_tasks; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_tasks (id, workflow_instance_id, enqueued_at, updated_at, start_after, task_name, parameters, task_state) FROM stdin;
\.


--
-- Data for Name: cereal_workflow_events; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_workflow_events (id, event_type, workflow_instance_id, enqueued_at, task_result_id) FROM stdin;
\.


--
-- Data for Name: cereal_workflow_instances; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_workflow_instances (id, instance_name, workflow_name, parameters, payload, start_at, updated_at, enqueued_tasks, completed_tasks, status) FROM stdin;
\.


--
-- Data for Name: cereal_workflow_results; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_workflow_results (id, instance_name, workflow_name, parameters, start_at, end_at, error, result) FROM stdin;
1	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:20:29.971136+00	2022-06-03 09:20:40.220255+00	\N	\\x6e756c6c
2	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:20:34.024751+00	2022-06-03 09:20:40.233092+00	\N	\\x6e756c6c
3	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:21:05.000386+00	2022-06-03 09:21:08.313538+00	\N	\\x6e756c6c
4	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:21:06.925759+00	2022-06-03 09:21:10.343561+00	\N	\\x6e756c6c
5	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:22:05.000305+00	2022-06-03 09:22:08.459658+00	\N	\\x6e756c6c
6	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:22:06.945049+00	2022-06-03 09:22:10.482644+00	\N	\\x6e756c6c
7	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:23:05.000405+00	2022-06-03 09:23:08.603754+00	\N	\\x6e756c6c
8	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:23:07.149742+00	2022-06-03 09:23:10.630168+00	\N	\\x6e756c6c
9	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:24:05.000346+00	2022-06-03 09:24:08.754328+00	\N	\\x6e756c6c
10	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:24:06.908268+00	2022-06-03 09:24:10.778608+00	\N	\\x6e756c6c
11	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:25:05.000324+00	2022-06-03 09:25:08.896668+00	\N	\\x6e756c6c
12	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:25:06.943328+00	2022-06-03 09:25:08.902661+00	\N	\\x6e756c6c
13	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:26:05.000426+00	2022-06-03 09:26:07.023435+00	\N	\\x6e756c6c
14	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:26:06.9829+00	2022-06-03 09:26:11.057709+00	\N	\\x6e756c6c
15	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:27:05.000428+00	2022-06-03 09:27:07.171528+00	\N	\\x6e756c6c
16	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:27:06.895821+00	2022-06-03 09:27:09.195514+00	\N	\\x6e756c6c
17	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:28:05.000361+00	2022-06-03 09:28:07.303974+00	\N	\\x6e756c6c
18	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:28:07.0136+00	2022-06-03 09:28:09.329228+00	\N	\\x6e756c6c
19	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:29:05.000499+00	2022-06-03 09:29:07.441547+00	\N	\\x6e756c6c
20	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:29:06.993052+00	2022-06-03 09:29:09.464207+00	\N	\\x6e756c6c
21	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:30:05.000447+00	2022-06-03 09:30:07.576141+00	\N	\\x6e756c6c
22	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:30:06.907251+00	2022-06-03 09:30:09.599058+00	\N	\\x6e756c6c
23	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:31:05.000321+00	2022-06-03 09:31:07.713562+00	\N	\\x6e756c6c
24	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:31:07.057091+00	2022-06-03 09:31:09.734591+00	\N	\\x6e756c6c
25	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:32:05.000484+00	2022-06-03 09:32:07.850132+00	\N	\\x6e756c6c
26	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:32:07.014732+00	2022-06-03 09:32:09.873569+00	\N	\\x6e756c6c
27	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:33:05.000424+00	2022-06-03 09:33:07.996035+00	\N	\\x6e756c6c
28	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:33:06.900428+00	2022-06-03 09:33:10.019876+00	\N	\\x6e756c6c
29	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:34:05.000377+00	2022-06-03 09:34:08.145282+00	\N	\\x6e756c6c
30	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:34:07.098109+00	2022-06-03 09:34:10.16782+00	\N	\\x6e756c6c
31	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:35:05.000421+00	2022-06-03 09:35:08.296156+00	\N	\\x6e756c6c
32	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:35:06.991258+00	2022-06-03 09:35:10.319408+00	\N	\\x6e756c6c
33	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:36:05.000402+00	2022-06-03 09:36:08.441637+00	\N	\\x6e756c6c
34	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:36:06.891593+00	2022-06-03 09:36:10.463099+00	\N	\\x6e756c6c
35	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:37:05.000478+00	2022-06-03 09:37:08.580755+00	\N	\\x6e756c6c
36	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:37:07.11136+00	2022-06-03 09:37:10.601529+00	\N	\\x6e756c6c
37	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:38:05.00042+00	2022-06-03 09:38:08.718825+00	\N	\\x6e756c6c
38	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:38:06.969017+00	2022-06-03 09:38:10.742146+00	\N	\\x6e756c6c
39	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:39:05.000377+00	2022-06-03 09:39:08.851401+00	\N	\\x6e756c6c
40	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:39:06.887232+00	2022-06-03 09:39:08.85778+00	\N	\\x6e756c6c
41	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:40:05.000358+00	2022-06-03 09:40:08.980276+00	\N	\\x6e756c6c
42	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:40:07.095199+00	2022-06-03 09:40:11.002239+00	\N	\\x6e756c6c
43	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:41:05.00032+00	2022-06-03 09:41:07.121244+00	\N	\\x6e756c6c
44	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:41:06.997101+00	2022-06-03 09:41:09.144633+00	\N	\\x6e756c6c
45	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:42:05.000433+00	2022-06-03 09:42:07.271397+00	\N	\\x6e756c6c
46	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:42:06.896134+00	2022-06-03 09:42:09.293396+00	\N	\\x6e756c6c
47	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:43:05.00049+00	2022-06-03 09:43:07.415105+00	\N	\\x6e756c6c
48	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:43:07.104736+00	2022-06-03 09:43:09.439395+00	\N	\\x6e756c6c
49	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:44:05.000353+00	2022-06-03 09:44:07.552611+00	\N	\\x6e756c6c
50	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:44:06.976889+00	2022-06-03 09:44:09.572649+00	\N	\\x6e756c6c
51	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:45:05.000445+00	2022-06-03 09:45:07.687115+00	\N	\\x6e756c6c
52	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:45:06.913076+00	2022-06-03 09:45:09.708338+00	\N	\\x6e756c6c
53	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:46:05.00028+00	2022-06-03 09:46:07.828145+00	\N	\\x6e756c6c
54	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:46:07.092632+00	2022-06-03 09:46:09.85112+00	\N	\\x6e756c6c
55	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:47:05.000341+00	2022-06-03 09:47:07.96676+00	\N	\\x6e756c6c
56	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:47:06.96939+00	2022-06-03 09:47:09.990373+00	\N	\\x6e756c6c
57	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:48:05.000387+00	2022-06-03 09:48:08.113003+00	\N	\\x6e756c6c
58	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:48:06.886939+00	2022-06-03 09:48:10.136719+00	\N	\\x6e756c6c
59	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:49:05.000503+00	2022-06-03 09:49:08.256226+00	\N	\\x6e756c6c
60	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:49:07.12108+00	2022-06-03 09:49:10.278489+00	\N	\\x6e756c6c
61	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:50:05.000483+00	2022-06-03 09:50:08.400356+00	\N	\\x6e756c6c
62	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:50:07.031239+00	2022-06-03 09:50:10.424144+00	\N	\\x6e756c6c
63	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:51:05.000479+00	2022-06-03 09:51:08.541972+00	\N	\\x6e756c6c
64	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:51:06.891234+00	2022-06-03 09:51:10.567306+00	\N	\\x6e756c6c
65	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:52:05.000497+00	2022-06-03 09:52:08.686348+00	\N	\\x6e756c6c
66	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:52:07.167365+00	2022-06-03 09:52:10.710045+00	\N	\\x6e756c6c
67	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:53:05.000454+00	2022-06-03 09:53:08.837385+00	\N	\\x6e756c6c
68	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:53:06.971822+00	2022-06-03 09:53:10.859452+00	\N	\\x6e756c6c
69	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:54:05.000418+00	2022-06-03 09:54:06.980609+00	\N	\\x6e756c6c
70	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:54:06.896633+00	2022-06-03 09:54:09.002813+00	\N	\\x6e756c6c
71	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:55:05.000371+00	2022-06-03 09:55:07.13135+00	\N	\\x6e756c6c
72	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:55:07.086336+00	2022-06-03 09:55:09.18871+00	\N	\\x6e756c6c
73	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:56:05.000375+00	2022-06-03 09:56:07.308016+00	\N	\\x6e756c6c
74	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:56:06.89785+00	2022-06-03 09:56:09.335372+00	\N	\\x6e756c6c
75	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:57:05.000414+00	2022-06-03 09:57:07.454443+00	\N	\\x6e756c6c
76	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:57:07.301939+00	2022-06-03 09:57:09.475358+00	\N	\\x6e756c6c
77	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:58:05.000503+00	2022-06-03 09:58:07.592563+00	\N	\\x6e756c6c
78	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:58:07.11618+00	2022-06-03 09:58:09.615177+00	\N	\\x6e756c6c
79	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 09:59:05.000403+00	2022-06-03 09:59:07.736109+00	\N	\\x6e756c6c
80	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 09:59:07.032128+00	2022-06-03 09:59:09.76013+00	\N	\\x6e756c6c
81	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 10:00:05.000425+00	2022-06-03 10:00:07.883429+00	\N	\\x6e756c6c
82	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 10:00:06.867745+00	2022-06-03 10:00:09.906496+00	\N	\\x6e756c6c
83	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 10:01:05.000355+00	2022-06-03 10:01:08.023895+00	\N	\\x6e756c6c
84	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 10:01:06.980985+00	2022-06-03 10:01:10.04875+00	\N	\\x6e756c6c
85	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 10:02:05.000516+00	2022-06-03 10:02:08.168158+00	\N	\\x6e756c6c
86	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 10:02:06.895359+00	2022-06-03 10:02:10.191341+00	\N	\\x6e756c6c
87	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 10:03:05.000331+00	2022-06-03 10:03:08.315752+00	\N	\\x6e756c6c
88	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 10:03:06.885917+00	2022-06-03 10:03:10.345553+00	\N	\\x6e756c6c
89	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 10:04:05.000321+00	2022-06-03 10:04:08.468511+00	\N	\\x6e756c6c
90	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 10:04:07.004471+00	2022-06-03 10:04:10.499284+00	\N	\\x6e756c6c
91	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 10:05:05.00038+00	2022-06-03 10:05:08.622784+00	\N	\\x6e756c6c
92	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 10:05:06.906174+00	2022-06-03 10:05:10.645768+00	\N	\\x6e756c6c
93	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 10:06:05.000314+00	2022-06-03 10:06:08.760919+00	\N	\\x6e756c6c
94	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 10:06:06.868386+00	2022-06-03 10:06:08.767018+00	\N	\\x6e756c6c
95	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 10:07:05.000399+00	2022-06-03 10:07:08.891794+00	\N	\\x6e756c6c
96	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 10:07:07.03841+00	2022-06-03 10:07:10.91608+00	\N	\\x6e756c6c
97	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 10:08:05.000318+00	2022-06-03 10:08:07.030264+00	\N	\\x6e756c6c
98	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 10:08:06.890959+00	2022-06-03 10:08:09.053273+00	\N	\\x6e756c6c
99	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 10:09:05.000383+00	2022-06-03 10:09:07.171955+00	\N	\\x6e756c6c
100	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 10:09:06.890995+00	2022-06-03 10:09:09.193431+00	\N	\\x6e756c6c
101	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	2022-06-03 10:10:05.000409+00	2022-06-03 10:10:07.306291+00	\N	\\x6e756c6c
102	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	2022-06-03 10:10:07.033149+00	2022-06-03 10:10:09.331218+00	\N	\\x6e756c6c
\.


--
-- Data for Name: cereal_workflow_schedules; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_workflow_schedules (id, instance_name, workflow_name, parameters, recurrence, enabled, created_at, last_enqueued_at, next_run_at) FROM stdin;
1	awsec2_polling_schedule	nodemanager-service/awsec2_polling	\\x	FREQ=MINUTELY;DTSTART=20220603T092031Z;INTERVAL=60	t	2022-06-03 09:19:25.149557+00	\N	2022-06-03 10:20:31+00
2	azurevm_polling_schedule	nodemanager-service/azurevm_polling	\\x	FREQ=MINUTELY;DTSTART=20220603T092031Z;INTERVAL=180	t	2022-06-03 09:19:25.166601+00	\N	2022-06-03 12:20:31+00
3	managers_polling_schedule	nodemanager-service/managers_polling	\\x	FREQ=MINUTELY;DTSTART=20220603T092031Z;INTERVAL=120	t	2022-06-03 09:19:25.171169+00	\N	2022-06-03 11:20:31+00
4	periodic-data-feed-workflow	data-feed-service/data-feed-workflow	\\x7b22466565645374617274223a22303030312d30312d30315430303a30303a30305a222c2246656564456e64223a22303030312d30312d30315430303a30303a30305a227d	FREQ=SECONDLY;DTSTART=20220603T080030Z;INTERVAL=14400	t	2022-06-03 09:19:26.937247+00	\N	2022-06-03 12:00:30+00
5	periodic_disconnected_services	applications/disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a22356d227d	FREQ=SECONDLY;DTSTART=20200612T182105Z;INTERVAL=60	t	2022-06-03 09:19:30.409646+00	2022-06-03 10:10:05.002057+00	2022-06-03 10:11:05+00
6	periodic_delete_disconnected_services	applications/delete_disconnected_services	\\x7b225468726573686f6c644475726174696f6e223a223764227d	FREQ=SECONDLY;DTSTART=20200612T182105Z;INTERVAL=60	t	2022-06-03 09:19:32.338094+00	2022-06-03 10:10:07.033827+00	2022-06-03 10:11:05+00
\.


--
-- Name: cereal_task_results_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.cereal_task_results_id_seq', 102, true);


--
-- Name: cereal_tasks_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.cereal_tasks_id_seq', 102, true);


--
-- Name: cereal_workflow_events_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.cereal_workflow_events_id_seq', 204, true);


--
-- Name: cereal_workflow_instances_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.cereal_workflow_instances_id_seq', 102, true);


--
-- Name: cereal_workflow_results_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.cereal_workflow_results_id_seq', 102, true);


--
-- Name: cereal_workflow_schedules_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.cereal_workflow_schedules_id_seq', 12, true);


--
-- Name: cereal_schema_version cereal_schema_version_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_schema_version
    ADD CONSTRAINT cereal_schema_version_pkey PRIMARY KEY (version);


--
-- Name: cereal_task_results cereal_task_results_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_task_results
    ADD CONSTRAINT cereal_task_results_pkey PRIMARY KEY (id);


--
-- Name: cereal_task_results cereal_task_results_task_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_task_results
    ADD CONSTRAINT cereal_task_results_task_id_key UNIQUE (task_id);


--
-- Name: cereal_tasks cereal_tasks_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_tasks
    ADD CONSTRAINT cereal_tasks_pkey PRIMARY KEY (id);


--
-- Name: cereal_workflow_events cereal_workflow_events_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_events
    ADD CONSTRAINT cereal_workflow_events_pkey PRIMARY KEY (id);


--
-- Name: cereal_workflow_instances cereal_workflow_instances_name; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_instances
    ADD CONSTRAINT cereal_workflow_instances_name UNIQUE (instance_name, workflow_name);


--
-- Name: cereal_workflow_instances cereal_workflow_instances_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_instances
    ADD CONSTRAINT cereal_workflow_instances_pkey PRIMARY KEY (id);


--
-- Name: cereal_workflow_results cereal_workflow_results_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_results
    ADD CONSTRAINT cereal_workflow_results_pkey PRIMARY KEY (id);


--
-- Name: cereal_workflow_schedules cereal_workflow_schedules_name; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_schedules
    ADD CONSTRAINT cereal_workflow_schedules_name UNIQUE (instance_name, workflow_name);


--
-- Name: cereal_workflow_schedules cereal_workflow_schedules_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_schedules
    ADD CONSTRAINT cereal_workflow_schedules_pkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--

--
-- Database "chef_compliance_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: chef_compliance_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE chef_compliance_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE chef_compliance_service OWNER TO postgres;

\connect chef_compliance_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: cereal_task_state; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.cereal_task_state AS ENUM (
    'queued',
    'running'
);


ALTER TYPE public.cereal_task_state OWNER TO postgres;

--
-- Name: cereal_task_status; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.cereal_task_status AS ENUM (
    'success',
    'failed',
    'lost'
);


ALTER TYPE public.cereal_task_status OWNER TO postgres;

--
-- Name: cereal_workflow_event_type; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.cereal_workflow_event_type AS ENUM (
    'start',
    'task_complete',
    'cancel'
);


ALTER TYPE public.cereal_workflow_event_type OWNER TO postgres;

--
-- Name: cereal_workflow_instance_status; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.cereal_workflow_instance_status AS ENUM (
    'starting',
    'running',
    'completed'
);


ALTER TYPE public.cereal_workflow_instance_status OWNER TO postgres;

--
-- Name: cereal_cancel_workflow(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_cancel_workflow(_instance_name text, _workflow_name text) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO cereal_workflow_events(event_type, workflow_instance_id)
        (SELECT 'cancel', id FROM cereal_workflow_instances WHERE instance_name = _instance_name
            AND workflow_name = _workflow_name);
    RETURN FOUND;
END
$$;


ALTER FUNCTION public.cereal_cancel_workflow(_instance_name text, _workflow_name text) OWNER TO postgres;

--
-- Name: cereal_complete_task(bigint, public.cereal_task_status, text, bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_complete_task(_tid bigint, _status public.cereal_task_status, _error text, _result bytea) RETURNS SETOF bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
    t cereal_tasks%rowtype;
    task_results_id BIGINT;
BEGIN
    FOR t IN
         SELECT * FROM cereal_tasks WHERE id = _tid FOR UPDATE
    LOOP
        INSERT INTO cereal_task_results(task_id, workflow_instance_id, parameters, task_name, enqueued_at, status, error, result)
        VALUES(t.id, t.workflow_instance_id, t.parameters, t.task_name, t.enqueued_at, _status, _error, _result)
        RETURNING id INTO task_results_id;

        INSERT INTO cereal_workflow_events(event_type, task_result_id, workflow_instance_id)
        VALUES('task_complete', task_results_id, t.workflow_instance_id);

        DELETE FROM cereal_tasks WHERE id = t.id;

        RETURN NEXT t.id;
    END LOOP;
    IF NOT FOUND THEN
        RAISE check_violation USING MESSAGE = 'Failed to update task: no such task_id';
    END IF;
END
$$;


ALTER FUNCTION public.cereal_complete_task(_tid bigint, _status public.cereal_task_status, _error text, _result bytea) OWNER TO postgres;

--
-- Name: cereal_complete_workflow(bigint, bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_complete_workflow(_wid bigint, _result bytea) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO cereal_workflow_results(instance_name, workflow_name, parameters, start_at, result)
        (SELECT instance_name, workflow_name, parameters, start_at, _result FROM cereal_workflow_instances WHERE id = _wid);

    DELETE FROM cereal_tasks WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_task_results WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_workflow_events WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_workflow_instances WHERE id = _wid;
END
$$;


ALTER FUNCTION public.cereal_complete_workflow(_wid bigint, _result bytea) OWNER TO postgres;

--
-- Name: cereal_continue_workflow(bigint, bigint, bytea, integer, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_continue_workflow(wid bigint, _eid bigint, _payload bytea, _enqueued_tasks integer, _completed_tasks integer) RETURNS void
    LANGUAGE sql
    AS $$
    UPDATE cereal_workflow_instances SET updated_at = NOW(), payload = _payload, status = 'running',
        enqueued_tasks = _enqueued_tasks, completed_tasks = _completed_tasks WHERE id = wid;
    -- We've decided there is more to do but are done processing this event.
    DELETE FROM cereal_workflow_events WHERE id = _eid
$$;


ALTER FUNCTION public.cereal_continue_workflow(wid bigint, _eid bigint, _payload bytea, _enqueued_tasks integer, _completed_tasks integer) OWNER TO postgres;

--
-- Name: cereal_dequeue_task(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_dequeue_task(_task_name text) RETURNS TABLE(id bigint, parameters bytea)
    LANGUAGE plpgsql
    AS $$
DECLARE
    r cereal_tasks%rowtype;
BEGIN
    FOR r IN
        SELECT * FROM cereal_tasks
        WHERE task_name = _task_name AND task_state = 'queued' AND start_after < NOW()
        ORDER BY enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    LOOP
        UPDATE cereal_tasks SET task_state = 'running', updated_at = NOW() WHERE cereal_tasks.id = r.id;

        id := r.id;
        parameters := r.parameters;
        RETURN NEXT;
    END LOOP;
END
$$;


ALTER FUNCTION public.cereal_dequeue_task(_task_name text) OWNER TO postgres;

--
-- Name: cereal_dequeue_task_v2(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_dequeue_task_v2(_task_name text) RETURNS TABLE(id bigint, parameters bytea, enqueued_at timestamp with time zone)
    LANGUAGE plpgsql
    AS $$
DECLARE
    r cereal_tasks%rowtype;
BEGIN
    FOR r IN
        SELECT * FROM cereal_tasks
        WHERE task_name = _task_name AND task_state = 'queued' AND start_after < NOW()
        ORDER BY enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    LOOP
        UPDATE cereal_tasks SET task_state = 'running', updated_at = NOW() WHERE cereal_tasks.id = r.id;

        id := r.id;
        parameters := r.parameters;
        enqueued_at := r.enqueued_at;
        RETURN NEXT;
    END LOOP;
END
$$;


ALTER FUNCTION public.cereal_dequeue_task_v2(_task_name text) OWNER TO postgres;

--
-- Name: cereal_dequeue_workflow(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_dequeue_workflow(VARIADIC _workflow_names text[]) RETURNS TABLE(workflow_instance_id bigint, instance_name text, workflow_name text, status public.cereal_workflow_instance_status, parameters bytea, payload bytea, event_id bigint, event_type public.cereal_workflow_event_type, task_result_id bigint, enqueued_tasks integer, completed_tasks integer)
    LANGUAGE sql
    AS $$
    WITH nextwinst AS (
        SELECT
            a.id id,
            a.instance_name instance_name,
            a.workflow_name workflow_name,
            a.status status,
            a.parameters parameters,
            a.payload payload,
            b.id event_id,
            b.event_type event_type,
            b.task_result_id task_result_id,
            a.enqueued_tasks,
            a.completed_tasks
        FROM cereal_workflow_instances a
        INNER JOIN cereal_workflow_events b ON a.id = b.workflow_instance_id
        WHERE a.workflow_name = ANY(_workflow_names)
        ORDER BY b.enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    ),
    updated AS (
        UPDATE cereal_workflow_instances w1 SET updated_at = NOW()
        WHERE w1.id = (
            SELECT id FROM nextwinst
        )
    )
    SELECT * from nextwinst
$$;


ALTER FUNCTION public.cereal_dequeue_workflow(VARIADIC _workflow_names text[]) OWNER TO postgres;

--
-- Name: cereal_dequeue_workflow_v2(text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_dequeue_workflow_v2(VARIADIC _workflow_names text[]) RETURNS TABLE(workflow_instance_id bigint, instance_name text, workflow_name text, status public.cereal_workflow_instance_status, parameters bytea, payload bytea, event_id bigint, event_type public.cereal_workflow_event_type, task_result_id bigint, enqueued_tasks integer, completed_tasks integer, enqueued_at timestamp with time zone)
    LANGUAGE sql
    AS $$
    WITH nextwinst AS (
        SELECT
            a.id id,
            a.instance_name instance_name,
            a.workflow_name workflow_name,
            a.status status,
            a.parameters parameters,
            a.payload payload,
            b.id event_id,
            b.event_type event_type,
            b.task_result_id task_result_id,
            a.enqueued_tasks,
            a.completed_tasks,
            b.enqueued_at
        FROM cereal_workflow_instances a
        INNER JOIN cereal_workflow_events b ON a.id = b.workflow_instance_id
        WHERE a.workflow_name = ANY(_workflow_names)
        ORDER BY b.enqueued_at FOR UPDATE SKIP LOCKED LIMIT 1
    ),
    updated AS (
        UPDATE cereal_workflow_instances w1 SET updated_at = NOW()
        WHERE w1.id = (
            SELECT id FROM nextwinst
        )
    )
    SELECT * from nextwinst
$$;


ALTER FUNCTION public.cereal_dequeue_workflow_v2(VARIADIC _workflow_names text[]) OWNER TO postgres;

--
-- Name: cereal_enqueue_task(bigint, timestamp with time zone, text, bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_enqueue_task(_workflow_instance_id bigint, _start_after timestamp with time zone, _task_name text, _parameters bytea) RETURNS void
    LANGUAGE sql
    AS $$
    INSERT INTO cereal_tasks(workflow_instance_id, start_after, task_name, parameters)
        VALUES(_workflow_instance_id, _start_after, _task_name, _parameters);
$$;


ALTER FUNCTION public.cereal_enqueue_task(_workflow_instance_id bigint, _start_after timestamp with time zone, _task_name text, _parameters bytea) OWNER TO postgres;

--
-- Name: cereal_enqueue_workflow(text, text, bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_enqueue_workflow(_instance_name text, _workflow_name text, _parameters bytea) RETURNS integer
    LANGUAGE sql
    AS $$
    WITH winst AS (
        INSERT INTO cereal_workflow_instances(instance_name, workflow_name, parameters)
            VALUES(_instance_name, _workflow_name, _parameters)
            ON CONFLICT DO NOTHING
            RETURNING id
        )
    INSERT INTO cereal_workflow_events(event_type, workflow_instance_id)
    (SELECT 'start', id FROM winst WHERE id IS NOT NULL)
    RETURNING 1
$$;


ALTER FUNCTION public.cereal_enqueue_workflow(_instance_name text, _workflow_name text, _parameters bytea) OWNER TO postgres;

--
-- Name: cereal_expire_tasks(bigint); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_expire_tasks(_task_timeout_seconds bigint) RETURNS TABLE(tid bigint, workflow_instance_id bigint)
    LANGUAGE plpgsql
    AS $$
DECLARE
    t cereal_tasks%rowtype;
    task_results_id BIGINT;
BEGIN
    FOR t IN
        SELECT *
        FROM cereal_tasks
        WHERE
            task_state = 'running' AND
            updated_at < NOW() - (_task_timeout_seconds || ' seconds')::interval
        FOR UPDATE SKIP LOCKED
    LOOP
        DELETE FROM cereal_tasks WHERE id = t.id;

        INSERT INTO cereal_task_results(task_id, workflow_instance_id, parameters, task_name, enqueued_at, status)
            VALUES(t.id, t.workflow_instance_id, t.parameters, t.task_name, t.enqueued_at, 'lost')
            RETURNING id INTO task_results_id;

        INSERT INTO cereal_workflow_events(event_type, task_result_id, workflow_instance_id)
            VALUES('task_complete', task_results_id, t.workflow_instance_id);

        tid := t.id;
        workflow_instance_id := t.workflow_instance_id;

        RETURN NEXT;
    END LOOP;
END
$$;


ALTER FUNCTION public.cereal_expire_tasks(_task_timeout_seconds bigint) OWNER TO postgres;

--
-- Name: cereal_fail_workflow(bigint, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_fail_workflow(_wid bigint, _error text) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO cereal_workflow_results(instance_name, workflow_name, parameters, start_at, error)
        (SELECT instance_name, workflow_name, parameters, start_at, _error FROM cereal_workflow_instances WHERE id = _wid);

    DELETE FROM cereal_tasks WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_task_results WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_workflow_events WHERE workflow_instance_id = _wid;
    DELETE FROM cereal_workflow_instances WHERE id = _wid;
END
$$;


ALTER FUNCTION public.cereal_fail_workflow(_wid bigint, _error text) OWNER TO postgres;

--
-- Name: cereal_kill_workflow(text, text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_kill_workflow(_instance_name text, _workflow_name text, _error text) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
DECLARE
    w cereal_workflow_instances%rowtype;
BEGIN
    FOR w IN
        SELECT id FROM cereal_workflow_instances WHERE instance_name = _instance_name AND workflow_name = _workflow_name FOR UPDATE
    LOOP
        PERFORM cereal_fail_workflow(w.id, _error);
    END LOOP;
    RETURN FOUND;
END
$$;


ALTER FUNCTION public.cereal_kill_workflow(_instance_name text, _workflow_name text, _error text) OWNER TO postgres;

--
-- Name: cereal_ping_task(bigint); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_ping_task(_task_id bigint) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    UPDATE cereal_tasks SET updated_at = NOW() WHERE id = _task_id;
    IF NOT FOUND THEN
        RAISE check_violation USING MESSAGE = 'Failed to update task: no such task_id';
    END IF;
END
$$;


ALTER FUNCTION public.cereal_ping_task(_task_id bigint) OWNER TO postgres;

--
-- Name: cereal_update_workflow_schedule_enabled(bigint, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_update_workflow_schedule_enabled(_id bigint, _enabled boolean) RETURNS void
    LANGUAGE sql
    AS $$
    WITH sched AS (
        SELECT * FROM cereal_workflow_schedules WHERE id = _id FOR UPDATE
    )
    UPDATE cereal_workflow_schedules SET enabled = _enabled WHERE id = _id;
$$;


ALTER FUNCTION public.cereal_update_workflow_schedule_enabled(_id bigint, _enabled boolean) OWNER TO postgres;

--
-- Name: cereal_update_workflow_schedule_parameters(bigint, bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_update_workflow_schedule_parameters(_id bigint, _parameters bytea) RETURNS void
    LANGUAGE sql
    AS $$
    WITH sched AS (
        SELECT * FROM cereal_workflow_schedules WHERE id = _id FOR UPDATE
    )
    UPDATE cereal_workflow_schedules SET parameters = _parameters WHERE id = _id;
$$;


ALTER FUNCTION public.cereal_update_workflow_schedule_parameters(_id bigint, _parameters bytea) OWNER TO postgres;

--
-- Name: cereal_update_workflow_schedule_recurrence(bigint, text, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_update_workflow_schedule_recurrence(_id bigint, _recurrence text, _next_run_at timestamp with time zone) RETURNS void
    LANGUAGE sql
    AS $$
    WITH sched AS (
        SELECT * FROM cereal_workflow_schedules WHERE id = _id FOR UPDATE
    )
    UPDATE cereal_workflow_schedules
    SET
        recurrence = _recurrence,
        next_run_at = _next_run_at WHERE id = _id;
$$;


ALTER FUNCTION public.cereal_update_workflow_schedule_recurrence(_id bigint, _recurrence text, _next_run_at timestamp with time zone) OWNER TO postgres;

--
-- Name: cereal_workflow_clean_workflow_results(bigint, bigint); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cereal_workflow_clean_workflow_results(_max_size bigint, _margin bigint) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
    count BIGINT;
BEGIN
SELECT COUNT(*) FROM cereal_workflow_results INTO count;
IF count > _max_size THEN
    DELETE FROM cereal_workflow_results
    WHERE id IN (
        SELECT id FROM cereal_workflow_results
        ORDER BY end_at DESC OFFSET _max_size - _margin
    );
    GET DIAGNOSTICS count = ROW_COUNT;
    RETURN count;
ELSE
    RETURN 0;
END IF;

END
$$;


ALTER FUNCTION public.cereal_workflow_clean_workflow_results(_max_size bigint, _margin bigint) OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: agents; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.agents (
    id text NOT NULL,
    type text DEFAULT ''::text NOT NULL,
    status text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.agents OWNER TO postgres;

--
-- Name: cereal_schema_version; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_schema_version (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.cereal_schema_version OWNER TO postgres;

--
-- Name: cereal_task_results; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_task_results (
    id bigint NOT NULL,
    task_id bigint NOT NULL,
    workflow_instance_id bigint NOT NULL,
    parameters bytea,
    task_name text NOT NULL,
    enqueued_at timestamp with time zone NOT NULL,
    completed_at timestamp with time zone DEFAULT now() NOT NULL,
    status public.cereal_task_status,
    error text DEFAULT ''::text,
    result bytea
);


ALTER TABLE public.cereal_task_results OWNER TO postgres;

--
-- Name: cereal_task_results_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cereal_task_results_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cereal_task_results_id_seq OWNER TO postgres;

--
-- Name: cereal_task_results_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cereal_task_results_id_seq OWNED BY public.cereal_task_results.id;


--
-- Name: cereal_tasks; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_tasks (
    id bigint NOT NULL,
    workflow_instance_id bigint NOT NULL,
    enqueued_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    start_after timestamp with time zone DEFAULT now() NOT NULL,
    task_name text NOT NULL,
    parameters bytea,
    task_state public.cereal_task_state DEFAULT 'queued'::public.cereal_task_state NOT NULL
);


ALTER TABLE public.cereal_tasks OWNER TO postgres;

--
-- Name: cereal_tasks_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cereal_tasks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cereal_tasks_id_seq OWNER TO postgres;

--
-- Name: cereal_tasks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cereal_tasks_id_seq OWNED BY public.cereal_tasks.id;


--
-- Name: cereal_workflow_events; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_workflow_events (
    id bigint NOT NULL,
    event_type public.cereal_workflow_event_type NOT NULL,
    workflow_instance_id bigint,
    enqueued_at timestamp with time zone DEFAULT now() NOT NULL,
    task_result_id bigint
);


ALTER TABLE public.cereal_workflow_events OWNER TO postgres;

--
-- Name: cereal_workflow_events_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cereal_workflow_events_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cereal_workflow_events_id_seq OWNER TO postgres;

--
-- Name: cereal_workflow_events_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cereal_workflow_events_id_seq OWNED BY public.cereal_workflow_events.id;


--
-- Name: cereal_workflow_instances; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_workflow_instances (
    id bigint NOT NULL,
    instance_name text NOT NULL,
    workflow_name text NOT NULL,
    parameters bytea,
    payload bytea,
    start_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    enqueued_tasks integer DEFAULT 0 NOT NULL,
    completed_tasks integer DEFAULT 0 NOT NULL,
    status public.cereal_workflow_instance_status DEFAULT 'starting'::public.cereal_workflow_instance_status NOT NULL
);


ALTER TABLE public.cereal_workflow_instances OWNER TO postgres;

--
-- Name: cereal_workflow_instances_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cereal_workflow_instances_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cereal_workflow_instances_id_seq OWNER TO postgres;

--
-- Name: cereal_workflow_instances_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cereal_workflow_instances_id_seq OWNED BY public.cereal_workflow_instances.id;


--
-- Name: cereal_workflow_results; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_workflow_results (
    id bigint NOT NULL,
    instance_name text NOT NULL,
    workflow_name text NOT NULL,
    parameters bytea,
    start_at timestamp with time zone NOT NULL,
    end_at timestamp with time zone DEFAULT now() NOT NULL,
    error text,
    result bytea
);


ALTER TABLE public.cereal_workflow_results OWNER TO postgres;

--
-- Name: cereal_workflow_results_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cereal_workflow_results_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cereal_workflow_results_id_seq OWNER TO postgres;

--
-- Name: cereal_workflow_results_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cereal_workflow_results_id_seq OWNED BY public.cereal_workflow_results.id;


--
-- Name: cereal_workflow_schedules; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.cereal_workflow_schedules (
    id bigint NOT NULL,
    instance_name text NOT NULL,
    workflow_name text NOT NULL,
    parameters bytea,
    recurrence text,
    enabled boolean,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    last_enqueued_at timestamp with time zone,
    next_run_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.cereal_workflow_schedules OWNER TO postgres;

--
-- Name: cereal_workflow_schedules_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.cereal_workflow_schedules_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cereal_workflow_schedules_id_seq OWNER TO postgres;

--
-- Name: cereal_workflow_schedules_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.cereal_workflow_schedules_id_seq OWNED BY public.cereal_workflow_schedules.id;


--
-- Name: jobs; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.jobs (
    id text NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    type text DEFAULT ''::text NOT NULL,
    timeout integer,
    retries integer,
    retries_left integer,
    status text DEFAULT 'unknown'::text,
    start_time timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone,
    end_time timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone,
    node_selectors json DEFAULT '[]'::json NOT NULL,
    scheduled_time timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone,
    recurrence text DEFAULT ''::text NOT NULL,
    parent_id text DEFAULT ''::text NOT NULL,
    job_count integer DEFAULT 0 NOT NULL,
    node_count integer,
    deleted boolean NOT NULL
);


ALTER TABLE public.jobs OWNER TO postgres;

--
-- Name: jobs_nodes; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.jobs_nodes (
    job_id text NOT NULL,
    node_id text NOT NULL
);


ALTER TABLE public.jobs_nodes OWNER TO postgres;

--
-- Name: jobs_profiles; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.jobs_profiles (
    job_id text NOT NULL,
    profile_id text NOT NULL
);


ALTER TABLE public.jobs_profiles OWNER TO postgres;

--
-- Name: jobs_tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.jobs_tags (
    job_id text NOT NULL,
    tag_id text NOT NULL
);


ALTER TABLE public.jobs_tags OWNER TO postgres;

--
-- Name: node_managers; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.node_managers (
    id text NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    type text DEFAULT ''::text NOT NULL,
    credentials text DEFAULT ''::text NOT NULL,
    status_type text DEFAULT ''::text NOT NULL,
    status_message text DEFAULT ''::text NOT NULL,
    instance_credentials json DEFAULT '[]'::json NOT NULL,
    account_id text,
    date_added timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone
);


ALTER TABLE public.node_managers OWNER TO postgres;

--
-- Name: node_managers_nodes; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.node_managers_nodes (
    manager_id text NOT NULL,
    node_id text NOT NULL
);


ALTER TABLE public.node_managers_nodes OWNER TO postgres;

--
-- Name: nodes; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nodes (
    id text NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    platform text DEFAULT ''::text NOT NULL,
    platform_version text DEFAULT ''::text NOT NULL,
    status text DEFAULT 'unknown'::text NOT NULL,
    last_contact timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone,
    manager text DEFAULT ''::text NOT NULL,
    target_config json,
    last_job text DEFAULT ''::text NOT NULL,
    source_id text,
    date_added timestamp without time zone DEFAULT now() NOT NULL,
    last_connection_attempt timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone,
    source_region text DEFAULT ''::text,
    source_state text DEFAULT ''::text,
    source_account_id text,
    connection_error text DEFAULT ''::text NOT NULL,
    statechange_timestamp timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone NOT NULL,
    environment text DEFAULT ''::text NOT NULL,
    report_id text DEFAULT ''::text
);


ALTER TABLE public.nodes OWNER TO postgres;

--
-- Name: nodes_agents; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nodes_agents (
    node_id text NOT NULL,
    agent_id text NOT NULL
);


ALTER TABLE public.nodes_agents OWNER TO postgres;

--
-- Name: nodes_projects; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nodes_projects (
    node_id text NOT NULL,
    project_id text NOT NULL,
    CONSTRAINT nodes_projects_node_id_check CHECK ((length(node_id) > 0)),
    CONSTRAINT nodes_projects_project_id_check CHECK ((length(project_id) > 0))
);


ALTER TABLE public.nodes_projects OWNER TO postgres;

--
-- Name: nodes_secrets; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nodes_secrets (
    node_id text NOT NULL,
    secret_id text NOT NULL
);


ALTER TABLE public.nodes_secrets OWNER TO postgres;

--
-- Name: nodes_tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nodes_tags (
    node_id text NOT NULL,
    tag_id text NOT NULL
);


ALTER TABLE public.nodes_tags OWNER TO postgres;

--
-- Name: profiles; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.profiles (
    id text NOT NULL,
    url text DEFAULT ''::text NOT NULL,
    namespace text DEFAULT 'admin'::text NOT NULL,
    name text DEFAULT 'none'::text NOT NULL,
    CONSTRAINT name_not_empty CHECK ((btrim(name) <> ''::text)),
    CONSTRAINT namespace_not_empty CHECK ((btrim(namespace) <> ''::text))
);


ALTER TABLE public.profiles OWNER TO postgres;

--
-- Name: projects; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.projects (
    id text NOT NULL,
    project_id text NOT NULL,
    CONSTRAINT projects_project_id_check CHECK ((length(project_id) > 0))
);


ALTER TABLE public.projects OWNER TO postgres;

--
-- Name: results; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.results (
    job_id text NOT NULL,
    node_id text NOT NULL,
    report_id text DEFAULT ''::text NOT NULL,
    status text NOT NULL,
    result text DEFAULT ''::text NOT NULL,
    start_time timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone,
    end_time timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone
);


ALTER TABLE public.results OWNER TO postgres;

--
-- Name: s_secrets; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.s_secrets (
    id text NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    type text DEFAULT ''::text NOT NULL,
    last_modified timestamp without time zone DEFAULT now() NOT NULL,
    data text NOT NULL
);


ALTER TABLE public.s_secrets OWNER TO postgres;

--
-- Name: s_secrets_tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.s_secrets_tags (
    secret_id text NOT NULL,
    tag_id text NOT NULL
);


ALTER TABLE public.s_secrets_tags OWNER TO postgres;

--
-- Name: s_tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.s_tags (
    id text NOT NULL,
    key text NOT NULL,
    value text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.s_tags OWNER TO postgres;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: store_market; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.store_market (
    sha256 text NOT NULL
);


ALTER TABLE public.store_market OWNER TO postgres;

--
-- Name: store_namespace; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.store_namespace (
    owner text NOT NULL,
    sha256 text NOT NULL
);


ALTER TABLE public.store_namespace OWNER TO postgres;

--
-- Name: store_profiles; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.store_profiles (
    sha256 text NOT NULL,
    tar bytea NOT NULL,
    info jsonb NOT NULL,
    CONSTRAINT store_profiles_sha256_check CHECK ((sha256 <> ''::text))
);


ALTER TABLE public.store_profiles OWNER TO postgres;

--
-- Name: tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tags (
    id text NOT NULL,
    key text NOT NULL,
    value text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.tags OWNER TO postgres;

--
-- Name: telemetry; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.telemetry (
    id text NOT NULL,
    last_telemetry_reported_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone NOT NULL
);


ALTER TABLE public.telemetry OWNER TO postgres;

--
-- Name: cereal_task_results id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_task_results ALTER COLUMN id SET DEFAULT nextval('public.cereal_task_results_id_seq'::regclass);


--
-- Name: cereal_tasks id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_tasks ALTER COLUMN id SET DEFAULT nextval('public.cereal_tasks_id_seq'::regclass);


--
-- Name: cereal_workflow_events id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_events ALTER COLUMN id SET DEFAULT nextval('public.cereal_workflow_events_id_seq'::regclass);


--
-- Name: cereal_workflow_instances id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_instances ALTER COLUMN id SET DEFAULT nextval('public.cereal_workflow_instances_id_seq'::regclass);


--
-- Name: cereal_workflow_results id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_results ALTER COLUMN id SET DEFAULT nextval('public.cereal_workflow_results_id_seq'::regclass);


--
-- Name: cereal_workflow_schedules id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_schedules ALTER COLUMN id SET DEFAULT nextval('public.cereal_workflow_schedules_id_seq'::regclass);


--
-- Data for Name: agents; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.agents (id, type, status) FROM stdin;
\.


--
-- Data for Name: cereal_schema_version; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_schema_version (version, dirty) FROM stdin;
3	f
\.


--
-- Data for Name: cereal_task_results; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_task_results (id, task_id, workflow_instance_id, parameters, task_name, enqueued_at, completed_at, status, error, result) FROM stdin;
\.


--
-- Data for Name: cereal_tasks; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_tasks (id, workflow_instance_id, enqueued_at, updated_at, start_after, task_name, parameters, task_state) FROM stdin;
\.


--
-- Data for Name: cereal_workflow_events; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_workflow_events (id, event_type, workflow_instance_id, enqueued_at, task_result_id) FROM stdin;
\.


--
-- Data for Name: cereal_workflow_instances; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_workflow_instances (id, instance_name, workflow_name, parameters, payload, start_at, updated_at, enqueued_tasks, completed_tasks, status) FROM stdin;
\.


--
-- Data for Name: cereal_workflow_results; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_workflow_results (id, instance_name, workflow_name, parameters, start_at, end_at, error, result) FROM stdin;
\.


--
-- Data for Name: cereal_workflow_schedules; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.cereal_workflow_schedules (id, instance_name, workflow_name, parameters, recurrence, enabled, created_at, last_enqueued_at, next_run_at) FROM stdin;
\.


--
-- Data for Name: jobs; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.jobs (id, name, type, timeout, retries, retries_left, status, start_time, end_time, node_selectors, scheduled_time, recurrence, parent_id, job_count, node_count, deleted) FROM stdin;
\.


--
-- Data for Name: jobs_nodes; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.jobs_nodes (job_id, node_id) FROM stdin;
\.


--
-- Data for Name: jobs_profiles; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.jobs_profiles (job_id, profile_id) FROM stdin;
\.


--
-- Data for Name: jobs_tags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.jobs_tags (job_id, tag_id) FROM stdin;
\.


--
-- Data for Name: node_managers; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.node_managers (id, name, type, credentials, status_type, status_message, instance_credentials, account_id, date_added) FROM stdin;
\.


--
-- Data for Name: node_managers_nodes; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.node_managers_nodes (manager_id, node_id) FROM stdin;
\.


--
-- Data for Name: nodes; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.nodes (id, name, platform, platform_version, status, last_contact, manager, target_config, last_job, source_id, date_added, last_connection_attempt, source_region, source_state, source_account_id, connection_error, statechange_timestamp, environment, report_id) FROM stdin;
\.


--
-- Data for Name: nodes_agents; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.nodes_agents (node_id, agent_id) FROM stdin;
\.


--
-- Data for Name: nodes_projects; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.nodes_projects (node_id, project_id) FROM stdin;
\.


--
-- Data for Name: nodes_secrets; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.nodes_secrets (node_id, secret_id) FROM stdin;
\.


--
-- Data for Name: nodes_tags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.nodes_tags (node_id, tag_id) FROM stdin;
\.


--
-- Data for Name: profiles; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.profiles (id, url, namespace, name) FROM stdin;
\.


--
-- Data for Name: projects; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.projects (id, project_id) FROM stdin;
\.


--
-- Data for Name: results; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.results (job_id, node_id, report_id, status, result, start_time, end_time) FROM stdin;
\.


--
-- Data for Name: s_secrets; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.s_secrets (id, name, type, last_modified, data) FROM stdin;
\.


--
-- Data for Name: s_secrets_tags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.s_secrets_tags (secret_id, tag_id) FROM stdin;
\.


--
-- Data for Name: s_tags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.s_tags (id, key, value) FROM stdin;
\.


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version, dirty) FROM stdin;
41	f
\.


--
-- Data for Name: store_market; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.store_market (sha256) FROM stdin;
\.


--
-- Data for Name: store_namespace; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.store_namespace (owner, sha256) FROM stdin;
\.


--
-- Data for Name: store_profiles; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.store_profiles (sha256, tar, info) FROM stdin;
\.


--
-- Data for Name: tags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.tags (id, key, value) FROM stdin;
\.


--
-- Data for Name: telemetry; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.telemetry (id, last_telemetry_reported_at, created_at) FROM stdin;
\.


--
-- Name: cereal_task_results_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.cereal_task_results_id_seq', 1, false);


--
-- Name: cereal_tasks_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.cereal_tasks_id_seq', 1, false);


--
-- Name: cereal_workflow_events_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.cereal_workflow_events_id_seq', 1, false);


--
-- Name: cereal_workflow_instances_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.cereal_workflow_instances_id_seq', 1, false);


--
-- Name: cereal_workflow_results_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.cereal_workflow_results_id_seq', 1, false);


--
-- Name: cereal_workflow_schedules_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.cereal_workflow_schedules_id_seq', 1, false);


--
-- Name: agents agents_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.agents
    ADD CONSTRAINT agents_pkey PRIMARY KEY (id);


--
-- Name: cereal_schema_version cereal_schema_version_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_schema_version
    ADD CONSTRAINT cereal_schema_version_pkey PRIMARY KEY (version);


--
-- Name: cereal_task_results cereal_task_results_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_task_results
    ADD CONSTRAINT cereal_task_results_pkey PRIMARY KEY (id);


--
-- Name: cereal_task_results cereal_task_results_task_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_task_results
    ADD CONSTRAINT cereal_task_results_task_id_key UNIQUE (task_id);


--
-- Name: cereal_tasks cereal_tasks_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_tasks
    ADD CONSTRAINT cereal_tasks_pkey PRIMARY KEY (id);


--
-- Name: cereal_workflow_events cereal_workflow_events_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_events
    ADD CONSTRAINT cereal_workflow_events_pkey PRIMARY KEY (id);


--
-- Name: cereal_workflow_instances cereal_workflow_instances_name; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_instances
    ADD CONSTRAINT cereal_workflow_instances_name UNIQUE (instance_name, workflow_name);


--
-- Name: cereal_workflow_instances cereal_workflow_instances_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_instances
    ADD CONSTRAINT cereal_workflow_instances_pkey PRIMARY KEY (id);


--
-- Name: cereal_workflow_results cereal_workflow_results_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_results
    ADD CONSTRAINT cereal_workflow_results_pkey PRIMARY KEY (id);


--
-- Name: cereal_workflow_schedules cereal_workflow_schedules_name; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_schedules
    ADD CONSTRAINT cereal_workflow_schedules_name UNIQUE (instance_name, workflow_name);


--
-- Name: cereal_workflow_schedules cereal_workflow_schedules_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.cereal_workflow_schedules
    ADD CONSTRAINT cereal_workflow_schedules_pkey PRIMARY KEY (id);


--
-- Name: jobs jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.jobs
    ADD CONSTRAINT jobs_pkey PRIMARY KEY (id);


--
-- Name: node_managers node_managers_credentials_type_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.node_managers
    ADD CONSTRAINT node_managers_credentials_type_key UNIQUE (credentials, type);


--
-- Name: node_managers_nodes node_managers_nodes_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.node_managers_nodes
    ADD CONSTRAINT node_managers_nodes_pkey PRIMARY KEY (manager_id, node_id);


--
-- Name: node_managers node_managers_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.node_managers
    ADD CONSTRAINT node_managers_pkey PRIMARY KEY (id);


--
-- Name: nodes nodes_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes
    ADD CONSTRAINT nodes_pkey PRIMARY KEY (id);


--
-- Name: nodes_projects nodes_projects_node_id_project_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes_projects
    ADD CONSTRAINT nodes_projects_node_id_project_id_key UNIQUE (node_id, project_id);


--
-- Name: nodes nodes_source_id_source_region_source_account_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes
    ADD CONSTRAINT nodes_source_id_source_region_source_account_id_key UNIQUE (source_id, source_region, source_account_id);


--
-- Name: profiles profiles_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.profiles
    ADD CONSTRAINT profiles_pkey PRIMARY KEY (id);


--
-- Name: projects projects_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_pkey PRIMARY KEY (id);


--
-- Name: projects projects_project_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_project_id_key UNIQUE (project_id);


--
-- Name: s_secrets s_secrets_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.s_secrets
    ADD CONSTRAINT s_secrets_pkey PRIMARY KEY (id);


--
-- Name: s_tags s_tags_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.s_tags
    ADD CONSTRAINT s_tags_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: store_market store_market_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.store_market
    ADD CONSTRAINT store_market_pkey PRIMARY KEY (sha256);


--
-- Name: store_namespace store_namespace_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.store_namespace
    ADD CONSTRAINT store_namespace_pkey PRIMARY KEY (owner, sha256);


--
-- Name: store_profiles store_profiles_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.store_profiles
    ADD CONSTRAINT store_profiles_pkey PRIMARY KEY (sha256);


--
-- Name: tags tags_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tags
    ADD CONSTRAINT tags_pkey PRIMARY KEY (id);


--
-- Name: telemetry telemetry_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.telemetry
    ADD CONSTRAINT telemetry_pkey PRIMARY KEY (id);


--
-- Name: deleted; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX deleted ON public.jobs USING btree (deleted);


--
-- Name: idxprofileinfoname; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idxprofileinfoname ON public.store_profiles USING btree (((info ->> 'name'::text)));


--
-- Name: idxprofileinfoversion; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX idxprofileinfoversion ON public.store_profiles USING btree (((info ->> 'version'::text)));


--
-- Name: store_profiles_info_name_version; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX store_profiles_info_name_version ON public.store_profiles USING btree (((info ->> 'name'::text)), ((info ->> 'version'::text)));


--
-- Name: store_profiles_info_title_version; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX store_profiles_info_title_version ON public.store_profiles USING btree (((info ->> 'title'::text)), ((info ->> 'version'::text)));


--
-- Name: jobs_nodes jobs_nodes_job_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.jobs_nodes
    ADD CONSTRAINT jobs_nodes_job_id_fkey FOREIGN KEY (job_id) REFERENCES public.jobs(id) ON DELETE CASCADE;


--
-- Name: jobs_profiles jobs_profiles_job_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.jobs_profiles
    ADD CONSTRAINT jobs_profiles_job_id_fkey FOREIGN KEY (job_id) REFERENCES public.jobs(id) ON DELETE CASCADE;


--
-- Name: jobs_profiles jobs_profiles_profile_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.jobs_profiles
    ADD CONSTRAINT jobs_profiles_profile_id_fkey FOREIGN KEY (profile_id) REFERENCES public.profiles(id);


--
-- Name: jobs_tags jobs_tags_job_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.jobs_tags
    ADD CONSTRAINT jobs_tags_job_id_fkey FOREIGN KEY (job_id) REFERENCES public.jobs(id) ON DELETE CASCADE;


--
-- Name: jobs_tags jobs_tags_tag_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.jobs_tags
    ADD CONSTRAINT jobs_tags_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES public.tags(id) ON DELETE CASCADE;


--
-- Name: node_managers_nodes node_managers_nodes_manager_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.node_managers_nodes
    ADD CONSTRAINT node_managers_nodes_manager_id_fkey FOREIGN KEY (manager_id) REFERENCES public.node_managers(id) ON DELETE CASCADE;


--
-- Name: node_managers_nodes node_managers_nodes_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.node_managers_nodes
    ADD CONSTRAINT node_managers_nodes_node_id_fkey FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- Name: nodes_agents nodes_agents_agent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes_agents
    ADD CONSTRAINT nodes_agents_agent_id_fkey FOREIGN KEY (agent_id) REFERENCES public.agents(id) ON DELETE CASCADE;


--
-- Name: nodes_agents nodes_agents_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes_agents
    ADD CONSTRAINT nodes_agents_node_id_fkey FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- Name: nodes_secrets nodes_secrets_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes_secrets
    ADD CONSTRAINT nodes_secrets_node_id_fkey FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- Name: nodes_tags nodes_tags_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes_tags
    ADD CONSTRAINT nodes_tags_node_id_fkey FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- Name: nodes_tags nodes_tags_tag_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes_tags
    ADD CONSTRAINT nodes_tags_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES public.tags(id) ON DELETE CASCADE;


--
-- Name: results results_job_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.results
    ADD CONSTRAINT results_job_id_fkey FOREIGN KEY (job_id) REFERENCES public.jobs(id) ON DELETE CASCADE;


--
-- Name: s_secrets_tags s_secrets_tags_secret_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.s_secrets_tags
    ADD CONSTRAINT s_secrets_tags_secret_id_fkey FOREIGN KEY (secret_id) REFERENCES public.s_secrets(id) ON DELETE CASCADE;


--
-- Name: s_secrets_tags s_secrets_tags_tag_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.s_secrets_tags
    ADD CONSTRAINT s_secrets_tags_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES public.s_tags(id) ON DELETE CASCADE;


--
-- Name: store_market store_market_sha256_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.store_market
    ADD CONSTRAINT store_market_sha256_fkey FOREIGN KEY (sha256) REFERENCES public.store_profiles(sha256);


--
-- Name: store_namespace store_namespace_sha256_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.store_namespace
    ADD CONSTRAINT store_namespace_sha256_fkey FOREIGN KEY (sha256) REFERENCES public.store_profiles(sha256);


--
-- PostgreSQL database dump complete
--

--
-- Database "chef_config_mgmt_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: chef_config_mgmt_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE chef_config_mgmt_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE chef_config_mgmt_service OWNER TO postgres;

\connect chef_config_mgmt_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- PostgreSQL database dump complete
--

--
-- Database "chef_infra_proxy" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: chef_infra_proxy; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE chef_infra_proxy WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE chef_infra_proxy OWNER TO postgres;

\connect chef_infra_proxy

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: projects_match(text[], text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.projects_match(_org_projects text[], _projects_filter text[]) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
    BEGIN
      RETURN (
        -- no projects filter requested (length 0)
        array_length(_projects_filter, 1) IS NULL
        -- projects filter intersects with projects for row
        OR _org_projects && _projects_filter
        -- projects for row is an empty array, check if (unassigned) in project filter
        OR (array_length(_org_projects, 1) IS NULL AND '{(unassigned)}' && _projects_filter)
      );
    END
$$;


ALTER FUNCTION public.projects_match(_org_projects text[], _projects_filter text[]) OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: orgs; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.orgs (
    id text NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    admin_user text DEFAULT ''::text NOT NULL,
    credential_id text DEFAULT ''::text NOT NULL,
    server_id text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    projects text[] DEFAULT '{}'::text[] NOT NULL
);


ALTER TABLE public.orgs OWNER TO postgres;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: servers; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.servers (
    id text NOT NULL,
    name text NOT NULL,
    fqdn text DEFAULT ''::text NOT NULL,
    ip_address text DEFAULT ''::text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


ALTER TABLE public.servers OWNER TO postgres;

--
-- Data for Name: orgs; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.orgs (id, name, admin_user, credential_id, server_id, created_at, updated_at, projects) FROM stdin;
\.


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version, dirty) FROM stdin;
5	f
\.


--
-- Data for Name: servers; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.servers (id, name, fqdn, ip_address, created_at, updated_at) FROM stdin;
\.


--
-- Name: orgs orgs_name_server_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.orgs
    ADD CONSTRAINT orgs_name_server_id_key UNIQUE (id, server_id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: servers servers_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.servers
    ADD CONSTRAINT servers_pkey PRIMARY KEY (id);


--
-- Name: orgs orgs_server_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.orgs
    ADD CONSTRAINT orgs_server_id_fkey FOREIGN KEY (server_id) REFERENCES public.servers(id) ON DELETE RESTRICT;


--
-- PostgreSQL database dump complete
--

--
-- Database "chef_ingest_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: chef_ingest_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE chef_ingest_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE chef_ingest_service OWNER TO postgres;

\connect chef_ingest_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- PostgreSQL database dump complete
--

--
-- Database "chef_license_control_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: chef_license_control_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE chef_license_control_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE chef_license_control_service OWNER TO postgres;

\connect chef_license_control_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: set_active_license_v1(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.set_active_license_v1(_license_data text) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
        UPDATE licenses SET active = NULL;

        INSERT INTO licenses(data, active) VALUES (_license_data, TRUE)
        ON CONFLICT (data)
        DO UPDATE SET configured_at = NOW(), active = TRUE;
END
$$;


ALTER FUNCTION public.set_active_license_v1(_license_data text) OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: deployment; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.deployment (
    id text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    type_id integer DEFAULT 0
);


ALTER TABLE public.deployment OWNER TO postgres;

--
-- Name: deployment_type; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.deployment_type (
    id integer NOT NULL,
    type text NOT NULL
);


ALTER TABLE public.deployment_type OWNER TO postgres;

--
-- Name: licenses; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.licenses (
    db_id bigint NOT NULL,
    active boolean,
    configured_at timestamp with time zone DEFAULT now(),
    data text,
    CONSTRAINT licence_active_true_or_null CHECK (active)
);


ALTER TABLE public.licenses OWNER TO postgres;

--
-- Name: licenses_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.licenses_db_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.licenses_db_id_seq OWNER TO postgres;

--
-- Name: licenses_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.licenses_db_id_seq OWNED BY public.licenses.db_id;


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: licenses db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.licenses ALTER COLUMN db_id SET DEFAULT nextval('public.licenses_db_id_seq'::regclass);


--
-- Data for Name: deployment; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.deployment (id, created_at, updated_at, type_id) FROM stdin;
eb21aa35-776e-4db8-aa4a-aadbe3c38ae8	2022-06-03 09:19:24.246949+00	2022-06-03 09:20:32.492551+00	0
\.


--
-- Data for Name: deployment_type; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.deployment_type (id, type) FROM stdin;
0	Standalone
1	HA
2	SAAS
\.


--
-- Data for Name: licenses; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.licenses (db_id, active, configured_at, data) FROM stdin;
1	t	2022-06-03 10:04:21.193132+00	eyJhbGciOiJFUzUxMiIsInR5cCI6IkpXVCJ9.eyJpZCI6IjBhNDk5YzhkLTJkYjAtNDE2OS1iNGM2LTJmMmIwOGVhZGNmMSIsInZlcnNpb24iOiIxIiwidHlwZSI6InRyaWFsIiwiZ2VuZXJhdG9yIjoiY2hlZi9saWNlbnNlLTIuMC4wIiwia2V5X3NoYTI1NiI6ImUwZGYyOGM4YmM2ODE1MGVkYmZlZjk4Y2Q2YjdkYzQzOWMxZjgwYzdlN2VmNzQ3ODkzYTY4OTNhMmY3YjYwZjciLCJnZW5lcmF0aW9uX2RhdGUiOnsic2Vjb25kcyI6MTY1NDI1MDY2MH0sImN1c3RvbWVyIjoiVml2ZWsgWWFkYXYgXHUwMDNjdml2ZWt5YWRhdi5qaXRAZ21haWwuY29tXHUwMDNlIC0gVFJJQUwiLCJjdXN0b21lcl9pZCI6ImY3YTk5ZThmLWExYTQtNDdjYi1hNDM3LWEwOWQwNjI3MzgwYSIsImN1c3RvbWVyX2lkX3ZlcnNpb24iOiIxIiwiZW50aXRsZW1lbnRzIjpbeyJuYW1lIjoiQ2hlZiBBdXRvbWF0ZSBUcmlhbCIsIm1lYXN1cmUiOiJkYXlzIiwibGltaXQiOjYwLCJzdGFydCI6eyJzZWNvbmRzIjoxNjU0MjUwNjYwfSwiZW5kIjp7InNlY29uZHMiOjE2NTk0MzQ2NjB9fV19.AQAoq8xKeN0v8aUzbw_3nxCkYHuWc9qMsCIlY3072mCrdje-VMjtL_-ILQnrbMrc8GHQo5n0leogz266O4ASslBXAcAs93Z3G9NIxsMKv4ovIMv3mh0ZhQrzC86JQPfqFbFTvRqJLnSaAqCtB7Y8gL_3Dw1d5dubRHAOgYY8bgsGxWVl
\.


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version, dirty) FROM stdin;
2	f
\.


--
-- Name: licenses_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.licenses_db_id_seq', 1, true);


--
-- Name: deployment deployment_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.deployment
    ADD CONSTRAINT deployment_pkey PRIMARY KEY (id);


--
-- Name: deployment_type deployment_type_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.deployment_type
    ADD CONSTRAINT deployment_type_pkey PRIMARY KEY (id);


--
-- Name: licenses licenses_active_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.licenses
    ADD CONSTRAINT licenses_active_key UNIQUE (active);


--
-- Name: licenses licenses_data_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.licenses
    ADD CONSTRAINT licenses_data_key UNIQUE (data);


--
-- Name: licenses licenses_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.licenses
    ADD CONSTRAINT licenses_pkey PRIMARY KEY (db_id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: deployment fk_deployment_type; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.deployment
    ADD CONSTRAINT fk_deployment_type FOREIGN KEY (type_id) REFERENCES public.deployment_type(id);


--
-- PostgreSQL database dump complete
--

--
-- Database "chef_session_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: chef_session_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE chef_session_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE chef_session_service OWNER TO postgres;

\connect chef_session_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: blacklisted_id_tokens; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.blacklisted_id_tokens (
    token text,
    inserted_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP
);


ALTER TABLE public.blacklisted_id_tokens OWNER TO postgres;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: sessions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.sessions (
    token text NOT NULL,
    data bytea NOT NULL,
    expiry timestamp with time zone NOT NULL
);


ALTER TABLE public.sessions OWNER TO postgres;

--
-- Data for Name: blacklisted_id_tokens; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.blacklisted_id_tokens (token, inserted_at) FROM stdin;
null	2022-06-03 10:02:56.658447
\.


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version, dirty) FROM stdin;
2	f
\.


--
-- Data for Name: sessions; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.sessions (token, data, expiry) FROM stdin;
5iR7UnucmXMT7HbrULGh7RlSBxpRqKdxaSkpmI1PE00	\\x7b2264617461223a7b22616c697665223a747275652c22726566726573685f746f6b656e223a2243686c32626a5676644735745958563364324e795a476b33596d39736457567663334a7845686c735a585a6e626e646b656e426c626e6c6e5957466c6447746c626e5a775a6e4e6f227d2c22646561646c696e65223a313635343333373431363239333435373135357d	2022-06-04 10:10:16.293457+00
\.


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: sessions sessions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.sessions
    ADD CONSTRAINT sessions_pkey PRIMARY KEY (token);


--
-- Name: blacklist_session_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX blacklist_session_idx ON public.blacklisted_id_tokens USING btree (token);


--
-- Name: sessions_expiry_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX sessions_expiry_idx ON public.sessions USING btree (expiry);


--
-- PostgreSQL database dump complete
--

--
-- Database "chef_teams_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: chef_teams_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE chef_teams_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE chef_teams_service OWNER TO postgres;

\connect chef_teams_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: cannot_delete_team_error(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.cannot_delete_team_error() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    RAISE EXCEPTION 'You cannot delete % with id % as it is not marked as deletable.', OLD.name, OLD.id USING
        ERRCODE='DRPTM';
END$$;


ALTER FUNCTION public.cannot_delete_team_error() OWNER TO postgres;

--
-- Name: projects_match(text[], text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.projects_match(_team_projects text[], _projects_filter text[]) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
    BEGIN
      RETURN (
        -- no projects filter requested (length 0) will be the case for v1.0 or v2.1 ["*"]
        array_length(_projects_filter, 1) IS NULL
        -- projects filter intersects with projects for row
        OR _team_projects && _projects_filter
        -- projects for row is an empty array, check if (unassigned) in project filter
        OR (array_length(_team_projects, 1) IS NULL AND '{(unassigned)}' && _projects_filter)
      );
    END
$$;


ALTER FUNCTION public.projects_match(_team_projects text[], _projects_filter text[]) OWNER TO postgres;

--
-- Name: team_db_id(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.team_db_id(_id text) RETURNS integer
    LANGUAGE sql
    AS $$
    SELECT
        db_id
    FROM
        teams
    WHERE
        id = _id;
$$;


ALTER FUNCTION public.team_db_id(_id text) OWNER TO postgres;

--
-- Name: team_db_id(uuid); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.team_db_id(_id uuid) RETURNS integer
    LANGUAGE sql
    AS $$
    SELECT
        db_id
    FROM
        teams
    WHERE
        id = _id;
$$;


ALTER FUNCTION public.team_db_id(_id uuid) OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: teams; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.teams (
    id text NOT NULL,
    name text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    deletable boolean DEFAULT true NOT NULL,
    projects text[] DEFAULT '{}'::text[] NOT NULL,
    db_id integer NOT NULL
);


ALTER TABLE public.teams OWNER TO postgres;

--
-- Name: teams_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.teams_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.teams_db_id_seq OWNER TO postgres;

--
-- Name: teams_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.teams_db_id_seq OWNED BY public.teams.db_id;


--
-- Name: teams_users_associations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.teams_users_associations (
    user_id text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    team_db_id integer NOT NULL
);


ALTER TABLE public.teams_users_associations OWNER TO postgres;

--
-- Name: teams_users_associations_team_db_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.teams_users_associations_team_db_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.teams_users_associations_team_db_id_seq OWNER TO postgres;

--
-- Name: teams_users_associations_team_db_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.teams_users_associations_team_db_id_seq OWNED BY public.teams_users_associations.team_db_id;


--
-- Name: teams db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.teams ALTER COLUMN db_id SET DEFAULT nextval('public.teams_db_id_seq'::regclass);


--
-- Name: teams_users_associations team_db_id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.teams_users_associations ALTER COLUMN team_db_id SET DEFAULT nextval('public.teams_users_associations_team_db_id_seq'::regclass);


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version, dirty) FROM stdin;
11	f
\.


--
-- Data for Name: teams; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.teams (id, name, created_at, updated_at, deletable, projects, db_id) FROM stdin;
editors	Editors	2022-06-03 09:19:26.034846+00	2022-06-03 09:19:26.034846+00	t	{}	2
viewers	Viewers	2022-06-03 09:19:26.034846+00	2022-06-03 09:19:26.034846+00	t	{}	3
admins	admins	2022-06-03 09:19:25.824964+00	2022-06-03 10:04:02.769461+00	f	{}	1
\.


--
-- Data for Name: teams_users_associations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.teams_users_associations (user_id, created_at, team_db_id) FROM stdin;
8574e7fd-f219-46ff-b5e3-cf9586c04795	2022-06-03 10:04:02.769461+00	1
\.


--
-- Name: teams_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.teams_db_id_seq', 3, true);


--
-- Name: teams_users_associations_team_db_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.teams_users_associations_team_db_id_seq', 1, false);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: teams teams_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.teams
    ADD CONSTRAINT teams_id_key UNIQUE (id);


--
-- Name: teams teams_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.teams
    ADD CONSTRAINT teams_pkey PRIMARY KEY (db_id);


--
-- Name: teams_users_associations teams_users_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.teams_users_associations
    ADD CONSTRAINT teams_users_pkey PRIMARY KEY (team_db_id, user_id);


--
-- Name: teams only_allow_delete_on_deletable_teams; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER only_allow_delete_on_deletable_teams BEFORE DELETE ON public.teams FOR EACH ROW WHEN ((old.deletable = false)) EXECUTE FUNCTION public.cannot_delete_team_error();


--
-- Name: teams_users_associations teams_db_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.teams_users_associations
    ADD CONSTRAINT teams_db_id_fkey FOREIGN KEY (team_db_id) REFERENCES public.teams(db_id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

--
-- Database "chef_user_settings_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: chef_user_settings_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE chef_user_settings_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE chef_user_settings_service OWNER TO postgres;

\connect chef_user_settings_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: user_settings; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.user_settings (
    id integer NOT NULL,
    user_name text DEFAULT ''::text NOT NULL,
    connector text DEFAULT 'local'::text NOT NULL,
    settings json DEFAULT '[]'::json NOT NULL
);


ALTER TABLE public.user_settings OWNER TO postgres;

--
-- Name: user_settings_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.user_settings_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_settings_id_seq OWNER TO postgres;

--
-- Name: user_settings_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.user_settings_id_seq OWNED BY public.user_settings.id;


--
-- Name: user_settings id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.user_settings ALTER COLUMN id SET DEFAULT nextval('public.user_settings_id_seq'::regclass);


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version, dirty) FROM stdin;
2	f
\.


--
-- Data for Name: user_settings; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.user_settings (id, user_name, connector, settings) FROM stdin;
1	_default	local	{\n          "date_format": {\n            "default_value": "ddd, DD MMM YYYY HH:mm:ss [UTC]",\n            "value": "ddd, DD MMM YYYY HH:mm:ss [UTC]",\n            "enabled": true,\n            "valid_values": [\n              "ddd, DD MMM YYYY HH:mm:ss [UTC]",\n              "YYYY-M-D",\n              "ddd, DD MMM YYYY",\n              "DD MMM YYYY",\n              "ddd, DD MMM",\n              "YYYY-MM-DD"\n            ]\n          }\n        }
\.


--
-- Name: user_settings_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.user_settings_id_seq', 1, true);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: user_settings user_settings_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.user_settings
    ADD CONSTRAINT user_settings_pkey PRIMARY KEY (id);


--
-- Name: user_settings user_settings_user_name_and_connector; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.user_settings
    ADD CONSTRAINT user_settings_user_name_and_connector UNIQUE (user_name, connector);


--
-- PostgreSQL database dump complete
--

--
-- Database "data_feed_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: data_feed_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE data_feed_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE data_feed_service OWNER TO postgres;

\connect data_feed_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: destinations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.destinations (
    id integer NOT NULL,
    name text NOT NULL,
    url text NOT NULL,
    secret text,
    services text DEFAULT ''::text NOT NULL,
    integration_types text DEFAULT ''::text NOT NULL,
    meta_data text DEFAULT ''::text NOT NULL,
    enable boolean DEFAULT true NOT NULL,
    CONSTRAINT name_not_empty CHECK ((name <> ''::text)),
    CONSTRAINT url_not_empty CHECK ((url <> ''::text))
);


ALTER TABLE public.destinations OWNER TO postgres;

--
-- Name: destinations_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.destinations_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.destinations_id_seq OWNER TO postgres;

--
-- Name: destinations_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.destinations_id_seq OWNED BY public.destinations.id;


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: destinations id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.destinations ALTER COLUMN id SET DEFAULT nextval('public.destinations_id_seq'::regclass);


--
-- Data for Name: destinations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.destinations (id, name, url, secret, services, integration_types, meta_data, enable) FROM stdin;
\.


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version, dirty) FROM stdin;
4	f
\.


--
-- Name: destinations_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.destinations_id_seq', 1, false);


--
-- Name: destinations destinations_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.destinations
    ADD CONSTRAINT destinations_name_key UNIQUE (name);


--
-- Name: destinations destinations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.destinations
    ADD CONSTRAINT destinations_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- PostgreSQL database dump complete
--

--
-- Database "dex" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: dex; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE dex WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE dex OWNER TO postgres;

\connect dex

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: auth_code; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.auth_code (
    id text NOT NULL,
    client_id text NOT NULL,
    scopes bytea NOT NULL,
    nonce text NOT NULL,
    redirect_uri text NOT NULL,
    claims_user_id text NOT NULL,
    claims_username text NOT NULL,
    claims_email text NOT NULL,
    claims_email_verified boolean NOT NULL,
    claims_groups bytea NOT NULL,
    connector_id text NOT NULL,
    connector_data bytea,
    expiry timestamp with time zone NOT NULL,
    claims_preferred_username text DEFAULT ''::text NOT NULL,
    code_challenge text DEFAULT ''::text NOT NULL,
    code_challenge_method text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.auth_code OWNER TO postgres;

--
-- Name: auth_request; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.auth_request (
    id text NOT NULL,
    client_id text NOT NULL,
    response_types bytea NOT NULL,
    scopes bytea NOT NULL,
    redirect_uri text NOT NULL,
    nonce text NOT NULL,
    state text NOT NULL,
    force_approval_prompt boolean NOT NULL,
    logged_in boolean NOT NULL,
    claims_user_id text NOT NULL,
    claims_username text NOT NULL,
    claims_email text NOT NULL,
    claims_email_verified boolean NOT NULL,
    claims_groups bytea NOT NULL,
    connector_id text NOT NULL,
    connector_data bytea,
    expiry timestamp with time zone NOT NULL,
    claims_preferred_username text DEFAULT ''::text NOT NULL,
    code_challenge text DEFAULT ''::text NOT NULL,
    code_challenge_method text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.auth_request OWNER TO postgres;

--
-- Name: client; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.client (
    id text NOT NULL,
    secret text NOT NULL,
    redirect_uris bytea NOT NULL,
    trusted_peers bytea NOT NULL,
    public boolean NOT NULL,
    name text NOT NULL,
    logo_url text NOT NULL
);


ALTER TABLE public.client OWNER TO postgres;

--
-- Name: connector; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.connector (
    id text NOT NULL,
    type text NOT NULL,
    name text NOT NULL,
    resource_version text NOT NULL,
    config bytea
);


ALTER TABLE public.connector OWNER TO postgres;

--
-- Name: device_request; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.device_request (
    user_code text NOT NULL,
    device_code text NOT NULL,
    client_id text NOT NULL,
    client_secret text,
    scopes bytea NOT NULL,
    expiry timestamp with time zone NOT NULL
);


ALTER TABLE public.device_request OWNER TO postgres;

--
-- Name: device_token; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.device_token (
    device_code text NOT NULL,
    status text NOT NULL,
    token bytea,
    expiry timestamp with time zone NOT NULL,
    last_request timestamp with time zone NOT NULL,
    poll_interval integer NOT NULL
);


ALTER TABLE public.device_token OWNER TO postgres;

--
-- Name: invalid_login_attempts; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.invalid_login_attempts (
    username_conn_id text NOT NULL,
    invalid_login_attempts_count integer DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT '0001-01-01 00:00:00+00'::timestamp with time zone NOT NULL
);


ALTER TABLE public.invalid_login_attempts OWNER TO postgres;

--
-- Name: keys; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.keys (
    id text NOT NULL,
    verification_keys bytea NOT NULL,
    signing_key bytea NOT NULL,
    signing_key_pub bytea NOT NULL,
    next_rotation timestamp with time zone NOT NULL
);


ALTER TABLE public.keys OWNER TO postgres;

--
-- Name: migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.migrations (
    num integer NOT NULL,
    at timestamp with time zone NOT NULL
);


ALTER TABLE public.migrations OWNER TO postgres;

--
-- Name: offline_session; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.offline_session (
    user_id text NOT NULL,
    conn_id text NOT NULL,
    refresh bytea NOT NULL,
    connector_data bytea
);


ALTER TABLE public.offline_session OWNER TO postgres;

--
-- Name: password; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.password (
    email text NOT NULL,
    hash bytea NOT NULL,
    username text NOT NULL,
    user_id text NOT NULL
);


ALTER TABLE public.password OWNER TO postgres;

--
-- Name: refresh_token; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.refresh_token (
    id text NOT NULL,
    client_id text NOT NULL,
    scopes bytea NOT NULL,
    nonce text NOT NULL,
    claims_user_id text NOT NULL,
    claims_username text NOT NULL,
    claims_email text NOT NULL,
    claims_email_verified boolean NOT NULL,
    claims_groups bytea NOT NULL,
    connector_id text NOT NULL,
    connector_data bytea,
    token text DEFAULT ''::text NOT NULL,
    created_at timestamp with time zone DEFAULT '0001-01-01 00:00:00+00'::timestamp with time zone NOT NULL,
    last_used timestamp with time zone DEFAULT '0001-01-01 00:00:00+00'::timestamp with time zone NOT NULL,
    claims_preferred_username text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.refresh_token OWNER TO postgres;

--
-- Data for Name: auth_code; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.auth_code (id, client_id, scopes, nonce, redirect_uri, claims_user_id, claims_username, claims_email, claims_email_verified, claims_groups, connector_id, connector_data, expiry, claims_preferred_username, code_challenge, code_challenge_method) FROM stdin;
\.


--
-- Data for Name: auth_request; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.auth_request (id, client_id, response_types, scopes, redirect_uri, nonce, state, force_approval_prompt, logged_in, claims_user_id, claims_username, claims_email, claims_email_verified, claims_groups, connector_id, connector_data, expiry, claims_preferred_username, code_challenge, code_challenge_method) FROM stdin;
\.


--
-- Data for Name: client; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.client (id, secret, redirect_uris, trusted_peers, public, name, logo_url) FROM stdin;
\.


--
-- Data for Name: connector; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.connector (id, type, name, resource_version, config) FROM stdin;
\.


--
-- Data for Name: device_request; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.device_request (user_code, device_code, client_id, client_secret, scopes, expiry) FROM stdin;
\.


--
-- Data for Name: device_token; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.device_token (device_code, status, token, expiry, last_request, poll_interval) FROM stdin;
\.


--
-- Data for Name: invalid_login_attempts; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.invalid_login_attempts (username_conn_id, invalid_login_attempts_count, updated_at) FROM stdin;
\.


--
-- Data for Name: keys; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.keys (id, verification_keys, signing_key, signing_key_pub, next_rotation) FROM stdin;
keys	\\x6e756c6c	\\x7b22757365223a22736967222c226b7479223a22525341222c226b6964223a2264663137653837393238323931383332633234626437613364386634613530356231373262346432222c22616c67223a225253323536222c226e223a22726b616e4f79327254417164343333484b4e2d6f31716a574f49475439395a694446524932414f6e4b7a71666a5f363374537166424d376c695a764a69645445357361497a49786d457a594e326d4770595231583574437478473871325137772d57654b4b3336617857676b704937526c5663793250434d6339516d31426144485053673054754d4b454444595a67754c525f69624938714a62776c446d386d6c31696f76515f58396b4d385652584a6b6d59355a5a5841654378354a4433486f4676656e55703364773059637a7635414831502d354e476f775564442d356a465969717263532d4348725475364179566841623079795359445744327a51706450487863326d465265773437775f346c706531424642626f556b6b772d595762546d6b732d4e4b6a6c45486175547a4b7142584b5168482d4f516250716c72346469774f364531546274626d6c4b32374e787a7951222c2265223a2241514142222c2264223a2258566665626d58786e394c33366b634f4379687761446b614e4433444954576b596e497161666a61367436465a447a4c374b3864615266665f61714263642d5362774462654b4f69634a7539493843335668715f59424e5a58794d6a3067716371786a332d6e5f664b65697a35366650506176737769466c4f517a696c7970506f3479724c69593031634b4b5935563839684a6a316a32414636494550777a746f4a614f476d574943495f73506251736443624e6d64394d797a5f766b365566535072705874675a474d38535f35757642365f505f756f6641496f62696762725562374d424979615f4867476342394d6c5436353464324d546b7366775362617059706f42336f32703676704d76323749694e7846504e6731417652745a4d617947396f6d522d394f646838666b6e35524e4b63706c4248315f3672665045754476573735456e366451574b4f4d4e535252764d2d51222c2270223a223258722d4a796263386f436e62567669497151466b5541647956714a64324c714e672d53716753627061327674435a754753614654656a7474636b447351427a53386a4d525764756d69536e766439635a533039595f6c50493138754d304b726b585f686e7a517a6f4b62445575437836346e6b53614a6d41414e53376c6d6d5557484c417a486d4f6f343150435f64425f715347536b55545647504a69414b335f61304279466a325073222c2271223a227a535375733339374669534977637a5673364c4d5445545665434237424531502d4b5346527275594e6b2d42314f526e795066765572474450637972414171384c443678486a626f37534948736576413642697a53483336424b6b79696544506a42396a644d4e7079645070324d5f6d76364344447835556e7337424c5675365f4a55712d6957594e374c7471686d7a44486a427170334930486859445767424a6b41486b55724f6b7773222c226470223a2269595162384f58434542554a795479775950635744694a63733156735676465535356f785178426d32767859666246504c4e674f73705432756c764132637450484478373765625a4b4e3745736d727149595f5a6f62326a4c4e71696641644d4e56534864674f7349596c524f4c6162796a51484267694b4e72666a574a4f586478345f4947594e446a6d6472394555456c642d79696f506a46526d4439567a437573554b753453464d45222c226471223a22444943427a52716c79374771535f564a794d725248497a4e5861624e4748514676616435483968713746526f4779745954393063547250577862746c61486b5f416f74486a416e597043734177513058456f332d7a786f755648356a51426664546a4c546774677436456a723558533249564f4d36537a32427a2d65636a38323542484c4c72645f305655734f734f614c395a6b4a7875366b306e695139376b77776e5f37424464436755222c227169223a225337513733694d7461433445524d674c397a79367764716e47326632774535782d33744d49706773656530736a616a41666a415f4d31316a76347a5a3763596e3361793777774c73414d566362696f776a5f57393370536a77614a7942715041446e5a3531545056675854524a55432d565664777975677a5170576e705453495a7138494d6f30443864504e6c516137334b446a51636e6c5147587570736a4e507438494d4b37417a774d227d	\\x7b22757365223a22736967222c226b7479223a22525341222c226b6964223a2264663137653837393238323931383332633234626437613364386634613530356231373262346432222c22616c67223a225253323536222c226e223a22726b616e4f79327254417164343333484b4e2d6f31716a574f49475439395a694446524932414f6e4b7a71666a5f363374537166424d376c695a764a69645445357361497a49786d457a594e326d4770595231583574437478473871325137772d57654b4b3336617857676b704937526c5663793250434d6339516d31426144485053673054754d4b454444595a67754c525f69624938714a62776c446d386d6c31696f76515f58396b4d385652584a6b6d59355a5a5841654378354a4433486f4676656e55703364773059637a7635414831502d354e476f775564442d356a465969717263532d4348725475364179566841623079795359445744327a51706450487863326d465265773437775f346c706531424642626f556b6b772d595762546d6b732d4e4b6a6c45486175547a4b7142584b5168482d4f516250716c72346469774f364531546274626d6c4b32374e787a7951222c2265223a2241514142227d	2022-06-03 15:19:28.847112+00
\.


--
-- Data for Name: migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.migrations (num, at) FROM stdin;
1	2022-06-03 09:19:27.215028+00
2	2022-06-03 09:19:27.315315+00
3	2022-06-03 09:19:27.327304+00
4	2022-06-03 09:19:27.361135+00
5	2022-06-03 09:19:27.377612+00
6	2022-06-03 09:19:27.382997+00
7	2022-06-03 09:19:27.386192+00
8	2022-06-03 09:19:27.411355+00
9	2022-06-03 09:19:27.419986+00
\.


--
-- Data for Name: offline_session; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.offline_session (user_id, conn_id, refresh, connector_data) FROM stdin;
8574e7fd-f219-46ff-b5e3-cf9586c04795	local	\\x7b226175746f6d6174652d73657373696f6e223a7b224944223a22766e356f746e6d617577776372646937626f6c75656f737271222c22436c69656e744944223a226175746f6d6174652d73657373696f6e222c22437265617465644174223a22323032322d30362d30335431303a30343a31332e3232313231313333355a222c224c61737455736564223a22323032322d30362d30335431303a31303a31362e3330383434313733335a227d7d	\\x
\.


--
-- Data for Name: password; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.password (email, hash, username, user_id) FROM stdin;
admin	\\x243261243130246c736430595a563665322e564b3850384a704336514f582f706c54634646522e4d554b724b6a547a326d58454275574763494c2e69	Local Administrator	8574e7fd-f219-46ff-b5e3-cf9586c04795
\.


--
-- Data for Name: refresh_token; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.refresh_token (id, client_id, scopes, nonce, claims_user_id, claims_username, claims_email, claims_email_verified, claims_groups, connector_id, connector_data, token, created_at, last_used, claims_preferred_username) FROM stdin;
vn5otnmauwwcrdi7bolueosrq	automate-session	\\x5b226f70656e6964222c2270726f66696c65222c22656d61696c222c226f66666c696e655f616363657373222c2267726f757073222c226665646572617465643a6964225d		8574e7fd-f219-46ff-b5e3-cf9586c04795	Local Administrator	admin	t	\\x6e756c6c	local	\\x	levgnwdzpenygaaetkenvpfsh	2022-06-03 10:04:13.221211+00	2022-06-03 10:10:16.308442+00	
\.


--
-- Name: auth_code auth_code_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_code
    ADD CONSTRAINT auth_code_pkey PRIMARY KEY (id);


--
-- Name: auth_request auth_request_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_request
    ADD CONSTRAINT auth_request_pkey PRIMARY KEY (id);


--
-- Name: client client_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.client
    ADD CONSTRAINT client_pkey PRIMARY KEY (id);


--
-- Name: connector connector_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.connector
    ADD CONSTRAINT connector_pkey PRIMARY KEY (id);


--
-- Name: device_request device_request_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.device_request
    ADD CONSTRAINT device_request_pkey PRIMARY KEY (user_code);


--
-- Name: device_token device_token_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.device_token
    ADD CONSTRAINT device_token_pkey PRIMARY KEY (device_code);


--
-- Name: invalid_login_attempts invalid_login_attempts_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.invalid_login_attempts
    ADD CONSTRAINT invalid_login_attempts_pkey PRIMARY KEY (username_conn_id);


--
-- Name: keys keys_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.keys
    ADD CONSTRAINT keys_pkey PRIMARY KEY (id);


--
-- Name: offline_session offline_session_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.offline_session
    ADD CONSTRAINT offline_session_pkey PRIMARY KEY (user_id, conn_id);


--
-- Name: password password_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.password
    ADD CONSTRAINT password_pkey PRIMARY KEY (email);


--
-- Name: refresh_token refresh_token_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.refresh_token
    ADD CONSTRAINT refresh_token_pkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--

--
-- Database "nodemanager_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: nodemanager_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE nodemanager_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE nodemanager_service OWNER TO postgres;

\connect nodemanager_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: upsert_by_source_id_run_data(text, text, text, text, text, timestamp without time zone, text, text, text, json, json, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.upsert_by_source_id_run_data(_id text, _name text, _platform text, _platform_version text, _source_state text, _last_contact timestamp without time zone, _source_id text, _source_region text, _source_account_id text, _last_run json, _projects_data json, _manager text) RETURNS text
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


ALTER FUNCTION public.upsert_by_source_id_run_data(_id text, _name text, _platform text, _platform_version text, _source_state text, _last_contact timestamp without time zone, _source_id text, _source_region text, _source_account_id text, _last_run json, _projects_data json, _manager text) OWNER TO postgres;

--
-- Name: upsert_by_source_id_scan_data(text, text, text, text, text, timestamp without time zone, text, text, text, text, json, json, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.upsert_by_source_id_scan_data(_id text, _name text, _platform text, _platform_version text, _source_state text, _last_contact timestamp without time zone, _source_id text, _source_region text, _source_account_id text, _last_job text, _last_scan json, _projects_data json, _manager text) RETURNS text
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


ALTER FUNCTION public.upsert_by_source_id_scan_data(_id text, _name text, _platform text, _platform_version text, _source_state text, _last_contact timestamp without time zone, _source_id text, _source_region text, _source_account_id text, _last_job text, _last_scan json, _projects_data json, _manager text) OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: node_managers; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.node_managers (
    id text NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    type text DEFAULT ''::text NOT NULL,
    credentials text DEFAULT ''::text NOT NULL,
    status_type text DEFAULT ''::text NOT NULL,
    status_message text DEFAULT ''::text NOT NULL,
    account_id text,
    instance_credentials json DEFAULT '[]'::json NOT NULL,
    date_added timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone
);


ALTER TABLE public.node_managers OWNER TO postgres;

--
-- Name: node_managers_nodes; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.node_managers_nodes (
    manager_id text NOT NULL,
    node_id text NOT NULL
);


ALTER TABLE public.node_managers_nodes OWNER TO postgres;

--
-- Name: nodes; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nodes (
    id text NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    platform text DEFAULT ''::text NOT NULL,
    platform_version text DEFAULT ''::text NOT NULL,
    status text DEFAULT 'unknown'::text NOT NULL,
    last_contact timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone,
    manager text DEFAULT ''::text NOT NULL,
    target_config json,
    last_job text DEFAULT ''::text NOT NULL,
    instance_credentials json DEFAULT '[]'::json NOT NULL,
    source_id text,
    date_added timestamp without time zone DEFAULT now() NOT NULL,
    last_connection_attempt timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone,
    source_region text DEFAULT ''::text,
    source_state text DEFAULT ''::text,
    source_account_id text DEFAULT ''::text,
    connection_error text DEFAULT ''::text NOT NULL,
    statechange_timestamp timestamp without time zone DEFAULT '0001-01-01 00:00:00'::timestamp without time zone NOT NULL,
    report_id text DEFAULT ''::text,
    last_scan json DEFAULT '{}'::json NOT NULL,
    last_run json DEFAULT '{}'::json NOT NULL,
    projects_data json DEFAULT '[]'::json NOT NULL
);


ALTER TABLE public.nodes OWNER TO postgres;

--
-- Name: nodes_projects; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nodes_projects (
    node_id text NOT NULL,
    project_id text NOT NULL,
    CONSTRAINT nodes_projects_node_id_check CHECK ((length(node_id) > 0)),
    CONSTRAINT nodes_projects_project_id_check CHECK ((length(project_id) > 0))
);


ALTER TABLE public.nodes_projects OWNER TO postgres;

--
-- Name: nodes_secrets; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nodes_secrets (
    node_id text NOT NULL,
    secret_id text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.nodes_secrets OWNER TO postgres;

--
-- Name: nodes_tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.nodes_tags (
    node_id text NOT NULL,
    tag_id text NOT NULL
);


ALTER TABLE public.nodes_tags OWNER TO postgres;

--
-- Name: projects; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.projects (
    id text NOT NULL,
    project_id text NOT NULL,
    CONSTRAINT projects_project_id_check CHECK ((length(project_id) > 0))
);


ALTER TABLE public.projects OWNER TO postgres;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Name: tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tags (
    id text NOT NULL,
    key text NOT NULL,
    value text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.tags OWNER TO postgres;

--
-- Data for Name: node_managers; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.node_managers (id, name, type, credentials, status_type, status_message, account_id, instance_credentials, date_added) FROM stdin;
e69dc612-7e67-43f2-9b19-256afd385820	Automate	automate					null	2022-06-03 09:19:28
\.


--
-- Data for Name: node_managers_nodes; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.node_managers_nodes (manager_id, node_id) FROM stdin;
\.


--
-- Data for Name: nodes; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.nodes (id, name, platform, platform_version, status, last_contact, manager, target_config, last_job, instance_credentials, source_id, date_added, last_connection_attempt, source_region, source_state, source_account_id, connection_error, statechange_timestamp, report_id, last_scan, last_run, projects_data) FROM stdin;
\.


--
-- Data for Name: nodes_projects; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.nodes_projects (node_id, project_id) FROM stdin;
\.


--
-- Data for Name: nodes_secrets; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.nodes_secrets (node_id, secret_id) FROM stdin;
\.


--
-- Data for Name: nodes_tags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.nodes_tags (node_id, tag_id) FROM stdin;
\.


--
-- Data for Name: projects; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.projects (id, project_id) FROM stdin;
\.


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version, dirty) FROM stdin;
7	f
\.


--
-- Data for Name: tags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.tags (id, key, value) FROM stdin;
\.


--
-- Name: node_managers node_managers_credentials_type_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.node_managers
    ADD CONSTRAINT node_managers_credentials_type_key UNIQUE (credentials, type);


--
-- Name: node_managers_nodes node_managers_nodes_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.node_managers_nodes
    ADD CONSTRAINT node_managers_nodes_pkey PRIMARY KEY (manager_id, node_id);


--
-- Name: node_managers node_managers_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.node_managers
    ADD CONSTRAINT node_managers_pkey PRIMARY KEY (id);


--
-- Name: nodes nodes_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes
    ADD CONSTRAINT nodes_pkey PRIMARY KEY (id);


--
-- Name: nodes_projects nodes_projects_node_id_project_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes_projects
    ADD CONSTRAINT nodes_projects_node_id_project_id_key UNIQUE (node_id, project_id);


--
-- Name: nodes nodes_source_id_source_region_source_account_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes
    ADD CONSTRAINT nodes_source_id_source_region_source_account_id_key UNIQUE (source_id, source_region, source_account_id);


--
-- Name: projects projects_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_pkey PRIMARY KEY (id);


--
-- Name: projects projects_project_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_project_id_key UNIQUE (project_id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: tags tags_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tags
    ADD CONSTRAINT tags_pkey PRIMARY KEY (id);


--
-- Name: nodes_tags_node_id_tag_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX nodes_tags_node_id_tag_id_idx ON public.nodes_tags USING btree (node_id, tag_id);


--
-- Name: node_managers_nodes node_managers_nodes_manager_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.node_managers_nodes
    ADD CONSTRAINT node_managers_nodes_manager_id_fkey FOREIGN KEY (manager_id) REFERENCES public.node_managers(id) ON DELETE CASCADE;


--
-- Name: node_managers_nodes node_managers_nodes_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.node_managers_nodes
    ADD CONSTRAINT node_managers_nodes_node_id_fkey FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- Name: nodes_projects nodes_projects_fk; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes_projects
    ADD CONSTRAINT nodes_projects_fk FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- Name: nodes_secrets nodes_secrets_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes_secrets
    ADD CONSTRAINT nodes_secrets_node_id_fkey FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- Name: nodes_tags nodes_tags_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes_tags
    ADD CONSTRAINT nodes_tags_node_id_fkey FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- Name: nodes_tags nodes_tags_tag_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.nodes_tags
    ADD CONSTRAINT nodes_tags_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES public.tags(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

--
-- Database "notifications_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: notifications_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE notifications_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE notifications_service OWNER TO postgres;

\connect notifications_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: rule_action; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.rule_action AS ENUM (
    'SlackAlert',
    'WebhookAlert',
    'ServiceNowAlert'
);


ALTER TYPE public.rule_action OWNER TO postgres;

--
-- Name: rule_event; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.rule_event AS ENUM (
    'CCRSuccess',
    'CCRFailure',
    'ComplianceSuccess',
    'ComplianceFailure',
    'Assets'
);


ALTER TYPE public.rule_event OWNER TO postgres;

--
-- Name: log_and_clean_event(character varying, public.rule_event, smallint); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.log_and_clean_event(id character varying, event_type public.rule_event, delete_older_than smallint) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
  DECLARE
    already_processed BOOLEAN;
BEGIN
    already_processed = false;
    -- First clean up old events:
    DELETE FROM processed_events
     WHERE at < (CURRENT_TIMESTAMP - (delete_older_than * interval '1 second'));

    -- Now try to insert - failure due to duplicate insert means we already
    -- processed the event.
    BEGIN
      INSERT INTO processed_events(inbound_id, event, at)
           VALUES (id, event_type, CURRENT_TIMESTAMP);
    EXCEPTION WHEN unique_violation THEN
      already_processed = true;
    END;
    RETURN already_processed;
  END;
  $$;


ALTER FUNCTION public.log_and_clean_event(id character varying, event_type public.rule_event, delete_older_than smallint) OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.migrations (
    num integer NOT NULL,
    descr text,
    at timestamp with time zone NOT NULL
);


ALTER TABLE public.migrations OWNER TO postgres;

--
-- Name: processed_events; Type: TABLE; Schema: public; Owner: postgres
--

CREATE UNLOGGED TABLE public.processed_events (
    inbound_id character varying(64) NOT NULL,
    event public.rule_event NOT NULL,
    at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE public.processed_events OWNER TO postgres;

--
-- Name: rules; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.rules (
    id uuid NOT NULL,
    name text NOT NULL,
    event public.rule_event NOT NULL,
    action public.rule_action NOT NULL,
    url text NOT NULL,
    secret_id character varying,
    critical_controls_only boolean DEFAULT false
);


ALTER TABLE public.rules OWNER TO postgres;

--
-- Data for Name: migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.migrations (num, descr, at) FROM stdin;
1	Migration tracking setup	2022-06-03 09:19:29.100153+00
2	Create notifications data types and relations	2022-06-03 09:19:29.102341+00
3	Enable deduplication of received events	2022-06-03 09:19:29.12397+00
4	Add ServiceNowAlert as a rule_action	2022-06-03 09:19:29.148168+00
5	Add secret ID to rules table	2022-06-03 09:19:29.153475+00
6	Add support for Assets event	2022-06-03 09:19:29.163009+00
7	Add critical_controls_only to rules table	2022-06-03 09:19:29.164284+00
8	UPDATE rules SET critical_controls_only=FALSE	2022-06-03 09:19:29.170817+00
9	DELETE rules with event='Assets'	2022-06-03 09:19:29.174197+00
\.


--
-- Data for Name: processed_events; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.processed_events (inbound_id, event, at) FROM stdin;
\.


--
-- Data for Name: rules; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.rules (id, name, event, action, url, secret_id, critical_controls_only) FROM stdin;
\.


--
-- Name: processed_events processed_events_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.processed_events
    ADD CONSTRAINT processed_events_pkey PRIMARY KEY (inbound_id, event);


--
-- Name: rules rules_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.rules
    ADD CONSTRAINT rules_name_key UNIQUE (name);


--
-- Name: rules rules_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.rules
    ADD CONSTRAINT rules_pkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--

--
-- Database "postgres" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

DROP DATABASE postgres;
--
-- Name: postgres; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE postgres WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE postgres OWNER TO postgres;

\connect postgres

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: DATABASE postgres; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON DATABASE postgres IS 'default administrative connection database';


--
-- PostgreSQL database dump complete
--

--
-- Database "secrets_service" dump
--

--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 14.3 (Ubuntu 14.3-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: secrets_service; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE secrets_service WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.utf8';


ALTER DATABASE secrets_service OWNER TO postgres;

\connect secrets_service

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: s_secrets; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.s_secrets (
    id text NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    type text DEFAULT ''::text NOT NULL,
    last_modified timestamp without time zone DEFAULT now() NOT NULL,
    data text NOT NULL
);


ALTER TABLE public.s_secrets OWNER TO postgres;

--
-- Name: s_secrets_tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.s_secrets_tags (
    secret_id text NOT NULL,
    tag_id text NOT NULL
);


ALTER TABLE public.s_secrets_tags OWNER TO postgres;

--
-- Name: s_tags; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.s_tags (
    id text NOT NULL,
    key text NOT NULL,
    value text DEFAULT ''::text NOT NULL
);


ALTER TABLE public.s_tags OWNER TO postgres;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    dirty boolean NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO postgres;

--
-- Data for Name: s_secrets; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.s_secrets (id, name, type, last_modified, data) FROM stdin;
\.


--
-- Data for Name: s_secrets_tags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.s_secrets_tags (secret_id, tag_id) FROM stdin;
\.


--
-- Data for Name: s_tags; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.s_tags (id, key, value) FROM stdin;
\.


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.schema_migrations (version, dirty) FROM stdin;
1	f
\.


--
-- Name: s_secrets s_secrets_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.s_secrets
    ADD CONSTRAINT s_secrets_pkey PRIMARY KEY (id);


--
-- Name: s_tags s_tags_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.s_tags
    ADD CONSTRAINT s_tags_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: s_secrets_tags s_secrets_tags_secret_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.s_secrets_tags
    ADD CONSTRAINT s_secrets_tags_secret_id_fkey FOREIGN KEY (secret_id) REFERENCES public.s_secrets(id) ON DELETE CASCADE;


--
-- Name: s_secrets_tags s_secrets_tags_tag_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.s_secrets_tags
    ADD CONSTRAINT s_secrets_tags_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES public.s_tags(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

--
-- PostgreSQL database cluster dump complete
--

