package integration

// PgDump1dot0Schema is the schema created by the 1.0 (elixir) version of the
// notifications service.
// * Exact version used: chef/notifications-service/1.0.0/20210208193336
// * pg_dump command:
/*
hab pkg exec core/postgresql-client pg_dump \
  "postgresql://notifications@127.0.0.1:10145/notifications_service?sslmode=verify-ca&sslcert=/hab/svc/notifications-service/config/service.crt&sslkey=/hab/svc/notifications-service/config/service.key&sslrootcert=/hab/svc/notifications-service/config/root_ca.crt" \
	--format=plain \
	--schema=public \
	--inserts \
	--no-privileges \
	--no-owner
*/
const PgDump1dot0Schema = `
--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.11
-- Dumped by pg_dump version 9.6.11

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: rule_action; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.rule_action AS ENUM (
    'SlackAlert',
    'WebhookAlert',
    'ServiceNowAlert'
);


--
-- Name: rule_event; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.rule_event AS ENUM (
    'CCRSuccess',
    'CCRFailure',
    'ComplianceSuccess',
    'ComplianceFailure',
    'Assets'
);


--
-- Name: log_and_clean_event(character varying, public.rule_event, smallint); Type: FUNCTION; Schema: public; Owner: -
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


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.migrations (
    num integer NOT NULL,
    descr text,
    at timestamp with time zone NOT NULL
);


--
-- Name: processed_events; Type: TABLE; Schema: public; Owner: -
--

CREATE UNLOGGED TABLE public.processed_events (
    inbound_id character varying(64) NOT NULL,
    event public.rule_event NOT NULL,
    at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: rules; Type: TABLE; Schema: public; Owner: -
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


--
-- Data for Name: migrations; Type: TABLE DATA; Schema: public; Owner: -
--

INSERT INTO public.migrations VALUES (1, 'Migration tracking setup', '2021-02-10 22:35:56.038134+00');
INSERT INTO public.migrations VALUES (2, 'Create notifications data types and relations', '2021-02-10 22:35:56.042387+00');
INSERT INTO public.migrations VALUES (3, 'Enable deduplication of received events', '2021-02-10 22:35:56.063957+00');
INSERT INTO public.migrations VALUES (4, 'Add ServiceNowAlert as a rule_action', '2021-02-10 22:35:56.080764+00');
INSERT INTO public.migrations VALUES (5, 'Add secret ID to rules table', '2021-02-10 22:35:56.085704+00');
INSERT INTO public.migrations VALUES (6, 'Add support for Assets event', '2021-02-10 22:35:56.089865+00');
INSERT INTO public.migrations VALUES (7, 'Add critical_controls_only to rules table', '2021-02-10 22:35:56.091723+00');
INSERT INTO public.migrations VALUES (8, 'UPDATE rules SET critical_controls_only=FALSE', '2021-02-10 22:35:56.102042+00');
INSERT INTO public.migrations VALUES (9, 'DELETE rules with event=''Assets''', '2021-02-10 22:35:56.107859+00');


--
-- Data for Name: processed_events; Type: TABLE DATA; Schema: public; Owner: -
--



--
-- Data for Name: rules; Type: TABLE DATA; Schema: public; Owner: -
--



--
-- Name: processed_events processed_events_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.processed_events
    ADD CONSTRAINT processed_events_pkey PRIMARY KEY (inbound_id, event);


--
-- Name: rules rules_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rules
    ADD CONSTRAINT rules_name_key UNIQUE (name);


--
-- Name: rules rules_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rules
    ADD CONSTRAINT rules_pkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--

`
