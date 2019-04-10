package main

// created via
// hab pkg exec core/postgresql pg_dump delivery --no-privileges --no-owner --clean --verbose -f /a1-migration/db.sql -N sqitch --if-exists -t s_tags -t s_secrets -t s_secrets_tags -t agents -t node_managers -t results -t profiles -t tags -t jobs -t jobs_nodes -t jobs_profiles -t jobs_tags -t nodes -t nodes_agents -t nodes_secrets -t nodes_tags
// in the a1-migration container (after `su` to chef-pgsql)
const defaultPGDumpContents = `
---
---  This SQL file was created by the Automate 2 pg_dump TEST stub.
---  To test real data import, install the chef/a1-migration-data package from habitat.
--
-- PostgreSQL database dump
--

-- Dumped from database version 9.3.14
-- Dumped by pg_dump version 9.6.9

-- Started on 2018-09-07 15:37:25 UTC

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

ALTER TABLE IF EXISTS ONLY public.s_secrets_tags DROP CONSTRAINT IF EXISTS s_secrets_tags_tag_id_fkey;
ALTER TABLE IF EXISTS ONLY public.s_secrets_tags DROP CONSTRAINT IF EXISTS s_secrets_tags_secret_id_fkey;
ALTER TABLE IF EXISTS ONLY public.results DROP CONSTRAINT IF EXISTS results_node_id_fkey;
ALTER TABLE IF EXISTS ONLY public.results DROP CONSTRAINT IF EXISTS results_job_id_fkey;
ALTER TABLE IF EXISTS ONLY public.nodes_tags DROP CONSTRAINT IF EXISTS nodes_tags_tag_id_fkey;
ALTER TABLE IF EXISTS ONLY public.nodes_tags DROP CONSTRAINT IF EXISTS nodes_tags_node_id_fkey;
ALTER TABLE IF EXISTS ONLY public.nodes_secrets DROP CONSTRAINT IF EXISTS nodes_secrets_node_id_fkey;
ALTER TABLE IF EXISTS ONLY public.nodes_agents DROP CONSTRAINT IF EXISTS nodes_agents_node_id_fkey;
ALTER TABLE IF EXISTS ONLY public.nodes_agents DROP CONSTRAINT IF EXISTS nodes_agents_agent_id_fkey;
ALTER TABLE IF EXISTS ONLY public.jobs_tags DROP CONSTRAINT IF EXISTS jobs_tags_tag_id_fkey;
ALTER TABLE IF EXISTS ONLY public.jobs_tags DROP CONSTRAINT IF EXISTS jobs_tags_job_id_fkey;
ALTER TABLE IF EXISTS ONLY public.jobs_profiles DROP CONSTRAINT IF EXISTS jobs_profiles_profile_id_fkey;
ALTER TABLE IF EXISTS ONLY public.jobs_profiles DROP CONSTRAINT IF EXISTS jobs_profiles_job_id_fkey;
ALTER TABLE IF EXISTS ONLY public.jobs_nodes DROP CONSTRAINT IF EXISTS jobs_nodes_node_id_fkey;
ALTER TABLE IF EXISTS ONLY public.jobs_nodes DROP CONSTRAINT IF EXISTS jobs_nodes_job_id_fkey;
ALTER TABLE IF EXISTS ONLY public.tags DROP CONSTRAINT IF EXISTS tags_pkey;
ALTER TABLE IF EXISTS ONLY public.s_tags DROP CONSTRAINT IF EXISTS s_tags_pkey;
ALTER TABLE IF EXISTS ONLY public.s_secrets DROP CONSTRAINT IF EXISTS s_secrets_pkey;
ALTER TABLE IF EXISTS ONLY public.profiles DROP CONSTRAINT IF EXISTS profiles_pkey;
ALTER TABLE IF EXISTS ONLY public.nodes DROP CONSTRAINT IF EXISTS nodes_pkey;
ALTER TABLE IF EXISTS ONLY public.node_managers DROP CONSTRAINT IF EXISTS node_managers_pkey;
ALTER TABLE IF EXISTS ONLY public.jobs DROP CONSTRAINT IF EXISTS jobs_pkey;
ALTER TABLE IF EXISTS ONLY public.agents DROP CONSTRAINT IF EXISTS agents_pkey;
DROP TABLE IF EXISTS public.tags;
DROP TABLE IF EXISTS public.s_tags;
DROP TABLE IF EXISTS public.s_secrets_tags;
DROP TABLE IF EXISTS public.s_secrets;
DROP TABLE IF EXISTS public.results;
DROP TABLE IF EXISTS public.profiles;
DROP TABLE IF EXISTS public.nodes_tags;
DROP TABLE IF EXISTS public.nodes_secrets;
DROP TABLE IF EXISTS public.nodes_agents;
DROP TABLE IF EXISTS public.nodes;
DROP TABLE IF EXISTS public.node_managers;
DROP TABLE IF EXISTS public.jobs_tags;
DROP TABLE IF EXISTS public.jobs_profiles;
DROP TABLE IF EXISTS public.jobs_nodes;
DROP TABLE IF EXISTS public.jobs;
DROP TABLE IF EXISTS public.agents;
SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 257 (class 1259 OID 17671)
-- Name: agents; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agents (
    id text NOT NULL,
    type text DEFAULT ''::text NOT NULL,
    status text DEFAULT ''::text NOT NULL
);


--
-- TOC entry 259 (class 1259 OID 17724)
-- Name: jobs; Type: TABLE; Schema: public; Owner: -
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
    job_count integer DEFAULT 0 NOT NULL
);


--
-- TOC entry 261 (class 1259 OID 17753)
-- Name: jobs_nodes; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.jobs_nodes (
    job_id text NOT NULL,
    node_id text NOT NULL
);


--
-- TOC entry 263 (class 1259 OID 17778)
-- Name: jobs_profiles; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.jobs_profiles (
    job_id text NOT NULL,
    profile_id text NOT NULL
);


--
-- TOC entry 260 (class 1259 OID 17737)
-- Name: jobs_tags; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.jobs_tags (
    job_id text NOT NULL,
    tag_id text NOT NULL
);


--
-- TOC entry 266 (class 1259 OID 17835)
-- Name: node_managers; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.node_managers (
    id text NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    type text DEFAULT ''::text NOT NULL,
    credentials text DEFAULT ''::text NOT NULL,
    status_type text DEFAULT ''::text NOT NULL,
    status_message text DEFAULT ''::text NOT NULL
);


--
-- TOC entry 255 (class 1259 OID 17641)
-- Name: nodes; Type: TABLE; Schema: public; Owner: -
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
    last_job text DEFAULT ''::text NOT NULL
);


--
-- TOC entry 258 (class 1259 OID 17681)
-- Name: nodes_agents; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.nodes_agents (
    node_id text NOT NULL,
    agent_id text NOT NULL
);


--
-- TOC entry 264 (class 1259 OID 17794)
-- Name: nodes_secrets; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.nodes_secrets (
    node_id text NOT NULL,
    secret_id text NOT NULL
);


--
-- TOC entry 256 (class 1259 OID 17655)
-- Name: nodes_tags; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.nodes_tags (
    node_id text NOT NULL,
    tag_id text NOT NULL
);


--
-- TOC entry 262 (class 1259 OID 17769)
-- Name: profiles; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.profiles (
    id text NOT NULL,
    url text DEFAULT ''::text NOT NULL
);


--
-- TOC entry 265 (class 1259 OID 17815)
-- Name: results; Type: TABLE; Schema: public; Owner: -
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


--
-- TOC entry 268 (class 1259 OID 17865)
-- Name: s_secrets; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.s_secrets (
    id text NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    type text DEFAULT ''::text NOT NULL,
    last_modified timestamp without time zone DEFAULT now() NOT NULL,
    data text NOT NULL
);


--
-- TOC entry 269 (class 1259 OID 17876)
-- Name: s_secrets_tags; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.s_secrets_tags (
    secret_id text NOT NULL,
    tag_id text NOT NULL
);


--
-- TOC entry 267 (class 1259 OID 17856)
-- Name: s_tags; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.s_tags (
    id text NOT NULL,
    key text NOT NULL,
    value text DEFAULT ''::text NOT NULL
);


--
-- TOC entry 254 (class 1259 OID 17632)
-- Name: tags; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tags (
    id text NOT NULL,
    key text NOT NULL,
    value text DEFAULT ''::text NOT NULL
);


--
-- TOC entry 2412 (class 0 OID 17671)
-- Dependencies: 257
-- Data for Name: agents; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.agents (id, type, status) FROM stdin;
\.


--
-- TOC entry 2414 (class 0 OID 17724)
-- Dependencies: 259
-- Data for Name: jobs; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.jobs (id, name, type, timeout, retries, retries_left, status, start_time, end_time, node_selectors, scheduled_time, recurrence, parent_id, job_count) FROM stdin;
\.


--
-- TOC entry 2416 (class 0 OID 17753)
-- Dependencies: 261
-- Data for Name: jobs_nodes; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.jobs_nodes (job_id, node_id) FROM stdin;
\.


--
-- TOC entry 2418 (class 0 OID 17778)
-- Dependencies: 263
-- Data for Name: jobs_profiles; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.jobs_profiles (job_id, profile_id) FROM stdin;
\.


--
-- TOC entry 2415 (class 0 OID 17737)
-- Dependencies: 260
-- Data for Name: jobs_tags; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.jobs_tags (job_id, tag_id) FROM stdin;
\.


--
-- TOC entry 2421 (class 0 OID 17835)
-- Dependencies: 266
-- Data for Name: node_managers; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.node_managers (id, name, type, credentials, status_type, status_message) FROM stdin;
\.


--
-- TOC entry 2410 (class 0 OID 17641)
-- Dependencies: 255
-- Data for Name: nodes; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.nodes (id, name, platform, platform_version, status, last_contact, manager, target_config, last_job) FROM stdin;
\.


--
-- TOC entry 2413 (class 0 OID 17681)
-- Dependencies: 258
-- Data for Name: nodes_agents; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.nodes_agents (node_id, agent_id) FROM stdin;
\.


--
-- TOC entry 2419 (class 0 OID 17794)
-- Dependencies: 264
-- Data for Name: nodes_secrets; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.nodes_secrets (node_id, secret_id) FROM stdin;
\.


--
-- TOC entry 2411 (class 0 OID 17655)
-- Dependencies: 256
-- Data for Name: nodes_tags; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.nodes_tags (node_id, tag_id) FROM stdin;
\.


--
-- TOC entry 2417 (class 0 OID 17769)
-- Dependencies: 262
-- Data for Name: profiles; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.profiles (id, url) FROM stdin;
\.


--
-- TOC entry 2420 (class 0 OID 17815)
-- Dependencies: 265
-- Data for Name: results; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.results (job_id, node_id, report_id, status, result, start_time, end_time) FROM stdin;
\.


--
-- TOC entry 2423 (class 0 OID 17865)
-- Dependencies: 268
-- Data for Name: s_secrets; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.s_secrets (id, name, type, last_modified, data) FROM stdin;
\.


--
-- TOC entry 2424 (class 0 OID 17876)
-- Dependencies: 269
-- Data for Name: s_secrets_tags; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.s_secrets_tags (secret_id, tag_id) FROM stdin;
\.


--
-- TOC entry 2422 (class 0 OID 17856)
-- Dependencies: 267
-- Data for Name: s_tags; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.s_tags (id, key, value) FROM stdin;
\.


--
-- TOC entry 2409 (class 0 OID 17632)
-- Dependencies: 254
-- Data for Name: tags; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.tags (id, key, value) FROM stdin;
\.


--
-- TOC entry 2269 (class 2606 OID 17680)
-- Name: agents agents_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agents
    ADD CONSTRAINT agents_pkey PRIMARY KEY (id);


--
-- TOC entry 2271 (class 2606 OID 17736)
-- Name: jobs jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.jobs
    ADD CONSTRAINT jobs_pkey PRIMARY KEY (id);


--
-- TOC entry 2275 (class 2606 OID 17847)
-- Name: node_managers node_managers_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.node_managers
    ADD CONSTRAINT node_managers_pkey PRIMARY KEY (id);


--
-- TOC entry 2267 (class 2606 OID 17654)
-- Name: nodes nodes_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nodes
    ADD CONSTRAINT nodes_pkey PRIMARY KEY (id);


--
-- TOC entry 2273 (class 2606 OID 17777)
-- Name: profiles profiles_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.profiles
    ADD CONSTRAINT profiles_pkey PRIMARY KEY (id);


--
-- TOC entry 2279 (class 2606 OID 17875)
-- Name: s_secrets s_secrets_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.s_secrets
    ADD CONSTRAINT s_secrets_pkey PRIMARY KEY (id);


--
-- TOC entry 2277 (class 2606 OID 17864)
-- Name: s_tags s_tags_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.s_tags
    ADD CONSTRAINT s_tags_pkey PRIMARY KEY (id);


--
-- TOC entry 2265 (class 2606 OID 17640)
-- Name: tags tags_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tags
    ADD CONSTRAINT tags_pkey PRIMARY KEY (id);


--
-- TOC entry 2287 (class 2606 OID 17759)
-- Name: jobs_nodes jobs_nodes_job_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.jobs_nodes
    ADD CONSTRAINT jobs_nodes_job_id_fkey FOREIGN KEY (job_id) REFERENCES public.jobs(id) ON DELETE CASCADE;


--
-- TOC entry 2286 (class 2606 OID 17764)
-- Name: jobs_nodes jobs_nodes_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.jobs_nodes
    ADD CONSTRAINT jobs_nodes_node_id_fkey FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- TOC entry 2289 (class 2606 OID 17784)
-- Name: jobs_profiles jobs_profiles_job_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.jobs_profiles
    ADD CONSTRAINT jobs_profiles_job_id_fkey FOREIGN KEY (job_id) REFERENCES public.jobs(id) ON DELETE CASCADE;


--
-- TOC entry 2288 (class 2606 OID 17810)
-- Name: jobs_profiles jobs_profiles_profile_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.jobs_profiles
    ADD CONSTRAINT jobs_profiles_profile_id_fkey FOREIGN KEY (profile_id) REFERENCES public.profiles(id);


--
-- TOC entry 2285 (class 2606 OID 17743)
-- Name: jobs_tags jobs_tags_job_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.jobs_tags
    ADD CONSTRAINT jobs_tags_job_id_fkey FOREIGN KEY (job_id) REFERENCES public.jobs(id) ON DELETE CASCADE;


--
-- TOC entry 2284 (class 2606 OID 17748)
-- Name: jobs_tags jobs_tags_tag_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.jobs_tags
    ADD CONSTRAINT jobs_tags_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES public.tags(id) ON DELETE CASCADE;


--
-- TOC entry 2282 (class 2606 OID 17692)
-- Name: nodes_agents nodes_agents_agent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nodes_agents
    ADD CONSTRAINT nodes_agents_agent_id_fkey FOREIGN KEY (agent_id) REFERENCES public.agents(id) ON DELETE CASCADE;


--
-- TOC entry 2283 (class 2606 OID 17687)
-- Name: nodes_agents nodes_agents_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nodes_agents
    ADD CONSTRAINT nodes_agents_node_id_fkey FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- TOC entry 2290 (class 2606 OID 17800)
-- Name: nodes_secrets nodes_secrets_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nodes_secrets
    ADD CONSTRAINT nodes_secrets_node_id_fkey FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- TOC entry 2281 (class 2606 OID 17661)
-- Name: nodes_tags nodes_tags_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nodes_tags
    ADD CONSTRAINT nodes_tags_node_id_fkey FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- TOC entry 2280 (class 2606 OID 17666)
-- Name: nodes_tags nodes_tags_tag_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.nodes_tags
    ADD CONSTRAINT nodes_tags_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES public.tags(id) ON DELETE CASCADE;


--
-- TOC entry 2292 (class 2606 OID 17825)
-- Name: results results_job_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.results
    ADD CONSTRAINT results_job_id_fkey FOREIGN KEY (job_id) REFERENCES public.jobs(id) ON DELETE CASCADE;


--
-- TOC entry 2291 (class 2606 OID 17830)
-- Name: results results_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.results
    ADD CONSTRAINT results_node_id_fkey FOREIGN KEY (node_id) REFERENCES public.nodes(id) ON DELETE CASCADE;


--
-- TOC entry 2294 (class 2606 OID 17882)
-- Name: s_secrets_tags s_secrets_tags_secret_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.s_secrets_tags
    ADD CONSTRAINT s_secrets_tags_secret_id_fkey FOREIGN KEY (secret_id) REFERENCES public.s_secrets(id) ON DELETE CASCADE;


--
-- TOC entry 2293 (class 2606 OID 17887)
-- Name: s_secrets_tags s_secrets_tags_tag_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.s_secrets_tags
    ADD CONSTRAINT s_secrets_tags_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES public.s_tags(id) ON DELETE CASCADE;


-- Completed on 2018-09-07 15:37:25 UTC

--
-- PostgreSQL database dump complete
--


`
