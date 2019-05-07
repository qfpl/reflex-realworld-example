--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.12
-- Dumped by pg_dump version 9.6.12

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
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.users (id, password, email, username, bio, image) FROM stdin;
5	14|8|1|9eJWoRDVyc5sKteli5y+LE0aFO4g1LhgD1ol3Yy2Tvg=|Uq8llEuBLZtdQ5pvPidP4VkeGpd1CqdeEJ+NFOcVRfeFYaDbftDxhY5sX05zdHc2nzoalzsjKgzdeFmcVcTSzw==	dashy@mlp	Dashy	Ponyville weather control expert & Trainee Wonderbolt!	/static/avatars/dashy.png
6	14|8|1|LNncVRkToJiBiULw9y8m3NQjWIdnExcTNCO+E1aYqLg=|PjTUfPPfNZVFD/p2tlT8c7kzTGI0SWT86v51CDmEUEi2ylg5llYGDXhCbjtebMR+GlaVSUgIThJYWyCUJQjbdw==	fluttershy@mlp	Fluttershy		/static/avatars/fluttershy.png
7	14|8|1|uyRA6mhsCBcVPywd8F1Jsqe8BE4bVgIthD5NwLhXCgw=|7+5+h+9OKFb68D1zvmnUkmQ0q7liuLwfKz1TCH07DBe7BH7Za9wQ65iSs8Peedfi0i6PgWrXAtqrCsnON83+zQ==	pinkiepie@mlp	PinkiePie	Loves to throw parties!	/static/avatars/pinkie.png
\.


--
-- Data for Name: articles; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.articles (id, body, slug, title, description, author__id, created_at, updated_at) FROM stdin;
3	# Things that you will need\n\n- Cake\n- Party Cannon	how-to-throw-an-awesome-party	How to Throw an Awesome Party!	Party organising techniques	7	2019-05-07 23:48:45.982494+00	2019-05-07 23:48:45.982494+00
4	TODO	how-to-become-at-least-20-cooler	How to become (at least) 20% Cooler	Being Cool	5	2019-05-07 23:51:44.856755+00	2019-05-07 23:51:44.856755+00
\.


--
-- Data for Name: tags; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.tags (name) FROM stdin;
angularjs
dragons
reactjs
\.


--
-- Data for Name: article_tags; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.article_tags (article__id, tag__name) FROM stdin;
\.


--
-- Name: articles_id_seq; Type: SEQUENCE SET; Schema: public; Owner: conduit
--

SELECT pg_catalog.setval('public.articles_id_seq', 4, true);


--
-- Data for Name: comments; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.comments (id, body, author__id, article__id, created_at, updated_at) FROM stdin;
\.


--
-- Name: comments_id_seq; Type: SEQUENCE SET; Schema: public; Owner: conduit
--

SELECT pg_catalog.setval('public.comments_id_seq', 2, true);


--
-- Data for Name: favorites; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.favorites (user__id, article__id) FROM stdin;
\.


--
-- Data for Name: follows; Type: TABLE DATA; Schema: public; Owner: conduit
--

COPY public.follows (follower__id, followee__id) FROM stdin;
\.


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: conduit
--

SELECT pg_catalog.setval('public.users_id_seq', 7, true);


--
-- PostgreSQL database dump complete
--

