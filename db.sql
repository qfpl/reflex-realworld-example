-- Deploy conduit:create-articles to pg
-- requires: create-users

BEGIN;

CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  password TEXT NOT NULL,
  email TEXT UNIQUE NOT NULL,
  username TEXT UNIQUE NOT NULL,
  bio TEXT NOT NULL,
  image TEXT
);

CREATE TABLE IF NOT EXISTS articles (
  id SERIAL PRIMARY KEY,
  body TEXT NOT NULL,
  slug TEXT UNIQUE NOT NULL,
  title TEXT NOT NULL,
  description TEXT NOT NULL,
  author__id INT REFERENCES users(id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE TABLE IF NOT EXISTS tags (
  name TEXT PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS article_tags (
  article__id INT REFERENCES articles(id) ON DELETE CASCADE,
  tag__name TEXT REFERENCES tags(name) ON DELETE CASCADE,
  UNIQUE (article__id, tag__name)
);

CREATE TABLE IF NOT EXISTS comments (
  id SERIAL PRIMARY KEY,
  body TEXT NOT NULL,
  author__id INT REFERENCES users(id) ON DELETE CASCADE,
  article__id INT REFERENCES articles(id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE TABLE IF NOT EXISTS favorites (
  user__id INT REFERENCES users(id) ON DELETE CASCADE,
  article__id INT REFERENCES articles(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS follows (
  follower__id INT REFERENCES users(id) ON DELETE CASCADE,
  followee__id INT REFERENCES users(id) ON DELETE CASCADE
);

COMMIT;
