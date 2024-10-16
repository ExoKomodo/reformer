CREATE TABLE IF NOT EXISTS Users (
  id INTEGER PRIMARY KEY,
  handle TEXT NOT NULL,
  password_hash TEXT NOT NULL
);
DELETE FROM Users;
CREATE TABLE IF NOT EXISTS Posts (
  id INTEGER PRIMARY KEY,
  content TEXT,
  poster INTEGER,
  FOREIGN KEY(poster) REFERENCES Users(id)
);
DELETE FROM Posts;
