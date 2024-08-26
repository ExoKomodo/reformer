CREATE TABLE IF NOT EXISTS Users (id INTEGER PRIMARY KEY, name STRING);
DELETE FROM Posts;
DELETE FROM Users;
INSERT INTO Users ('name') VALUES ('jamesaorson');
INSERT INTO Users ('name') VALUES ('nbarlow');
CREATE TABLE IF NOT EXISTS Posts (id INTEGER PRIMARY KEY, content STRING, user INTEGER, FOREIGN KEY(user) REFERENCES Users(id));
INSERT INTO Posts ('content', 'user') VALUES ('Hello brosefus', 1);
INSERT INTO Posts ('content', 'user') VALUES ('bro', 2);

