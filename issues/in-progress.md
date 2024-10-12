# In-Progress Issues

Once completed, move the issue into [closed](./closed.md).

## Create the initial README

### Branch - [initial-readme](https://git.sr.ht/~jamesaorson/reformer/tree/initial-readme)

### Overview

README is entirely lacking. Provide info about the vision and purpose of the site. Also provide basic development info.
For now, structure is more important than content.

### Instructions

Make a nice README, my guy.

### Acceptance Criteria

Have info about:

- the purpose of the website.
- technologies chosen (and why).
- the development process.

## Passwordless Email Login 

### Branch - [passwordless](https://git.sr.ht/~jamesaorson/reformer/tree/passwordless)

### Overview

Instead of logging in with a password, provide an email which we send an auth code to.

### Instructions

- Send emails over SMTP
- Send a generated code
- Store the code in a table in Postgres for known passwordless auth codes

### Acceptance Criteria

Have info about:

- login without passwords
- no password concept in the app
- allow to easily swap out for something like Redis later

