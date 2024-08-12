# Closed Issues

## Setup Nginx

### Branch - [setup-nginx](https://git.sr.ht/~jamesaorson/reformer/tree/setup-nginx)

### Overview

[`nginx`](https://nginx.org/en/) is a super simple load balancer/web server, which also serves static file serving.

### Instructions

Set up a simple nginx, with a proxy pass from port 88 to 8080

Something like this

```nginx
server {
  listen        88;

  access_log /var/log/nginx/access.log;
  error_log /var/log/nginx/error.log notice;
  rewrite_log on;

  location / {
    proxy_http_version 1.1;
    proxy_set_header   Upgrade $http_upgrade;
    proxy_set_header   Connection keep-alive;
    proxy_set_header   Host $host;
    proxy_cache_bypass $http_upgrade;
    proxy_set_header   X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header   X-Forwarded-Proto $scheme;

    proxy_pass         http://0.0.0.0:8080;
  }
}
```

### Acceptance Criteria

- Have nginx run on port 88
- Have port 88 and 8080 exposing the same thing, but only 88 goes through the LB
- Get Digital Ocean to pass through the LB port

## Setup hosting solution

### Branch (n/a)

### Overview

Since the backend does SSR of the page, we need a hosting solution that will run the code and be routable,
since we won't be generating static webpages.

### Instructions

1. Deploy to a hosting solution (Using Digital Ocean)
1. Have a CD pipeline to automatically update the hosted site (builds a docker container on every push to main)

### Acceptance Criteria

- Automatically updated hosting site
- Automatic SSL updates
- Cheap


## Public Domain

### Branch - [public-domain](https://git.sr.ht/~jamesaorson/reformer/tree/public-domain)

### Overview

Project should be public domain, ala [`The Dorean Principle`](https://thedoreanprinciple.org) found throughout scripture.

### Instructions

Add a public domain disclaimer to the codebase

### Acceptance Criteria

Do something like SQLite's `LICENSE.md`:

```markdown
The author disclaims copyright to this source code.  In place of
a legal notice, here is a blessing:

  *   May you do good and not evil.
  *   May you find forgiveness for yourself and forgive others.
  *   May you share freely, never taking more than you give.
```

## Add directory for external docs

### Branch - [external-docs](https://git.sr.ht/~jamesaorson/reformer/tree/external-docs)

### Overview

Many of the Lisp technologies used have comprehensive HTML manuals. It would be great to have these available for offline.
development.

### Instructions

Where possible, we should store these documents local to this project. We intend to eventually post the docs for
[`Reformer`](https://reformer.fyi) on a website, so put these in a location that would not be included. So do not use the
[`docs/`](../docs) directory, but rather [`external/docs/`](../external/docs).

### Acceptance Criteria

- Create the [`external/docs/`](../external/docs) directory.
- Link to this directory from the main [`README.md`](../README.md).

## Setup ASDF project

### Branch - [setup-asdf](https://git.sr.ht/~jamesaorson/reformer/tree/setup-asdf)

### Overview

[`ASDF`](https://asdf.common-lisp.dev/) is the ubiquitous build facility/project management tool for Common Lisp.

### Instructions

Set up the base project/system definition required by `ASDF`, and store the docs local to our project.

### Acceptance Criteria

- Add `ASDF` to the `deps` target in the [`Makefile`](../Makefile).
- Add `ASDF` manual to [`external/docs/`](../external/docs).
- Create [`reformer.asd`](../src/reformer.asd).
