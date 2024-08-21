FROM debian:12-slim

RUN apt-get update -y \
    && apt-get install -y \
    curl \
    guile-3.0 \
    libsqlite3-dev \
    lsof \
    make \
    nginx \
    procps \
		sqlite3 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

COPY ./Makefile /app/Makefile
COPY ./src /app/src
WORKDIR /app
RUN make setup \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*
RUN make setup-lb

 # Nginx lb port
EXPOSE 80
 # Reformer server port
EXPOSE 8080
 # REPL listen port
EXPOSE 1689

CMD [ "make", "lb", "run-with-repl" ]
