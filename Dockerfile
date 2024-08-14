FROM debian:12-slim

RUN apt-get update -y \
    && apt-get install -y \
        curl \
        guile-3.0 \
        lsof \
        make \
        nginx \
        procps \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

COPY ./Makefile /app/Makefile
COPY ./external /app/external
COPY ./src /app/src
WORKDIR /app
RUN make setup \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*
RUN make setup-lb

EXPOSE 88
EXPOSE 8080

CMD [ "make", "run-with-lb" ]
