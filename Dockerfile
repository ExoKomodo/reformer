FROM debian:12-slim

RUN apt-get update -y \
    && apt-get install -y \
        curl \
        guile-3.0 \
        lsof \
        make \
        procps \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

COPY ./Makefile /app/Makefile
COPY ./external /app/external
WORKDIR /app
RUN make setup \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*
COPY ./src /app/src

CMD [ "make", "run" ]
