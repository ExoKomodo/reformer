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

COPY ./nginx/nginx.conf /etc/nginx/nginx.conf
COPY ./Makefile /app/Makefile
COPY ./external /app/external
COPY ./nginx /app/nginx
COPY ./www /app/www
WORKDIR /app
RUN make setup \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*
RUN make setup-lb
COPY ./src /app/src

EXPOSE 88
EXPOSE 8080

CMD [ "make", "run-with-lb" ]
