ARG IMAGE_NAME=debian
# ARG IMAGE_NAME=arm64v8/debian

FROM ${IMAGE_NAME}:12-slim

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y \
    && apt-get install -y \
    curl \
    git \
    lsof \
    make \
    procps \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

COPY ./Makefile /app/Makefile
COPY ./src /app/src
WORKDIR /app
RUN make setup \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/* \
    && make setup-lb
COPY ./lib /app/lib
RUN rm -rf /app/lib/src/sdl2*

 # Nginx lb port
EXPOSE 80
 # Reformer server port
EXPOSE 8080
 # REPL listen port
EXPOSE 1689

CMD [ "make", "lb", "run-with-repl" ]
