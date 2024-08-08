FROM ubuntu:noble

RUN apt-get update -y
RUN apt-get install -y \
    guile-3.0 \
    make

COPY ./src /app/src
COPY ./Makefile /app/Makefile

WORKDIR /app

RUN ls -al /app

CMD [ "make", "run" ]
