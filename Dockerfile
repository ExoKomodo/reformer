FROM ubuntu:noble

RUN apt-get update -y
RUN apt-get install -y \
    guile-3.0 \
    make

COPY ./src /app
COPY ./Makefile /app/Makefile

WORKDIR /app

RUN ls -al .

CMD [ "make", "run" ]
