FROM ubuntu:noble

COPY . .

RUN apt-get update -y
RUN apt-get install -y \
    guile \
    make

CMD [ "make", "run" ]
