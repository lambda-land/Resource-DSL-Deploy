FROM haskell:8

WORKDIR /usr/src

RUN apt-get update
RUN apt-get install -y build-essential

COPY . .
RUN stack setup
RUN stack build
