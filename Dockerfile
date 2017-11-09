FROM haskell:8

WORKDIR /usr/src

RUN apt-get update
RUN apt-get install -y build-essential

COPY . .
RUN stack setup
RUN stack build --copy-bins


# Executable image
FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -y libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /usr/src
COPY --from=0 /root/.local/bin/resource-dsl .
CMD ["./resource-dsl", "--help"]
