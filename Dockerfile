# Builder image
FROM haskell:8.0.1

WORKDIR /usr/src

COPY Resource-DSL.cabal stack.yaml ./
RUN stack update && stack build \
    --system-ghc \
    --only-dependencies

COPY . .
RUN stack install \
    --local-bin-path /usr/src \
    --system-ghc



# Executable image
FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -y libgmp10 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /usr/src
COPY --from=0 /usr/src/resource-dsl .
CMD ["./resource-dsl", "--help"]
