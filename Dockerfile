FROM fpco/stack-build

WORKDIR /usr/src

COPY . .
RUN stack setup
RUN stack build
