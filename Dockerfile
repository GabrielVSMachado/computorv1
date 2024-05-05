FROM haskell:9.4.8-slim

WORKDIR /computorv1

COPY . .

RUN cabal v2-install --lib HUnit

ENTRYPOINT [ "make" ]
