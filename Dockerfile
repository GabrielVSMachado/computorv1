FROM haskell:9.4.8-slim

COPY . .

ENTRYPOINT [ "make" ]
