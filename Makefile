NAME="compurtov"

run:
	cabal run

clean:
	cabal clean

re: clean run

test:
	cabal run tests

docker/build:
	docker build -t $(NAME):latest .

docker/run: docker/build
	docker run --rm --name $(NAME) -it $(NAME)

docker/clean:
	docker image rm -f $(NAME):latest

.PHONY: all run clean re docker/build docker/run docker/clean test
