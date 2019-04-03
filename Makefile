.PHONY: build
build:
	stack build

.PHONY: install
install:
	stack install

.PHONY: run
run:
	stack run

.PHONY: test
test:
	stack test

.PHONY: build/linux
build/linux:
	docker run -it -v `pwd`:/tmp/hoge hoge bin/bash -c "cd /tmp/hoge; stack build"
