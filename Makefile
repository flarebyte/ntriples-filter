SRC = src
BUILD = build

build: test build-directory js

build-directory:
	mkdir -p $(BUILD)

js:
	elm-make src/Ntriples/Filter.elm --output $(BUILD)/filter.js

test:
	cd tests && elm-test Main.elm
