SRC_FILES = $(wildcard src/*.elm)
index.html: $(SRC_FILES)
	elm make src/War.elm
