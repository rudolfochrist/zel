FILES = $(cask files)

.PHONY: install
install:
	cask install

*.elc: $(FILES)
	cask build

.PHONY: clean
clean:
	cask clean-elc

.PHONY: test
test: clean *.elc
	cask exec ert-runner -L . --reporter ert+duration
