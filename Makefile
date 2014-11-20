SOURCES=$(shell find src -type f -name '*.hs')

HOTHASKTAGS=$(shell which hothasktags 2>/dev/null)
CTAGS=$(if $(HOTHASKTAGS),$(HOTHASKTAGS),/bin/false)

STYLISHHASKELL=$(shell which stylish-haskell 2>/dev/null)
STYLISH=$(if $(STYLISHHASKELL),$(STYLISHHASKELL),/bin/false)

all: tags format

.PHONY: all test clean format

tags: $(SOURCES)
	@if [ "$(HOTHASKTAGS)" ] ; then /bin/echo -e "CTAGS\ttags" ; fi
	@$(CTAGS) $^ > tags $(REDIRECT)

clean:
	@rm -f tags
	cabal clean >/dev/null

lint: $(SOURCES)
	for i in $^; do hlint $$i; done

test:
	cabal test

build:
	cabal build

format: .stylish-haskell.yaml $(SOURCES)
	$(STYLISH) -c $< -i $(filter-out $<,$^)
