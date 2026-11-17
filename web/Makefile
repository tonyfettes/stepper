PNPM ?= pnpm
PYTHON ?= python3
PORT ?= 8001

.PHONY: all build deps dev serve

all: build

build:
	$(PNPM) run build

deps:
	opam install ./stepper-web.opam --deps-only
	$(PNPM) install --frozen-lockfile

dev:
	$(PNPM) run dev

serve:
	@test -f dist/index.html || { echo "Run 'make' in web/ before starting the server."; exit 1; }
	$(PYTHON) -m http.server $(PORT) --bind 127.0.0.1 --directory dist
