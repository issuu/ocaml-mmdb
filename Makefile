.PHONY: build
build: ## Build the code
	dune build

.PHONY: clean
clean: ## Clean the source tree
	dune clean
	rm -rf etc

.PHONY: format
format: ## Reformat all code
	dune build @fmt --auto-promote

.PHONY: run
run: build etc/GeoLite2-City.mmdb
	_build/default/src/example/example.exe

etc/GeoLite2-City.mmdb:
	mkdir -p etc
	curl https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.tar.gz | tar -x -C etc
	mv etc/GeoLite2-City_*/* etc
	rm -r etc/GeoLite2-City_*

.PHONY: help
help: ## Display this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
