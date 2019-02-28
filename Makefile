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

.PHONY: example
example: etc/GeoLite2-City.mmdb ## Build and run the example
	dune exec src/example/example.exe

etc/GeoLite2-City.mmdb:
	mkdir -p etc
	curl https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.tar.gz | tar -xz -C etc
	mv etc/GeoLite2-City_*/* etc
	rm -r etc/GeoLite2-City_*

.PHONY: help
help: ## Display this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: docker-ci
docker-ci: ## Build the code and run the example on Docker
	docker build -t ocaml-mmdb-ci -f Dockerfile.ci .
	docker run ocaml-mmdb-ci
