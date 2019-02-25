clean:
	dune clean
	rm -rf etc

build:
	dune build

format:
	dune build @fmt --auto-promote

run: build etc/GeoLite2-City.mmdb
	_build/default/src/example/example.exe

etc/GeoLite2-City.mmdb:
	mkdir -p etc
	curl https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.tar.gz | tar -x -C etc
	mv etc/GeoLite2-City_*/* etc
	rm -r etc/GeoLite2-City_*
