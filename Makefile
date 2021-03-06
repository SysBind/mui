build: src/*.elm #src/**/*.elm
	elm make src/App.elm --output mui.js

dist: src/*.elm #src/**/*.elm
	elm make  --optimize src/App.elm --output mui.js

dev: build
	cp -v index.html mui.js ../moodle/

clean:
	rm -f demo.js

distclean: clean
	rm -rf elm-stuff
