clean: clean-e2e
	rm -rf dist/*

depclean:
	rm -rf node_modules
	npm cache clean

allclean: clean depclean

install: installnodedeps installwebdriver

installnodedeps:
	npm install

installwebdriver:
ifndef OMNIBUS
	npm run install-webdriver
endif

build:
	npm run build:prod

devbuild:
	npm run build:dev

test: unit

unit:
	npm run test:unit

e2e: clean-e2e
	$(MAKE) build-e2e run-e2e

e2e-ci: clean-e2e
	$(MAKE) build-e2e run-e2e-ci

startdev: install
	$(MAKE) build watch

watch:
	npm run watch

build-e2e: build
	mkdir -p .e2ebuild/e/Chef
	cp -r dist/* .e2ebuild
	cp -r dist/* .e2ebuild/e/Chef

run-e2e:
	npm run test:e2e

run-e2e-ci:
	npm run test:e2e:ci

clean-e2e:
	rm -rf .e2ebuild

.PHONY: clean depclean allclean \
	devbuild build \
	install installnodedeps installwebdriver \
	test unit e2e e2e-ci build-e2e run-e2e run-e2e-ci clean-e2e \
	startdev watch

# Leaving these in here, but I was tempted to remove them because
# I'm not sure they're used anymore. Can anyone comment?

.PHONY: e2e-stack
e2e-stack: clean-e2e
	$(MAKE) build-e2e run-e2e-stack

.PHONY: run-e2e-stack
run-e2e-stack:
	node_modules/.bin/protractor test/e2e/protractor.stack.conf.js --verbose

.PHONY: e2e-sauce
e2e-sauce: clean-e2e
	$(MAKE) build-e2e run-e2e-sauce

.PHONY: run-e2e-sauce
run-e2e-sauce:
	node_modules/.bin/protractor test/e2e/protractor.sauce.conf.js --verbose
