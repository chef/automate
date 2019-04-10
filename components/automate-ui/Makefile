PAGE_COMPONENT_DIR := page-components
PAGE_COMPONENT_FLAGS := --module app
COMPONENT_PREFIX := chef
COMPONENT_DIR := components
COMPONENT_FLAGS := --prefix $(COMPONENT_PREFIX) --export true
DEMO_DIR := pages/component-library/demos
DEMO_FLAGS := --spec false
NG_CMD := npm run ng --

install:
	npm install

build:
	hab studio run "source .studiorc; build"

test: unit e2e

e2e:
	npm run e2e

unit:
	npm run test

unit-all-browsers:
	npm run test -- --browsers 'Chrome,Firefox,Safari'

lint: lint-typescript lint-sass lint-html

lint-sass:
	npm run lint:sass

lint-html:
	npm run lint:html

lint-typescript:
	npm run lint
	@echo "⚠️  The warning 'select is deprecated' is no longer true as of 7.0 \
	so it is suppressed from the output: see https://github.com/ngrx/platform/pull/1361."

pr-ready: unit-all-browsers lint e2e

serve:
	npm run serve:hab

update-ui-lib:
	scripts/build_chef_ui_lib && npm run copy-ui-lib

start: install
	npm run start

start-for-vm: install
	@echo '*************************************************************************************'
	@echo 'After the server starts, load http://10.0.2.2:4200 in your browser to launch the app.'
	@echo 'WARNING: "make start-for-vm" is insecure and should only be used when you need it.'
	@echo '*************************************************************************************'
	npm run start -- --disable-host-check=true

license_scout:
	rm -rf /tmp/license_scout
	git clone --depth=1 --quiet https://github.com/chef/license_scout.git /tmp/license_scout
	cd /tmp/license_scout && bundle install --without=development --quiet && echo "license_scout version" && git rev-parse HEAD
	/tmp/license_scout/bin/license_scout

demo/%:
	$(NG_CMD) generate component $(DEMO_DIR)/demo-$(@F) $(DEMO_FLAGS)

component/%:
	$(NG_CMD) generate component $(COMPONENT_DIR)/$(@F) $(COMPONENT_FLAGS)

page-component/%:
	$(NG_CMD) generate component $(PAGE_COMPONENT_DIR)/$(@F) $(PAGE_COMPONENT_FLAGS)

.PHONY: build install test lint unit e2e lint-sass lint-typescript license_scout peer-dependencies e2e-aot unit-all-browsers pr-ready start
