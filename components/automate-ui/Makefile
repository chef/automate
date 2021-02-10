PAGE_DIR := pages
PAGE_FLAGS := --module app
PAGE_COMPONENT_DIR := page-components
PAGE_COMPONENT_FLAGS := --module app
COMPONENT_PREFIX := chef
COMPONENT_DIR := components
COMPONENT_FLAGS := --prefix $(COMPONENT_PREFIX) --export true
DEMO_DIR := pages/component-library/demos
DEMO_FLAGS := --spec false
NG_CMD := npm run ng --
REPOROOT=../..

# Semgrep by default respects .gitignore; these are additive:
SEMGREP_IGNORE := --exclude *.spec.ts --exclude coverage --exclude modernizr-custom.js
SEMGREP_CONFIG := https://semgrep.dev/p/r2c-ci


install:
	npm install
	npm run install:ui-library

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

#: Security validation via semgrep
semgrep: semgrep-custom semgrep-published

semgrep-custom:
	semgrep --config $(REPOROOT)/.semgrep $(SEMGREP_IGNORE)

semgrep-published:
	semgrep --config $(SEMGREP_CONFIG) $(SEMGREP_IGNORE)

#: Security validation via semgrep; autofix where possible
semgrep-and-fix:
	semgrep --config $(REPOROOT)/.semgrep $(SEMGREP_IGNORE) --autofix
	semgrep --config $(SEMGREP_CONFIG) $(SEMGREP_IGNORE) --autofix

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

# These are shortcuts for generating new components in various subdirs.
# Example:
#    $ make page/TopNavLanding
#    ng "generate" "component" "pages/TopNavLanding" "--module" "app"
#    CREATE src/app/pages/top-nav-landing/top-nav-landing.component.scss (0 bytes)
#    CREATE src/app/pages/top-nav-landing/top-nav-landing.component.html (30 bytes)
#    CREATE src/app/pages/top-nav-landing/top-nav-landing.component.spec.ts (679 bytes)
#    CREATE src/app/pages/top-nav-landing/top-nav-landing.component.ts (310 bytes)
#    UPDATE src/app/app.module.ts (14252 bytes)

demo/%:
	$(NG_CMD) generate component $(DEMO_DIR)/demo-$(@F) $(DEMO_FLAGS)

component/%:
	$(NG_CMD) generate component $(COMPONENT_DIR)/$(@F) $(COMPONENT_FLAGS)

page/%:
	$(NG_CMD) generate component $(PAGE_DIR)/$(@F) $(PAGE_FLAGS)

page-component/%:
	$(NG_CMD) generate component $(PAGE_COMPONENT_DIR)/$(@F) $(PAGE_COMPONENT_FLAGS)

# Adapted from Makefile.common_go
spell:
	@pushd $(REPOROOT) > /dev/null; \
	./scripts/spellcheck.sh components/automate-ui; \
	EXIT_CODE=$$?; \
	popd > /dev/null; \
	exit $$EXIT_CODE

.PHONY: build install test lint lint-html lint-sass lint-typescript semgrep semgrep-and-fix semgrep-custom semgrep-published unit e2e license_scout peer-dependencies e2e-aot unit-all-browsers pr-ready start
