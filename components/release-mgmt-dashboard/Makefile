NG_ENV?=dev
DNS_SUFFIX?=dev

build: print-version-info install
	ng --version
	ng build --prod --env=$(NG_ENV)

install:
	npm install

deploy: build
	aws --profile chef-cd s3 sync dist s3://a2-$(DNS_SUFFIX).cd.chef.co

print-version-info:
	@echo " === VERSION INFORMATION === "
	npm --version
	node --version

.PHONY: build install deploy print-version-info
