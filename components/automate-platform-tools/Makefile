include ../../Makefile.common_go

TOOLS_CMD_PKG="github.com/chef/automate/components/automate-platform-tools/cmd"

build:
	mkdir -p bin/
	go build -o bin/secrets-helper $(TOOLS_CMD_PKG)/secrets-helper
	go build -o bin/service-health $(TOOLS_CMD_PKG)/service-health
	go build -o bin/pg-helper $(TOOLS_CMD_PKG)/pg-helper

unit:
	go test -cover -v ./...

static: lint

.DEFAULT_GOAL=build
.PHONY: unit build
