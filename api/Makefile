unit:
	@go test $(shell go list ./...)

LINTERARGS=--exclude-use-default -D lll ./...
REPOROOT=../

ifneq ("$(wildcard ../Makefile.common_go)", "")
include ../Makefile.common_go
endif
