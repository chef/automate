include ../../Makefile.common_go

GO_PROTOC=protoc -Iexample -I../../protvendor --go_out=plugins=grpc:example
PROTO_FILES = $(wildcard example/*.proto)

unit:
	go test -cover -v ./...
.PHONY: unit

static: lint

