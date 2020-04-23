include ../../Makefile.common_go

BIN_DIR=$(PWD)/bin
TEST_PACKAGES = ./builder ./notifier

PROTO_DIR=../../components/notifications-service/server/priv/
BASE_PROTO_FILES=$(PROTO_DIR)/notifications.proto $(PROTO_DIR)/rules.proto $(PROTO_DIR)/server.proto $(PROTO_DIR)/health.proto

test:
	go test -v $(TEST_PACKAGES)

$(BIN_DIR)/gen_server_test_data:
	go build -o $(BIN_DIR)/gen_server_test_data cmd/gen_server_test_data.go

proto: bin/protoc-gen-go
	protoc -I../.. --go_out=plugins=grpc:./api $(BASE_PROTO_FILES)

bin/protoc-gen-go:
	go install -v ../../vendor/github.com/golang/protobuf/protoc-gen-go


clean:
	rm -r ./bin
