package server

import (
	"testing"

	"google.golang.org/grpc/metadata"
)

func TestRequestFromMD(t *testing.T) {
	t.Run("includes the standard authorization header",
		checkRequest("authorization", "authorization", "value"))
	t.Run("includes the grpcgateway-authorization header",
		checkRequest("authorization", "grpcgateway-authorization", "value"))
	t.Run("includes the grpcgateway-x-data-collector-token header",
		checkRequest("x-data-collector-token", "grpcgateway-x-data-collector-token", "value"))
	t.Run("includes the grpcgateway-api-token header",
		checkRequest("api-token", "grpcgateway-api-token", "value"))
}

func checkRequest(expectedHeader string, mdKey ...string) func(*testing.T) {
	return func(t *testing.T) {
		expectedValue := mdKey[len(mdKey)-1]
		md := metadata.Pairs(mdKey...)
		req, err := reqFromMD(md)
		if err != nil {
			t.Fatalf("expected err==nil, got %v", err)
		}
		if val := req.Header.Get(expectedHeader); val != expectedValue {
			t.Errorf("expected header %v to be %v, got %v", expectedHeader, expectedValue, val)
		}
	}
}
