package config

import "testing"

var acceptedStatusCodes []int32 = []int32{200, 201, 202, 203, 204}

func TestIsAcceptedStatusCode200(t *testing.T) {
	code := int32(200)
	testCodeTrue(t, code)
}

func TestIsAcceptedStatusCode201(t *testing.T) {
	code := int32(201)
	testCodeTrue(t, code)
}

func TestIsAcceptedStatusCode202(t *testing.T) {
	code := int32(202)
	testCodeTrue(t, code)
}

func TestIsAcceptedStatusCode203(t *testing.T) {
	code := int32(203)
	testCodeTrue(t, code)
}

func TestIsAcceptedStatusCode204(t *testing.T) {
	code := int32(204)
	testCodeTrue(t, code)
}

func TestIsAcceptedStatusCode205(t *testing.T) {
	code := int32(205)
	testCodeFalse(t, code)
}

func TestIsAcceptedStatusCode404(t *testing.T) {
	code := int32(404)
	testCodeFalse(t, code)
}

func testCodeTrue(t *testing.T, code int32) {
	testCode(t, code, true)
}

func testCodeFalse(t *testing.T, code int32) {
	testCode(t, code, false)
}

func testCode(t *testing.T, code int32, expected bool) {
	t.Logf("code %v, codes %v", code, acceptedStatusCodes)
	result := IsAcceptedStatusCode(code, acceptedStatusCodes)
	if result != expected {
		t.Logf("expected %d to be %v, got %v ", code, expected, !expected)
		t.Fail()
	}
}
