package config

import (
	"container/list"
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"crypto/x509/pkix"
	"fmt"
	"math/big"
	"testing"
	"time"
)

func TestValidateStringBasedBoolean(t *testing.T) {
	tests := []struct {
		value      string
		fieldName  string
		isRequired bool
		expected   error
	}{
		// Non-required field with empty value
		{value: "", fieldName: "lb_access_logs", isRequired: false, expected: nil},
		// Non-required field with valid boolean values
		{value: "true", fieldName: "lb_access_logs", isRequired: false, expected: nil},
		{value: "false", fieldName: "lb_access_logs", isRequired: false, expected: nil},
		{value: "TRUE", fieldName: "lb_access_logs", isRequired: false, expected: nil},
		{value: "FALSE", fieldName: "lb_access_logs", isRequired: false, expected: nil},
		// Required field with empty value
		{value: "", fieldName: "lb_access_logs", isRequired: true, expected: fmt.Errorf(INVALID_FIELD_VALUE, "lb_access_logs", "")},
		// Required field with invalid boolean values
		{value: "yes", fieldName: "lb_access_logs", isRequired: true, expected: fmt.Errorf(INVALID_FIELD_VALUE, "lb_access_logs", "yes")},
		{value: "no", fieldName: "lb_access_logs", isRequired: true, expected: fmt.Errorf(INVALID_FIELD_VALUE, "lb_access_logs", "no")},
	}

	for _, test := range tests {
		err := validateStringBasedBoolean(test.value, test.fieldName, test.isRequired)
		if (err == nil && test.expected != nil) || (err != nil && test.expected == nil) {
			t.Errorf("Test failed for value: %s, fieldName: %s, isRequired: %t", test.value, test.fieldName, test.isRequired)
		}
		if err != nil && test.expected != nil && err.Error() != test.expected.Error() {
			t.Errorf("Test failed for value: %s, fieldName: %s, isRequired: %t. Expected error: %s, Actual error: %s",
				test.value, test.fieldName, test.isRequired, test.expected, err)
		}
	}
}

func TestValidateNumberField(t *testing.T) {
	tests := []struct {
		value      string
		fieldName  string
		isRequired bool
		expected   error
	}{
		// Non-required field with empty value
		{value: "", fieldName: "Instance_count", isRequired: false, expected: nil},
		// Non-required field with valid number values
		{value: "8080", fieldName: "Instance_count", isRequired: false, expected: nil},
		{value: "12345", fieldName: "Instance_count", isRequired: false, expected: nil},
		// Required field with empty value
		{value: "", fieldName: "Instance_count", isRequired: true, expected: fmt.Errorf(EMPTY_VALUE, "Instance_count")},
		// Required field with invalid number values
		{value: "abc", fieldName: "Instance_count", isRequired: true, expected: fmt.Errorf(INVALID_FIELD_VALUE, "Instance_count", "abc")},
		{value: "12.34", fieldName: "Instance_count", isRequired: true, expected: fmt.Errorf(INVALID_FIELD_VALUE, "Instance_count", "12.34")},
	}

	for _, test := range tests {
		err := validateNumberField(test.value, test.fieldName, test.isRequired)
		if (err == nil && test.expected != nil) || (err != nil && test.expected == nil) {
			t.Errorf("Test failed for value: %s, fieldName: %s, isRequired: %t", test.value, test.fieldName, test.isRequired)
		}
		if err != nil && test.expected != nil && err.Error() != test.expected.Error() {
			t.Errorf("Test failed for value: %s, fieldName: %s, isRequired: %t. Expected error: %s, Actual error: %s",
				test.value, test.fieldName, test.isRequired, test.expected, err)
		}
	}
}

func TestValidateRequiredPathField(t *testing.T) {
	tests := []struct {
		value     string
		fieldName string
		expected  error
	}{
		{value: "./testdata/A2HA.pem", fieldName: "file", expected: nil},
		{value: "./testdata", fieldName: "directory", expected: nil},
		{value: "", fieldName: "empty", expected: fmt.Errorf(INVALID_EMPTY_VALUE, "empty")},
		{value: "/nonexistent/file.txt", fieldName: "nonexistent", expected: fmt.Errorf("invalid nonexistent: /nonexistent/file.txt (stat /nonexistent/file.txt: no such file or directory)")},
	}

	for _, test := range tests {
		err := validateRequiredPathField(test.value, test.fieldName)
		if (err == nil && test.expected != nil) || (err != nil && test.expected == nil) {
			t.Errorf("Test failed for value: %s, fieldName: %s", test.value, test.fieldName)
		}
		if err != nil && test.expected != nil && err.Error() != test.expected.Error() {
			t.Errorf("Test failed for value: %s, fieldName: %s. Expected error: %s, Actual error: %s",
				test.value, test.fieldName, test.expected, err)
		}
	}
}

func TestValidateRequiredString(t *testing.T) {
	tests := []struct {
		value          string
		fieldName      string
		expectedValues []string
		expected       error
	}{
		{value: "option1", fieldName: "field", expectedValues: []string{"option1", "option2"}, expected: nil},
		{value: "option2", fieldName: "field", expectedValues: []string{"option1", "option2"}, expected: nil},
		{value: "option3", fieldName: "field", expectedValues: []string{"option1", "option2"}, expected: fmt.Errorf("invalid value 'option3' for field 'field'. Expected values are: option1, option2")},
		{value: "", fieldName: "field", expectedValues: []string{"option1", "option2"}, expected: fmt.Errorf(INVALID_EMPTY_VALUE, "field")},
	}

	for _, test := range tests {
		err := validateRequiredString(test.value, test.fieldName, test.expectedValues...)
		if (err == nil && test.expected != nil) || (err != nil && test.expected == nil) {
			t.Errorf("Test failed for value: %s, fieldName: %s", test.value, test.fieldName)
		}
		if err != nil && test.expected != nil && err.Error() != test.expected.Error() {
			t.Errorf("Test failed for value: %s, fieldName: %s. Expected error: %s, Actual error: %s",
				test.value, test.fieldName, test.expected, err)
		}
	}
}

func TestValidatePort(t *testing.T) {
	tests := []struct {
		value      string
		fieldName  string
		isRequired bool
		expected   error
	}{
		{value: "8080", fieldName: "port", isRequired: true, expected: nil},
		{value: "12345", fieldName: "port", isRequired: true, expected: nil},
		{value: "0", fieldName: "port", isRequired: true, expected: fmt.Errorf(INVALID_PORT_NUMBER, "0", "port")},
		{value: "65536", fieldName: "port", isRequired: true, expected: fmt.Errorf(INVALID_PORT_NUMBER, "65536", "port")},
		{value: "", fieldName: "port", isRequired: false, expected: nil},
		{value: "", fieldName: "port", isRequired: true, expected: fmt.Errorf(INVALID_PORT_NUMBER, "", "port")},
	}

	for _, test := range tests {
		err := validatePort(test.value, test.fieldName, test.isRequired)
		if (err == nil && test.expected != nil) || (err != nil && test.expected == nil) {
			t.Errorf("Test failed for value: %s, fieldName: %s, isRequired: %v", test.value, test.fieldName, test.isRequired)
		}
		if err != nil && test.expected != nil && err.Error() != test.expected.Error() {
			t.Errorf("Test failed for value: %s, fieldName: %s, isRequired: %v. Expected error: %s, Actual error: %s",
				test.value, test.fieldName, test.isRequired, test.expected, err)
		}
	}
}

func TestValidateRequiredStringListField(t *testing.T) {
	tests := []struct {
		value     []string
		fieldName string
		expected  error
	}{
		{value: []string{"120.0.1.3", "13.55.311", "127.0.1.1"}, fieldName: "automate_private_ips", expected: nil},
		{value: []string{}, fieldName: "automate_private_ips", expected: fmt.Errorf(INVALID_EMPTY_VALUE, "automate_private_ips")},
		{value: nil, fieldName: "automate_private_ips", expected: fmt.Errorf(INVALID_EMPTY_VALUE, "automate_private_ips")},
	}

	for _, test := range tests {
		err := validateRequiredStringListField(test.value, test.fieldName)
		if (err == nil && test.expected != nil) || (err != nil && test.expected == nil) {
			t.Errorf("Test failed for value: %v, fieldName: %s", test.value, test.fieldName)
		}
		if err != nil && test.expected != nil && err.Error() != test.expected.Error() {
			t.Errorf("Test failed for value: %v, fieldName: %s. Expected error: %s, Actual error: %s",
				test.value, test.fieldName, test.expected, err)
		}
	}
}

func TestGetSingleErrorFromList(t *testing.T) {
	testCases := []struct {
		name          string
		errorList     *list.List
		expectedError error
	}{
		{
			name:          "Empty Error List",
			errorList:     list.New(),
			expectedError: nil,
		},
		{
			name: "Error List with Single Error",
			errorList: func() *list.List {
				l := list.New()
				l.PushBack(fmt.Errorf("first error"))
				return l
			}(),
			expectedError: fmt.Errorf("first error"),
		},
		{
			name: "Error List with Multiple Errors",
			errorList: func() *list.List {
				l := list.New()
				l.PushBack(fmt.Errorf("first error"))
				l.PushBack(fmt.Errorf("second error"))
				l.PushBack(fmt.Errorf("third error"))
				return l
			}(),
			expectedError: fmt.Errorf("first error\nsecond error\nthird error"),
		},
		{
			name: "Error List with Mixed Types",
			errorList: func() *list.List {
				l := list.New()
				l.PushBack(fmt.Errorf("first error"))
				l.PushBack("second error")
				l.PushBack(123) // unknown error type
				return l
			}(),
			expectedError: fmt.Errorf("first error\nsecond error\nunknown error type: 123"),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			err := getSingleErrorFromList(tc.errorList)
			if (err == nil && tc.expectedError != nil) || (err != nil && err.Error() != tc.expectedError.Error()) {
				t.Errorf("Expected error: %v, but got: %v", tc.expectedError, err)
			}
		})
	}
}

func TestValidateFQDN(t *testing.T) {
	// Valid URLs
	validURLs := []string{
		"example.com",
		"subdomain.example.com",
		"example123.com",
		"example.com:8080",
		"subdomain.example.com:8080",
	}
	for _, url := range validURLs {
		err := validateFQDN(url, "url")
		if err != nil {
			t.Errorf("Test failed for URL: %s. Expected no error, got: %v", url, err)
		}
	}

	// Empty URL
	err := validateFQDN("", "url")
	expectedError := "invalid or empty URL: url"
	if err == nil {
		t.Errorf("Test failed for empty URL. Expected error: %s, got nil", expectedError)
	} else if err.Error() != expectedError {
		t.Errorf("Test failed for empty URL. Expected error: %s, got: %v", expectedError, err)
	}

	// URL with spaces
	err = validateFQDN("example .com", "url")
	expectedError = "domain name cannot contain spaces: url"
	if err == nil {
		t.Errorf("Test failed for URL with spaces. Expected error: %s, got nil", expectedError)
	} else if err.Error() != expectedError {
		t.Errorf("Test failed for URL with spaces. Expected error: %s, got: %v", expectedError, err)
	}

	// URL with protocol
	err = validateFQDN("http://example.com", "url")
	expectedError = "url should not include the protocol (http:// or https://): url"
	if err == nil {
		t.Errorf("Test failed for URL with protocol. Expected error: %s, got nil", expectedError)
	} else if err.Error() != expectedError {
		t.Errorf("Test failed for URL with protocol. Expected error: %s, got: %v", expectedError, err)
	}

	// Invalid URL format
	invalidURLs := []string{
		"example",
		"8081",
		".com",
		"test..com:80",
	}
	for _, url := range invalidURLs {
		err := validateFQDN(url, "url")
		expectedError := "invalid URL format: url"
		if err == nil {
			t.Errorf("Test failed for invalid URL: %s. Expected error: %s, got nil", url, expectedError)
		} else if err.Error() != expectedError {
			t.Errorf("Test failed for invalid URL: %s. Expected error: %s, got: %v", url, expectedError, err)
		}
	}
}

func TestValidateUrlWithPort(t *testing.T) {
	// Valid URLs
	validURLs := []string{
		"example.com:8080",
		"subdomain.example.com:8080",
		"managed-rds-db.cww4poze5gkx.ap-northeast-1.rds.amazonaws.com:5432",
	}
	for _, url := range validURLs {
		err := validateUrlWithPort(url, "url")
		if err != nil {
			t.Errorf("Test failed for URL: %s. Expected no error, got: %v", url, err)
		}
	}

	// Invalid URL format
	invalidURLs := []string{
		"example.com",
		"8081",
		".com",
		"test..com:80",
	}
	for _, url := range invalidURLs {
		err := validateUrlWithPort(url, "url")
		expectedError := "invalid URL format: url"
		if err == nil {
			t.Errorf("Test failed for invalid URL: %s. Expected error: %s, got nil", url, expectedError)
		} else if err.Error() != expectedError {
			t.Errorf("Test failed for invalid URL: %s. Expected error: %s, got: %v", url, expectedError, err)
		}
	}
}

func TestValidateS3AWSRegion(t *testing.T) {
	validRegions := []string{
		"us-east-1",
		"us-west-2",
		"ap-southeast-1",
		"eu-central-1",
		"sa-east-1",
		"US-EAST-2",
	}

	invalidRegions := []string{
		"us-west-3",
		"ap-north-1",
		"eu-west-4",
		"sa-south-1",
	}

	// Test valid regions
	for _, region := range validRegions {
		err := validateS3AWSRegion(region)
		if err != nil {
			t.Errorf("Expected region %s to be valid, but got error: %s", region, err.Error())
		}
	}

	// Test invalid regions
	for _, region := range invalidRegions {
		err := validateS3AWSRegion(region)
		if err == nil {
			t.Errorf("Expected region %s to be invalid, but got no error", region)
		}
	}
}

func TestValidateS3Endpoint(t *testing.T) {
	testCases := []struct {
		name          string
		endpoint      string
		expectedError bool
	}{
		{
			name:          "Empty Endpoint",
			endpoint:      "",
			expectedError: true,
		},
		{
			name:          "Valid Endpoint with HTTPS",
			endpoint:      "https://s3.amazonaws.com",
			expectedError: false,
		},
		{
			name:          "Valid Endpoint without HTTP/HTTPS",
			endpoint:      "s3.amazonaws.com",
			expectedError: false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			err := validateS3Endpoint(tc.endpoint)
			if (err != nil && !tc.expectedError) || (err == nil && tc.expectedError) {
				t.Errorf("Expected error: %v, but got: %v", tc.expectedError, err)
			}
		})
	}
}

func TestValidateRootCACertificate(t *testing.T) {

	svc := "example_service"
	// Test case: Invalid root CA certificate (failed parsing)
	invalidCert := []byte("invalid_certificate")
	err := validateRootCACertificate(invalidCert, svc)
	expectedError := fmt.Errorf("failed to parse root_ca for %s:", svc)
	if err == nil || err.Error()[:len(expectedError.Error())] != expectedError.Error() {
		t.Errorf("Expected error '%s' for invalid root CA certificate, got: %v", expectedError, err)
	}

	// Test case: Expired root CA certificate
	expiredCert := generateExpiredCert()
	err = validateRootCACertificate(expiredCert, svc)
	expectedError = fmt.Errorf("root_ca for %s has expired", svc)
	if err == nil || err.Error() != expectedError.Error() {
		t.Errorf("Expected error '%s' for expired root CA certificate, got: %v", expectedError, err)
	}
}

// Helper function to generate an expired certificate for testing
func generateExpiredCert() []byte {
	template := &x509.Certificate{
		NotBefore:    time.Now().Add(-time.Hour),        // Set NotBefore to 1 hour ago
		NotAfter:     time.Now().Add(-30 * time.Minute), // Set NotAfter to 30 minutes ago (expired)
		SerialNumber: big.NewInt(123),                   // Random serial number
		Subject: pkix.Name{
			CommonName: "Example Root CA",
		},
	}

	privateKey, _ := rsa.GenerateKey(rand.Reader, 2048)
	publicKey := privateKey.PublicKey

	derBytes, _ := x509.CreateCertificate(rand.Reader, template, template, &publicKey, privateKey)
	return derBytes
}
