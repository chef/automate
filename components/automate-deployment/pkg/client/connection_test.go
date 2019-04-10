package client

import "testing"
import "os"

func TestGetAddress(t *testing.T) {
	tests := []struct {
		name     string
		expected string
		wantErr  bool
		address  string
	}{
		{"returns address if formatted correctly", "0.0.0.0:1234", false, "0.0.0.0:1234"},
		{"returns default if address empty", "localhost:10160", false, ""},
		{"returns err if malformed", "", true, "0.0.1234IHeartDogs"},
		{"returns default if env variable is not set", "localhost:10160", false, "nil"},
	}

	for _, tt := range tests {
		if tt.address != "nil" {
			os.Setenv(ServiceAddressKey, tt.address)
		}

		t.Run(tt.name, func(t *testing.T) {
			got, err := GetAddress()
			if (err != nil) != tt.wantErr {
				t.Errorf("GetAddress() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if got != tt.expected {
				t.Errorf("GetAddress() = %v, want %v", got, tt.expected)
			}
		})

		// reset our env var
		os.Unsetenv(ServiceAddressKey)
	}
}

func TestGetHostPort(t *testing.T) {
	tests := []struct {
		name      string
		expected  string
		expected1 string
		wantErr   bool
		address   string
	}{
		{"returns err if cannot get address", "", "", true, "bad addy"},
		{"returns err if cannot get address", "0.0.0.0", "1234", false, "0.0.0.0:1234"},
	}

	for _, tt := range tests {
		os.Setenv(ServiceAddressKey, tt.address)

		t.Run(tt.name, func(t *testing.T) {
			got, got1, err := GetHostPort()
			if (err != nil) != tt.wantErr {
				t.Errorf("GetHostPort() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if got != tt.expected {
				t.Errorf("GetHostPort() got = %v, expected %v", got, tt.expected)
			}
			if got1 != tt.expected1 {
				t.Errorf("GetHostPort() got1 = %v, expected %v", got1, tt.expected1)
			}
		})

		// reset our env var
		os.Unsetenv(ServiceAddressKey)
	}
}
