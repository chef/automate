package middleware

import (
	"context"
	"testing"
	"time"

	license_middleware "github.com/chef/automate/components/automate-gateway/gateway/middleware/license"
	"github.com/stretchr/testify/assert"
)

func Test_contains(t *testing.T) {
	tests := []struct {
		name         string
		listOfApis   []string
		functionName string
		want         bool
	}{
		{
			name:         "List contains the string",
			listOfApis:   []string{"test1", "test2", "test3"},
			functionName: "test1",
			want:         true,
		},
		{
			name:         "List doesn't contain the string",
			listOfApis:   []string{"test1", "test2", "test3"},
			functionName: "test4",
			want:         false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := contains(tt.listOfApis, tt.functionName)
			assert.Equal(t, tt.want, got)
		})
	}
}

func Test_licenseInterceptor_refreshLicenseDetails(t *testing.T) {
	tests := []struct {
		name              string
		licenseClient     license_middleware.ILicense
		wantErr           bool
		licenseStatus     *LicenseStatus
		wantLicenseStatus *LicenseStatus
	}{
		{
			name:          "license details are present, but validity time expired",
			licenseClient: &license_middleware.MockLicense{},
			wantErr:       false,
			licenseStatus: &LicenseStatus{
				LicenseDetails: &license_middleware.LicenseDetails{
					LicenseType: "trial",
					ExpiryDate:  time.Date(2024, 5, 6, 14, 30, 45, 100, time.Local),
				},
				DetailsValidity: time.Date(2024, 5, 5, 14, 30, 45, 100, time.Local),
			},
			wantLicenseStatus: &LicenseStatus{
				LicenseDetails: &license_middleware.LicenseDetails{
					LicenseType: "internal",
					ExpiryDate:  time.Date(2024, 5, 10, 14, 30, 45, 100, time.Local),
				},
				DetailsValidity: time.Now().Add(time.Hour * time.Duration(6)),
			},
		}, {
			name:          "license details are present,validity time not expired, needs hard refresh",
			licenseClient: &license_middleware.MockLicense{},
			wantErr:       false,
			licenseStatus: &LicenseStatus{
				LicenseDetails: &license_middleware.LicenseDetails{
					LicenseType: "trial",
					ExpiryDate:  time.Date(2024, 5, 6, 14, 30, 45, 100, time.Local),
				},
				DetailsValidity:       time.Now().Add(time.Hour * time.Duration(7)),
				LicenseDetailsRefresh: true,
			},
			wantLicenseStatus: &LicenseStatus{
				LicenseDetails: &license_middleware.LicenseDetails{
					LicenseType: "internal",
					ExpiryDate:  time.Date(2024, 5, 10, 14, 30, 45, 100, time.Local),
				},
				DetailsValidity: time.Now().Add(time.Hour * time.Duration(6)),
			},
		}, {
			name:          "License details are nil",
			licenseClient: &license_middleware.MockLicense{},
			wantErr:       false,
			licenseStatus: &LicenseStatus{
				LicenseDetails: nil,
			},
			wantLicenseStatus: &LicenseStatus{
				LicenseDetails: &license_middleware.LicenseDetails{
					LicenseType: "internal",
					ExpiryDate:  time.Date(2024, 5, 10, 14, 30, 45, 100, time.Local),
				},
				DetailsValidity: time.Now().Add(time.Hour * time.Duration(6)),
			},
		}, {
			name:          "Unable to fetch details",
			licenseClient: &license_middleware.MockLicense{WantError: true},
			wantErr:       true,
			licenseStatus: &LicenseStatus{
				LicenseDetails: nil,
			},
			wantLicenseStatus: &LicenseStatus{
				LicenseDetails: nil,
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := &licenseInterceptor{
				licenseClient: tt.licenseClient,
				licenseStatus: tt.licenseStatus,
			}
			ctx := context.Background()
			err := l.refreshLicenseDetails(ctx)
			if tt.wantErr {
				assert.NotNil(t, err)
			}

			assert.Equal(t, tt.wantLicenseStatus.LicenseDetails, l.licenseStatus.LicenseDetails)
		})
	}
}

func Test_licenseInterceptor_isValidLicense(t *testing.T) {
	tests := []struct {
		name          string
		licenseClient license_middleware.ILicense
		licenseStatus *LicenseStatus
		want          bool
	}{
		{
			name:          "License is valid",
			licenseClient: &license_middleware.MockLicense{WantError: false},
			licenseStatus: &LicenseStatus{
				LicenseDetails: &license_middleware.LicenseDetails{
					LicenseType: "trial",
					ExpiryDate:  time.Now().AddDate(0, 0, 10),
				},
				DetailsValidity:       time.Now().Add(time.Hour * time.Duration(7)),
				LicenseDetailsRefresh: false,
			},
			want: true,
		}, {
			name:          "License is InValid",
			licenseClient: &license_middleware.MockLicense{WantError: false},
			licenseStatus: &LicenseStatus{
				LicenseDetails: &license_middleware.LicenseDetails{
					LicenseType: "trial",
					ExpiryDate:  time.Now().AddDate(0, 0, -10),
				},
				DetailsValidity:       time.Now().Add(time.Hour * time.Duration(7)),
				LicenseDetailsRefresh: false,
			},
			want: false,
		}, {
			name:          "Checking grace period for commercial license for 60 days",
			licenseClient: &license_middleware.MockLicense{WantError: false},
			licenseStatus: &LicenseStatus{
				LicenseDetails: &license_middleware.LicenseDetails{
					LicenseType: "commercial",
					ExpiryDate:  time.Now().AddDate(0, 0, -1),
				},
				DetailsValidity:       time.Now().Add(time.Hour * time.Duration(60)),
				LicenseDetailsRefresh: true,
			},
			want: true,
		}, {
			name:          "Checking grace period for commercial license for 60 days is expired",
			licenseClient: &license_middleware.MockLicense{WantError: false},
			licenseStatus: &LicenseStatus{
				LicenseDetails: &license_middleware.LicenseDetails{
					LicenseType: "commercial",
					ExpiryDate:  time.Now().AddDate(0, 0, -65),
				},
				DetailsValidity:       time.Now().Add(time.Hour * time.Duration(60)),
				LicenseDetailsRefresh: true,
			},
			want: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := &licenseInterceptor{
				licenseClient: tt.licenseClient,
				licenseStatus: tt.licenseStatus,
			}
			got := l.isValidLicense()
			assert.Equal(t, tt.want, got)
		})
	}
}
