package license

import (
	"context"
	"errors"
	"time"
)

type MockLicense struct {
	WantError bool
}

func (l *MockLicense) GetLicenseDetails(ctx context.Context) (*LicenseDetails, error) {
	if l.WantError {
		return nil, errors.New("got Error")
	}
	return &LicenseDetails{
		LicenseType: "internal",
		ExpiryDate:  time.Date(2024, 5, 10, 14, 30, 45, 100, time.Local),
	}, nil

}
