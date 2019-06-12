package postgres

import (
	"testing"

	"github.com/lib/pq"
	"github.com/stretchr/testify/assert"
)

func TestServiceGroupHealthReleaseStringEmpty(t *testing.T) {
	var (
		subject  = new(serviceGroupHealth)
		expected = "Unknown"
	)
	actual := subject.ReleaseString()
	assert.Equal(t, expected, actual)
}

func TestServiceGroupHealthPackageStringEmpty(t *testing.T) {
	var (
		subject  = new(serviceGroupHealth)
		expected = "Unknown"
	)
	actual := subject.PackageString()
	assert.Equal(t, expected, actual)
}

func TestServiceGroupHealthReleaseStringMalformedReleaseIdent(t *testing.T) {
	var (
		subject = serviceGroupHealth{
			Releases: pq.StringArray{"malformed"},
		}
		expected = "Unknown"
	)
	actual := subject.ReleaseString()
	assert.Equal(t, expected, actual)
}

func TestServiceGroupHealthPackageStringMalformedReleaseIdent(t *testing.T) {
	var (
		subject = serviceGroupHealth{
			Releases: pq.StringArray{"kind/of/malformed"},
		}
		expected = "Unknown"
	)
	actual := subject.PackageString()
	assert.Equal(t, expected, actual)
}

func TestServiceGroupHealthWithMultipleReleaseIdents(t *testing.T) {
	cases := []struct {
		message         string
		subject         serviceGroupHealth
		expectedPackage string
		expectedRelease string
	}{

		{
			message: "with single package_ident",
			subject: serviceGroupHealth{
				Releases: pq.StringArray{"core/redis/0.1.0/2020010101000000"},
			},
			expectedPackage: "core/redis",
			expectedRelease: "0.1.0/2020010101000000",
		},
		{
			message: "with multiple package_ident that has multiple releases",
			subject: serviceGroupHealth{
				Releases: pq.StringArray{
					"core/redis/0.1.0/2020010101000000",
					"core/redis/0.1.0/2020010101000001",
					"core/redis/0.1.0/2020010101000002",
					"core/redis/0.1.0/2020010101000003",
				},
			},
			expectedPackage: "core/redis",
			expectedRelease: "Several (4)",
		},
		{
			message: "with multiple package_ident that has multiple package_name (origin)",
			subject: serviceGroupHealth{
				Releases: pq.StringArray{
					"core/redis/0.1.0/2020010101000000",
					"custom/redis/0.1.0/2020010101000000",
					"personal/redis/0.1.0/2020010101000000",
				},
			},
			expectedPackage: "Several (3)",
			expectedRelease: "0.1.0/2020010101000000",
		},
		{
			message: "with multiple package_ident that has both, multiple package and releases",
			subject: serviceGroupHealth{
				Releases: pq.StringArray{
					"core/redis/0.1.0/2020010101000000",
					"core/redis/0.1.0/2020010101000001",
					"custom/redis/0.1.0/2020010101000002",
					"custom/redis/0.1.0/2020010101000003",
					"personal/redis/0.1.0/2020010101000004",
					"personal/redis/0.1.0/2020010101000000",
				},
			},
			expectedPackage: "Several (3)",
			expectedRelease: "Several (5)",
		},
		{
			message: "with a both, good and malformed package_ident should drop malformed",
			subject: serviceGroupHealth{
				Releases: pq.StringArray{
					"core/redis/0.1.0/2020010101000000",
					"missing/fields",
				},
			},
			expectedPackage: "core/redis",
			expectedRelease: "0.1.0/2020010101000000",
		},
	}

	for _, test := range cases {

		t.Run("PackageString()"+test.message, func(t *testing.T) {
			actualPackage := test.subject.PackageString()
			assert.Equal(t, test.expectedPackage, actualPackage)
		})

		t.Run("ReleaseString()"+test.message, func(t *testing.T) {
			actualRelease := test.subject.ReleaseString()
			assert.Equal(t, test.expectedRelease, actualRelease)
		})

	}
}
