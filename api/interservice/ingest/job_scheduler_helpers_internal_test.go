package ingest

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

// Test validateDateMath()
func TestValidateDataMath(t *testing.T) {
	cases := []struct {
		message   string
		value     string
		shouldErr bool
		withErr   string
	}{
		{
			message:   "simple value",
			value:     "1d",
			shouldErr: false,
		},
		{
			message:   "should accept year unit",
			value:     "10y",
			shouldErr: false,
		},
		{
			message:   "should accept month unit",
			value:     "3M",
			shouldErr: false,
		},
		{
			message:   "should accept week unit",
			value:     "4w",
			shouldErr: false,
		},
		{
			message:   "should accept day unit",
			value:     "30d",
			shouldErr: false,
		},
		{
			message:   "should accept hour unit",
			value:     "9h",
			shouldErr: false,
		},
		{
			message:   "should accept second format of hour unit",
			value:     "90H",
			shouldErr: false,
		},
		{
			message:   "should accept minute unit",
			value:     "1m",
			shouldErr: false,
		},
		{
			message:   "should accept second unit",
			value:     "86400s", // a day :)
			shouldErr: false,
		},
		{
			message:   "multiple units",
			value:     "1w5h35m",
			shouldErr: false,
		},
		{
			message:   "zero time with unit",
			value:     "0h",
			shouldErr: false,
		},

		// Errors start here
		{
			message:   "should error when: empty value",
			value:     "",
			shouldErr: true,
			withErr:   "invalid time unit: ''",
		},
		{
			message:   "should error when: only number",
			value:     "1",
			shouldErr: true,
			withErr:   "invalid time unit: '1'",
		},
		{
			message:   "should error when: only unit",
			value:     "h",
			shouldErr: true,
			withErr:   "invalid time unit: 'h'",
		},
		{
			message:   "should error when: invalid unit",
			value:     "4z",
			shouldErr: true,
			withErr:   "invalid time unit: '4z'",
		},
		{
			message:   "should error when: invalid char",
			value:     "-1d",
			shouldErr: true,
			withErr:   "invalid time unit: '-1d'",
		},
		{
			message:   "should error when: invalid char (1)",
			value:     "+1y",
			shouldErr: true,
			withErr:   "invalid time unit: '+1y'",
		},
		{
			message:   "should error when: invalid char (2)",
			value:     "123456789!$/(",
			shouldErr: true,
			withErr:   "invalid time unit: '123456789!$/('",
		},
		{
			message:   "should error when: float number is provided",
			value:     "1.5d",
			shouldErr: true,
			withErr:   "invalid time unit: '1.5d'",
		},
		{
			message:   "should error when: multiple units without number are provided",
			value:     "20mm",
			shouldErr: true,
			withErr:   "invalid time unit: '20mm'",
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			result := validateDateMath(test.value)
			if test.shouldErr {
				if assert.NotNil(t, result) {
					assert.Equal(t, test.withErr, result.Error())
				}
			} else {
				assert.Nil(t, result)
			}
		})
	}

}
