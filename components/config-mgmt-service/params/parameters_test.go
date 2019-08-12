//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

// This new file is extending the `rest` package so that we can
// test the its private methods, if the functionality you are
// trying to test is not private it shouldn't be here
package params_test

import (
	"fmt"
	"testing"

	subject "github.com/chef/automate/components/config-mgmt-service/params"
	"github.com/stretchr/testify/assert"
)

func TestFormatFiltersEmptyArray(t *testing.T) {
	empty := make([]string, 0)
	expected := make(map[string][]string, 0)
	filters, err := subject.FormatNodeFilters(empty)
	assert.Nil(t, err)
	assert.Equal(t, expected, filters)
}

func TestStringDateRangeToTimeWithInvalidDatesReturnFalse(t *testing.T) {
	cases := []string{
		"01-01-2017", // DD-MM-YYYY
		"2017-01",    // YYYY-MM
		"2017",       // YYYY
		"YY-01-01",   // YY-MM-DD
		"2017:01:01", // YYYY:MM:DD
		"01-01-2017", // DD-MM-YYYY
		"2017-01",    // YYYY-MM
		"2017",       // YYYY
		"17-12-12",   // YY-MM-DD
		"2017:01:01", // YYYY:MM:DD

		"2017-31-12", // YYYY-DD-MM (Wrong month & day order)
		"2017-01-32", // YYYY-DD-MM (Wrong month & day order)
		"1999-31-01", // YYYY-DD-MM (Wrong month & day order)
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with date '%v' it should be 'ok=false'", test), func(t *testing.T) {
			_, ok := subject.StringDateRangeToTime(test)
			assert.False(t, ok)
		})
	}
}

func TestStringDateRangeToTimeWithValidDatesReturnTrue(t *testing.T) {
	cases := []string{
		"2017-12-01", // YYYY-MM-DD
		"2017-01-31", // YYYY-MM-DD
		"1988-07-07", // YYYY-MM-DD

		"1222-07-07", // YYYY-MM-DD  Uhmmm!
		"8777-07-07", // YYYY-MM-DD  Wooow!
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with date '%v' it should be 'ok=true'", test), func(t *testing.T) {
			_, ok := subject.StringDateRangeToTime(test)
			assert.True(t, ok)
		})
	}
}

func TestValidateDateRangeWithInvalidDatesReturnFalse(t *testing.T) {
	cases := []struct {
		expected bool
		start    string
		end      string
	}{
		{expected: true, start: "2017-01-01", end: "2017-01-01"},  // Equal
		{expected: false, start: "2017-01-02", end: "2017-01-01"}, // Start > End
		{expected: true, start: "2017-01-02", end: "2017-01-03"},  // Start < End
		{expected: false, start: "2000-01-01", end: "1999-01-01"}, // Start > End
		{expected: true, start: "1999-01-01", end: "2000-01-01"},  // Start < End

		{expected: false, start: "99-01-01", end: "2000-01-01"},   // Wrong format
		{expected: false, start: "1999-01-01", end: "01-01-10"},   // Wrong format
		{expected: false, start: "1888:01:01", end: "2000:01:01"}, // Wrong format
		{expected: false, start: "2027/01/01", end: "2127/01/01"}, // Wrong format
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with dates start=%s end=%s it should return %v",
			test.start, test.end, test.expected), func(t *testing.T) {
			if test.expected {
				assert.True(t, subject.ValidateDateRange(test.start, test.end))
			} else {
				assert.False(t, subject.ValidateDateRange(test.start, test.end))
			}
		})
	}
}

// TODO: Add more tests! :smile:
