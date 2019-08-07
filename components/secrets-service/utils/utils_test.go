package utils

import (
	"testing"
)

func TestIsSafeUUID(t *testing.T) {
	var cases = []struct {
		input string
		want  bool
	}{
		{`foo`, false},
		{`f68f5e86-7ca6-11e8-adc0-fa7ae01bbebc`, true},
		{`f68f5e86-7ca6-11e8-adc0-fa7ae01$bebc`, false},
		{`f68f5e86-7ca6-11?8-adc0-fa7ae01bbebc`, false},
		{`f68f5e86-7ca6-118-adc0-fa7ae01bbebc`, false},
	}

	for _, test := range cases {
		got := IsSafeUUID(test.input)
		if got != test.want {
			t.Errorf("IsSafeUUID(%q) = %v want %v", test.input, got, test.want)
		}
	}
}
