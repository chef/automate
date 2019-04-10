package utils

import (
	"testing"
)

func TestEscapeLiteralForPG(t *testing.T) {
	var cases = []struct {
		input string
		want  string
	}{
		{`foo`, `foo`},
		{`foo'bar`, `foo''bar`},
		{`foo'bar'baz`, `foo''bar''baz`},
		{`foo"bar`, `foo"bar`},
		{`foo\bar`, `foo\\bar`},
		{`foo\bar'baz`, `foo\\bar''baz`},
	}

	for _, test := range cases {
		got := EscapeLiteralForPG(test.input)
		if got != test.want {
			t.Errorf("EscapeLiteralForPG(%q) = %v want %v", test.input, got, test.want)
		}
	}
}

func TestEscapeLiteralForPGPatternMatch(t *testing.T) {
	var cases = []struct {
		input string
		want  string
	}{
		{`foo`, `foo`},
		{`foo'bar`, `foo''bar`},
		{`foo'bar'baz`, `foo''bar''baz`},
		{`foo"bar`, `foo"bar`},
		{`foo\bar`, `foo\\bar`},
		{`foo\bar'baz`, `foo\\bar''baz`},
		{`foo\bar_`, `foo\\bar\_`},
		{`foo\bar%`, `foo\\bar\%`},
	}

	for _, test := range cases {
		got := EscapeLiteralForPGPatternMatch(test.input)
		if got != test.want {
			t.Errorf("EscapeLiteralForPGPatternMatch(%q) = %v want %v", test.input, got, test.want)
		}
	}
}

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
