package pgutils_test

import (
	"testing"

	"github.com/chef/automate/lib/pgutils"
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
		got := pgutils.EscapeLiteralForPG(test.input)
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
		got := pgutils.EscapeLiteralForPGPatternMatch(test.input)
		if got != test.want {
			t.Errorf("EscapeLiteralForPGPatternMatch(%q) = %v want %v", test.input, got, test.want)
		}
	}
}
