package utils

import (
	"testing"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/stretchr/testify/assert"
)

func TestKvMatches(t *testing.T) {
	/*
		- foo* -> HasPrefix
		- *foo -> HasSuffix
		- *foo* -> Contains
		- foo -> Exact match
	*/
	// test first case
	match := KvMatches("Name", "vj*", &common.Kv{Key: "Name", Value: "vj"})
	assert.Equal(t, match, true)
	match = KvMatches("Name", "vj*", &common.Kv{Key: "Name", Value: "vj-test"})
	assert.Equal(t, match, true)
	match = KvMatches("Name", "vj*", &common.Kv{Key: "Name", Value: "test-vj"})
	assert.Equal(t, match, false)

	// test second case
	match = KvMatches("Name", "*test", &common.Kv{Key: "Name", Value: "vj-test-1"})
	assert.Equal(t, match, false)
	match = KvMatches("Name", "*test", &common.Kv{Key: "Name", Value: "vj-test"})
	assert.Equal(t, match, true)

	// test third case
	match = KvMatches("Name", "*test*", &common.Kv{Key: "Name", Value: "vj-test-1"})
	assert.Equal(t, match, true)
	match = KvMatches("Name", "*test*", &common.Kv{Key: "Name", Value: "vj-test DO NOT DELETE"})
	assert.Equal(t, match, true)
	match = KvMatches("Name", "*test*", &common.Kv{Key: "Name", Value: "test"})
	assert.Equal(t, match, true)
	match = KvMatches("Name", "*test*", &common.Kv{Key: "Name", Value: "bla"})
	assert.Equal(t, match, false)

	// test fourth case
	match = KvMatches("Name", "vj", &common.Kv{Key: "Name", Value: "vj-test"})
	assert.Equal(t, match, false)
	match = KvMatches("Name", "vj", &common.Kv{Key: "Name", Value: "vj"})
	assert.Equal(t, match, true)
}

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

func TestUniqueStringSlice(t *testing.T) {
	arr := []string{"a", "b", "c", "b", "f", "a"}
	assert.Equal(t, []string{"a", "b", "c", "f"}, UniqueStringSlice(arr))
}

func TestDeDupSlice(t *testing.T) {
	arr := []string{"item1", "item2", "item2"}
	arr = deDupSlice(arr)
	assert.Equal(t, []string{"item1", "item2"}, arr)

	arr = []string{"item1"}
	arr = deDupSlice(arr)
	assert.Equal(t, []string{"item1"}, arr)

	arr = []string{}
	arr = deDupSlice(arr)
	assert.Equal(t, []string{}, arr)

	arr = []string{"item1", "item1", "item1", "item3", "item2", "item2"}
	arr = deDupSlice(arr)
	assert.Equal(t, []string{"item1", "item3", "item2"}, arr)
}

func TestDeDupFilters(t *testing.T) {
	filters := make(map[string][]string)
	filters["profile_id"] = []string{"prof1", "prof2"}
	DeDupFilters(filters)
	assert.Equal(t, []string{"prof1", "prof2"}, filters["profile_id"])

	filters["profile_id"] = []string{"prof1", "prof2", "prof2"}
	DeDupFilters(filters)
	assert.Equal(t, []string{"prof1", "prof2"}, filters["profile_id"])

	filters["profile_id"] = []string{"prof1", "prof2", "prof2", "prof1", "prof11"}
	DeDupFilters(filters)
	assert.Equal(t, []string{"prof1", "prof2", "prof11"}, filters["profile_id"])
}
