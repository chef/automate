package stringutils_test

import (
	"testing"

	"github.com/chef/automate/lib/stringutils"
	"github.com/stretchr/testify/assert"
)

func TestMaskPGCredInURI(t *testing.T) {
	tests := []struct {
		source   string
		expected string
	}{
		{source: "postgres://user:pass@127.0.0.1:10145", expected: "postgres://<USER>:<PASSWORD>@127.0.0.1:10145"},
		{source: "postgres://user:@127.0.0.1:10145", expected: "postgres://<USER>:<PASSWORD>@127.0.0.1:10145"},
		{source: "postgres://:pass@127.0.0.1:10145", expected: "postgres://<USER>:<PASSWORD>@127.0.0.1:10145"},
		{source: "postgres://127.0.0.1:10145", expected: "postgres://127.0.0.1:10145"},
		{source: "postgres://:@127.0.0.1:10145", expected: "postgres://<USER>:<PASSWORD>@127.0.0.1:10145"},
		{source: "postgres://user:pass127.0.0.1:10145", expected: "postgres://user:pass127.0.0.1:10145"},
	}

	for _, test := range tests {
		actual := stringutils.MaskPGCredInURI(test.source)

		assert.Equal(t, test.expected, actual)
	}
}
