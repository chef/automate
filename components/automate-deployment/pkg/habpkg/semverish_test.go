package habpkg

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var semverTests = []struct {
	input  string
	output SemverishVersion
}{
	// The basics
	{"1", SemverishVersion{parts: []int{1}}},
	{"1.2", SemverishVersion{parts: []int{1, 2}}},
	{"1.2.3", SemverishVersion{parts: []int{1, 2, 3}}},
	{"123.456.789", SemverishVersion{parts: []int{123, 456, 789}}},
	// We support arbitrary parts
	{"1.2.3.4", SemverishVersion{parts: []int{1, 2, 3, 4}}},
	{"1.2.3.4.5", SemverishVersion{parts: []int{1, 2, 3, 4, 5}}},

	// Parts with 0s
	{"0.0.3", SemverishVersion{parts: []int{0, 0, 3}}},
	{"0.2.3", SemverishVersion{parts: []int{0, 2, 3}}},

	// Leading zeros
	{"01", SemverishVersion{parts: []int{1}}},
	{"00.02", SemverishVersion{parts: []int{0, 2}}},

	// Initial v
	{"v1.2.3", SemverishVersion{parts: []int{1, 2, 3}}},

	// Prerelease and metadata
	{"1.2.3-dev", SemverishVersion{parts: []int{1, 2, 3}, prerelease: "dev"}},
	{"1.2.3-dev+123", SemverishVersion{parts: []int{1, 2, 3}, prerelease: "dev", metadata: "123"}},
	{"1.2.3+123", SemverishVersion{parts: []int{1, 2, 3}, metadata: "123"}},
	{"1.2.3-dev.dev+123.456", SemverishVersion{parts: []int{1, 2, 3}, prerelease: "dev.dev", metadata: "123.456"}},
	{"1.2.3-dev-dev+123-456", SemverishVersion{parts: []int{1, 2, 3}, prerelease: "dev-dev", metadata: "123-456"}},
}

func TestParseSemverString(t *testing.T) {
	for _, test := range semverTests {
		parsed, err := ParseSemverishVersion(test.input)
		assert.NoError(t, err)
		assert.Equal(t, test.output, parsed)
	}
}

func TestParseSemverStringErrorCases(t *testing.T) {
	t.Run("version must have one decimal part", func(t *testing.T) {
		_, err := ParseSemverishVersion("a10")
		assert.Error(t, err)
		_, err = ParseSemverishVersion("+a10")
		assert.Error(t, err)
		_, err = ParseSemverishVersion("-a10")
		assert.Error(t, err)
	})
	t.Run("all dotted parts must be decimal", func(t *testing.T) {
		_, err := ParseSemverishVersion("10.a")
		assert.Error(t, err)
	})
	t.Run("all dotted parts must be decimal", func(t *testing.T) {
		_, err := ParseSemverishVersion("10.a")
		assert.Error(t, err)
	})
	t.Run("non-dotted parts must look like pre-release or metadata", func(t *testing.T) {
		_, err := ParseSemverishVersion("10~foo")
		assert.Error(t, err)
	})
}

func TestSemverCompare(t *testing.T) {
	compareTests := []struct {
		a      string
		b      string
		result int
	}{
		{"1.2.3", "1.2.3", 0},
		{"1.2.3", "1.2.3-pre", 1},
		{"1", "1", 0},
		{"1", "2", -1},
		{"1.2", "1", 1},
		{"1.2.3-pre1", "1.2.3-pre2", -1},
	}
	for _, test := range compareTests {
		t.Run(fmt.Sprintf("Compare(%s, %s) == %d", test.a, test.b, test.result),
			func(t *testing.T) {
				a, err := ParseSemverishVersion(test.a)
				require.NoError(t, err)
				b, err := ParseSemverishVersion(test.b)
				require.NoError(t, err)
				assert.Equal(t, test.result, CompareSemverish(a, b))
			})
		if test.result != 0 {
			t.Run(fmt.Sprintf("Compare(%s, %s) == %d", test.b, test.a, -test.result),
				func(t *testing.T) {
					a, err := ParseSemverishVersion(test.a)
					require.NoError(t, err)
					b, err := ParseSemverishVersion(test.b)
					require.NoError(t, err)
					assert.Equal(t, -test.result, CompareSemverish(b, a))
				})
		}
	}
}

func BenchmarkParseSemverishVersionBasic(b *testing.B) {
	v := "1.2.3"
	for n := 0; n < b.N; n++ {
		ParseSemverishVersion(v)
	}
}

func BenchmarkParseSemverishVersionAll(b *testing.B) {
	l := len(semverTests)
	for n := 0; n < b.N; n++ {
		ParseSemverishVersion(semverTests[n%l].input)
	}
}

func BenchmarkCompareBasic(b *testing.B) {
	v0, err := ParseSemverishVersion("1.2.3")
	assert.NoError(b, err)
	v1, err := ParseSemverishVersion("1.2.4")
	assert.NoError(b, err)
	for n := 0; n < b.N; n++ {
		CompareSemverish(v0, v1)
	}
}
