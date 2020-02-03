package backup

import (
	"io"
	"sort"
	"testing"

	"github.com/chef/automate/lib/stringutils"
	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"
)

func sortedUniqueify(in []string) []string {
	if len(in) == 0 {
		return in
	}
	// https://github.com/golang/go/wiki/SliceTricks#in-place-deduplicate-comparable
	j := 0
	for i := 1; i < len(in); i++ {
		if in[j] == in[i] {
			continue
		}
		j++
		in[j] = in[i]
	}
	return in[:j+1]
}

func sortStrings(in []string) []string {
	sort.Strings(in)
	return in
}

func isSorted(in []string) bool {
	return sort.IsSorted(sort.StringSlice(in))
}

func isSortedAnUnique(in []string) bool {
	if len(in) == 0 {
		return true
	}
	if !isSorted(in) {
		return false
	}
	last := in[0]
	for i := 1; i < len(in); i++ {
		if in[i] == last {
			return false
		}
	}
	return true
}

func allContained(haystack []string, needles []string) bool {
	j := 0
	for i := 0; i < len(needles); i++ {
	INNER:
		for {
			if needles[i] < haystack[j] {
				return false
			} else if needles[i] > haystack[j] {
				j++
			} else {
				break INNER
			}
		}
	}
	return true
}

func anyContained(haystack []string, needles []string) bool {
	for i := 0; i < len(needles); i++ {
		if stringutils.SliceContains(haystack, needles[i]) {
			return true
		}
	}
	return false
}

func stringSliceEquals(a []string, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := 0; i < len(a); i++ {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func smallStringListGen() gopter.Gen {
	return gen.SliceOf(
		gen.OneGenOf(
			gen.SliceOfN(2, gen.AlphaNumChar()),
			gen.SliceOfN(1, gen.AlphaNumChar()),
		).Map(func(r []rune) string {
			return string(r)
		}),
	).Map(sortStrings).Map(sortedUniqueify)
}

func mergeStringLists(t *testing.T, stringLists ...[]string) []string {
	streams := make([]ArtifactStream, len(stringLists))
	for i, s := range stringLists {
		streams[i] = NewArrayStream(s)
	}
	mergedStream := Merge(streams...)
	return consume(t, mergedStream)
}

func subStringLists(t *testing.T, a []string, b []string) (_vals []string, _consumed bool) {
	streamA := NewArrayStream(a)
	streamB := NewArrayStream(b)
	subStream := Sub(streamA, streamB)
	vals := consume(t, subStream)
	_, err := streamA.Next()
	consumed := err == io.EOF
	return vals, consumed
}

func TestMergePropertyBased(t *testing.T) {
	parameters := gopter.DefaultTestParameters()
	parameters.MinSuccessfulTests = 10000
	properties := gopter.NewProperties(parameters)

	properties.Property("Merged list is sorted and contains no duplicates", prop.ForAll(
		func(a []string, b []string, c []string) bool {
			return isSortedAnUnique(mergeStringLists(t, a, b, c))
		},
		smallStringListGen().WithLabel("a"),
		smallStringListGen().WithLabel("b"),
		smallStringListGen().WithLabel("c"),
	))

	properties.Property("Merged list contains all elements from all lists", prop.ForAll(
		func(a []string, b []string, c []string) bool {
			merged := mergeStringLists(t, a, b, c)
			return allContained(merged, a) && allContained(merged, b) && allContained(merged, c)
		},
		smallStringListGen().WithLabel("a"),
		smallStringListGen().WithLabel("b"),
		smallStringListGen().WithLabel("c"),
	))

	properties.TestingRun(t)
}

func TestSubPropertyBased(t *testing.T) {
	parameters := gopter.DefaultTestParameters()
	parameters.MinSuccessfulTests = 10000
	properties := gopter.NewProperties(parameters)

	properties.Property("a - a = 0", prop.ForAll(
		func(a []string) bool {
			v, _ := subStringLists(t, a, a)
			return len(v) == 0
		},
		smallStringListGen().WithLabel("a"),
	))

	properties.Property("a - 0 = a", prop.ForAll(
		func(a []string) bool {
			v, _ := subStringLists(t, a, []string{})
			return stringSliceEquals(a, v)
		},
		smallStringListGen().WithLabel("a"),
	))

	properties.Property("0 - a = 0", prop.ForAll(
		func(a []string) bool {
			v, _ := subStringLists(t, []string{}, a)
			return len(v) == 0
		},
		smallStringListGen().WithLabel("a"),
	))

	properties.Property("Result is sorted and contains no duplicates", prop.ForAll(
		func(a []string, b []string) bool {
			v, _ := subStringLists(t, a, b)
			return isSortedAnUnique(v)
		},
		smallStringListGen().WithLabel("a"),
		smallStringListGen().WithLabel("b"),
	))

	properties.Property("Left stream is entirely consumed", prop.ForAll(
		func(a []string, b []string) bool {
			_, consumed := subStringLists(t, a, b)
			return consumed
		},
		smallStringListGen().WithLabel("a"),
		smallStringListGen().WithLabel("b"),
	))

	properties.Property("Result does not contain items in b", prop.ForAll(
		func(a []string, b []string) bool {
			v, _ := subStringLists(t, a, b)
			return !anyContained(v, b)
		},
		smallStringListGen().WithLabel("a"),
		smallStringListGen().WithLabel("b"),
	))

	properties.Property("Result does not contain items not in a", prop.ForAll(
		func(a []string, b []string) bool {
			v, _ := subStringLists(t, a, b)
			return allContained(a, v)
		},
		smallStringListGen().WithLabel("a"),
		smallStringListGen().WithLabel("b"),
	))

	properties.TestingRun(t)
}
