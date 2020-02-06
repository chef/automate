package backup

import (
	"io"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/prop"
)

func mergeStringLists(t *testing.T, stringLists ...[]string) []string {
	t.Helper()

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
			return isSortedAndUnique(mergeStringLists(t, a, b, c))
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
			return isSortedAndUnique(v)
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
