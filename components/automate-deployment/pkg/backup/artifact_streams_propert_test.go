package backup

import (
	"sort"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"
)

func sortedUniqueify(in []string) []string {
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

func smallStringListGen() gopter.Gen {
	return gen.SliceOf(
		gen.SliceOfN(2, gen.AlphaNumChar()).Map(func(r []rune) string {
			return string(r)
		}).WithShrinker(gen.StringShrinker),
	).SuchThat(func(s []string) bool {
		return len(s) > 0
	}).Map(sortStrings).Map(sortedUniqueify)
}

func mergeStringLists(t *testing.T, stringLists ...[]string) []string {
	streams := make([]ArtifactStream, len(stringLists))
	for i, s := range stringLists {
		streams[i] = NewArrayStream(s)
	}
	mergedStream := Merge(streams...)
	return consume(t, mergedStream)
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
