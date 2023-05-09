package stringutils

import (
	"errors"
	"fmt"
)

var ErrNotFound = errors.New("Not found")

func SubSlice(s1 []string, s2 []string) bool {
	if len(s1) > len(s2) {
		return false
	}
	for _, e := range s1 {
		if !SliceContains(s2, e) {
			return false
		}
	}
	return true
}

// This method is used to return comman elements between the two slices
func SliceIntersection(first, second []string) []string {
	out := []string{}
	bucket := map[string]bool{}

	for _, i := range first {
		for _, j := range second {
			if i == j && !bucket[i] {
				out = append(out, i)
				bucket[i] = true
			}
		}
	}

	return out
}

// This method is used to return uncomman elements between the two slices. It returns (A-B) operation
func SliceDifference(first, second []string) []string {
	mb := make(map[string]struct{}, len(second))
	for _, x := range second {
		mb[x] = struct{}{}
	}
	var diff []string
	for _, x := range first {
		if _, found := mb[x]; !found {
			diff = append(diff, x)
		}
	}
	return diff
}

func SliceContains(haystack []string, needle string) bool {
	for _, s := range haystack {
		if s == needle {
			return true
		}
	}
	return false
}

// SliceFilter calls the input function on each element of the input slice and
// returns a slice of all elements for which the function returns true.
// From here: https://gobyexample.com/collection-functions
func SliceFilter(in []string, f func(string) bool) []string {
	list := make([]string, 0)
	for _, s := range in {
		if f(s) {
			list = append(list, s)
		}
	}
	return list
}

func IndexOf(haystack []string, needle string) (int, error) {
	for i := range haystack {
		if needle == haystack[i] {
			return i, nil
		}
	}
	return -1, ErrNotFound
}

// SliceReject takes a slice of strings and a string to removed a returns a slice
// with any matching reject strings removed.
func SliceReject(haystack []string, needle string) []string {
	res := []string{}

	for _, hay := range haystack {
		if hay != needle {
			res = append(res, hay)
		}
	}

	return res
}

func GetFullPlatformName(name, release string) string {
	return fmt.Sprintf("%s %s", name, release)
}

func GetFullProfileName(name, release string) string {
	return fmt.Sprintf("%s, v%s", name, release)
}
