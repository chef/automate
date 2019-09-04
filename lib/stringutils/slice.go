package stringutils

import "errors"

var ErrNotFound = errors.New("Not found")

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
