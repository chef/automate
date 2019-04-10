package stringutils

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
