package arrayutils

// This method is used to find the difference between the two slices and return the uncommon elements in the first set. It returns (A-B) operation
func SliceDifference(first, second []int) []int {
	mb := make(map[int]struct{}, len(second))
	for _, x := range second {
		mb[x] = struct{}{}
	}
	var diff []int
	for _, x := range first {
		if _, found := mb[x]; !found {
			diff = append(diff, x)
		}
	}
	return diff
}
