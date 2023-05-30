package arrayutils

// This method is used to return uncomman elements between the two slices. It returns (A-B) operation
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
