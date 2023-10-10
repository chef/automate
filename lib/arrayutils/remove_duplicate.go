package arrayutils

func RemoveStringDuplicates(inputArray []string) []string {
	arrayWithoutDuplicate := []string{}
	for _, item := range inputArray {
		if !contains(arrayWithoutDuplicate, item) {
			arrayWithoutDuplicate = append(arrayWithoutDuplicate, item)
		}
	}
	return arrayWithoutDuplicate
}

func contains(arrayWithoutDuplicate []string, item string) bool {
	index := -1
	for i := range arrayWithoutDuplicate {
		if item == arrayWithoutDuplicate[i] {
			index = i
		}
	}
	return index >= 0
}
