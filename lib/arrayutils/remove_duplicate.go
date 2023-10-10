package arrayutils

import (
	"golang.org/x/exp/slices"
)

func RemoveStringDuplicates(inputArray []string) []string {
	arrayWithoutDuplicate := []string{}
	for _, item := range inputArray {
		if !slices.Contains(arrayWithoutDuplicate, item) {
			arrayWithoutDuplicate = append(arrayWithoutDuplicate, item)
		}
	}
	return arrayWithoutDuplicate
}
