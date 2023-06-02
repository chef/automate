package stringutils

import "strings"

func GetFileName(path string) string {
	if strings.Contains(path, "/") {
		filePath := strings.Split(path, "/")
		lastFileidx := len(filePath) - 1
		return filePath[lastFileidx]
	}
	return path
}
