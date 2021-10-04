package stringutils

import "strings"

func TrimAfter(s, c string) string {
	if idx := strings.Index(s, c); idx != -1 {
		return s[:idx+len(c)]
	}
	return s
}

func TrimBefore(s, c string) string {
	if idx := strings.Index(s, c); idx != -1 {
		return s[idx:]
	}
	return s
}
