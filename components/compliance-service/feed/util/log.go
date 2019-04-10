package util

import (
	"runtime"
	"strings"
)

// NameOfFunc returns the name of the caller function
func NameOfFunc() string {
	pc, _, _, ok := runtime.Caller(1)
	details := runtime.FuncForPC(pc)
	if ok && details != nil {
		d := strings.Split(details.Name(), ".")
		return d[len(d)-1]
	}
	return "unknown"
}
