// +build debug

// Add -tags debug to go run or go build to turn on debug output

package chef

import "log"

func debug(fmt string, args ...interface{}) {
	log.Printf(fmt, args...)
}

func debug_on() bool {
	return true
}
