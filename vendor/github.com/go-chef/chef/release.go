// +build !debug

package chef

func debug(fmt string, args ...interface{}) {
}

func debug_on() bool {
	return false
}
