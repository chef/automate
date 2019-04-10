//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package server

import (
	"runtime"
	"strings"
)

// nameOfFunc returns the name of the caller function
func nameOfFunc() string {
	pc, _, _, ok := runtime.Caller(1)
	details := runtime.FuncForPC(pc)
	if ok && details != nil {
		d := strings.Split(details.Name(), ".")
		return d[len(d)-1]
	}
	return "unknown"
}
