//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package handler

import (
	"runtime"
	"strings"
)

// nameOfFunc returns the name of the caller function
//
// Use this method to log out what rpc function is being called
//
// Example: Log an rpc call where the `request` is a `proto.Message`
//
// ```
//  log.WithFields(log.Fields{
//    "request": request.String(),
//    "func":    nameOfFunc(),
//  }).Info("rpc call")
// ```
//
// This will display the following log output:
//
// ```
// time="2017-10-25T18:26:50Z" level=info msg="rpc call" func=GetRuns request=
// ```
func nameOfFunc() string {
	pc, _, _, ok := runtime.Caller(1)
	details := runtime.FuncForPC(pc)
	if ok && details != nil {
		d := strings.Split(details.Name(), ".")
		return d[len(d)-1]
	}
	return "unknown"
}
