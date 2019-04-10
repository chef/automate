//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

// A set of functions to provide metrics and visibility to multiple parts
// of the backend package, we can do things like measure the time a func
// takes to execute, useful for example to measure the time ES queries takes
// to execute.

package backend

import (
	"time"

	log "github.com/sirupsen/logrus"
)

// ClockFn will call the provided func and clock the time it took to run
//
// Example:
// ```
//  f := func() error {
//    return aFuncToMeasure()
//  }
// err := backend.ClockFn(f)
// ```
//
func ClockFn(fn func() error) error {
	return ClockFnWithFields(fn, log.Fields{})
}

// ClockFnWithFields will call the provided func and clock the time it took
// to run plus adding the provided Fields to the log message
//
// Example: Clock the time to insert a elasticsearch document
// ```
//  f := func() error {
//    // Add a document on a particular
//    _, err := bkend.client.Index().
//      Index(index).
//      Type(mapping.Type).
//      BodyJson(data).
//      Do(ctx)
//    return err
//  }
//  fields := log.Fields{
//    "metric": "elasticsearch",
//    "type":   "doc_insert",
//    "index":  index,
//  }
// err := backend.ClockFnWithFields(f, fields)
// ```
//
// This example will log a message similar to:
// => time="" level=info msg=metric index=converge-history-2017.12.12 metric=elasticsearch ms=12 type=doc_insert
//
func ClockFnWithFields(fn func() error, fields log.Fields) error {
	var (
		start   = time.Now()
		err     = fn()
		t       = time.Now()
		elapsed = t.Sub(start)
	)

	fields["ms"] = elapsed.Nanoseconds() / int64(time.Millisecond)
	log.WithFields(fields).Debug("metric")

	return err
}
