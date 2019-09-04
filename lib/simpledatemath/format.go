package simpledatemath

import (
	"time"

	"github.com/pkg/errors"
)

var errLeadingIntOverflow = errors.New("integer too large for int64") // never printed

var unitMap = map[string]time.Duration{
	"s": time.Second,
	"m": time.Minute,
	"h": time.Hour,
	"H": time.Hour,
	"d": 24 * time.Hour,
}

func Parse(s string) (time.Duration, error) {
	if s == "" {
		return 0, errors.Errorf("simpledatemath: invalid duration expression %q", s)
	}
	if !('0' <= s[0] && s[0] <= '9') {
		return 0, errors.Errorf("simpledatemath: duration expression %q does not start with a number", s)
	}

	integer, unitStr, err := leadingInt(s)
	if err != nil {
		return 0, errors.Errorf("simpledatemath: duration expression integer is too large %q", s)
	}

	if len(unitStr) == 0 {
		return 0, errors.Errorf("simpledatemath: duration expression %q does not have a unit", s)
	}

	if '.' == unitStr[0] {
		return 0, errors.Errorf("simpledatemath: duration expression containing decimal fractions is not supported %q", s)
	}

	unit, ok := unitMap[unitStr]
	if !ok {
		return 0, errors.Errorf("simpledatemath: duration expression %q has an invalid or unsupported unit %q", s, unitStr)
	}

	return time.Duration(integer) * unit, nil
}

func Validate(s string) error {
	_, err := Parse(s)
	return err
}

// Copied mostly verbatim from src/time/format.go in the go source.
// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
func leadingInt(s string) (x int64, rem string, err error) {
	i := 0
	for ; i < len(s); i++ {
		c := s[i]
		if c < '0' || c > '9' {
			break
		}
		if x > (1<<63-1)/10 {
			// overflow
			return 0, "", errLeadingIntOverflow
		}
		x = x*10 + int64(c) - '0'
		if x < 0 {
			// overflow
			return 0, "", errLeadingIntOverflow
		}
	}
	return x, s[i:], nil
}
