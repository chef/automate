package preflight

import (
	"fmt"
	"path"
	"strconv"
	"strings"

	"github.com/pkg/errors"
)

type SysctlCheck struct {
	Name       string
	RangeLower uint64
	RangeUpper uint64
	FixValue   uint64
}

func (s SysctlCheck) IsRange() bool {
	return s.RangeUpper > s.RangeLower
}

func (s SysctlCheck) IsValid(val uint64) bool {
	if val < s.RangeLower {
		return false
	}

	if s.IsRange() && val > s.RangeUpper {
		return false
	}

	return true
}

func (s SysctlCheck) SysctlString() string {
	val := s.RangeLower
	if s.FixValue != 0 {
		val = s.FixValue
	}
	return fmt.Sprintf("%s=%d", s.Name, val)
}

func readSysctl(t TestProbe, sysctlName string) (uint64, error) {
	sysctlPath := path.Join("/proc/sys", strings.Replace(sysctlName, ".", "/", -1))
	bytes, err := t.File(sysctlPath)
	if err != nil {
		return 0, errors.Wrapf(err, "could not get the value for %s", sysctlName)
	}
	str := strings.TrimSpace(string(bytes))
	val, err := strconv.ParseUint(str, 10, 64)
	if err != nil {
		return 0, errors.Wrapf(err, "could not parse the value for %s (%s)", sysctlName, str)
	}

	return val, nil
}
