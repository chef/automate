package proc

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"regexp"
	"strconv"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type KernelVersion struct {
	Major int
	Minor int
}

func (k KernelVersion) String() string {
	return fmt.Sprintf("%d.%d", k.Major, k.Minor)
}

// /proc/meminfo
func ParseMemInfoMemTotal(reader io.Reader) (int, error) {
	scanner := bufio.NewScanner(reader)
	memTotalKiloBytes := 0
	for scanner.Scan() {
		if bytes.HasPrefix(scanner.Bytes(), []byte("MemTotal:")) {
			_, err := fmt.Sscanf(scanner.Text(), "MemTotal:%d", &memTotalKiloBytes)
			if err != nil {
				logrus.WithError(err).Debug("failed to parse MemTotal")
				return 0, errors.Wrap(err, "could not determine available memory")
			}
			return memTotalKiloBytes, nil
		}
	}
	return 0, errors.New("MemTotal not found in /proc/meminfo")
}

// /proc/sys/kernel/osrelease
func ParseOSRelease(data []byte) (KernelVersion, error) {
	kernelVersion := strings.TrimSpace(string(data))
	kernelVersionRegex := regexp.MustCompile(`^(?P<major>\d+)(\.(?P<minor>\d+))?(\.(?P<revision>\d+))`)
	m := kernelVersionRegex.FindStringSubmatch(kernelVersion)

	if m == nil {
		logrus.WithField("osrelease", string(data)).Debug("Failed to parse kernel version")
		return KernelVersion{}, errors.New("Failed to parse os version")
	}

	version := map[string]string{}
	for i, name := range kernelVersionRegex.SubexpNames() {
		if i != 0 && name != "" {
			version[name] = m[i]
		}
	}

	major, _ := strconv.Atoi(version["major"])
	minor, _ := strconv.Atoi(version["minor"])

	return KernelVersion{
		Major: major,
		Minor: minor,
	}, nil
}

// /proc/mounts
// Currently just gets the "mntonname" since that is all we need.
func ParseProcMounts(data []byte) ([]string, error) {
	entries := strings.Split(string(data), "\n")
	ret := make([]string, len(entries)-1)
	for i, e := range entries {
		if e == "" {
			continue
		}
		isSpace := func(c rune) bool {
			// According to the man page it can be
			// separated by tabs too but I don't have an
			// example of it.
			return c == ' ' || c == '\t'
		}
		fields := strings.FieldsFunc(e, isSpace)
		// /proc/mounts encodes all whitespace so this
		// shouldn't happen. But just in case.
		if len(fields) != 6 {
			return nil, errors.Errorf("unexpected number of fields in /proc/mounts (got %d, expected 6): %s", len(fields), e)
		}
		ret[i] = unescape(fields[1])
	}

	return ret, nil
}

// From http://man7.org/linux/man-pages/man3/getmntent.3.html:
//
// Since fields in the mtab and fstab files are separated by whitespace,
// octal escapes are used to represent the characters space (\040), tab
// (\011), newline (\012), and backslash (\\) in those files when they
// occur in one of the four strings in a mntent structure.  The routines
// addmntent() and getmntent() will convert from string representation
// to escaped representation and back.  When converting from escaped
// representation, the sequence \134 is also converted to a backslash.
func unescape(s string) string {
	s = strings.Replace(s, "\\040", " ", -1)
	s = strings.Replace(s, "\\011", "\t", -1)
	s = strings.Replace(s, "\\012", "\n", -1)
	s = strings.Replace(s, "\\\\", "\\", -1)
	s = strings.Replace(s, "\\134", "\\", -1)
	return s
}
