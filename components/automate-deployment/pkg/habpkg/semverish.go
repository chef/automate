package habpkg

import (
	"strconv"
	"strings"

	"github.com/pkg/errors"
)

// SemverishVersion holds a semver-like version. It supports the
// following format:
//
// [v]VERSION_PART[.VERSION_PART...][-PRERELEASE][+METADATA]
//
// Unlike traditional semver, it supports any non-zero number of
// version parts.
//
type SemverishVersion struct {
	parts      []int
	prerelease string
	metadata   string
}

func ParseSemverishVersion(s string) (SemverishVersion, error) {
	var (
		ret = SemverishVersion{
			parts: make([]int, 0, 3),
		}
		err     error
		nextInt int
	)

	if s[0] == 'v' {
		s = s[1:]
	}
	nextInt, s, err = getInt(s)
	if err != nil {
		return ret, err
	}

	ret.parts = append(ret.parts, nextInt)
	for len(s) != 0 {
		switch s[0] {
		case '.':
			s = s[1:]
			nextInt, s, err = getInt(s)
			if err != nil {
				return ret, err
			}
			ret.parts = append(ret.parts, nextInt)
		case '-':
			s = s[1:]
			idx := strings.Index(s, "+")
			if idx == -1 {
				ret.prerelease = s
				return ret, nil
			}
			ret.prerelease = s[0:idx]
			s = s[idx:]
		case '+':
			ret.metadata = s[1:]
			return ret, nil
		default:
			return ret, errors.Errorf("unexpected character in semver version: %s", s)
		}
	}
	return ret, nil
}

func getInt(s string) (int, string, error) {
	end := 0
	for len(s) > end && s[end] >= '0' && s[end] <= '9' {
		end++
	}
	if end == 0 {
		return 0, s, errors.New("could not parse expected integer")
	}
	ret, err := strconv.Atoi(s[0:end])
	return ret, s[end:], err
}

const (
	SemverishGreater = 1
	SemverishEqual   = 0
	SemverishLess    = -1
)

func CompareSemverish(a SemverishVersion, b SemverishVersion) int {
	for i := 0; i < len(a.parts); i++ {
		if len(b.parts) > i {
			if a.parts[i] < b.parts[i] {
				return SemverishLess
			} else if a.parts[i] > b.parts[i] {
				return SemverishGreater
			}
		} else {
			return SemverishGreater
		}
	}
	if len(b.parts) > len(a.parts) {
		return SemverishLess
	}

	// No-prerelease always wins
	if a.prerelease == "" && b.prerelease != "" {
		return SemverishGreater
	} else if a.prerelease != "" && b.prerelease == "" {
		return SemverishLess
	}

	// Otherwise, just lexicographically sort the pre-release, this
	// is not what the spec says to do.
	if a.prerelease < b.prerelease {
		return SemverishLess
	} else if a.prerelease > b.prerelease {
		return SemverishGreater
	}

	return SemverishEqual
}

// GreaterOrEqual returns true if a is greater or equal to b, assuming
// both VersionedArtifacts use Semverish versions. It assumes
// Release() return lexically sortable string in case of a tie.
func SemverishGreaterOrEqual(a VersionedArtifact, b VersionedArtifact) (bool, error) {
	semverA, err := ParseSemverishVersion(a.Version())
	if err != nil {
		return false, err
	}
	semverB, err := ParseSemverishVersion(b.Version())
	if err != nil {
		return false, err
	}

	switch CompareSemverish(semverA, semverB) {
	case SemverishGreater:
		return true, nil
	case SemverishLess:
		return false, nil
	case SemverishEqual:
		return a.Release() >= b.Release(), nil
	default:
		return false, nil
	}
}
