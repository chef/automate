// Copyright © 2017 Chef Software

package habpkg

import (
	"fmt"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"github.com/pkg/errors"
)

// SortableHarts are paths to hartifacts on disk.  We expect the files
// to follow the hartifact naming conventions and sort them by the
// version and timestamp embedded in the filename.
type SortableHarts []Hart

// A Hart represents a hartifact archive on disk.
type Hart struct {
	origin    string
	name      string
	path      string
	major     int
	minor     int
	patch     int
	timestamp int
}

// Path returns the on-disk path to the hart
func (h *Hart) Path() string {
	return h.path
}

// Timestamp returns the Release as an integer
func (h *Hart) Timestamp() int {
	return h.timestamp
}

// VersionedPackage interface implementation

// Origin returns the file-name-inferred origin for the hart.
func (h *Hart) Origin() string {
	return h.origin
}

// Name returns the file-name-inferred name for the hart.
func (h *Hart) Name() string {
	return h.name
}

// Release returns the file-name-inferred release for the hart.
func (h *Hart) Release() string {
	return strconv.FormatInt(int64(h.timestamp), 10)
}

// Version returns the file-name-inferred version for the hart.
func (h *Hart) Version() string {
	return fmt.Sprintf("%d.%d.%d", h.major, h.minor, h.patch)
}

func (h *Hart) InstallIdent() string {
	return h.path
}

// HartDir represents a collection of harts
type HartDir struct {
	path string
}

// NewHartDir constructs a hartDir for the given path
func NewHartDir(path string) HartDir {
	return HartDir{
		path: path,
	}
}

// FindHart returns the the newest hartifact for the given
// origin and name contained in the HartDir. If no hart exists for the
// given origin and name, an empty string is returned.
func (d *HartDir) FindHart(origin string, name string) (*Hart, error) {
	candidates, err := d.sortedHartsFor(origin, name)
	if err != nil {
		return nil, err
	}

	if len(candidates) != 0 {
		return &candidates[0], nil
	}

	return nil, nil
}

// SortedHarts returns a sorted array of hartifacts from the given
// hart directory with the given name and origin.
//
// Assumes that package versions start with a digit.
func (d *HartDir) sortedHartsFor(origin string, name string) (SortableHarts, error) {
	glob := fmt.Sprintf("%s/%s-%s-[0-9]*.hart", d.path, origin, name)
	harts, err := filepath.Glob(glob)
	if err != nil {
		return nil, errors.Wrap(err, "glob failed in hartifact dir")
	}

	sortableHarts, err := sortableHartsFromPaths(origin, name, harts)
	if err != nil {
		return nil, errors.Wrap(err, "hartifact path contains harts that could not be parsed")
	}

	sort.Sort(sort.Reverse(sortableHarts))
	return sortableHarts, nil
}

func sortableHartsFromPaths(origin string, name string, paths []string) (SortableHarts, error) {
	ret := make([]Hart, 0, len(paths))
	for _, path := range paths {
		v, err := HartFromPath(path)

		if err != nil {
			return ret, err
		}

		v.origin = origin
		v.name = name

		ret = append(ret, v)
	}

	return ret, nil
}

// Implement sort.Interface for sortableHarts allowing us to use the
// routines in the sort package to do any sorting.
func (h SortableHarts) Len() int {
	return len(h)
}

func (h SortableHarts) Swap(i int, j int) {
	h[i], h[j] = h[j], h[i]
}

func (h SortableHarts) Less(i int, j int) bool {
	hartA, hartB := h[i], h[j]
	if hartA.name < hartB.name {
		return true
	}

	if hartA.name > hartB.name {
		return false
	}

	if hartA.major < hartB.major {
		return true
	}

	if hartA.major > hartB.major {
		return false
	}

	if hartA.minor < hartB.minor {
		return true
	}

	if hartA.minor > hartB.minor {
		return false
	}

	if hartA.patch < hartB.patch {
		return true
	}

	if hartA.patch > hartB.patch {
		return false
	}

	return hartA.timestamp < hartB.timestamp
}

// MarshalText turns a HabPkg into text
func (h *Hart) MarshalText() ([]byte, error) {
	text := fmt.Sprintf("%s/%s:%s", h.Origin(), h.Name(), h.Path())
	return []byte(text), nil
}

// UnmarshalText can take text and parse a HabPkg
func (h *Hart) UnmarshalText(text []byte) error {
	partsHartStr := strings.Split(string(text), ":")
	if len(partsHartStr) != 2 {
		return errors.New("failed to parse hart: not enough parts")
	}

	partsHartPkg := strings.Split(partsHartStr[0], "/")
	if len(partsHartPkg) != 2 {
		return errors.New("failed to parse hart package identifier: not enough parts")
	}

	parsed, err := HartFromPath(partsHartStr[1])
	if err != nil {
		return err
	}
	*h = parsed
	h.origin = partsHartPkg[0]
	h.name = partsHartPkg[1]

	return nil
}

// HartFromPath parses a path into a Hart
func HartFromPath(path string) (Hart, error) {
	var err error

	versionParts := make([]int, 4)

	filename := filepath.Base(path)
	// The hartifact filename is in the form of
	//
	// ORIGIN-PACKAGE_NAME-VERSION-RELEASE-TARGET.hart
	//
	// Where
	//       ORIGIN and PACKAGE_NAME are arbitrary strings
	//
	//       VERSION is also an arbitrary string but most packages
	//               have a d.d.d style version number
	//
	//       RELEASE is a timestamp with the format YYYYMMDDHHMMSS
	//
	//       TARGET is ARCH-OS where ARCH and OS are collected
	//              from uname. Currently this is unlikely to be
	//              anything other than x86_64-linux for our
	//              packages.
	//
	// Since nearly all components of this name are arbitrary and
	// can (and do!) include -'s, we use a regular expression
	// based on the non-arbitrary RELEASE and our standard VERSION
	// scheme and rely on the fact that we won't name our services
	// anything too absurd.
	r := regexp.MustCompile(`.*-(\d+)\.(\d+)\.(\d+)-(\d{14})-.*\.hart$`)
	match := r.FindStringSubmatch(filename)
	if match == nil {
		return Hart{}, errors.Errorf("failed to parse version of hart %s", filename)
	}

	for i := 1; i < 5; i++ {
		versionParts[i-1], err = strconv.Atoi(match[i])
		if err != nil {
			return Hart{}, errors.Errorf("failed to parse version of hart %s", filename)
		}
	}

	return Hart{
		// origin and name are ambiguous in the path. We'll add them in later
		path:      path,
		major:     versionParts[0],
		minor:     versionParts[1],
		patch:     versionParts[2],
		timestamp: versionParts[3]}, nil
}

// WithName sets the name of the hartifact
func (h *Hart) WithName(name string) *Hart {
	h.name = name
	return h
}

// WithOrigin sets the origin of the hartifact
func (h *Hart) WithOrigin(origin string) *Hart {
	h.origin = origin
	return h
}

func (h *Hart) String() string {
	return h.InstallIdent()
}
