package backup

import (
	"bufio"
	"compress/gzip"
	"io"
	"os"
	"path"
	"sort"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/require"
)

// helper funcs
func allContained(haystack []string, needles []string) bool {
	j := 0
	for i := 0; i < len(needles); i++ {
	INNER:
		for {
			if needles[i] < haystack[j] {
				return false
			} else if needles[i] > haystack[j] {
				j++
			} else {
				break INNER
			}
		}
	}
	return true
}

func anyContained(haystack []string, needles []string) bool {
	for i := 0; i < len(needles); i++ {
		if stringutils.SliceContains(haystack, needles[i]) {
			return true
		}
	}
	return false
}

func checkArtifactsExist(baseDir string, artifacts []string) (bool, error) {
	for _, artifact := range artifacts {
		p := path.Join(baseDir, "shared/builder/artifacts", artifact)
		exists, err := fileutils.PathExists(p)
		if err != nil {
			return false, errors.Wrap(err, "could not check artifact file")
		}
		if !exists {
			logrus.Infof("Could not find %q", p)
			return false, nil
		}
	}
	return true, nil
}

func consume(t *testing.T, stream ArtifactStream) []string {
	t.Helper()

	ret := []string{}
	for {
		v, err := stream.Next()
		if err != nil {
			if err == io.EOF {
				break
			}
			require.NoError(t, err)
		}
		ret = append(ret, v)
	}
	return ret
}

func readGzipFile(filePath string) ([]string, error) {
	lines := []string{}
	f, err := os.Open(filePath)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	g, err := gzip.NewReader(f)
	if err != nil {
		return nil, err
	}
	defer g.Close()

	scanner := bufio.NewScanner(g)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines, scanner.Err()
}

func sortedUniquify(in []string) []string {
	if len(in) == 0 {
		return in
	}
	// https://github.com/golang/go/wiki/SliceTricks#in-place-deduplicate-comparable
	j := 0
	for i := 1; i < len(in); i++ {
		if in[j] == in[i] {
			continue
		}
		j++
		in[j] = in[i]
	}
	return in[:j+1]
}

func sortStrings(in []string) []string {
	sort.Strings(in)
	return in
}

func isSorted(in []string) bool {
	return sort.IsSorted(sort.StringSlice(in))
}

func isSortedAndUnique(in []string) bool {
	if len(in) == 0 {
		return true
	}
	if !isSorted(in) {
		return false
	}
	last := in[0]
	for i := 1; i < len(in); i++ {
		if in[i] == last {
			return false
		}
	}
	return true
}

func stringSliceEquals(a []string, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := 0; i < len(a); i++ {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// gopter helpers
func stringGen(size int) gopter.Gen {
	return gen.SliceOfN(size, gen.AlphaNumChar()).Map(func(r []rune) string {
		return string(r)
	}).WithShrinker(gen.StringShrinker)
}

func smallStringListGen() gopter.Gen {
	return gen.SliceOf(
		gen.OneGenOf(
			gen.SliceOfN(2, gen.AlphaNumChar()),
			gen.SliceOfN(1, gen.AlphaNumChar()),
		).Map(func(r []rune) string {
			return string(r)
		}),
	).Map(sortStrings).Map(sortedUniquify)
}
