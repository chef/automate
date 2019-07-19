package hab

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"path"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/crypto/blake2b"

	"github.com/chef/automate/components/automate-deployment/pkg/depot"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/lib/stringutils"
)

type VerifyErrChecksumMismatch struct {
	pkg                string
	filename           string
	expectedChecksum   string
	calculatedChecksum string
}

type VerifyErrBadFileManifest struct {
	pkg string
	err error
}

type VerifyErrMissingFile struct {
	pkg      string
	filename string
}

type VerifyErrExtraFile struct {
	pkg      string
	filename string
}

func (v *VerifyErrMissingFile) Error() string {
	return fmt.Sprintf("%s: missing file: %s", v.pkg, v.filename)
}

func (v *VerifyErrExtraFile) Error() string {
	return fmt.Sprintf("%s: extra file: %s", v.pkg, v.filename)
}

func (v *VerifyErrBadFileManifest) Error() string {
	return fmt.Sprintf("%s: bad FILES file: %s", v.pkg, v.err.Error())
}

func (v *VerifyErrChecksumMismatch) Error() string {
	return fmt.Sprintf("%s: mismatched checksum: %s (expected=%s, calculated=%s)",
		v.pkg, v.filename, v.expectedChecksum, v.calculatedChecksum)
}

type pkgFile struct {
	filename string
	checksum string
}

func loadFILESFile(pkg habpkg.HabPkg) (map[string]string, error) {
	pkgPath := path.Join("/hab/pkgs", pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release())
	pkgFILESPath := path.Join(pkgPath, "FILES")
	f, err := os.Open(pkgFILESPath)
	if err != nil {
		if os.IsNotExist(err) && isKnownMissingFILES(pkgFILESPath) {
			logrus.Debugf("%s is known to not exist. Skipping", pkgFILESPath)
			return nil, nil
		}
		return nil, err
	}
	defer func() {
		_ = f.Close()
	}()

	files := make(map[string]string)
	bufReader := bufio.NewReader(f)

	// Read the Header
	for {
		val, err := bufReader.ReadString('\n')
		if err != nil {
			return nil, errors.Wrapf(err, "failed to read %s", pkgPath)
		}
		if val == "\n" {
			break
		}
	}

	// Read the (checksum, file) pairs
	for {
		val, err := bufReader.ReadString('\n')

		if err != nil {
			if err == io.EOF {
				if val == "" {
					break
				}
			} else {
				return nil, errors.Wrapf(err, "failed to read %s", pkgPath)
			}
		}
		fields := strings.SplitAfterN(val, " ", 2)
		if len(fields) != 2 {
			return nil, errors.Errorf("invalid line %q for file %s", pkgPath, val)
		}
		files[strings.TrimSpace(fields[1])] = strings.TrimSpace(fields[0])

		if err != nil {
			break
		}
	}

	if len(files) == 0 {
		return nil, errors.Errorf("no files found in package %s", pkgPath)
	}

	return files, nil
}

func listFilesInPkg(pkg habpkg.HabPkg) ([]pkgFile, error) {
	pkgFiles := []pkgFile{}
	pkgPath := path.Join("/hab/pkgs", pkg.Origin(), pkg.Name(), pkg.Version(), pkg.Release())
	err := filepath.Walk(pkgPath, func(filePath string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}

		if info.Mode()&os.ModeSymlink != 0 {
			logrus.Debugf("%s is a symlink. Skipping", filePath)
			return nil
		}

		relPath, err := filepath.Rel(pkgPath, filePath)
		if err != nil {
			return errors.Wrapf(err, "Could not get relative path for %s", filePath)
		}

		if path.Base(relPath) == "FILES" {
			return nil
		}

		if path.Base(relPath) == "MANIFEST" {
			return nil
		}

		f, err := os.Open(filePath)
		if err != nil {
			return errors.Wrapf(err, "failed to open file %s", filePath)
		}
		defer func() {
			_ = f.Close()
		}()

		hasher, err := blake2b.New256(nil)
		if err != nil {
			return errors.Wrap(err, "failed to create hasher")
		}

		if _, err := io.Copy(hasher, f); err != nil {
			return errors.Wrapf(err, "failed to checksum %s", filePath)
		}

		pkgFiles = append(pkgFiles, pkgFile{
			filename: filePath,
			checksum: fmt.Sprintf("%x", hasher.Sum(nil)),
		})

		return nil
	})
	if err != nil {
		return nil, err
	}

	return pkgFiles, nil
}

var knownMissingFILES = []string{
	"/hab/pkgs/core/libarchive/3.3.2/20171018164107/FILES",
}

func isKnownMissingFILES(filePath string) bool {
	return stringutils.SliceContains(knownMissingFILES, filePath)
}

func VerifyHabPackages() error {
	cache := depot.FromLocalCache()
	pkgs, err := cache.ListAllPackages()
	if err != nil {
		return errors.Wrap(err, "failed to list packages")
	}

	errs := []error{}
	for _, pkg := range pkgs {
		expectedFiles, err := loadFILESFile(pkg)
		if err != nil {
			errs = append(errs, &VerifyErrBadFileManifest{
				pkg: habpkg.Ident(&pkg),
				err: err,
			})
			continue
		}
		if expectedFiles == nil {
			continue
		}
		presentFiles, err := listFilesInPkg(pkg)
		if err != nil {
			return err
		}

		for name := range expectedFiles {
			found := false
			for _, f := range presentFiles {
				if f.filename == name {
					found = true
					break
				}
			}
			if !found {
				errs = append(errs, &VerifyErrMissingFile{
					pkg:      habpkg.Ident(&pkg),
					filename: name,
				})
			}
		}

		for _, f := range presentFiles {
			if expectedChecksum, ok := expectedFiles[f.filename]; ok {
				if expectedChecksum != f.checksum {
					errs = append(errs, &VerifyErrChecksumMismatch{
						pkg:                habpkg.Ident(&pkg),
						filename:           f.filename,
						expectedChecksum:   expectedChecksum,
						calculatedChecksum: f.checksum,
					})
				}
			} else {
				errs = append(errs, &VerifyErrExtraFile{
					pkg:      habpkg.Ident(&pkg),
					filename: f.filename,
				})
			}
		}
	}

	if len(errs) == 0 {
		return nil
	}

	for _, err := range errs {
		fmt.Println(err.Error())
	}

	return errors.New("Failed to verify installed packages")
}
