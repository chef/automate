package airgap

import (
	"archive/tar"
	"bufio"
	"io"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"strings"

	"github.com/chef/automate/lib/io/fileutils"

	"github.com/pkg/errors"
)

// InstallBundleFileTypeFlag signifies which type of file an item in the install
// bundle is
type InstallBundleFileTypeFlag byte

const (
	// Hartifact signifies that the file is a .hart
	Hartifact InstallBundleFileTypeFlag = iota
	// OriginKey signifies the file is one of the public origin keys
	OriginKey
	// HabBin signifies the file is the hab executable
	HabBin
	// Manifest signifies the file is the manifest.json
	Manifest
)

// InstallBundleItem represents an item in the install bundle
type InstallBundleItem struct {
	Typeflag InstallBundleFileTypeFlag
	Name     string
	FileInfo os.FileInfo
}

// ReadVisitFunc is the func called for each item in the install bundle. The
// receiver may choose to ignore it or read from the provided reader
type ReadVisitFunc func(InstallBundleItem, io.Reader) error

// Read reads an install bundle. It calls the provided ReadVisitFunc for each
// item in the install bundle.
func Read(installBundlePath string, f ReadVisitFunc) error {
	installBundleFile, err := os.Open(installBundlePath)
	if err != nil {
		return errors.Wrapf(err, "Failed to open install bundle file %s", installBundlePath)
	}
	defer installBundleFile.Close() // nolint: errcheck

	bufReader := bufio.NewReader(installBundleFile)
	// Read the header:
	// AIB-1\n\n
	s, err := bufReader.ReadString('\n')
	if err != nil {
		return errors.Wrap(err, "Could not read artifact file")
	}
	if s != "AIB-1\n" {
		return errors.New("Malformed install bundle file")
	}

	s, err = bufReader.ReadString('\n')
	if err != nil {
		return err
	}

	if s != "\n" {
		return errors.New("Malformed install bundle file")
	}

	reader := tar.NewReader(bufReader)

	for {
		header, err := reader.Next()
		if err != nil {
			if err == io.EOF {
				break
			}
			return errors.Wrap(err, "Could not read archive")
		}
		installBundleItem := InstallBundleItem{}
		if header.Typeflag != tar.TypeReg {
			continue
		}

		if header.Name == "manifest.json" {
			installBundleItem.Typeflag = Manifest
		} else if header.Name == "bin/hab" {
			installBundleItem.Typeflag = HabBin
		} else if strings.HasPrefix(header.Name, "hab/cache/artifacts/") {
			installBundleItem.Typeflag = Hartifact
		} else if strings.HasPrefix(header.Name, "hab/cache/keys/") {
			installBundleItem.Typeflag = OriginKey
		} else {
			continue
		}

		installBundleItem.Name = filepath.Base(header.Name)
		installBundleItem.FileInfo = header.FileInfo()

		err = f(installBundleItem, reader)
		if err != nil {
			return err
		}
	}
	return nil
}

// UnpackMetadata is the result of unpacking an install bundle. It contains the paths
// to the files that were unpacked
type UnpackMetadata struct {
	HartifactPaths []string
	OriginKeyPaths []string
	ManifestPath   string
	HabBinPath     string
}

type unpackOpts struct {
	root      string
	hartsOnly bool
}

// UnpackOpt are functional options for Unpack
type UnpackOpt func(*unpackOpts)

// WithUnpackRoot sets the root directory for unpacking. By default,
// it is /
func WithUnpackRoot(root string) UnpackOpt {
	return func(opts *unpackOpts) {
		opts.root = root
	}
}

// WithUnpackHartsOnly only unpacks the hartifacts and their keys.
func WithUnpackHartsOnly(hartsOnly bool) UnpackOpt {
	return func(opts *unpackOpts) {
		opts.hartsOnly = hartsOnly
	}
}

// Unpack puts the files in the given install bundle at the right place on disk
func Unpack(installBundlePath string, opts ...UnpackOpt) (UnpackMetadata, error) {
	conf := unpackOpts{
		root: "/",
	}

	for _, o := range opts {
		o(&conf)
	}

	metadata := UnpackMetadata{
		HartifactPaths: []string{},
		OriginKeyPaths: []string{},
	}

	hartsDir := path.Join(conf.root, "hab/cache/artifacts")
	keysDir := path.Join(conf.root, "hab/cache/keys")
	manifestDir := path.Join(conf.root, "hab/svc/deployment-service/data/airgap")
	tmpDir := path.Join(conf.root, "hab/tmp")

	requiredDirectories := []string{
		hartsDir,
		keysDir,
	}

	if !conf.hartsOnly {
		requiredDirectories = append(requiredDirectories,
			manifestDir,
			tmpDir,
		)
	}

	for _, d := range requiredDirectories {
		err := os.MkdirAll(d, 0755)
		if err != nil {
			return metadata, errors.Wrapf(err, "Could not create directory %s", d)

		}
	}

	err := Read(installBundlePath, func(item InstallBundleItem, reader io.Reader) error {
		if conf.hartsOnly && !(item.Typeflag == Hartifact || item.Typeflag == OriginKey) {
			return nil
		}
		switch item.Typeflag {
		case Hartifact:
			hartPath := path.Join(hartsDir, item.Name)
			metadata.HartifactPaths = append(metadata.HartifactPaths, hartPath)
			err := fileutils.AtomicWrite(hartPath, reader)
			if err != nil {
				return errors.Wrapf(err, "Could not write hartifact %s", item.Name)
			}
		case OriginKey:
			keyPath := path.Join(keysDir, item.Name)
			metadata.OriginKeyPaths = append(metadata.OriginKeyPaths, keyPath)
			err := fileutils.AtomicWrite(keyPath, reader)
			if err != nil {
				return errors.Wrapf(err, "Could not write origin key %s", item.Name)
			}
		case HabBin:
			// Unpack to a directory other than /hab/bin to avoid binlinking
			// issues in the studio.
			metadata.HabBinPath = path.Join(tmpDir, "hab")
			err := fileutils.AtomicWrite(metadata.HabBinPath, reader, fileutils.WithAtomicWriteFileMode(0755))
			if err != nil {
				return errors.Wrapf(err, "Could not write hab binary")
			}
		case Manifest:
			metadata.ManifestPath = path.Join(manifestDir, "manifest.json")
			err := fileutils.AtomicWrite(metadata.ManifestPath, reader)
			if err != nil {
				return errors.Wrap(err, "Could not write manifest")
			}
		}
		return nil
	})

	return metadata, err
}

func GetMetadata(installBundlePath string) (UnpackMetadata, []byte, error) {
	metadata := UnpackMetadata{
		HartifactPaths: []string{},
		OriginKeyPaths: []string{},
	}

	var (
		hartsDir    = "/hab/cache/artifacts"
		keysDir     = "/hab/cache/keys"
		manifestDir = "/hab/svc/deployment-service/data/airgap"
		tmpDir      = "/hab/tmp"
	)

	manifestBytes := []byte{}
	err := Read(installBundlePath, func(item InstallBundleItem, reader io.Reader) error {
		switch item.Typeflag {
		case Hartifact:
			hartPath := path.Join(hartsDir, item.Name)
			metadata.HartifactPaths = append(metadata.HartifactPaths, hartPath)
		case OriginKey:
			keyPath := path.Join(keysDir, item.Name)
			metadata.OriginKeyPaths = append(metadata.OriginKeyPaths, keyPath)
		case HabBin:
			metadata.HabBinPath = path.Join(tmpDir, "hab")
		case Manifest:
			var err error
			metadata.ManifestPath = path.Join(manifestDir, "manifest.json")
			manifestBytes, err = ioutil.ReadAll(reader)
			if err != nil {
				return errors.Wrap(err, "Could not read manifest")
			}
		}
		return nil

	})

	return metadata, manifestBytes, err
}
