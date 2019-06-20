package bootstrap

import (
	"archive/tar"
	"io"
	"os"
	"os/user"
	"path"
	"strconv"
	"strings"

	"github.com/chef/automate/components/automate-deployment/pkg/services"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/product"
	"github.com/pkg/errors"
)

var (
	rootUname   = "root"
	rootGname   = "root"
	rootUserID  = 0
	rootGroupID = 0
	habUname    = "hab"
	habGname    = "hab"
	habUserID   = 1
	habGroupID  = 1
)

const habSvcDir = "/hab/svc"

// BundleCreator creates installation bundles
type BundleCreator struct {
	rootDir string
}

func NewBundleCreator() *BundleCreator {
	return &BundleCreator{
		rootDir: habSvcDir,
	}
}

func (b *BundleCreator) writeFile(tarReader *tar.Reader, hdr *tar.Header) error {
	absPath := path.Join(b.rootDir, hdr.Name)
	err := fileutils.AtomicWrite(
		absPath,
		tarReader,
		fileutils.WithAtomicWriteFileMode(hdr.FileInfo().Mode()),
		fileutils.WithAtomicWriteChown(hdr.Uid, hdr.Gid),
	)
	if err != nil {
		return err
	}
	// atomic write respects the umask, so we do this one non atomic thing
	// to get the right mode
	if err := os.Chmod(absPath, hdr.FileInfo().Mode()); err != nil {
		return err
	}
	return nil
}

func (b *BundleCreator) mkdir(tarReader *tar.Reader, hdr *tar.Header) error {
	absPath := path.Join(b.rootDir, hdr.Name)
	// TODO (jaym): make atomic?
	err := os.Mkdir(absPath, hdr.FileInfo().Mode())
	if err != nil {
		if os.IsExist(err) {
			return nil
		}
		return err
	}
	if err := os.Chown(absPath, hdr.Uid, hdr.Gid); err != nil {
		return err
	}
	return nil
}

func (b *BundleCreator) Unpack(in io.Reader) error {
	tarReader := tar.NewReader(in)

	habUser, err := user.Lookup(habUname)
	if err != nil {
		return err
	}
	hUID, err := strconv.Atoi(habUser.Uid)
	if err != nil {
		return err
	}

	habGroup, err := user.LookupGroup(habGname)
	if err != nil {
		return err
	}
	hGID, err := strconv.Atoi(habGroup.Gid)
	if err != nil {
		return err
	}

	rootUser, err := user.Lookup(rootUname)
	if err != nil {
		return err
	}
	rUID, err := strconv.Atoi(rootUser.Uid)
	if err != nil {
		return err
	}

	rootGroup, err := user.LookupGroup(rootGname)
	if err != nil {
		return err
	}
	rGID, err := strconv.Atoi(rootGroup.Gid)
	if err != nil {
		return err
	}

	exists, err := fileutils.PathExists(b.rootDir)
	if err != nil {
		return err
	}
	if !exists {
		return errors.Errorf("cannot unpack into %q because it does not", b.rootDir)
	}
	for {
		hdr, err := tarReader.Next()
		if err != nil {
			if err == io.EOF {
				break
			}
		}
		switch hdr.Uid {
		case habUserID:
			hdr.Uid = hUID
		case rootUserID:
			hdr.Uid = rUID
		default:
			return errors.New("Unknown user id")
		}

		switch hdr.Gid {
		case habGroupID:
			hdr.Gid = hGID
		case rootUserID:
			hdr.Gid = rGID
		default:
			return errors.New("Unknown user id")
		}

		if hdr.Typeflag == tar.TypeDir {
			if err := b.mkdir(tarReader, hdr); err != nil {
				return err
			}
		} else if hdr.Typeflag == tar.TypeReg {
			if err := b.writeFile(tarReader, hdr); err != nil {
				return err
			}
		} else {
			return errors.Errorf("Unknown tar header type %q", hdr.Typeflag)
		}
	}
	return nil
}

func (b *BundleCreator) Create(pkgNames []string, out io.Writer) error {
	allDirsSet := make(map[string]*tar.Header)
	allDirs := []*tar.Header{}
	files := make(map[string]*tar.Header)
	for _, svc := range pkgNames {
		metadata := services.MetadataForPackage(svc)
		if metadata != nil && len(metadata.Bootstrap) > 0 {
			for _, bootstrapSepc := range metadata.Bootstrap {
				if bootstrapSepc.Type != product.BootstrapTypeFile {
					return errors.New("Unknown type")
				}
				dname, fname := path.Split(bootstrapSepc.Path)
				if fname == "" {
					return errors.New("Not a file")
				}
				dirs, err := b.headersForDirectory(path.Join(svc, dname))
				if err != nil {
					return err
				}
				for _, d := range dirs {

					if allDirsSet[d.Name] == nil {
						allDirsSet[d.Name] = d
						allDirs = append(allDirs, d)
					}
				}
				fpath := path.Join(svc, bootstrapSepc.Path)
				f, err := b.headerForFile(fpath)
				if err != nil {
					return err
				}
				files[fpath] = f
			}
		}
	}

	tarWriter := tar.NewWriter(out)
	for _, hdr := range allDirs {
		if err := tarWriter.WriteHeader(hdr); err != nil {
			return err
		}
	}

	for fpath, hdr := range files {
		if err := tarWriter.WriteHeader(hdr); err != nil {
			return err
		}
		if err := b.copyFile(tarWriter, fpath); err != nil {
			return err
		}
		tarWriter.Flush()
	}

	return tarWriter.Close()
}

func (b *BundleCreator) copyFile(tarWriter *tar.Writer, fpath string) error {
	absPath := path.Join(b.rootDir, fpath)
	f, err := os.Open(absPath)
	if err != nil {
		return err
	}
	defer f.Close()
	if _, err := io.Copy(tarWriter, f); err != nil {
		return err
	}
	return nil
}

func (b *BundleCreator) headerForFile(relPath string) (*tar.Header, error) {
	absPath := path.Join(b.rootDir, relPath)
	fileInfo, err := os.Stat(absPath)
	if err != nil {
		return nil, err
	}

	if fileInfo.Mode().IsRegular() || fileInfo.Mode().IsDir() {
		header, err := tar.FileInfoHeader(fileInfo, "")
		if err != nil {
			return nil, err
		}

		header.Name = relPath

		switch header.Uname {
		case habUname:
			header.Uid = habUserID
		case rootUname:
			header.Uid = rootUserID
		default:
			return nil, errors.Errorf("%q owner is not root or hab", absPath)
		}

		switch header.Gname {
		case habGname:
			header.Gid = habGroupID
		case rootGname:
			header.Gid = rootGroupID
		default:
			return nil, errors.Errorf("%q group is not root or hab", absPath)
		}

		return header, nil
	}

	return nil, errors.New("unsupported file type")
}

func (b *BundleCreator) headersForDirectory(dname string) ([]*tar.Header, error) {
	headers := []*tar.Header{}
	parts := strings.Split(dname, "/")
	for i := range parts {
		header, err := b.headerForFile(path.Join(parts[0 : i+1]...))
		if err != nil {
			return nil, err
		}
		headers = append(headers, header)
	}
	return headers, nil
}
