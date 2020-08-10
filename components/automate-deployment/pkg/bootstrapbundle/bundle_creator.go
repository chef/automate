package bootstrapbundle

import (
	"archive/tar"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"strconv"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/product"
	"github.com/chef/automate/lib/secrets"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/automate/lib/user"
)

const magicHeader = "ABB-1\n\n"
const habSvcDir = "/hab/svc"
const secretDirName = "__secrets"

var ErrNoFiles = errors.New("No files to bundle")
var ErrMalformedBundle = errors.New("Malformed install bundle file")

type CreatorOpt func(*Creator)

func WithRootDir(rootDir string) CreatorOpt {
	return func(b *Creator) {
		if rootDir != "" {
			b.rootDir = rootDir
		}
	}
}

// Creator creates and unpacks bootstrap bundles
type Creator struct {
	rootDir       string
	allowedUsers  []string
	allowedGroups []string
	secretStore   secrets.SecretStore
}

func NewCreator(secretStore secrets.SecretStore, opts ...CreatorOpt) *Creator {
	b := &Creator{
		rootDir:       habSvcDir,
		allowedUsers:  []string{"root", "hab"},
		allowedGroups: []string{"root", "hab"},
		secretStore:   secretStore,
	}
	for _, f := range opts {
		f(b)
	}
	return b
}

func (b *Creator) writeFile(tarReader *tar.Reader, hdr *tar.Header) error {
	absPath := path.Join(b.rootDir, hdr.Name)
	err := fileutils.AtomicWrite(
		absPath,
		tarReader,
		fileutils.WithAtomicWriteFileMode(hdr.FileInfo().Mode()),
		fileutils.WithAtomicWriteChown(hdr.Uid, hdr.Gid),
		fileutils.WithAtomicWriteChmod(hdr.FileInfo().Mode()),
	)
	if err != nil {
		return err
	}
	return nil
}

func (b *Creator) mkdir(tarReader *tar.Reader, hdr *tar.Header) error {
	absPath := path.Join(b.rootDir, hdr.Name)
	// TODO (jaym): make atomic?
	// This is not MkdirAll because we don't want to create the root directory.
	// Our bundle does not keep track of the correct ownership and permissions
	// of it.
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

func (b *Creator) readHeader(in io.Reader) error {
	header := make([]byte, len(magicHeader))
	if _, err := io.ReadFull(in, header); err != nil {
		return errors.Wrap(err, "failed to read bootstrap bundle header")
	}

	if string(header) != magicHeader {
		return ErrMalformedBundle
	}

	return nil
}

func (b *Creator) Unpack(in io.Reader) error {
	if err := b.readHeader(in); err != nil {
		return err
	}
	tarReader := tar.NewReader(in)

	users := map[string]int{}
	groups := map[string]int{}

	for _, uname := range b.allowedUsers {
		u, err := user.Lookup(uname)
		if err != nil {
			return errors.Wrapf(err, "Expected user %q to exist", uname)
		}
		uid, err := strconv.Atoi(u.Uid)
		if err != nil {
			return err
		}
		users[uname] = uid
	}

	for _, gname := range b.allowedGroups {
		g, err := user.LookupGroup(gname)
		if err != nil {
			return errors.Wrapf(err, "Expected group %q to exist", gname)
		}
		gid, err := strconv.Atoi(g.Gid)
		if err != nil {
			return err
		}
		groups[gname] = gid
	}

	exists, err := fileutils.PathExists(b.rootDir)
	if err != nil {
		return err
	}
	if !exists {
		return errors.Errorf("cannot unpack into %q because it does not exist", b.rootDir)
	}
	for {
		hdr, err := tarReader.Next()
		if err != nil {
			if err == io.EOF {
				break
			}
		}

		if b.isSecret(hdr) {
			secretName, secretValue, err := b.extractSecret(hdr, tarReader)
			if err != nil {
				return err
			}
			if err := b.secretStore.SetSecret(secretName, secretValue); err != nil {
				return err
			}
			continue
		}

		if uid, found := users[hdr.Uname]; found {
			hdr.Uid = uid
		} else {
			return errors.Errorf("Unknown user %q", hdr.Uname)
		}

		if gid, found := groups[hdr.Gname]; found {
			hdr.Gid = gid
		} else {
			return errors.Errorf("Unknown user %q", hdr.Uname)
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

func (b *Creator) Create(pkgMetadatas []*product.PackageMetadata, out io.Writer) error {
	allDirsSet := make(map[string]*tar.Header)
	allDirs := []*tar.Header{}
	files := make(map[string]*tar.Header)
	secretsData := map[*tar.Header][]byte{}

	for _, metadata := range pkgMetadatas {
		if metadata != nil && len(metadata.Bootstrap) > 0 {
			svc := metadata.Name.Name
			for _, bootstrapSpec := range metadata.Bootstrap {
				switch bootstrapSpec.Type {
				case product.BootstrapTypeFile:
					dname, fname := path.Split(bootstrapSpec.Path)
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
					fpath := path.Join(svc, bootstrapSpec.Path)
					f, err := b.headerForFile(fpath)
					if err != nil {
						return err
					}
					files[fpath] = f
				case product.BootstrapTypeSecret:
					secretName, err := secrets.SecretNameFromString(bootstrapSpec.SecretSpec)
					if err != nil {
						return err
					}
					secretValue, err := b.secretStore.GetSecret(secretName)
					if err != nil {
						return err
					}
					secretTarHeader := b.headerForSecrets(secretName)
					secretsData[secretTarHeader] = secretValue
				}
			}
		}
	}

	if len(allDirs)+len(files) <= 0 {
		return ErrNoFiles
	}

	if _, err := io.WriteString(out, magicHeader); err != nil {
		return errors.Wrap(err, "Could not write archive header")
	}

	tarWriter := tar.NewWriter(out)
	for _, hdr := range allDirs {
		if err := tarWriter.WriteHeader(hdr); err != nil {
			return err
		}
	}

	for hdr, value := range secretsData {
		hdr.Size = int64(len(value))
		if err := tarWriter.WriteHeader(hdr); err != nil {
			return err
		}

		if _, err := tarWriter.Write(value); err != nil {
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

func (b *Creator) copyFile(tarWriter *tar.Writer, fpath string) error {
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

func (b *Creator) headerForFile(relPath string) (*tar.Header, error) {
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

		// We need to make sure tar was able to figure out the user and group.
		// It will fail to do so if the user/group is not local. In that case,
		// we will use our own lookup mechanism
		if header.Uname == "" {
			u, err := user.LookupId(strconv.Itoa(header.Uid))
			if err != nil {
				return nil, errors.Errorf("could not determine user with id %d for %q", header.Uid, relPath)
			}
			header.Uname = u.Username
		}

		if header.Gname == "" {
			g, err := user.LookupGroupId(strconv.Itoa(header.Gid))
			if err != nil {
				return nil, errors.Errorf("could not determine group with id %d for %q", header.Gid, relPath)
			}
			header.Gname = g.Name
		}

		if uid, err := stringutils.IndexOf(b.allowedUsers, header.Uname); err != nil {
			return nil, errors.Errorf("%q is not an allowed user", header.Uname)
		} else {
			header.Uid = uid
		}

		if gid, err := stringutils.IndexOf(b.allowedGroups, header.Gname); err != nil {
			return nil, errors.Errorf("%q is not an allowed group", header.Gname)
		} else {
			header.Gid = gid
		}

		return header, nil
	}

	return nil, errors.New("unsupported file type")
}

func (b *Creator) headersForDirectory(dname string) ([]*tar.Header, error) {
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

func (b *Creator) headerForSecrets(secretName secrets.SecretName) *tar.Header {
	secretSpec := fmt.Sprintf("%s.%s", secretName.Group, secretName.Name)
	headerSecret := tar.Header{
		Name:     path.Join(secretDirName, secretSpec),
		Typeflag: tar.TypeReg,
		Uid:      0,
		Gid:      0,
		Uname:    "root",
		Gname:    "root",
	}

	return &headerSecret
}

func (b *Creator) isSecret(hdr *tar.Header) bool {
	dname, _ := path.Split(hdr.Name)
	return strings.TrimRight(dname, "/") == secretDirName
}

func (b *Creator) extractSecret(hdr *tar.Header, tarReader *tar.Reader) (secrets.SecretName, []byte, error) {
	_, fname := path.Split(hdr.Name)
	secretName, err := secrets.SecretNameFromString(fname)
	if err != nil {
		return secretName, nil, err
	}
	value, err := ioutil.ReadAll(tarReader)

	return secretName, value, err
}
