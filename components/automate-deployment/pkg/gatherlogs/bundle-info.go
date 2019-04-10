package gatherlogs

import (
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"path"
)

// BundleInfo is a type for a bundle's information
type BundleInfo struct {
	Checksum string
	Name     string
	Path     string
	Size     int64
}

// NewBundleInfo extrapolates information about bundle from its path
func NewBundleInfo(bundlePath string) (BundleInfo, error) {
	file, err := os.Open(bundlePath)
	if err != nil {
		return BundleInfo{}, err
	}

	fileInfo, err := file.Stat()
	if err != nil {
		return BundleInfo{}, err
	}

	checksum, err := bundleChecksum(bundlePath)
	if err != nil {
		return BundleInfo{}, err
	}

	return BundleInfo{
		Checksum: checksum,
		Name:     path.Base(bundlePath),
		Path:     bundlePath,
		Size:     fileInfo.Size(),
	}, nil
}

// BundleChecksum returns the SHA256 Checksum for the bundle
func bundleChecksum(path string) (string, error) {
	bufferSize := 262144

	buffer := make([]byte, bufferSize)

	file, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer file.Close()

	hash := sha256.New()
	if _, err := io.CopyBuffer(hash, file, buffer); err != nil {
		return "", err
	}

	return fmt.Sprintf("%x", hash.Sum(nil)), nil
}
