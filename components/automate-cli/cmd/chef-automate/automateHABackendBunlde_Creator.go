package main

import (
	"archive/tar"
	"io"
	"log"
	"os"
	"path/filepath"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
)

func createBackendBundleTar(requiredBackendPackages []string) error {
	log.Println("entry 1")
	log.Printf("---Required Backend Packages----- : %s", requiredBackendPackages)

	err := fileutils.AtomicWriter("automate-4.2.22.aib", func(w io.Writer) error {
		log.Println("entry 3")
		tarWriter := tar.NewWriter(w)
		log.Println("entry 4")

		for _, path := range requiredBackendPackages {
			log.Println("entry 6")
			var info os.FileInfo
			log.Println("entry 7")
			header, err := tar.FileInfoHeader(info, path)
			log.Println("entry 8")
			if err != nil {
				log.Println("entry 9")
				return status.Wrap(err, status.FileAccessError, "Failed to create archive file header")
			}
			log.Println("entry 10")
			p, err := filepath.Rel("/hab/cache/artifacts/", path)
			log.Println("entry 11")
			if err != nil {
				log.Println("entry 12")
				return status.Wrap(err, status.FileAccessError, "Failed to get base archive path")
			}
			log.Println("entry 13")
			header.Name = p
			log.Println("entry 14")
			// write the header
			if err := tarWriter.WriteHeader(header); err != nil {
				log.Println("entry 15")
				return status.Wrap(err, status.FileAccessError, "Failed to write archive file header")
			}
			log.Println("entry 16")
			if info.IsDir() {
				log.Println("entry 17")
				return nil
			}
			log.Println("entry 18")
			file, err := os.Open(path)
			log.Println("entry 19")
			if err != nil {
				log.Println("entry 20")
				return status.Wrap(err, status.FileAccessError, "Failed to open archive file")
			}
			log.Println("entry 21")
			defer file.Close() // nolint errcheck

			// copy file data into tar writer
			log.Println("entry 22")
			if _, err := io.Copy(tarWriter, file); err != nil {
				log.Println("entry 23")
				return status.Wrap(err, status.FileAccessError, "Failed to write archive file")
			}
			log.Println("entry 24")
			// manually close here after each file operation; defering would cause each file close
			// to wait until all operations have completed.
			file.Close()
			log.Println("entry 25")
		}
		log.Println("entry 26")
		if err := tarWriter.Close(); err != nil {
			log.Println("entry 27")
			return status.Wrap(err, status.FileAccessError, "Failed to close archive")
		}
		log.Println("entry 28")
		return nil
	})
	log.Println("entry 29")
	if err != nil {
		log.Println("entry 30")
		return errors.Wrap(err, "Could not write archive")
	}
	log.Println("entry 31")
	return nil
}
