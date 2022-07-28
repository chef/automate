package main

import (
	"archive/tar"
	"io"
	"log"
	"os"
	"path/filepath"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
)

func createBackendBundleTar(requiredBackendPackages []string) error {
	log.Println("entry 1")

	var airgapBundlePath string
	var airgapMetadata airgap.UnpackMetadata

	metadata, _ := airgap.Unpack(airgapBundlePath)
	log.Printf("....---Metadata---..... : %s", metadata)

	// if err != nil {
	// 	return status.Annotate(err, status.AirgapUnpackInstallBundleError)
	// }

	airgapMetadata = metadata
	log.Printf("...entry 2.....metadata: %s\n", airgapMetadata)

	requiredBackendPackages = backendBundles(airgapMetadata)
	log.Printf(".....----req packages---...: %s\n", requiredBackendPackages)

	err := fileutils.AtomicWriter("automate-4.2.22.aib", func(w io.Writer) error {
		log.Println("entry 3")
		tarWriter := tar.NewWriter(w)
		log.Println("entry 4")

		// var airgapMetadata airgap.UnpackMetadata
		// log.Printf("...entry 4.....metadata: %s\n", airgapMetadata)
		// requiredBackendPackages = backendBundles(airgapMetadata)
		// log.Printf(".....req packages...: %s\n", requiredBackendPackages)

		if len(requiredBackendPackages) < 1 {
			log.Println("Bundle list is empty")
		}
		
		log.Println("entry 5")
		for _, path := range requiredBackendPackages {
			log.Println("entry 6")
			var info os.FileInfo
			log.Println("entry 7")
			header, err := tar.FileInfoHeader(info, info.Name())
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
