package server

import (
	"archive/tar"
	"compress/gzip"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
	"github.com/chef/automate/lib/io/chunks"
	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"
)

type bootstrapFile struct {
	Filename string `json:"filename"`
	Path     string `json:"path"`
}

type serviceForExport struct {
	Service        string          `json:"service"`
	BootstrapFiles []bootstrapFile `json:"bootstrap_files"`
}

type servicesForExport struct {
	Services []serviceForExport `json:"services"`
}

//
// We want to be able to gather up files installed on one node and package them up so they can be made available when installing a second node.
// Then we want to unpackage them and put them in the right place, with the right ownership.
// LATER: We need to make sure a new version of the package gets generated when a file in that thing changes, but that is beyond
// current scope.
// PUT IT ELSEWHERE: For now I'm going to stuff the list of files in here, but we should probably put it elsewhere.
// Also, we probably need to do it per service, which means we need to know who the running services in the installation are.
//

// BootstrapBundleCreate makes a bootstrap bundle
func (s *server) BootstrapBundle(req *api.BootstrapBundleRequest, stream api.Deployment_BootstrapBundleServer) error {
	collection := req.CollectionName
	logrus.Infof("COLLECTION NAME is %s\n", collection)

	// TODO: Check the collection is actually deployed.

	// Create (and clean up) a staging area to copy files to
	stagingDir := stagingDir(s.serverConfig)
	logrus.Infof("STAGING DIR is %s\n", stagingDir)
	archiveRoot, err := createTempDir(stagingDir, "bootstrap_bundle")
	if err != nil {
		return err
	}
	logrus.Infof("ARCHIVE ROOT is %s\n", archiveRoot)
	defer os.RemoveAll(archiveRoot)

	// Jay will be providing a file where I can put package metadata; exportables should probably go in there.
	// For now, we have a variable that lists exportables.
	// (Yes, but not sure how yet.): Do you want to keep track of filepath and permissions?

	var svcsForExport servicesForExport
	err = json.Unmarshal([]byte(servicesJSON), &svcsForExport)
	svcsInCollection, err := services.ServicesInCollection(collection)
	logrus.Infof("SVCSINCOLLECTION is %+v", svcsInCollection)
	if err != nil {
		return err
	}
	for _, svc := range svcsInCollection {
		svcName := svc.Name()
		logrus.Infof("SVCNAME is %s\n", svcName)
		for _, s := range svcsForExport.Services {
			if svcName == s.Service {
				logrus.Infof("MATCHED ON service name: %s\n", svcName)
				for _, b := range s.BootstrapFiles {

					from, err := os.Open(b.Path)
					if err != nil {
						panic(err)
					}
					defer from.Close()

					target := filepath.Join(archiveRoot, b.Filename)
					to, err := os.OpenFile(target, os.O_RDWR|os.O_CREATE, 0666)
					if err != nil {
						panic(err)
					}
					defer to.Close()

					_, err = io.Copy(to, from)
					if err != nil {
						panic(err)
					}
					logrus.Infof("COPIED FILE: %s\n", target)
				}
			} else {
				continue
			}
		}
	}

	tgzFilepath := filepath.Join(stagingDir, "bootstrap-bundle.tgz")
	fmt.Printf("TARBALL FILEPATH: %s\n", tgzFilepath)
	tarAndGzipDirectory(archiveRoot, tgzFilepath)

	// tgzInfo, err := os.Stat(tgzFilepath)
	// if err != nil {
	// 	return err
	// }

	// Is there a way to compute the checksum as we go instead of opening the file again? (not sure it matters, though)
	//f, err := os.Open(tgzFilepath)
	//if err != nil {
	//	return err
	//}
	//defer f.Close()

	// hasher := sha256.New()
	//if _, err := io.Copy(hasher, f); err != nil {
	//	return err
	//}
	// cksum := hex.EncodeToString(hasher.Sum(nil))

	// This is a mistake
	// bundlePath := path.Join(stagingDir, tgzFilepath)

	defer func() {
		err := os.Remove(tgzFilepath)
		if err != nil {
			log.WithError(err).Warn("Failed to remove bootstrap bundle file.")
		}
	}()

	file, err := os.Open(tgzFilepath)
	if err != nil {
		return err
	}
	defer file.Close()

	buffer := make([]byte, defaultChunkSize)
	writer := chunks.NewWriter(defaultChunkSize, func(p []byte) error {
		return stream.Send(&api.BootstrapBundleResponse{Data: p})
	})
	_, err = io.CopyBuffer(writer, file, buffer)
	return err
}

func tarAndGzipDirectory(inDir string, tgzFilepath string) error {
	fmt.Println("MAKING THE TARBALL")
	f, _ := os.Create(tgzFilepath)

	zw := gzip.NewWriter(f)
	defer zw.Close()

	tw := tar.NewWriter(zw)
	defer tw.Close()

	// I need to self-review this more carefully.
	return filepath.Walk(inDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		header, err := tar.FileInfoHeader(info, info.Name())
		if err != nil {
			return err
		}

		header.Name = filepath.Join(inDir, strings.TrimPrefix(path, inDir))

		if err := tw.WriteHeader(header); err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		file, err := os.Open(path)
		if err != nil {
			return err
		}

		if _, err = io.Copy(tw, file); err != nil {
			return err
		}

		// close each file as you deal with it
		file.Close()
		return nil
	})

}

// TODO: move to products.json.
// Not guaranteed to be complete/correct right now. The idea is to export *something*.
const servicesJSON = `
{
  "services": [
	{
      "service": "automate-cs-oc-erchef",
      "bootstrap_files": [
		  {
			"filename": "pivotal.pem",
			"path": "/hab/svc/automate-cs-oc-erchef/data/pivotal.pem"
		  },
		  {
			"filename": "pivotal.pub.pem",
		  	"path": "/hab/svc/automate-cs-oc-erchef/data/pivotal.pub.pem"
		  },
		  {
			"filename": "webui_priv.pem",
			"path": "/hab/svc/automate-cs-oc-erchef/data/webui_priv.pem"
		  },
		  {
			"filename": "webui_pub.pem",
			"path": "/hab/svc/automate-cs-oc-erchef/data/webui_pub.pem"
		  },
		  {
			"filename": "dark_launch_features.json",
			"path": "/hab/svc/automate-cs-oc-erchef/data/dark_launch_features.json"
		  }
        ]
    }
  ]
}
`
