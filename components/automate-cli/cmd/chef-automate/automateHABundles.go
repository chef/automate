package main

import (
	"bufio"
	"crypto/md5"
	"encoding/hex"
	"io"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"strings"

	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/pkg/errors"
)

func moveFrontendBackendAirgapToTransferDir(airgapMetadata airgap.UnpackMetadata, airgapBundle string) error {
	if len(airgapBundle) > 0 {
		bundleName, err := getFrontendBundleName(airgapBundle)
		if err != nil {
			return err
		}
		err = generateFrontendBundles(bundleName, airgapBundle)
		if err != nil {
			return err
		}
		backendBundleName, err := getBackendBundleName(airgapBundle)
		if err != nil {
			return err
		}
		backendBundleFilePath := path.Join(AIRGAP_HA_TRANS_DIR_PATH, backendBundleName)
		err = generateMiniBackendBundles(airgapMetadata, backendBundleFilePath)
		if err != nil {
			return err
		}
		/* err = generateBackendBundles(bundleName, airgapBundle)
		if err != nil {
			return err
		} */
		//generate manifest auto tfvars
		err = generateA2HAManifestTfvars(airgapMetadata)
		if err != nil {
			return err
		}
	}
	return nil
}

func moveAirgapFrontendBundlesOnlyToTransferDir(airgapMetadata airgap.UnpackMetadata, airgapBundle string) error {
	if len(airgapBundle) > 0 {
		bundleName, err := getFrontendBundleName(airgapBundle)
		if err != nil {
			return err
		}
		err = generateFrontendBundles(bundleName, airgapBundle)
		if err != nil {
			return err
		}
	}
	return nil
}

func moveAirgapBackendBundlesOnlyToTransferDir(airgapMetadata airgap.UnpackMetadata, airgapBundle string) error {
	if len(airgapBundle) > 0 {
		/* bundleName, err := getFrontendBundleName(airgapBundle)
		if err != nil {
			return err
		} */
		/* err = generateBackendBundles(bundleName, airgapBundle)
		if err != nil {
			return err
		} */
		bundleName, err := getBackendBundleName(airgapBundle)
		if err != nil {
			return err
		}
		backendBundleFilePath := path.Join(AIRGAP_HA_TRANS_DIR_PATH, bundleName)
		err = generateMiniBackendBundles(airgapMetadata, backendBundleFilePath)
		if err != nil {
			return err
		}
		//generate manifest auto tfvars
		err = generateA2HAManifestTfvars(airgapMetadata)
		if err != nil {
			return err
		}
	}
	return nil
}

func getFrontendBundleName(airgapPath string) (string, error) {
	version, err := getVersion(airgapPath)
	if err != nil {
		return "", err
	}
	return "frontend-" + version + ".aib", nil
}
func getBackendBundleName(airgapPath string) (string, error) {
	version, err := getVersion(airgapPath)
	if err != nil {
		return "", err
	}
	return "backend-" + version + ".aib", nil
}
func generateFrontendBundles(bundleName string, airgapPath string) error {
	err := copyFileContents(airgapPath, (AIRGAP_HA_TRANS_DIR_PATH + bundleName))
	if err != nil {
		return err
	}
	//generating md5 sum for frontend bundle
	err = generateChecksumFile(AIRGAP_HA_TRANS_DIR_PATH+bundleName, AIRGAP_HA_TRANS_DIR_PATH+bundleName+".md5")
	if err != nil {
		return err
	}
	//generating frontend auto tfvars
	frontendTfvars := getBytesFromTempalte("frontendTfvars", frontendAutotfvarsTemplate, map[string]interface{}{
		"bundleName": bundleName,
	})
	err = ioutil.WriteFile(AUTOMATE_HA_TERRAFORM_DIR+"a2ha_aib_fe.auto.tfvars", frontendTfvars, AUTOMATE_HA_FILE_PERMISSION_0755) // nosemgrep
	if err != nil {
		return err
	}
	return nil
}

func generateBackendBundles(bundleName string, airgapPath string) error {
	//generating backend bundle
	backendBundleFile := AIRGAP_HA_TRANS_DIR_PATH + (strings.ReplaceAll(bundleName, "frontend", "backend"))
	err := generateBackendAIB(airgapPath, backendBundleFile)
	if err != nil {
		return err
	}
	err = generateChecksumFile(backendBundleFile, backendBundleFile+".md5")
	if err != nil {
		return err
	}
	//generating backend auto tfvars
	backendTfvars := getBytesFromTempalte("backendTfvars", backendAutotfvarsTemplate, map[string]interface{}{
		"backendBundleFile": filepath.Base(backendBundleFile),
	})
	err = ioutil.WriteFile(AUTOMATE_HA_TERRAFORM_DIR+"a2ha_aib_be.auto.tfvars", backendTfvars, AUTOMATE_HA_FILE_PERMISSION_0755) // nosemgrep
	if err != nil {
		return err
	}
	return nil
}

func generateA2HAManifestTfvars(airgapMetadata airgap.UnpackMetadata) error {
	var deployablePackages []string
	for _, h := range airgapMetadata.HartifactPaths {
		if strings.Contains(h, AUTOMATE_HA_PKG_PG_LDR_CHK) {
			deployablePackages = append(deployablePackages, "pgleaderchk_pkg_ident = \""+getBldrSupportedPkgName(h)+"\"")
		}
		if strings.Contains(h, AUTOMATE_HA_PKG_PG) {
			deployablePackages = append(deployablePackages, "postgresql_pkg_ident = \""+getBldrSupportedPkgName(h)+"\"")
		}
		if strings.Contains(h, AUTOMATE_HA_PKG_HA_PROXY) {
			deployablePackages = append(deployablePackages, "proxy_pkg_ident = \""+getBldrSupportedPkgName(h)+"\"")
		}
		if strings.Contains(h, AUTOMATE_HA_OS) {
			deployablePackages = append(deployablePackages, "opensearch_pkg_ident = \""+getBldrSupportedPkgName(h)+"\"")
		}
		if strings.Contains(h, AUTOMATE_HA_ES_CAR) {
			deployablePackages = append(deployablePackages, "elasticsidecar_pkg_ident = \""+getBldrSupportedPkgName(h)+"\"")
		}
	}
	return ioutil.WriteFile(AUTOMATE_HA_TERRAFORM_DIR+"a2ha_manifest.auto.tfvars", []byte(strings.Join(deployablePackages[:], "\n")), AUTOMATE_HA_FILE_PERMISSION_0755) // nosemgrep
}

func generateBackendAIB(filePath string, destPath string) error {
	installBundleFile, err := os.Open(filePath)
	if err != nil {
		return errors.New(err.Error() + " Failed to open install bundle file " + filePath)
	}
	defer installBundleFile.Close() // nolint: errcheck

	bufReader := bufio.NewReader(installBundleFile)
	// Read the header:
	// AIB-1\n\n
	s, err := bufReader.ReadString('\n')
	if err != nil {
		return errors.New(err.Error() + " Could not read artifact file")
	}
	if s != "AIB-1\n" {
		return errors.New("malformed install bundle file")
	}

	s, err = bufReader.ReadString('\n')
	if err != nil {
		return err
	}

	if s != "\n" {
		return errors.New("malformed install bundle file")
	}

	fo, err := os.Create(destPath)
	if err != nil {
		panic(err)
	}
	defer func() {
		if err := fo.Close(); err != nil {
			panic(err)
		}
	}()

	bufWriter := bufio.NewWriter(fo)
	buf := make([]byte, 1024)
	for {
		n, err := bufReader.Read(buf)
		if err != nil && err != io.EOF {
			panic(err)
		}
		if n == 0 {
			break
		}
		if _, err := bufWriter.Write(buf[:n]); err != nil {
			panic(err)
		}
	}

	if err = bufWriter.Flush(); err != nil {
		panic(err)
	}

	return nil
}

func generateChecksumFile(sourceFileName string, checksumFileName string) error {
	sfile, err := os.Open(sourceFileName)
	if err != nil {
		return err
	}
	defer sfile.Close()
	hash := md5.New() // nosemgrep
	if _, err := io.Copy(hash, sfile); err != nil {
		return err
	}
	sum := hash.Sum(nil)
	hashedFileContent := hex.EncodeToString(sum) + "  " + filepath.Base(sfile.Name())
	err = ioutil.WriteFile(checksumFileName, []byte(hashedFileContent), AUTOMATE_HA_FILE_PERMISSION_0644) // nosemgrep
	if err != nil {
		return err
	}
	return nil
}
