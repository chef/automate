//
//  Author:: Salim Afiune <afiune@chef.io>
//

package chef

import (
	"crypto/md5"
	"fmt"
	"io"
	"os"
	"path"
)

// Download downloads a cookbook to the current directory on disk
func (c *CookbookService) Download(name, version string) error {
	cwd, err := os.Getwd()
	if err != nil {
		return err
	}

	return c.DownloadTo(name, version, cwd)
}

// DownloadTo downloads a cookbook to the specified local directory on disk
func (c *CookbookService) DownloadTo(name, version, localDir string) error {
	// If the version is set to 'latest' or it is empty ("") then,
	// we will set the version to '_latest' which is the default endpoint
	if version == "" || version == "latest" {
		version = "_latest"
	}

	cookbook, err := c.GetVersion(name, version)
	if err != nil {
		return err
	}

	debug("Downloading %s cookbook version %s\n", cookbook.CookbookName, cookbook.Version)

	// We use 'cookbook.Name' since it returns the string '{NAME}-{VERSION}'. Ex: 'apache-0.1.0'
	cookbookPath := path.Join(localDir, cookbook.Name)

	downloadErrs := []error{
		c.downloadCookbookItems(cookbook.RootFiles, "root_files", cookbookPath),
		c.downloadCookbookItems(cookbook.Files, "files", path.Join(cookbookPath, "files")),
		c.downloadCookbookItems(cookbook.Templates, "templates", path.Join(cookbookPath, "templates")),
		c.downloadCookbookItems(cookbook.Attributes, "attributes", path.Join(cookbookPath, "attributes")),
		c.downloadCookbookItems(cookbook.Recipes, "recipes", path.Join(cookbookPath, "recipes")),
		c.downloadCookbookItems(cookbook.Definitions, "definitions", path.Join(cookbookPath, "definitions")),
		c.downloadCookbookItems(cookbook.Libraries, "libraries", path.Join(cookbookPath, "libraries")),
		c.downloadCookbookItems(cookbook.Providers, "providers", path.Join(cookbookPath, "providers")),
		c.downloadCookbookItems(cookbook.Resources, "resources", path.Join(cookbookPath, "resources")),
	}

	for _, err := range downloadErrs {
		if err != nil {
			return err
		}
	}

	debug("Cookbook downloaded to %s\n", cookbookPath)
	return nil
}

// downloadCookbookItems downloads all the provided cookbook items into the provided
// local path, it also ensures that the provided directory exists by creating it
func (c *CookbookService) downloadCookbookItems(items []CookbookItem, itemType, localPath string) error {
	if len(items) == 0 {
		return nil
	}

	debug("Downloading %s\n", itemType)
	if err := os.MkdirAll(localPath, 0755); err != nil {
		return err
	}

	for _, item := range items {
		if err := c.downloadCookbookFile(item, localPath); err != nil {
			return err
		}
	}

	return nil
}

// downloadCookbookFile downloads a single cookbook file to disk
func (c *CookbookService) downloadCookbookFile(item CookbookItem, localPath string) error {
	filePath := path.Join(localPath, item.Name)

	// First check and see if the file is already there - if it is and the checksum
	// matches, there's no need to redownload it.
	if verifyMD5Checksum(filePath, item.Checksum) {
		return nil
	}

	request, err := c.client.NewRequest("GET", item.Url, nil)
	if err != nil {
		return err
	}

	response, err := c.client.Do(request, nil)
	if response != nil {
		defer response.Body.Close()
	}
	if err != nil {
		return err
	}

	f, err := os.Create(filePath)
	if err != nil {
		return err
	}
	defer f.Close()

	if _, err := io.Copy(f, response.Body); err != nil {
		return err
	}

	if verifyMD5Checksum(filePath, item.Checksum) {
		return nil
	}

	return fmt.Errorf(
		"cookbook file '%s' checksum mismatch. (expected:%s)",
		filePath,
		item.Checksum,
	)
}

func verifyMD5Checksum(filePath, checksum string) bool {
	file, err := os.Open(filePath)
	if err != nil {
		return false
	}
	defer file.Close()

	hash := md5.New()
	if _, err := io.Copy(hash, file); err != nil {
		return false
	}

	md5String := fmt.Sprintf("%x", hash.Sum(nil))
	if md5String == checksum {
		return true
	}
	return false
}
