package manifest

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
)

const (
	automateVersionsURLFmt = "https://packages.chef.io/manifests/%s/automate/versions.json"
)

// A ReleaseManifestProvider is something that can return a manifest
type ReleaseManifestProvider interface {
	GetCurrentManifest(ctx context.Context, channel string) (*A2, error)
	// GetManifest forces fetching the specified manifest
	GetManifest(ctx context.Context, release string) (*A2, error)
}

// A LocalHartManifestProvider can add local harts to the manifest for another
// manifest provider.
type LocalHartManifestProvider struct {
	baseProvider   ReleaseManifestProvider
	hartPath       string
	overrideOrigin string
}

// CachingReleaseManifestProvider caches the manifest entry for a configured amount of time.
// A refresh can be forced with RefreshManifest
type CachingReleaseManifestProvider interface {
	ReleaseManifestProvider
	// RefreshManifest forces fetching a new manifest from the given channel
	RefreshManifest(ctx context.Context, channel string) (*A2, error)

	//GetCompatibleVersion fetches the next compatible version for the given version
	GetCompatibleVersion(ctx context.Context, channel, version string) (isMinorAvailable, isMajorAvailable bool,
		compVersion string, err error)
}

type cacheEntry struct {
	nowProvider func() time.Time
	validFor    time.Duration

	lastCheck      time.Time
	cachedManifest *A2
}
type cachingReleaseManifestProvider struct {
	// cache maps a channel to a cacheEntry
	cache        map[string]cacheEntry
	baseProvider ReleaseManifestProvider
	cacheTime    time.Duration
	nowProvider  func() time.Time
}

// NewCachingReleaseManifestProvider creates a caching ReleaseManifestProvider
func NewCachingReleaseManifestProvider(baseProvider ReleaseManifestProvider, cacheTime time.Duration) CachingReleaseManifestProvider {
	return newCachingReleaseManifestProvider(baseProvider, cacheTime)
}

func newCachingReleaseManifestProvider(baseProvider ReleaseManifestProvider, cacheTime time.Duration) *cachingReleaseManifestProvider {
	return &cachingReleaseManifestProvider{
		cache:        make(map[string]cacheEntry),
		baseProvider: baseProvider,
		cacheTime:    cacheTime,
		nowProvider:  time.Now,
	}
}

func (c *cachingReleaseManifestProvider) GetCurrentManifest(ctx context.Context, channel string) (*A2, error) {
	entry, ok := c.cache[channel]
	logctx := logrus.WithFields(
		logrus.Fields{
			"found":      ok,
			"last_check": entry.lastCheck,
		},
	)
	if !ok || entry.expired() {
		logctx.Debug("Refreshing expired manifest")
		return c.RefreshManifest(ctx, channel)
	}
	logctx.Debug("Using cached manifest")
	return entry.cachedManifest, nil
}

func (c *cachingReleaseManifestProvider) GetManifest(ctx context.Context, release string) (*A2, error) {
	m, err := c.baseProvider.GetManifest(ctx, release)
	if err == nil {
		// Clear the cache to make sure that
		// GetCurrentManifest won't return an older version.  Maybe unnecessary?
		c.cache = make(map[string]cacheEntry)
	}

	return m, err
}

func (c *cachingReleaseManifestProvider) RefreshManifest(ctx context.Context, channel string) (*A2, error) {
	logrus.Info("Refreshing manifest")
	m, err := c.baseProvider.GetCurrentManifest(ctx, channel)

	if err != nil {
		return nil, err
	}

	// Todo(milestone) -- Add a check if the next version is a major version.

	c.cache[channel] = cacheEntry{
		lastCheck:      c.nowProvider(),
		cachedManifest: m,
		nowProvider:    c.nowProvider,
		validFor:       c.cacheTime,
	}

	return m, nil
}

func (c cacheEntry) expired() bool {
	expiry := c.lastCheck.Add(c.validFor)
	return c.nowProvider().After(expiry)
}

// NewLocalHartManifestProvider creates a LocalHartManifestProvider
func NewLocalHartManifestProvider(baseProvider ReleaseManifestProvider, hartPath string, overrideOrigin string) ReleaseManifestProvider {
	return &LocalHartManifestProvider{
		baseProvider:   baseProvider,
		hartPath:       hartPath,
		overrideOrigin: overrideOrigin,
	}
}

// GetCurrentManifest returns a release manifest
func (provider *LocalHartManifestProvider) GetCurrentManifest(ctx context.Context, channel string) (*A2, error) {
	baseManifest, err := provider.baseProvider.GetCurrentManifest(ctx, channel)
	if err != nil {
		return nil, err
	}

	return provider.overlayLocalHarts(baseManifest)
}

func (provider *LocalHartManifestProvider) GetManifest(ctx context.Context, release string) (*A2, error) {
	baseManifest, err := provider.baseProvider.GetManifest(ctx, release)
	if err != nil {
		return nil, err
	}

	return provider.overlayLocalHarts(baseManifest)
}

func (provider *LocalHartManifestProvider) overlayLocalHarts(baseManifest *A2) (*A2, error) {
	if provider.hartPath != "" && provider.overrideOrigin != "" {
		hartDir := habpkg.NewHartDir(provider.hartPath)
		checked := make(map[string]bool)

		internalServiceNames, err := services.AllServices()
		if err != nil {
			return nil, err
		}

		for _, source := range [][]habpkg.HabPkg{baseManifest.Packages, internalServiceNames} {
			for _, pkg := range source {
				if checked[pkg.Name()] {
					continue
				}
				checked[pkg.Name()] = true
				hart, err := hartDir.FindHart(provider.overrideOrigin, pkg.Name())
				if err != nil {
					return nil, err
				}

				if hart != nil {
					baseManifest.HartOverrides = append(baseManifest.HartOverrides, *hart)
				}
			}
		}
	}

	return baseManifest, nil
}

func (c *cachingReleaseManifestProvider) GetCompatibleVersion(ctx context.Context, channel,
	version string) (isMinorAvailable, isMajorAvailable bool, compVersion string, err error) {
	//return c.baseProvider.GetCompatibleVersion(ctx, channel, version)
	url := fmt.Sprintf(automateVersionsURLFmt, channel)
	return getCompatibleManifestVersion(ctx, version, url)
}

func getCompatibleManifestVersion(ctx context.Context, version, url string) (isMinorAvailable, isMajorAvailable bool, compVersion string, err error) {
	//get the list of all versions
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return false, false, "", errors.Wrap(err, "error in preparing the request to get the list of available versions")
	}
	req = req.WithContext(ctx)
	httpClient := &http.Client{}
	response, err := httpClient.Do(req)
	if err != nil {
		return false, false, "", errors.Wrap(err, fmt.Sprintf("error in invoking the endpoint %s", url))
	}
	defer response.Body.Close() // nolint: errcheck

	if response.StatusCode != http.StatusOK {
		return false, false, "", errors.Errorf("Unexpected HTTP response from %s: %s", url, response.Status)
	}

	body, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return false, false, "", errors.Wrap(err, fmt.Sprintf("error in reading the response of the endpoint %s", url))
	}

	var allVersions []string
	err = json.Unmarshal(body, &allVersions)
	if err != nil {
		return false, false, "", errors.Wrap(err, fmt.Sprintf("error in converting the response of the endpoint %s to slice", url))
	}

	currentVersionIndex := len(allVersions)
	for i := len(allVersions) - 1; i >= 0; i-- {
		if allVersions[i] == version {
			currentVersionIndex = i
			break
		}
	}

	if currentVersionIndex == len(allVersions) {
		return false, false, "", fmt.Errorf("invalid version, given version %s is not available in valid list of versions from %s", version, url)
	}

	//reached the end, no further updates are available
	if currentVersionIndex == len(allVersions)-1 {
		return false, false, version, nil
	}

	//trim the slice, as we doesn't required the previous releases
	allVersions = allVersions[currentVersionIndex:]

	//get the minor/patch versions are available or not.
	//check the current version is timestamp or semantic version
	currentMajor, isSemVersion := isSemVersionFmt(version)

	if !isSemVersion {
		//check for any minor
		index, timeStampVersion := findLatestTimeStampVersion(allVersions)
		if index != 0 {
			return true, false, timeStampVersion, nil
		}
		_, nextMajorVersion := findNextMajorVersionForTimeStamp(allVersions)
		return false, true, nextMajorVersion, nil
	} else {
		index, patchVersion := findPatchVersionForSemantic(currentMajor, allVersions)
		if index != 0 {
			return true, false, patchVersion, nil
		}
		_, nextMajorVersion := findNextMajorVersionForSemantic(currentMajor, allVersions)
		return false, true, nextMajorVersion, nil
	}
}

//isSemVersionFmt checks the provided version is in semantic version format, if yes, will return the major version
func isSemVersionFmt(version string) (string, bool) {
	splitStrings := strings.Split(version, ".")
	if len(splitStrings) > 1 {
		return splitStrings[0], true
	}
	return "", false
}

func findLatestTimeStampVersion(list []string) (index int, version string) {
	for index, item := range list {
		if _, isSem := isSemVersionFmt(item); isSem {
			return index - 1, list[index-1]
		} else if index == len(list)-1 { //reached to end
			return index, list[index]
		}
	}
	//ideally, this should not execute
	return 0, list[0]
}

func findNextMajorVersionForTimeStamp(list []string) (int, string) {
	majorVersion := ""
	for index, item := range list {
		if major, isSem := isSemVersionFmt(item); isSem {
			if majorVersion == "" {
				majorVersion = major
			} else if majorVersion != major {
				return index - 1, list[index-1]
			}
			if index == len(list)-1 && majorVersion != "" {
				return index, list[index]
			}
		}
	}
	return 0, ""
}

func findPatchVersionForSemantic(currentMajor string, list []string) (int, string) {
	for index, item := range list {
		if major, _ := isSemVersionFmt(item); currentMajor != major {
			return index - 1, list[index-1]
		} else if index == len(list)-1 { //reached to end
			return index, list[index]
		}
	}
	//ideally, this should not execute
	return 0, list[0]
}

func findNextMajorVersionForSemantic(currentMajor string, list []string) (int, string) {
	nextMajorVersion := ""
	for index, item := range list {
		if major, _ := isSemVersionFmt(item); currentMajor != major {
			if nextMajorVersion == "" {
				nextMajorVersion = major
			} else if nextMajorVersion != major {
				return index - 1, list[index-1]
			}
			if nextMajorVersion != "" && index == len(list)-1 {
				return index, list[index]
			}
		}
	}
	return 0, ""
}
