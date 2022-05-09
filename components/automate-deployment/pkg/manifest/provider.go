package manifest

import (
	"context"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"sort"
	"strconv"
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
	GetCompatibleVersion(ctx context.Context, channel, version, versionsPath string) (isMinorAvailable, isMajorAvailable bool,
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
	version, versionsPath string) (isMinorAvailable, isMajorAvailable bool, compVersion string, err error) {
	return getCompatibleManifestVersion(ctx, version, channel, versionsPath)
}

func getCompatibleManifestVersion(ctx context.Context, version, channel, versionsPath string, optionalURL ...string) (isMinorAvailable, isMajorAvailable bool, compVersion string, err error) {
	//get the list of all versions on ascending order
	allVersions, err := GetAllVersions(ctx, channel, versionsPath, optionalURL...)
	if err != nil {
		return false, false, "", errors.Wrap(err, fmt.Sprintf("error in getting the versions from %s channel", channel))
	}

	currentVersionIndex := len(allVersions)
	for i := len(allVersions) - 1; i >= 0; i-- {
		if allVersions[i] == version {
			currentVersionIndex = i
			break
		}
	}

	if currentVersionIndex == len(allVersions) {
		return false, false, "", fmt.Errorf("invalid version, given version %s is not available in valid list of versions from %s channel", version, channel)
	}

	//reached the end, no further updates are available
	if currentVersionIndex == len(allVersions)-1 {
		return false, false, version, nil
	}

	//trim the slice, as we doesn't required the previous releases
	allVersions = allVersions[currentVersionIndex:]

	//get the minor/patch versions are available or not.
	//check the current version is timestamp or semantic version
	currentMajor, isSemVersion := IsSemVersionFmt(version)

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

func GetMinimumCurrentManifestVersion(ctx context.Context, version, channel, versionsPath string, optionalURL ...string) (compVersion string, err error) {
	//get the list of all versions on ascending order
	allVersions, err := GetAllVersions(ctx, channel, versionsPath, optionalURL...)
	if err != nil {
		return "", errors.Wrap(err, fmt.Sprintf("error in getting the versions from %s channel", channel))
	}

	currentVersionIndex := len(allVersions)
	for i := len(allVersions) - 1; i >= 0; i-- {
		if allVersions[i] == version {
			currentVersionIndex = i
			break
		}
	}

	if currentVersionIndex == len(allVersions) {
		return "", fmt.Errorf("invalid version, given version %s is not available in valid list of versions from %s channel", version, channel)
	}

	//your version is the first in the list, nothing may update to this version
	if currentVersionIndex == 0 {
		return version, nil
	}

	//trim the slice, as we don't require next releases
	allVersions = allVersions[:currentVersionIndex+1]

	//get the minor/patch versions are available or not.
	//check the current version is timestamp or semantic version
	currentMajor, isSemVersion := IsSemVersionFmt(version)

	if !isSemVersion {
		//just get the earliest
		//todo (rick) make sure the first item is in the timestamp format and that it's less than passed in version
		timeStampVersion := findEarliestTimeStampVersion(allVersions)
		return timeStampVersion, nil
	} else {
		minPrevVersion := findMinPreviousForSemantic(currentMajor, allVersions)
		return minPrevVersion, nil
	}
}

//isSemVersionFmt checks the provided version is in semantic version format, if yes, will return the major version
func IsSemVersionFmt(version string) (string, bool) {
	splitStrings := strings.Split(version, ".")
	if len(splitStrings) > 1 {
		return splitStrings[0], true
	}
	return "", false
}

func findEarliestTimeStampVersion(list []string) (version string) {
	//the list is sorted alphabetically and the first item in it is the earliest
	return list[0]
}

func findMinPreviousForSemantic(currentMajor string, list []string) string {
	//set start out as the current version
	item := list[len(list)-1]
	for i := len(list) - 1; i >= 0; i-- {
		item = list[i]
		if major, _ := IsSemVersionFmt(item); currentMajor != major {
			//as soon as we find a version that doesn't match the current major, it's either the
			//previous major or the final timestamp version
			return item
		} else if i == 0 { //reached the beginning of the list
			item = list[i]
		}
	}
	return item
}

func findLatestTimeStampVersion(list []string) (index int, version string) {
	for index, item := range list {
		if _, isSem := IsSemVersionFmt(item); isSem {
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
		if major, isSem := IsSemVersionFmt(item); isSem {
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
		if major, _ := IsSemVersionFmt(item); currentMajor != major {
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
		if major, _ := IsSemVersionFmt(item); currentMajor != major {
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

//GetAllVersions gives the list of all released versions of given channel in ascending order of version numbers
func GetAllVersions(ctx context.Context, channel, versionsPath string, optionalURL ...string) ([]string, error) {
	if versionsPath != "" {
		return getVersionsFromPath(versionsPath)
	}

	var url string
	//optionalURL is only for testing, used to override the actual url with mockurl
	if len(optionalURL) > 0 && optionalURL[0] != "" {
		url = optionalURL[0]
	} else {
		url = fmt.Sprintf(automateVersionsURLFmt, channel)
	}

	return getAllVersions(ctx, url)
}

func getVersionsFromPath(path string) ([]string, error) {
	stat, err := os.Stat(path)
	if err != nil {
		return nil, err
	}
	if stat.IsDir() {
		return nil, fmt.Errorf("invalid file path")
	}
	body, err := ioutil.ReadFile(path) // nosemgrep
	if err != nil {
		return nil, err
	}

	var allVersions []string
	err = json.Unmarshal(body, &allVersions)
	if err != nil {
		return []string{}, errors.Wrap(err, fmt.Sprintf("error in converting the content of the file %s to slice", path))
	}

	allVersions = getSortedList(allVersions)
	return allVersions, nil
}

func getSortedList(versions []string) []string {
	sort.SliceStable(versions, func(i, j int) bool {
		v1 := versions[i]
		v2 := versions[j]
		_, isV1Sem := IsSemVersionFmt(v1)
		_, isV2Sem := IsSemVersionFmt(v2)

		if !isV1Sem && !isV2Sem { //both are in timestamp version format
			return v1 < v2
		} else if !isV1Sem && isV2Sem {
			return true
		} else if isV1Sem && !isV2Sem {
			return false
		} else {
			return IsCompareSemVersions(v1, v2)
		}
	})
	return versions
}

func getAllVersions(ctx context.Context, url string) ([]string, error) {
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return []string{}, errors.Wrap(err, "error in preparing the request to get the list of available versions")
	}
	req = req.WithContext(ctx)

	config := &http.Transport{
		TLSClientConfig: &tls.Config{
			MinVersion:         tls.VersionTLS12,
			InsecureSkipVerify: true,
		},
	}
	httpClient := &http.Client{Transport: config}
	response, err := httpClient.Do(req)
	if err != nil {
		return []string{}, errors.Wrap(err, fmt.Sprintf("error in invoking the endpoint %s", url))
	}
	defer response.Body.Close() // nolint: errcheck

	if response.StatusCode != http.StatusOK {
		return []string{}, errors.Errorf("Unexpected HTTP response from %s: %s", url, response.Status)
	}

	body, err := ioutil.ReadAll(response.Body) // nosemgrep
	if err != nil {
		return []string{}, errors.Wrap(err, fmt.Sprintf("error in reading the response of the endpoint %s", url))
	}

	var allVersions []string
	err = json.Unmarshal(body, &allVersions)
	if err != nil {
		return []string{}, errors.Wrap(err, fmt.Sprintf("error in converting the response of the endpoint %s to slice", url))
	}

	allVersions = getSortedList(allVersions)
	return allVersions, nil
}

// isCompareSemVersions return true if v1 is less than or equal to v2
func IsCompareSemVersions(v1, v2 string) bool {
	if v1 == v2 {
		return true
	}
	v1Major, v1Minor, v1Patch := fetchVersions(v1)
	v2Major, v2Minor, v2Patch := fetchVersions(v2)

	v1MajorInt, err := strconv.Atoi(v1Major)
	if err != nil {
		logrus.WithError(err).Error("cannot convert major version to integer format")
		return false
	}

	v2MajorInt, err := strconv.Atoi(v2Major)
	if err != nil {
		logrus.WithError(err).Error("cannot convert major version to integer format")
		return false
	}

	if v1MajorInt < v2MajorInt {
		return true
	}
	if v1MajorInt == v2MajorInt {
		v1MinorInt, err := strconv.Atoi(v1Minor)
		if err != nil {
			logrus.WithError(err).Error("cannot convert minor version to integer format")
			return false
		}

		v2MinorInt, err := strconv.Atoi(v2Minor)
		if err != nil {
			logrus.WithError(err).Error("cannot convert minor version to integer format")
			return false
		}

		if v1MinorInt < v2MinorInt {
			return true
		}
		if v1MinorInt == v2MinorInt {
			v1PatchInt, err := strconv.Atoi(v1Patch)
			if err != nil {
				logrus.WithError(err).Error("cannot convert minor version to integer format")
				return false
			}

			v2PatchInt, err := strconv.Atoi(v2Patch)
			if err != nil {
				logrus.WithError(err).Error("cannot convert minor version to integer format")
				return false
			}

			if v1PatchInt < v2PatchInt {
				return true
			}
		}
	}
	return false
}

// fetch versions returns the major, minor, and patch versions of a release if it is in semantic versioned.
// return empty values, if the release is in timestamp mode
func fetchVersions(release string) (major, minor, patch string) {
	if release == "" {
		return "", "", ""
	}
	splitStrings := strings.Split(release, ".")
	if len(splitStrings) == 1 {
		return "", "", ""
	}
	return splitStrings[0], splitStrings[1], splitStrings[2]
}
