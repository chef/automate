package manifest

import (
	"context"
	"time"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
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
