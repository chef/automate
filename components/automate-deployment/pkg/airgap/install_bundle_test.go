package airgap

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"io"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"os"
	"path"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const sampleManifest = `
{
	"schema_version": "2",
	"build": "20180611145755",
	"hab": [
		"core/hab/1.6.1205/20241107140309",
		"core/hab-sup/1.6.1205/20241107150331",
		"core/hab-launcher/16260/20220603161305"
	],
	"hab_build": "core/hab/1.6.1243/20241227194506",
	"git_sha": "095673404808228c12df1ee71b2494a67a1368e0",
	"packages": [
	  "core/rsync/3.2.3/20240107034222"
	]
  }
  `

type createProgressCollector struct {
	downloading []string
	complete    []string
	cached      []string
	//	Downloading(name string)
	//	DownloadComplete(name string, wasCached bool)
}

func newCreateInstallBundleProgressCollector() *createProgressCollector {
	return &createProgressCollector{
		downloading: []string{},
		complete:    []string{},
		cached:      []string{},
	}
}

func (p *createProgressCollector) Downloading(name string, _ int) {
	p.downloading = append(p.downloading, name)
}

func (p *createProgressCollector) DownloadComplete(name string, wasCached bool) {
	p.complete = append(p.downloading, name)
	if wasCached {
		p.cached = append(p.cached, name)
	}
}

func (p *createProgressCollector) RetriableDownloadError(_ string, _ string, _ time.Duration) {}

func TestRoundTrip(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resp := []string{
			"20180611145500",
			"20180611145755",
		}
		bytes, _ := json.Marshal(resp)
		w.Write(bytes)
	}))
	defer ts.Close()

	tmpdir := t.TempDir()

	workspaceDir := path.Join(tmpdir, "workspace")
	manifestFile := path.Join(tmpdir, "manifest.json")
	outputFile := path.Join(tmpdir, "test.aib")
	unpackRoot := path.Join(tmpdir, "unpack")
	unpackRootHartsOnly := path.Join(tmpdir, "unpack_harts")

	err := ioutil.WriteFile(manifestFile, []byte(sampleManifest), 0644)
	require.NoError(t, err)

	progress := newCreateInstallBundleProgressCollector()
	creator := NewInstallBundleCreator(
		WithInstallBundleManifestFile(manifestFile),
		WithInstallBundleWorkspacePath(workspaceDir),
		WithInstallBundleOutputPath(outputFile),
		WithInstallBundleVersionsPath(""),
	)
	creator.optionalURL = ts.URL //mocking for GetMinimumCurrentManifestVersion.

	_, err = creator.Create(progress)
	require.NoError(t, err)
	downloaded := []string{
		"core/hab/1.6.1205/20241107140309",
		"core/acl/2.3.1/20240105212642",
		"core/attr/2.5.1/20240105212555",
		"core/glibc/2.34/20220311081742",
		"core/linux-headers/5.19.8/20241017082755",
		"core/perl/5.34.0/20240105215658",
		"core/busybox-static/1.36.1/20241017124523",
		"core/bzip2/1.0.8/20240105212113",
		"core/cacerts/2021.10.26/20240105224256",
		"core/gcc-libs/9.4.0/20220311083308",
		"core/openssl/1.0.2zi/20240105224424",
		"core/zeromq/4.3.4/20241017163014",
		"core/zlib/1.2.11/20220311082914",
		"core/hab-sup/1.6.1205/20241107150331",
		"core/hab-launcher/16260/20220603161305",
	}
	for _, d := range downloaded {
		assert.Contains(t, progress.downloading, d, "Expected %s to report downloading", d)
		assert.Contains(t, progress.complete, d, "Expected %s to report complete", d)
		assert.NotContains(t, progress.cached, d, "Expected %s not to report cached", d)
	}

	metadata, err := Unpack(outputFile, WithUnpackRoot(unpackRoot))
	require.NoError(t, err)
	assert.Equal(t, path.Join(unpackRoot, "hab/svc/deployment-service/data/airgap/manifest.json"), metadata.ManifestPath)
	assert.Equal(t, path.Join(unpackRoot, "hab/tmp/hab"), metadata.HabBinPath)

	metadataHartsOnly, err := Unpack(outputFile, WithUnpackRoot(unpackRootHartsOnly), WithUnpackHartsOnly(true))
	require.NoError(t, err)
	assert.Equal(t, "", metadataHartsOnly.ManifestPath)
	assert.Equal(t, "", metadataHartsOnly.HabBinPath)

	shasumsAutomate := map[string]string{
		"1b97f9a0ce48df2aa96686eb833c4e183dd12cf6767d95d7cffd5f3511577558": "hab/svc/deployment-service/data/airgap/manifest.json",
		"2f423768274257ed2f3b4aa0d5426289f98ffedf10a307c5e6f0376cbfa40916": "hab/tmp/hab",
	}

	shasumsHarts := map[string]string{
		"ecbf088a7a77e0e09e9d3ebf79eb6820f92c5cd455598efea501ba62ad3fdaa9": "hab/cache/keys/core-20180119235000.pub",
		"a12c5aecfb1084f9c844e3d17c56afb9be9a54745fd2cec63eb65add81be38f4": "hab/cache/keys/core-20220215190604.pub",
		"0f4a23881adf1f51726891b694e704615c4767b12e566c3f774d93a7609842f1": "hab/cache/artifacts/core-zlib-1.2.11-20220311082914-x86_64-linux.hart",
		"a127399491b5f58f8bbb13f2054a21ce5984d5a3c4a86dc644ca589c8db787ec": "hab/cache/artifacts/core-zeromq-4.3.4-20241017163014-x86_64-linux.hart",
		"5e522f97a89fab69ece7c9c7a7188697889638a99392ef959c8560cb23d1da15": "hab/cache/artifacts/core-openssl-1.0.2zi-20240105224424-x86_64-linux.hart",
		"30cf437bb1fd58850e280e195e22fa1a928e389ffc7af13d514988936657d13f": "hab/cache/artifacts/core-linux-headers-5.19.8-20241017082755-x86_64-linux.hart",
		"b36f26e2dfe501210b917bd18270770ef9d25de7621f6407d18176955c447d75": "hab/cache/artifacts/core-hab-sup-1.6.1205-20241107150331-x86_64-linux.hart",
		"621f1c8929e9fc89c348f7e855a3651705de86101501ad66fcbe8e87c2936db5": "hab/cache/artifacts/core-hab-launcher-16260-20220603161305-x86_64-linux.hart",
		"f9da0b07a5dc33f6187d38180348e21e53e5589b0ba0829724320a0b78b667e5": "hab/cache/artifacts/core-hab-1.6.1205-20241107140309-x86_64-linux.hart",
		"2edb50c3e11aa8820afaf587615fdfbb68a4576661b134a505ac665339f4c12a": "hab/cache/artifacts/core-glibc-2.34-20220311081742-x86_64-linux.hart",
		"e922b9be9a234ef35c3feca9fdd0ad6402440a74991f8f2acc4d16dac92a6ce9": "hab/cache/artifacts/core-gcc-libs-9.4.0-20220311083308-x86_64-linux.hart",
		"c670af129e8d72e4ec973887a3ace0d17e0c9db5e1b53f32a37477dd3caeb819": "hab/cache/artifacts/core-cacerts-2021.10.26-20240105224256-x86_64-linux.hart",
		"816385d36e894e8f4a613f836342dbe886ae4311a235adbf038a9d05941d5dad": "hab/cache/artifacts/core-bzip2-1.0.8-20240105212113-x86_64-linux.hart",
		"4e5e4a1e638f43774260bf564d7d70eb5f12a98d7317601d2d5f5093c26b4601": "hab/cache/artifacts/core-busybox-static-1.36.1-20241017124523-x86_64-linux.hart",
		"cf6393d85fcc6032a047bc307f73482813d9ea7cf23c9bdb91370aba657d090f": "hab/cache/artifacts/core-attr-2.5.1-20240105212555-x86_64-linux.hart",
		"d4ba3398421da3d97e0323141096a92db43d21c1df238327bf721283bc96ccd6": "hab/cache/artifacts/core-acl-2.3.1-20240105212642-x86_64-linux.hart",
	}

	for shasum, filename := range shasumsAutomate {
		assertSHASumEqual(t, path.Join(unpackRoot, filename), shasum)
		assertFileNotExists(t, path.Join(unpackRootHartsOnly, filename))
	}

	for shasum, filename := range shasumsHarts {
		assertSHASumEqual(t, path.Join(unpackRoot, filename), shasum)
		assertSHASumEqual(t, path.Join(unpackRootHartsOnly, filename), shasum)
	}
}

func assertSHASumEqual(t *testing.T, filename string, hexshasum string) {
	f, err := os.Open(filename)
	defer f.Close()
	require.NoError(t, err, "Could not open %s", filename)
	buf := sha256.New()
	_, err = io.Copy(buf, f)
	require.NoError(t, err, "Could not shasum file %s", filename)
	assert.Equal(t, hexshasum, hex.EncodeToString(buf.Sum(nil)))
}

func assertFileNotExists(t *testing.T, filename string) {
	f, err := os.Open(filename)
	defer f.Close()
	if !os.IsNotExist(err) {
		assert.Failf(t, "expected file to not exist", "%s", filename)
	}
}
