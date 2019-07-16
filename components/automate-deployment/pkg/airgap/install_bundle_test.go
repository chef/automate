package airgap

import (
	"crypto/sha256"
	"encoding/hex"
	"io"
	"io/ioutil"
	"os"
	"path"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const sampleManifest = `
{
	"schema_version": "1",
	"build": "20180611145755",
	"hab": [
	  "core/hab/0.55.0/20180321220925",
	  "core/hab-sup/0.55.0/20180321222338",
	  "core/hab-launcher/7241/20180321162126"
	],
	"hab_build": "core/hab/0.54.0/20180221022026",
	"git_sha": "c6e9e90b46b03ca9ea27cfd87125b26ea2cfafbb",
	"packages": [
	  "core/tar/1.29/20170513213607"
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
	tmpdir, err := ioutil.TempDir("", "a2-install-bundle-test")
	require.NoError(t, err)
	defer os.RemoveAll(tmpdir)

	workspaceDir := path.Join(tmpdir, "workspace")
	manifestFile := path.Join(tmpdir, "manifest.json")
	outputFile := path.Join(tmpdir, "test.aib")
	unpackRoot := path.Join(tmpdir, "unpack")
	unpackRootHartsOnly := path.Join(tmpdir, "unpack_harts")

	err = ioutil.WriteFile(manifestFile, []byte(sampleManifest), 0644)
	require.NoError(t, err)

	progress := newCreateInstallBundleProgressCollector()
	creator := NewInstallBundleCreator(
		WithInstallBundleManifestFile(manifestFile),
		WithInstallBundleWorkspacePath(workspaceDir),
		WithInstallBundleOutputPath(outputFile),
	)
	_, err = creator.Create(progress)
	require.NoError(t, err)

	downloaded := []string{
		"core/hab/0.55.0/20180321220925",
		"core/acl/2.2.52/20170513213108",
		"core/attr/2.4.47/20170513213059",
		"core/glibc/2.22/20170513201042",
		"core/linux-headers/4.3/20170513200956",
		"core/tar/1.29/20170513213607",
		"core/hab/0.55.0/20180321220925",
		"core/busybox-static/1.24.2/20170513215502",
		"core/bzip2/1.0.6/20170513212938",
		"core/cacerts/2017.09.20/20171014212239",
		"core/gcc-libs/5.2.0/20170513212920",
		"core/libarchive/3.3.2/20171018164107",
		"core/libsodium/1.0.13/20170905223149",
		"core/openssl/1.0.2l/20171014213633",
		"core/xz/5.2.2/20170513214327",
		"core/zeromq/4.2.2/20171018132502",
		"core/zlib/1.2.8/20170513201911",
		"core/hab-sup/0.55.0/20180321222338",
		"core/hab-launcher/7241/20180321162126",
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
		"0b9f27d820d54fef5a8418c0fc16ada222f65fc07eb5714dc7f898550f22707f": "hab/svc/deployment-service/data/airgap/manifest.json",
		"7d73666fd246819bd6925e7c50afeac6a6f27e2b3e970ea0e045855b7357536d": "hab/tmp/hab",
	}

	shasumsHarts := map[string]string{
		"ecbf088a7a77e0e09e9d3ebf79eb6820f92c5cd455598efea501ba62ad3fdaa9": "hab/cache/keys/core-20180119235000.pub",
		"310fb32158a62d15d13957462336a991f27b3e35da7c600cfa2c01ad7f500139": "hab/cache/keys/core-20160810182414.pub",
		"33099486b1381a804aa8b0a491253e3b77a77cf0ef09585def7a65ba6a5cd614": "hab/cache/artifacts/core-zlib-1.2.8-20170513201911-x86_64-linux.hart",
		"568d5e5711bd68351611b3bccfb31b9fdd7b13bec27a4f27ba0f7a49a8b0d20c": "hab/cache/artifacts/core-zeromq-4.2.2-20171018132502-x86_64-linux.hart",
		"ef0f0dc5f22138393e3603643b325017bca5a81ea9d190311e0c434fae012d6f": "hab/cache/artifacts/core-xz-5.2.2-20170513214327-x86_64-linux.hart",
		"f54fc047afe92b3b9c716abee26107f7345e7fdf0884169bb5b4f092349d16c8": "hab/cache/artifacts/core-tar-1.29-20170513213607-x86_64-linux.hart",
		"a29837662df3fff87131e013dca72a43e4c46ce2971b859a9695401f9f351407": "hab/cache/artifacts/core-openssl-1.0.2l-20171014213633-x86_64-linux.hart",
		"9fc7445f8fc86d712b59c7ffabe0b1b832e0deb4ed0c61715349f0d2aee87b50": "hab/cache/artifacts/core-linux-headers-4.3-20170513200956-x86_64-linux.hart",
		"17eef07687e0e0b57a813e3a49fcf9a774b7613a23cecf70ad34cf927064507e": "hab/cache/artifacts/core-libsodium-1.0.13-20170905223149-x86_64-linux.hart",
		"c2b4f00a9c11b53d4dc5fbe13a82c39f9978661468c9319cda6dd6d2118baf1f": "hab/cache/artifacts/core-libarchive-3.3.2-20171018164107-x86_64-linux.hart",
		"542626d97f85dd19359f7959dc51248d1242b131ac473a491b5d65a9aaf190d1": "hab/cache/artifacts/core-hab-sup-0.55.0-20180321222338-x86_64-linux.hart",
		"f8bb3b00e69855dccd738ea7d5cd2df4bc887400972f903be283792dea7b65e9": "hab/cache/artifacts/core-hab-launcher-7241-20180321162126-x86_64-linux.hart",
		"0b2678b232c4b42b40a68672aaa14dc8668b6e78244958abc86ff7568f58cee1": "hab/cache/artifacts/core-hab-0.55.0-20180321220925-x86_64-linux.hart",
		"7d579a9e762f6094a6ea79c76397fbfbb97f1baf2b34e7ddece5f3dcb23162ff": "hab/cache/artifacts/core-glibc-2.22-20170513201042-x86_64-linux.hart",
		"8045cfd3653ae07dcb2c1caa0c92e831bd129b5954c3e36c4f112d805a6473d9": "hab/cache/artifacts/core-gcc-libs-5.2.0-20170513212920-x86_64-linux.hart",
		"7f948a24b991eabe537797b9e153dbac9d9b5a95a0242b4c289402bc9d03b46e": "hab/cache/artifacts/core-cacerts-2017.09.20-20171014212239-x86_64-linux.hart",
		"7c669e5c1d8f8ddb8fdc9d158c51d59bef8d351b205aa6cd711fc7bd442fbd6d": "hab/cache/artifacts/core-bzip2-1.0.6-20170513212938-x86_64-linux.hart",
		"8b86241397d520d0376920a32c2e0c3b49164b798100e606c0f4a82a0070ac50": "hab/cache/artifacts/core-busybox-static-1.24.2-20170513215502-x86_64-linux.hart",
		"3d7554c8a9e3913d13716d7175c5897bec2a583fc9089e0ade4129804a8f6b7a": "hab/cache/artifacts/core-attr-2.4.47-20170513213059-x86_64-linux.hart",
		"44e7d0a8dbc8c398bb339c6c66b88f1f196024f55e13070faed3f783fa961f7a": "hab/cache/artifacts/core-acl-2.2.52-20170513213108-x86_64-linux.hart",
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
