package depot_test

import (
	"crypto/sha256"
	"encoding/hex"
	"io/ioutil"
	"testing"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/depot"

	"github.com/stretchr/testify/assert"
)

func mustPkg(ident string) habpkg.HabPkg {
	p, err := habpkg.FromString(ident)
	if err != nil {
		panic(err)
	}
	return p
}
func TestFetchTDeps(t *testing.T) {
	c := depot.NewClient()
	pkg := mustPkg("core/bash/4.3.42/20180419145051")
	tdeps, err := c.TDepsForPackage(&pkg)
	assert.NoError(t, err)
	assert.Equal(t, []habpkg.HabPkg{
		mustPkg("core/gcc-libs/5.2.0/20170513212920"),
		mustPkg("core/glibc/2.22/20170513201042"),
		mustPkg("core/linux-headers/4.3/20170513200956"),
		mustPkg("core/ncurses/6.0/20180418232647"),
		mustPkg("core/readline/6.3.8/20180419035606"),
	}, tdeps)
}

func TestDownloadPackage(t *testing.T) {
	c := depot.NewClient()
	buf := sha256.New()
	pkg := mustPkg("core/cacerts/2021.10.26/20240105224256")
	header, err := c.DownloadPackage(&pkg, buf)
	require.NoError(t, err)
	assert.Equal(t, &depot.HartHeader{
		FormatVersion: "HART-1",
		KeyName:       "core-20231114082941",
	}, header)
	assert.Equal(t, "c670af129e8d72e4ec973887a3ace0d17e0c9db5e1b53f32a37477dd3caeb819", hex.EncodeToString(buf.Sum(nil)))
}

func TestDownloadOriginKey(t *testing.T) {
	c := depot.NewClient()
	pkg := mustPkg("core/cacerts/2021.10.26/20240105224256")
	header, err := c.DownloadPackage(&pkg, ioutil.Discard)
	require.NoError(t, err)
	buf := sha256.New()
	err = c.DownloadOriginKey(header.KeyName, buf)
	require.NoError(t, err)
	assert.Equal(t, "1abe8edb6292fd23ee81016ae8103eea909410ef29f69000e098674f7345cac1", hex.EncodeToString(buf.Sum(nil)))
}

func TestDownloadOriginKeyWithDash(t *testing.T) {
	c := depot.NewClient()
	pkg := mustPkg("chef/automate-debug/0.1.0/20250617103704")
	header, err := c.DownloadPackage(&pkg, ioutil.Discard)
	require.NoError(t, err)
	buf := sha256.New()
	err = c.DownloadOriginKey(header.KeyName, buf)
	require.NoError(t, err)
	assert.Equal(t, "8aefb8c21814dabe00279c8b5dcfdcf19bd9f9886d90d7e9dc7bf0de5402997f", hex.EncodeToString(buf.Sum(nil)))
}
