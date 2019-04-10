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
	pkg := mustPkg("core/cacerts/2018.03.07/20180418223647")
	header, err := c.DownloadPackage(&pkg, buf)
	require.NoError(t, err)
	assert.Equal(t, &depot.HartHeader{
		FormatVersion: "HART-1",
		KeyName:       "core-20180119235000",
	}, header)
	assert.Equal(t, "5267e42b32c9dd12f3baabd818e538f97ce171b6516cf19804a0f63bcd711183", hex.EncodeToString(buf.Sum(nil)))
}

func TestDownloadOriginKey(t *testing.T) {
	c := depot.NewClient()
	pkg := mustPkg("core/cacerts/2018.03.07/20180418223647")
	header, err := c.DownloadPackage(&pkg, ioutil.Discard)
	require.NoError(t, err)
	buf := sha256.New()
	err = c.DownloadOriginKey(header.KeyName, buf)
	require.NoError(t, err)
	assert.Equal(t, "ecbf088a7a77e0e09e9d3ebf79eb6820f92c5cd455598efea501ba62ad3fdaa9", hex.EncodeToString(buf.Sum(nil)))
}

func TestDownloadOriginKeyWithDash(t *testing.T) {
	c := depot.NewClient()
	pkg := mustPkg("chef-fips/automate-debug/0.1.0/20180822195139")
	header, err := c.DownloadPackage(&pkg, ioutil.Discard)
	require.NoError(t, err)
	buf := sha256.New()
	err = c.DownloadOriginKey(header.KeyName, buf)
	require.NoError(t, err)
	assert.Equal(t, "f774733be6c35f2c65324c7529bc34339e79783d17370fb78e97d136ff052092", hex.EncodeToString(buf.Sum(nil)))
}
