package shared

import (
	"reflect"
	"testing"

	"github.com/stretchr/testify/assert"

	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestMerge(t *testing.T) {
	t.Run("merges into existing configuration", func(t *testing.T) {
		existing := &GlobalConfig{
			V1: &V1{
				Backups: &Backups{
					Location: w.String("filesystem"),
					Filesystem: &Backups_Filesystem{
						Path: w.String("/var/opt/chef-automate/backups"),
					},
				},
				Mlsa: &Mlsa{
					Accept: w.Bool(true),
				},
			},
		}

		overwrite := &GlobalConfig{
			V1: &V1{
				Backups: &Backups{
					Location: w.String("s3"),
					Filesystem: &Backups_Filesystem{
						Path: w.String("/var/opt/chef-automate/backups"),
					},
					S3: &Backups_S3{
						Bucket: &Backups_S3_Bucket{
							Name:     w.String("test-bucket"),
							Endpoint: w.String("s3.amazonaws.com"),
						},
						Credentials: &Backups_S3_AWSCredentials{
							AccessKey: w.String("AKIACCESSKEY"),
							SecretKey: w.String("SECRETKEY"),
						},
					},
				},
			},
		}

		expected := &GlobalConfig{
			V1: &V1{
				Backups: &Backups{
					Location: w.String("s3"),
					Filesystem: &Backups_Filesystem{
						Path: w.String("/var/opt/chef-automate/backups"),
					},
					S3: &Backups_S3{
						Bucket: &Backups_S3_Bucket{
							Name:     w.String("test-bucket"),
							Endpoint: w.String("s3.amazonaws.com"),
						},
						Credentials: &Backups_S3_AWSCredentials{
							AccessKey: w.String("AKIACCESSKEY"),
							SecretKey: w.String("SECRETKEY"),
						},
					},
				},
				Mlsa: &Mlsa{
					Accept: w.Bool(true),
				},
			},
		}

		merged := &GlobalConfig{}
		err := Merge(existing, overwrite, merged)
		assert.NoError(t, err)
		assert.True(t, reflect.DeepEqual(expected, merged), "Failed to merged")
	})
}
