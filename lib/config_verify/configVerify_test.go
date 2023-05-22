package config_verify

import (
	"testing"
)

func TestConfigValidateOnPrem(t *testing.T) {
	t.Run("Test Success config Validation", func(t *testing.T) {
		config := getTestOnPremConfigData()

		cv := &ConfigVerifyImpl{}
		err := cv.ConfigValidateOnPrem(config)
		if err != nil {
			t.Errorf("ConfigInitials validation failed: %s", err)
		}
	})

	t.Run("Test Failed Config Validation", func(t *testing.T) {
		config := getTestOnPremFailedConfigData()

		cv := &ConfigVerifyImpl{}
		err := cv.ConfigValidateOnPrem(config)
		if err != nil {
			t.Errorf("ConfigInitials validation failed: %s", err)
		}
	})

	t.Run("Test Success AWS Config Validation", func(t *testing.T) {
		config := getTestAWSConfigData()

		cv := &ConfigVerifyImpl{}
		err := cv.ConfigValidateAWS(config)
		if err != nil {
			t.Errorf("ConfigInitials validation failed: %s", err)
		}
	})
}
