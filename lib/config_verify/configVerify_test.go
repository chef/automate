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
			return
		}
		// If there is no error, fail the test case
		t.Fail()
	})

	t.Run("Test Success AWS Config Validation", func(t *testing.T) {
		config := getTestAWSConfigData()

		cv := &ConfigVerifyImpl{}
		err := cv.ConfigValidateAWS(config)
		if err != nil {
			t.Errorf("ConfigInitials validation failed: %s", err)
		}
	})

	t.Run("Test Failed AWS Config Validation", func(t *testing.T) {
		config := getFailedTestAWSConfigData()

		cv := &ConfigVerifyImpl{}
		err := cv.ConfigValidateAWS(config)
		if err != nil {
			return
		}
		// If there is no error, fail the test case
		t.Fail()
	})

	t.Run("Test Success Automate SC Config Validation", func(t *testing.T) {
		config := getAutomateScConfigTestData()
		cv := &ConfigVerifyImpl{}
		err := cv.ConfigValidateStandalone(config)
		if err != nil {
			t.Errorf("ConfigInitials validation failed: %s", err)
		}
	})

	t.Run("Test Failed Automate SC Config Validation", func(t *testing.T) {
		config := getFAiledAutomateScConfigTestData()
		cv := &ConfigVerifyImpl{}
		err := cv.ConfigValidateStandalone(config)
		if err != nil {
			return
		}
		// If there is no error, fail the test case
		t.Fail()
	})

}
