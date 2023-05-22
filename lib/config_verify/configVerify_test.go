package config_verify

import (
	"testing"
)

func TestConfigValidateOnPrem(t *testing.T) {
	t.Run("Test Success config Validation", func(t *testing.T) {
		// Create a test instance of ConfigInitials
		config := getTestOnPremConfigData()

		cv := &ConfigVerifyImpl{}
		// Call the function being tested
		err := cv.ConfigValidateOnPrem(config)

		// Check the expected result
		if err != nil {
			t.Errorf("ConfigInitials validation failed: %s", err)
		}
	})

	t.Run("Test Failed Config Validation", func(t *testing.T) {
		// Create a test instance of ConfigInitials
		config := getTestOnPremFailedConfigData()

		cv := &ConfigVerifyImpl{}
		// Call the function being tested
		err := cv.ConfigValidateOnPrem(config)

		// Check the expected result
		if err != nil {
			t.Errorf("ConfigInitials validation failed: %s", err)
		}
	})

	t.Run("Test Success AWS Config Validation", func(t *testing.T) {
		// Create a test instance of ConfigInitials
		config := getTestAWSConfigData()

		cv := &ConfigVerifyImpl{}
		// Call the function being tested
		err := cv.ConfigValidateAWS(config)

		// Check the expected result
		if err != nil {
			t.Errorf("ConfigInitials validation failed: %s", err)
		}
	})
	// Add more test cases for other sections: Opensearch, Postgresql, ExistingInfra, etc.
}
