package integration

import "github.com/chef/automate/components/automate-cli/pkg/diagnostics"

// type Diagnostic struct {
// 	Name     string
// 	Tags     Tags
// 	Generate func(TestContext) error
// 	Verify   func(VerificationTestContext)
// 	Cleanup  func(TestContext) error
// 	// Skip allows skipping diagnostics based on some condition
// 	Skip func(TestContext) (bool, string, error)
// }

func CreateApplicationsDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name:     "applications-svc-data",
		Tags:     diagnostics.Tags{"applications"},
		Generate: AppsLoadData,
		Verify:   AppsVerifyData,
		Cleanup:  AppsCleanupData,
	}
}

func AppsLoadData(testCtx diagnostics.TestContext) error {
	return nil
}

func AppsVerifyData(verifyCtx diagnostics.VerificationTestContext) {
}

func AppsCleanupData(testCtx diagnostics.TestContext) error {
	return nil
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateApplicationsDiagnostic(),
	)
}
