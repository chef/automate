package reporting

const ESize = 999999

const FAILED = "failed"

const PASSED = "passed"

const WAIVED = "waived"

const SKIPPED = "skipped"

type Status struct {
	EndTime float64
	Status  string
}
