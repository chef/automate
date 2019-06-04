package status

// List of labels used to differentiate between migrations that can run independent of each other
const (
	MigrationLabelESa1   = "ElasticSearch_A1"
	MigrationLabelESa2v1 = "ElasticSearch_A2_v1"
	MigrationLabelESa2v2 = "ElasticSearch_A2_v2"
	MigrationLabelPG     = "PostgreSQL"
	MigrationLabelPRO    = "Profiles"
	//MigrationLabelFEEDS = "ElasticSearch_Feeds_1"
)

const MaxMigrations = 5        // Total migrations should match the number of constants above
const TotalMigrationSteps = 24 // Max number of migration LogEntry items we can have across all migrations

// Special message sent by the services to flag the end of a migration either failed or successful
const MigrationFailedMsg = "FAILED"
const MigrationCompletedMsg = "COMPLETED"
