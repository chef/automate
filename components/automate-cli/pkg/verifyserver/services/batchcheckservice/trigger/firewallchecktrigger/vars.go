package firewallchecktrigger

var (
	postgresqlTCPPorts        = [4]string{"9631", "7432", "5432", "6432"}
	opensearchTCPPorts        = [3]string{"9631", "9200", "9300"}
	frontendToPostgresqlPorts = [4]string{"7432", "9631", "5432", "6432"}
	frontendToOpensearchPorts = [2]string{"9631", "9200"}
)
