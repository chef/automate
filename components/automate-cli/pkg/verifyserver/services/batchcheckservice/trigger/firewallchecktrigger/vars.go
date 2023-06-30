package firewallchecktrigger

var (
	postgresqlTCPPorts   = [4]string{"9631", "7432", "5432", "6432"}
	opensearchTCPPorts   = [3]string{"9631", "9200", "9300"}
	a2CsTCPPorts         = [1]string{"22"}
	postgresBastionPorts = [1]string{"22"}
	ossBastionPorts      = [1]string{"22"}
)
