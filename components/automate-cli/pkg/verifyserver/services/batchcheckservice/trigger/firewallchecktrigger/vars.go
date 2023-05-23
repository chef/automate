package firewallchecktrigger

var (
	postgresqlTCPPorts   = [5]string{"9631", "7432", "5432", "6432", "9638"}
	opensearchTCPPorts   = [4]string{"9631", "9200", "9300", "9638"}
	a2CsTCPPorts         = [3]string{"22", "9631", "9638"}
	postgresBastionPorts = [4]string{"22", "9631", "9638", "7432"}
	ossBastionPorts      = [4]string{"22", "9631", "9638", "9200"}
)
