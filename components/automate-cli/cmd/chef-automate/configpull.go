package main

import (
	"github.com/chef/toml"
)

func getOpensearchConfig(sshUtil SSHUtil, remoteIps []string) (string, error) {
	scriptCommands := `sudo source /hab/sup/default/SystemdEnvironmentFile.sh
	sudo automate-backend-ctl show --svc=automate-ha-opensearch`
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return "", err
	}
	remoteIps = infra.Outputs.OpensearchPrivateIps.Value
	var array []map[string]interface{}
	for i := 0; i < len(remoteIps); i++ {
		sshUtil.getSSHConfig().hostIP = remoteIps[i]
		rawOutput, err := sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
		if err != nil {
			writer.Error(err.Error())
			return "", err
		}
		writer.Println(rawOutput)
		//converting to struct
		var src OpensearchConfig
		if _, err := toml.Decode(cleanToml(rawOutput), &src); err != nil {
			return "", err
		}
		maparray := filteringRequiredFieldsFromOpensearchConfig(infra, &src)
		array = append(array, maparray)

	}
	return "", nil
}

func getPostgresConfig(sshUtil SSHUtil, remoteIps []string) (string, error) {
	scriptCommands := `sudo source /hab/sup/default/SystemdEnvironmentFile.sh
	sudo automate-backend-ctl show --svc=automate-ha-postgresql`
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return "", err
	}
	remoteIps = infra.Outputs.PostgresqlPrivateIps.Value
	var array1 []map[string]interface{}
	for i := 0; i < len(remoteIps); i++ {
		sshUtil.getSSHConfig().hostIP = remoteIps[i]
		rawOutput, err := sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
		if err != nil {
			writer.Error(err.Error())
			return "", err
		}
		writer.Println(rawOutput)
		//converting to struct
		var src PostgresqlConfig
		if _, err := toml.Decode(cleanToml(rawOutput), &src); err != nil {
			return "", err
		}
		maparray := filteringRequiredFieldsFromPostgresqlConfig(infra, &src)
		array1 = append(array1, maparray)

	}

	return "", nil
}
func filteringRequiredFieldsFromOpensearchConfig(infra *AutomteHAInfraDetails, osconfig *OpensearchConfig) map[string]interface{} {
	userData := make(map[string]interface{})
	userData["ip"] = infra.Outputs.OpensearchPrivateIps.Value
	userData["public_key"] = osconfig.TLS.SslCert
	userData["private_key"] = osconfig.TLS.SslKey

	//mapArraysMap1["opensearch"] = map[string]string{{"ip": infra.Outputs.OpensearchPrivateIps.Value},{"public_key": osconfig.TLS.SslCert}, {"private_key": osconfig.TLS.SslKey}}
	return userData
}

func filteringRequiredFieldsFromPostgresqlConfig(infra *AutomteHAInfraDetails, pgconfig *PostgresqlConfig) map[string]interface{} {
	userData1 := make(map[string]interface{})
	userData1["ip"] = infra.Outputs.OpensearchPrivateIps.Value
	userData1["public_key"] = pgconfig.Ssl.SslCert
	userData1["private_key"] = pgconfig.Ssl.SslKey

	return userData1
}
