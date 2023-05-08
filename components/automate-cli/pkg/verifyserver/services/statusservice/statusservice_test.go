package statusservice_test

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/statusservice"
	"github.com/stretchr/testify/assert"
)

const (
	automateStatusOutputOnA2 = `Status from deployment with channel [current] and type [local]

	Service Name            Process State  Health Check  Uptime (s) PID 
	deployment-service      running        ok            1287       4951
	backup-gateway          running        ok            1181       5984
	automate-pg-gateway     running        ok            1180       6060
	automate-es-gateway     running        ok            1181       6019
	automate-ui             running        ok            1180       6079
	pg-sidecar-service      running        ok            1177       6339
	cereal-service          running        ok            1174       6550
	event-service           running        ok            1180       6102
	authz-service           running        ok            1174       6677
	es-sidecar-service      running        ok            1179       6171
	event-feed-service      running        ok            1177       6274
	automate-dex            running        ok            1173       6807
	teams-service           running        ok            1176       6362
	session-service         running        ok            1165       7503
	authn-service           running        ok            1176       6429
	secrets-service         running        ok            1175       6463
	applications-service    running        ok            1174       6573
	notifications-service   running        ok            1175       6520
	nodemanager-service     running        ok            1173       6765
	compliance-service      running        ok            1174       6715
	license-control-service running        ok            1173       6869
	local-user-service      running        ok            1173       6836
	config-mgmt-service     running        ok            1172       6938
	ingest-service          running        ok            1170       7025
	infra-proxy-service     running        ok            1171       7003
	data-feed-service       running        ok            1169       7115
	event-gateway           running        ok            1170       7082
	report-manager-service  running        ok            1169       7151
	user-settings-service   running        ok            1169       7171
	automate-gateway        running        ok            1168       7243
	automate-cs-bookshelf   running        ok            1167       7295
	automate-cs-oc-bifrost  running        ok            1166       7322
	automate-cs-oc-erchef   running        ok            1164       7619
	automate-cs-nginx       running        ok            1163       7671
	automate-load-balancer  running        ok            1164       7640`

	habSvcStatusWithLicenseOutputOnA2 = `+---------------------------------------------+

	Chef License Acceptance


Before you can continue, 1 product license must be accepted.
View the license at https://www.chef.io/end-user-license-agreement

License that needs accepting:

* Habitat

Do you accept the 1 product license? [yes/No/quit] yes

Accepting 1 product license...
✓  1 product license accepted.

+---------------------------------------------+

package                                            type        desired  state  elapsed (s)  pid   group
chef/backup-gateway/0.1.0/20230223070223           standalone  up       up     1217         5984  backup-gateway.default
chef/report-manager-service/0.1.0/20230309152830   standalone  up       up     1205         7151  report-manager-service.default
chef/user-settings-service/0.1.0/20230417072436    standalone  up       up     1205         7171  user-settings-service.default
chef/compliance-service/1.11.1/20230417072436      standalone  up       up     1210         6715  compliance-service.default
chef/license-control-service/1.0.0/20230223070129  standalone  up       up     1209         6869  license-control-service.default
chef/automate-ui/2.0.0/20230426102739              standalone  up       up     1216         6079  automate-ui.default
chef/config-mgmt-service/0.1.0/20230224142110      standalone  up       up     1208         6938  config-mgmt-service.default
chef/event-gateway/0.1.0/20230223070110            standalone  up       up     1206         7082  event-gateway.default
chef/teams-service/0.1.0/20230223070128            standalone  up       up     1212         6362  teams-service.default
chef/deployment-service/0.1.0/20230502070345       standalone  up       up     1323         4951  deployment-service.default
chef/event-service/0.1.0/20230309152624            standalone  up       up     1216         6102  event-service.default
chef/applications-service/1.0.0/20230223070130     standalone  up       up     1210         6573  applications-service.default
chef/automate-cs-bookshelf/15.4.0/20230410161619   standalone  up       up     1203         7295  automate-cs-bookshelf.default
chef/event-feed-service/1.0.0/20230223070128       standalone  up       up     1213         6274  event-feed-service.default
chef/automate-dex/0.1.0/20230223070225             standalone  up       up     1209         6807  automate-dex.default
chef/session-service/0.1.0/20230223070128          standalone  up       up     1201         7503  session-service.default
chef/automate-es-gateway/0.1.0/20230223070033      standalone  up       up     1217         6019  automate-es-gateway.default
chef/authz-service/0.1.0/20230223070223            standalone  up       up     1210         6677  authz-service.default
chef/local-user-service/0.1.0/20230223065533       standalone  up       up     1209         6836  local-user-service.default
chef/pg-sidecar-service/0.0.1/20230223070131       standalone  up       up     1213         6339  pg-sidecar-service.default
chef/notifications-service/1.0.0/20230323141551    standalone  up       up     1211         6520  notifications-service.default
chef/data-feed-service/1.0.0/20230309152831        standalone  up       up     1205         7115  data-feed-service.default
chef/infra-proxy-service/0.1.0/20230427125632      standalone  up       up     1207         7003  infra-proxy-service.default
chef/automate-pg-gateway/0.0.1/20230130151627      standalone  up       up     1216         6060  automate-pg-gateway.default
chef/automate-load-balancer/0.1.0/20230427090837   standalone  up       up     1200         7640  automate-load-balancer.default
chef/authn-service/0.1.0/20230223070225            standalone  up       up     1212         6429  authn-service.default
chef/nodemanager-service/1.0.0/20230417072436      standalone  up       up     1209         6765  nodemanager-service.default
chef/ingest-service/0.1.0/20230309152831           standalone  up       up     1206         7025  ingest-service.default
chef/automate-cs-oc-bifrost/15.4.0/20230223070128  standalone  up       up     1202         7322  automate-cs-oc-bifrost.default
chef/es-sidecar-service/1.0.0/20230130152441       standalone  up       up     1215         6171  es-sidecar-service.default
chef/automate-gateway/0.1.0/20230417072436         standalone  up       up     1204         7243  automate-gateway.default
chef/automate-cs-nginx/15.4.0/20230223065651       standalone  up       up     1199         7671  automate-cs-nginx.default
chef/cereal-service/0.1.0/20230223070128           standalone  up       up     1210         6550  cereal-service.default
chef/secrets-service/1.0.0/20230223070129          standalone  up       up     1211         6463  secrets-service.default
chef/automate-cs-oc-erchef/15.4.0/20230410161619   standalone  up       up     1200         7619  automate-cs-oc-erchef.default`

	automateStatusOutputOnCS = `Status from deployment with channel [current] and type [local]

	Service Name            Process State  Health Check  Uptime (s) PID 
	deployment-service      running        ok            954        7631
	backup-gateway          running        ok            898        8171
	automate-pg-gateway     running        ok            898        8239
	automate-es-gateway     running        ok            897        8292
	pg-sidecar-service      running        ok            895        8547
	es-sidecar-service      running        ok            897        8311
	license-control-service running        ok            893        8578
	automate-cs-bookshelf   running        ok            891        8633
	automate-cs-oc-bifrost  running        ok            892        8605
	automate-cs-oc-erchef   running        ok            891        8678
	automate-cs-nginx       running        ok            895        8502
	automate-load-balancer  running        ok            862        9278
	`

	habSvcStatusOutputOnCS = `package                                            type        desired  state  elapsed (s)  pid   group
	chef/deployment-service/0.1.0/20230502070345       standalone  up       up     974          7631  deployment-service.default
	chef/license-control-service/1.0.0/20230223070129  standalone  up       up     913          8578  license-control-service.default
	chef/automate-load-balancer/0.1.0/20230427090837   standalone  up       up     882          9278  automate-load-balancer.default
	chef/backup-gateway/0.1.0/20230223070223           standalone  up       up     918          8171  backup-gateway.default
	chef/pg-sidecar-service/0.0.1/20230223070131       standalone  up       up     915          8547  pg-sidecar-service.default
	chef/automate-cs-oc-bifrost/15.4.0/20230223070128  standalone  up       up     912          8605  automate-cs-oc-bifrost.default
	chef/automate-cs-bookshelf/15.4.0/20230410161619   standalone  up       up     911          8633  automate-cs-bookshelf.default
	chef/automate-cs-nginx/15.4.0/20230223065651       standalone  up       up     915          8502  automate-cs-nginx.default
	chef/automate-es-gateway/0.1.0/20230223070033      standalone  up       up     917          8292  automate-es-gateway.default
	chef/es-sidecar-service/1.0.0/20230130152441       standalone  up       up     917          8311  es-sidecar-service.default
	chef/automate-cs-oc-erchef/15.4.0/20230410161619   standalone  up       up     911          8678  automate-cs-oc-erchef.default
	chef/automate-pg-gateway/0.0.1/20230130151627      standalone  up       up     918          8239  automate-pg-gateway.default
	`

	habSvcStatusOutputOnPG = `package                                            type        desired  state  elapsed (s)  pid   group
	chef/automate-ha-pgleaderchk/0.1.0/20230130152444  standalone  up       up     1507         6325  automate-ha-pgleaderchk.default
	chef/automate-ha-haproxy/2.2.14/20230130151541     standalone  up       up     1507         6343  automate-ha-haproxy.default
	chef/automate-ha-postgresql/13.5.0/20230130151541  standalone  up       up     1501         6462  automate-ha-postgresql.default`

	habSvcStatusOutputOnOS = `
	package                                               type        desired  state  elapsed (s)  pid   group
	chef/automate-ha-opensearch/1.3.7/20230223065900      standalone  up       up     1538         5653  automate-ha-opensearch.default
	chef/automate-ha-elasticsidecar/0.1.0/20230223070538  standalone  up       up     1537         5739  automate-ha-elasticsidecar.default
	`

	automateStatusOutputOnBE = `FileAccessError: Unable to access the file or directory: Failed to read deployment-service TLS certificates: Could not read the service cert: open /hab/svc/deployment-service/data/deployment-service.crt: no such file or directory`
)

func TestStatusService(t *testing.T) {
	ss := statusservice.NewStatusService(func(name string, arg []string) ([]byte, error) {
		return nil, nil
	})
	services, _ := ss.GetServices()
	assert.Equal(t, []models.ServiceDetails{}, services)
}

func TestParseChefAutomateStatusOnA2(t *testing.T) {
	output := automateStatusOutputOnA2

	ss := statusservice.NewStatusService(func(name string, arg []string) ([]byte, error) {
		return nil, nil
	})

	ss.ParseChefAutomateStatus(output)
}

func TestParseHabSvcStatusOnA2(t *testing.T) {
	output := habSvcStatusWithLicenseOutputOnA2

	ss := statusservice.NewStatusService(func(name string, arg []string) ([]byte, error) {
		return nil, nil
	})

	ss.ParseHabSvcStatus(output)
}

func TestParseChefAutomateStatusOnCS(t *testing.T) {
	output := automateStatusOutputOnCS

	ss := statusservice.NewStatusService(func(name string, arg []string) ([]byte, error) {
		return nil, nil
	})

	ss.ParseChefAutomateStatus(output)
}

func TestParseHabSvcStatusOnCS(t *testing.T) {
	output := habSvcStatusOutputOnCS

	ss := statusservice.NewStatusService(func(name string, arg []string) ([]byte, error) {
		return nil, nil
	})

	ss.ParseHabSvcStatus(output)
}

func TestParseChefAutomateStatusOnPG(t *testing.T) {
	output := automateStatusOutputOnBE

	ss := statusservice.NewStatusService(func(name string, arg []string) ([]byte, error) {
		return nil, nil
	})

	ss.ParseChefAutomateStatus(output)
}

func TestParseHabSvcStatusOnPG(t *testing.T) {
	output := habSvcStatusOutputOnPG

	ss := statusservice.NewStatusService(func(name string, arg []string) ([]byte, error) {
		return nil, nil
	})

	ss.ParseHabSvcStatus(output)
}

func TestParseChefAutomateStatusOnOS(t *testing.T) {
	output := ``

	ss := statusservice.NewStatusService(func(name string, arg []string) ([]byte, error) {
		return nil, nil
	})

	ss.ParseChefAutomateStatus(output)
}

func TestParseHabSvcStatusOnOS(t *testing.T) {
	output := habSvcStatusOutputOnOS

	ss := statusservice.NewStatusService(func(name string, arg []string) ([]byte, error) {
		return nil, nil
	})

	ss.ParseHabSvcStatus(output)
}
