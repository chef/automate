package v2

import (
	"context"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/authz-service/engine"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/logger"
)

var ErrMessageBoxFull = errors.New("Message box full")

type PolicyRefresher interface {
	Refresh(ctx context.Context) error
	RefreshAsync() error
}

type policyRefresher struct {
	log                      logger.Logger
	store                    storage.Storage
	engine                   engine.V2Writer
	refreshRequests          chan policyRefresherMessageRefresh
	antiEntropyTimerDuration time.Duration
	changeNotifier           storage.PolicyChangeNotifier
}

type policyRefresherMessageRefresh struct {
	ctx    context.Context
	status chan error
}

func (m *policyRefresherMessageRefresh) Respond(err error) {
	if m.status != nil {
		select {
		case m.status <- err:
		default:
		}
		close(m.status)
	}
}

func (m *policyRefresherMessageRefresh) Err() error {
	return <-m.status
}

func NewPolicyRefresher(ctx context.Context, log logger.Logger, store storage.Storage,
	engine engine.V2Writer) (PolicyRefresher, error) {
	changeNotifier, err := store.GetPolicyChangeNotifier(ctx)
	if err != nil {
		return nil, err
	}
	refresher := &policyRefresher{
		log:                      log,
		store:                    store,
		engine:                   engine,
		refreshRequests:          make(chan policyRefresherMessageRefresh, 1),
		antiEntropyTimerDuration: 10 * time.Second,
		changeNotifier:           changeNotifier,
	}
	go refresher.run(ctx)
	return refresher, nil
}

func (refresher *policyRefresher) run(ctx context.Context) {
	lastPolicyID := ""
	antiEntropyTimer := time.NewTimer(refresher.antiEntropyTimerDuration)
RUNLOOP:
	for {
		select {
		case <-ctx.Done():
			refresher.log.WithError(ctx.Err()).Info("Policy refresher exiting")
			break RUNLOOP
		case <-refresher.changeNotifier.C():
			refresher.log.Info("Received policy change notification")
			var err error
			lastPolicyID, err = refresher.refresh(context.Background(), lastPolicyID)
			if err != nil {
				refresher.log.WithError(err).Warn("Failed to refresh policies")
			}
			if !antiEntropyTimer.Stop() {
				<-antiEntropyTimer.C
			}
		case m := <-refresher.refreshRequests:
			refresher.log.Info("Received local policy refresh request")
			var err error
			lastPolicyID, err = refresher.refresh(m.ctx, lastPolicyID)
			m.Respond(err)
			if !antiEntropyTimer.Stop() {
				<-antiEntropyTimer.C
			}
		case <-antiEntropyTimer.C:
			var err error
			lastPolicyID, err = refresher.refresh(ctx, lastPolicyID)
			if err != nil {
				refresher.log.WithError(err).Warn("Anti-entropy refresh failed")
			}
		}

		antiEntropyTimer.Reset(refresher.antiEntropyTimerDuration)
	}
	refresher.log.Info("Shutting down policy refresh loop")
	close(refresher.refreshRequests)
}

func (refresher *policyRefresher) refresh(ctx context.Context, lastPolicyID string) (string, error) {
	curPolicyID, err := refresher.store.GetPolicyChangeID(ctx)
	if err != nil {
		refresher.log.WithError(err).Warn("Failed to get current policy change ID")
		return lastPolicyID, err
	}
	if curPolicyID != lastPolicyID {
		refresher.log.WithFields(logrus.Fields{
			"lastPolicyID": lastPolicyID,
			"curPolicyID":  curPolicyID,
		}).Debug("Refreshing engine store")

		if err := refresher.updateEngineStore(ctx); err != nil {
			refresher.log.WithError(err).Warn("Failed to refresh engine store")
			return lastPolicyID, err
		}
	}
	return curPolicyID, nil
}

func (refresher *policyRefresher) Refresh(ctx context.Context) error {
	m := policyRefresherMessageRefresh{
		ctx:    ctx,
		status: make(chan error, 1),
	}
	refresher.refreshRequests <- m
	return m.Err()
}

func (refresher *policyRefresher) RefreshAsync() error {
	m := policyRefresherMessageRefresh{
		ctx: context.Background(),
	}
	select {
	case refresher.refreshRequests <- m:
	default:
		refresher.log.Warn("Refresher message box full")
		return ErrMessageBoxFull
	}

	return nil
}

// updates OPA engine store with policy
func (refresher *policyRefresher) updateEngineStore(ctx context.Context) error {
	// Engine updates need unfiltered access to all data.
	ctx = auth_context.ContextWithoutProjects(ctx)

	policyMap, err := refresher.getPolicyMap(ctx)
	if err != nil {
		return err
	}
	roleMap, err := refresher.getRoleMap(ctx)
	if err != nil {
		return err
	}
	ruleMap, err := refresher.getRuleMap(ctx)
	if err != nil {
		return err
	}

	return refresher.engine.V2SetPolicies(ctx, policyMap, roleMap, ruleMap)
}

func (refresher *policyRefresher) getPolicyMap(ctx context.Context) (map[string]interface{}, error) {
	var policies []*storage.Policy
	var err error

	if policies, err = refresher.store.ListPolicies(ctx); err != nil {
		return nil, err
	}
	refresher.log.Infof("initializing OPA store with %d V2 policies", len(policies))

	policies = append(policies, SystemPolicies()...)

	// OPA requires this format
	data := make(map[string]interface{})
	for _, p := range policies {

		statements := make(map[string]interface{})
		for _, st := range p.Statements {
			stmt := map[string]interface{}{
				"effect":   st.Effect.String(),
				"projects": st.Projects,
			}
			// Only set these if provided
			if st.Role != "" {
				stmt["role"] = st.Role
			}
			if len(st.Actions) != 0 {
				stmt["actions"] = st.Actions
			}
			if len(st.Resources) != 0 {
				stmt["resources"] = st.Resources
			}
			statements[st.ID.String()] = stmt
		}

		members := make([]string, len(p.Members))
		for i, member := range p.Members {
			members[i] = member.Name
		}

		data[p.ID] = map[string]interface{}{
			"type":       p.Type.String(),
			"members":    members,
			"statements": statements,
		}
	}
	return data, nil
}

func (refresher *policyRefresher) getRoleMap(ctx context.Context) (map[string]interface{}, error) {
	var roles []*storage.Role
	var err error
	if roles, err = refresher.store.ListRoles(ctx); err != nil {
		return nil, err
	}
	refresher.log.Infof("initializing OPA store with %d V2 roles", len(roles))

	// OPA requires this format
	data := make(map[string]interface{})
	for _, r := range roles {
		data[r.ID] = map[string]interface{}{
			"actions": r.Actions,
		}
	}
	return data, nil
}

// TODO: mocked struct that will eventually be
// the storage struct.
type rule struct {
	ID     string
	Type   string
	Values []string
}

// TODO: nolint can go away when connected to the database
// nolint: unparam
func (refresher *policyRefresher) getRuleMap(_ context.Context) (map[string][]interface{}, error) {
	// Mocked rule data
	// notlint: gofmt
	// rules := [5]*rule{
	// 	{
	// 		ID:     "project1",
	// 		Type:   "ChefServers",
	// 		Values: []string{"chef-server-1", "chef-server-2", "chef-server-3"},
	// 	},
	// 	{
	// 		ID:     "project2",
	// 		Type:   "ChefOrgs",
	// 		Values: []string{"Org1", "Org2"},
	// 	},
	// 	{
	// 		ID:     "project2",
	// 		Type:   "ChefServers",
	// 		Values: []string{"chef-server-3", "chef-server-4", "chef-server-5"},
	// 	},
	// 	{
	// 		ID:     "project3",
	// 		Type:   "ChefEnvironment",
	// 		Values: []string{"env-1", "env-2", "env-3"},
	// 	},
	// 	{
	// 		ID:     "project4",
	// 		Type:   "ChefEnvironment",
	// 		Values: []string{"env-4", "env-5", "env-6"},
	// 	},
	// }
	rules := []*rule{}

	refresher.log.Infof("initializing OPA store with %d V2 project rule mappings", len(rules))

	// OPA requires this format
	data := make(map[string][]interface{})
	for _, r := range rules {
		if _, ok := data[r.ID]; !ok {
			data[r.ID] = make([]interface{}, 0)
		}
		data[r.ID] = append(data[r.ID],
			map[string]interface{}{
				"type":   r.Type,
				"values": r.Values,
			})
	}
	return data, nil
}
