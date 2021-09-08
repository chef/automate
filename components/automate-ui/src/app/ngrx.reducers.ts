import { Params, RouterStateSnapshot, UrlSegment } from '@angular/router';
import * as router from '@ngrx/router-store';
import { Action } from '@ngrx/store';
import { set, get, pipe, map } from 'lodash/fp';

import * as destinationEntity from './entities/destinations/destination.reducer';
import * as destinationConfigEntity from './entities/global-config/destination-config.reducer';
import * as scanner from './pages/+compliance/+scanner/state/scanner.state';
import * as eventFeed from './services/event-feed/event-feed.reducer';
import * as projectsFilter from './services/projects-filter/projects-filter.reducer';
import * as adminKeyEntity from './entities/reset-admin-key/reset-admin-key.reducer';
import * as apiToken from './entities/api-tokens/api-token.reducer';
import * as automateSettings from './entities/automate-settings/automate-settings.reducer';
import * as cdsEntity from './entities/cds/cds.reducer';
import * as clientEntity from './entities/clients/client.reducer';
import * as clientDetailsEntity from './entities/clients/client-details.reducer';
import * as clientRuns from './entities/client-runs/client-runs.reducer';
import * as cookbookEntity from './entities/cookbooks/cookbook.reducer';
import * as cookbookDetailsEntity from './entities/cookbooks/cookbook-details.reducer';
import * as cookbookVersionsEntity from './entities/cookbooks/cookbook-versions.reducer';
import * as credential from './entities/credentials/credential.reducer';
import * as dataBagEntity from './entities/data-bags/data-bags.reducer';
import * as dataBagItemsEntity from './entities/data-bags/data-bag-details.reducer';
import * as dataBagItemDetailsEntity from './entities/data-bags/data-bag-item-details.reducer';
import * as desktopEntity from './entities/desktop/desktop.reducer';
import * as environmentEntity from './entities/environments/environment.reducer';
import * as environmentDetailsEntity from './entities/environments/environment-details.reducer';
import * as infraNodeEntity from './entities/infra-nodes/infra-nodes.reducer';
import * as infraNodeDetailsEntity from './entities/infra-nodes/infra-node-details.reducer';
import * as infraRoleEntity from './entities/infra-roles/infra-role.reducer';
import * as infraRoleDetailsEntity from './entities/infra-roles/infra-role-details.reducer';
import * as integrationsAdd from './pages/integrations/add/integration-add.reducer';
import * as integrationsDetail from './pages/integrations/detail/integrations-detail.reducer';
import * as integrationsEdit from './pages/integrations/edit/integrations-edit.reducer';
import * as layout from './entities/layout/layout.reducer';
import * as license from './entities/license/license.reducer';
import * as jobAdd from './pages/job-add/job-add.reducer';
import * as jobEdit from './pages/job-edit/job-edit.reducer';
import * as jobEntity from './entities/jobs/job.reducer';
import * as jobList from './pages/job-list/job-list.reducer';
import * as manager from './entities/managers/manager.reducer';
import * as nodeRunlistEntity from './entities/nodeRunlists/nodeRunlists.reducer';
import * as notificationEntity from './entities/notifications/notification.reducer';
import * as NotificationRuleEntity from './entities/notification_rules/notification_rule.reducer';
import * as orgEntity from './entities/orgs/org.reducer';
import * as permEntity from './entities/userperms/userperms.reducer';
import * as policyEntity from './entities/policies/policy.reducer';
import * as policyFileEntity from './entities/policy-files/policy-file.reducer';
import * as policyFileDetailsEntity from './entities/policy-files/policy-file-details.reducer';
import * as policyGroupEntity from './entities/policy-files/policy-group.reducer';
import * as policyGroupDetailsEntity from './entities/policy-files/policy-group-details.reducer';
import * as profileEntity from './entities/profiles/profile.reducer';
import * as projectEntity from './entities/projects/project.reducer';
import * as recipeEntity from './entities/recipes/recipe.reducer';
import * as revisionEntity from './entities/revisions/revision.reducer';
import * as roleEnvironmentEntity from './entities/role-environments/role-environments.reducer';
import * as roleEntity from './entities/roles/role.reducer';
import * as ruleEntity from './entities/rules/rule.reducer';
import * as runlistEntity from './entities/runlists/runlists.reducer';
import * as serverEntity from './entities/servers/server.reducer';
import * as serviceGroups from './entities/service-groups/service-groups.reducer';
import * as nodesEntity from './entities/nodes/nodes.reducer';
import * as nodeCredentialEntity from './entities/node-credentials/node-credential.reducer';
import * as nodeCredentialDetailsEntity from './entities/node-credentials/node-credential-details.reducer';
import * as nodeCredentialList from './pages/+compliance/+node-credentials/node-credentials-list/node-credential-list.reducer';
import * as teamEntity from './entities/teams/team.reducer';
import * as userEntity from './entities/users/user.reducer';
import * as userPreferencesEntity from './services/user-preferences/user-preferences.reducer';
import * as userSelfEntity from './entities/users/userself.reducer';

import { LayoutActionTypes, UpdateSidebars } from './entities/layout/layout.actions';

// AOT likely won't allow dynamic object property names here even when the underlying
// typescript bug preventing it is fixed
export interface NgrxStateAtom {
  event_feed: eventFeed.EventFeedState;
  router: RouterReducerState;
  scanner: scanner.ScannerState;
  layout: layout.LayoutEntityState;
  projectsFilter: projectsFilter.ProjectsFilterState;

  // UI State
  integrations_add: integrationsAdd.IntegrationsAddState;
  integrations_detail: integrationsDetail.IntegrationsDetailState;
  integrations_edit: integrationsEdit.IntegrationsEditState;
  job_add: jobAdd.JobAddState;
  job_edit: jobEdit.JobEditState;
  job_list: jobList.JobListState;
  nodeCredential_list: nodeCredentialList.NodeCredentialListState;

  // Entities
  adminKey: adminKeyEntity.AdminKeyEntityState;
  apiTokens: apiToken.ApiTokenEntityState;
  automateSettings: automateSettings.AutomateSettingsEntityState;
  cds: cdsEntity.CdsEntityState;
  clientRunsEntity: clientRuns.ClientRunsEntityState;
  cookbooks: cookbookEntity.CookbookEntityState;
  clients: clientEntity.ClientEntityState;
  clientDetail: clientDetailsEntity.ClientDetailsEntityState;
  cookbookDetails: cookbookDetailsEntity.CookbookDetailsEntityState;
  cookbookVersions: cookbookVersionsEntity.CookbookVersionsEntityState;
  // Named credentialEntity until we refactor the credentials page
  credentialEntity: credential.CredentialState;
  dataBag: dataBagEntity.DataBagEntityState;
  dataBagItems: dataBagItemsEntity.DataBagItemsEntityState;
  dataBagItemDetails: dataBagItemDetailsEntity.DataBagItemDetailsEntityState;
  desktops: desktopEntity.DesktopEntityState;
  destinations: destinationEntity.DestinationEntityState;
  globalDataFeedConfig: destinationConfigEntity.GlobalConfigEntityState;
  environments: environmentEntity.EnvironmentEntityState;
  environmentDetails: environmentDetailsEntity.EnvironmentDetailsEntityState;
  infraNodes: infraNodeEntity.InfraNodeEntityState;
  infraNodeDetails: infraNodeDetailsEntity.InfraNodeDetailsEntityState;
  infraRoles: infraRoleEntity.InfraRoleEntityState;
  infraRoleDetails: infraRoleDetailsEntity.InfraRoleDetailsEntityState;
  jobs: jobEntity.JobEntityState;
  licenseStatus: license.LicenseStatusEntityState;
  managers: manager.ManagerEntityState;
  nodes: nodesEntity.NodesEntityState;
  nodeRunlist: nodeRunlistEntity.NodeRunlistEntityState;
  notifications: notificationEntity.NotificationEntityState;
  notificationRules: NotificationRuleEntity.NotificationRuleEntityState;
  orgs: orgEntity.OrgEntityState;
  policies: policyEntity.PolicyEntityState;
  policyFiles: policyFileEntity.PolicyFileEntityState;
  policyFileDetails: policyFileDetailsEntity.PolicyFileDetailsEntityState;
  policyGroups: policyGroupEntity.PolicyGroupEntityState;
  policyGroupDetails: policyGroupDetailsEntity.PolicyGroupDetailsEntityState;
  profiles: profileEntity.ProfileEntityState;
  projects: projectEntity.ProjectEntityState;
  recipes: recipeEntity.RecipeEntityState;
  revisions: revisionEntity.RevisionEntityState;
  roleEnvironments: roleEnvironmentEntity.RoleEnvironmentEntityState;
  roles: roleEntity.RoleEntityState;
  rules: ruleEntity.RuleEntityState;
  runlist: runlistEntity.RunlistEntityState;
  servers: serverEntity.ServerEntityState;
  nodeCredential: nodeCredentialEntity.NodeCredentialEntityState;
  nodeCredentialDetails: nodeCredentialDetailsEntity.NodeCredentialDetailsEntityState;
  serviceGroups: serviceGroups.ServiceGroupsEntityState;
  teams: teamEntity.TeamEntityState;
  userperms: permEntity.PermEntityState;
  users: userEntity.UserEntityState;
  userPreferences: userPreferencesEntity.UserPreferencesEntityState;
  userSelf: userSelfEntity.UserSelfEntityState;
}

export interface RouterReducerState {
  state: RouterState;
  previousRoute?: RouterState;
  navigationId?: number;
}

export interface RouterState {
  url: string;
  queryParams: Params;
  params: Params;
  fragment: string;
  path: UrlSegment[];
}

// Adapted from https://ngrx.io/guide/router-store/configuration
export class RouterSerializer implements router.RouterStateSerializer<RouterState> {
  serialize(routerState: RouterStateSnapshot): RouterState {
    let route = routerState.root;

    while (route.firstChild) {
      route = route.firstChild;
    }
    const path = ['/', ...pipe(
      get('_urlSegment.segments'),
      map('path'))(route)];
    const { url, root: { queryParams } } = routerState;
    const { params, fragment } = route;

    return { url, params, queryParams, fragment, path };
  }
}

export const runtimeChecks = {
  strictStateImmutability: false,
  strictActionImmutability: false,
  strictStateSerializability: false,
  strictActionSerializability: false
};

export const defaultRouterRouterState = {
  url: '/',
  queryParams: {},
  params: {},
  fragment: '',
  path: ['/']
};

export const defaultRouterState = {
  state: defaultRouterRouterState,
  previousRoute: {},
  navigationId: 0
};

export function routerReducer(state = defaultRouterState, action) {
  switch (action.type) {
    case router.ROUTER_NAVIGATION:
      const newRouterState =
        set('previousRoute', get('state', state), router.routerReducer(state, action));
      return newRouterState;
    default:
      return state;
  }
}

// Note: this is used ONLY for unit tests, but better to be defined here since
// it needs to mirror the ngrxReducers.
export const defaultInitialState = {
  router: defaultRouterState,
  event_feed: eventFeed.initialState,
  scanner: scanner.initialState,
  layout: layout.InitialState,
  projectsFilter: projectsFilter.projectsFilterInitialState,

  // UI State
  integrations_add: integrationsAdd.IntegrationsAddInitialState,
  integrations_detail: integrationsDetail.IntegrationsDetailInitialState,
  integrations_edit: integrationsEdit.IntegrationsEditInitialState,
  job_add: jobAdd.JobAddInitialState,
  job_edit: jobEdit.JobEditInitialState,
  job_list: jobList.JobListInitialState,
  nodeCredential_list: nodeCredentialList.NodeCredentialListInitialState,


  // Entities
  adminKey: adminKeyEntity.AdminKeyEntityInitialState,
  apiTokens: apiToken.ApiTokenEntityInitialState,
  automateSettings: automateSettings.AutomateSettingsEntityInitialState,
  cds: cdsEntity.cdsEntityInitialState,
  clients: clientEntity.ClientEntityInitialState,
  clientDetails: clientDetailsEntity.ClientEntityInitialState,
  clientRunsEntity: clientRuns.ClientRunsEntityInitialState,
  cookbooks: cookbookEntity.CookbookEntityInitialState,
  cookbookDetails: cookbookDetailsEntity.CookbookDetailsEntityInitialState,
  cookbookVersions: cookbookVersionsEntity.CookbookVersionsEntityInitialState,
  dataBag: dataBagEntity.DataBagEntityInitialState,
  dataBagItems: dataBagItemsEntity.DataBagItemsEntityInitialState,
  dataBagItemDetails: dataBagItemDetailsEntity.DataBagItemDetailsEntityInitialState,
  destinations: destinationEntity.DestinationEntityInitialState,
  globalDataFeedConfig: destinationConfigEntity.GlobalConfigEntityInitialState,
  environments: environmentEntity.EnvironmentEntityInitialState,
  environmentDetails: environmentDetailsEntity.EnvironmentEntityInitialState,
  infraNodes: infraNodeEntity.InfraNodeEntityInitialState,
  infraNodeDetails: infraNodeDetailsEntity.InfraNodeEntityInitialState,
  infraRoles: infraRoleEntity.InfraRoleEntityInitialState,
  infraRoleDetails: infraRoleDetailsEntity.InfraRoleEntityInitialState,
  jobs: jobEntity.JobEntityInitialState,
  licenseStatus: license.LicenseStatusEntityInitialState,
  managers: manager.ManagerEntityInitialState,
  nodes: nodesEntity.NodesEntityInitialState,
  nodeRunlist: nodeRunlistEntity.NodeRunlistEntityInitialState,
  notifications: notificationEntity.InitialState,
  notificationRules: NotificationRuleEntity.NotificationRuleEntityInitialState,
  policies: policyEntity.PolicyEntityInitialState,
  policyFiles: policyFileEntity.PolicyFileEntityInitialState,
  policyFileDetails: policyFileDetailsEntity.PolicyFileEntityInitialState,
  policyGroups: policyGroupEntity.PolicyFileEntityInitialState,
  policyGroupDetails: policyGroupDetailsEntity.PolicyFileEntityInitialState,
  profiles: profileEntity.ProfileEntityInitialState,
  projects: projectEntity.ProjectEntityInitialState,
  recipes: recipeEntity.RecipeEntityInitialState,
  revisions: revisionEntity.RevisionEntityInitialState,
  roleEnvironments: roleEnvironmentEntity.RoleEnvironmentEntityInitialState,
  roles: roleEntity.RoleEntityInitialState,
  rules: ruleEntity.RuleEntityInitialState,
  runlist: runlistEntity.RunlistEntityInitialState,
  nodeCredential: nodeCredentialEntity.NodeCredentialEntityInitialState,
  nodeCredentialDetails: nodeCredentialDetailsEntity.NodeCredentialEntityInitialState,
  servers: serverEntity.ServerEntityInitialState,
  orgs: orgEntity.OrgEntityInitialState,
  serviceGroups: serviceGroups.ServiceGroupEntityInitialState,
  teams: teamEntity.TeamEntityInitialState,
  desktops: desktopEntity.desktopEntityInitialState,
  userperms: permEntity.initialState,
  users: userEntity.UserEntityInitialState,
  userPreferences: userPreferencesEntity.UserPreferencesEntityInitialState,
  userSelf: userSelfEntity.UserSelfEntityInitialState
};

export const ngrxReducers = {
  // AOT doesn't like dynamic object property names here
  scanner: scanner.scannerReducer,
  router: routerReducer,
  event_feed: eventFeed.eventFeedReducer,
  projectsFilter: projectsFilter.projectsFilterReducer,
  layout: layout.layoutEntityReducer,

  // UI State
  job_add: jobAdd.jobAddReducer,
  job_edit: jobEdit.jobEditReducer,
  job_list: jobList.jobListReducer,
  integrations_add: integrationsAdd.integrationsAddReducer,
  integrations_detail: integrationsDetail.integrationsDetailReducer,
  integrations_edit: integrationsEdit.integrationsEditReducer,
  nodeCredential_list: nodeCredentialList.nodeCredentialListReducer,


  // Entities
  adminKey: adminKeyEntity.adminKeyEntityReducer,
  apiTokens: apiToken.apiTokenEntityReducer,
  automateSettings: automateSettings.automateSettingsEntityReducer,
  cds: cdsEntity.desktopEntityReducer,
  clients: clientEntity.clientEntityReducer,
  clientDetails: clientDetailsEntity.clientDetailsEntityReducer,
  clientRunsEntity: clientRuns.clientRunsEntityReducer,
  cookbooks: cookbookEntity.cookbookEntityReducer,
  cookbookDetails: cookbookDetailsEntity.cookbookDetailsEntityReducer,
  cookbookVersions: cookbookVersionsEntity.cookbookVersionsEntityReducer,
  credentialEntity: credential.credentialReducer,
  dataBag: dataBagEntity.dataBagEntityReducer,
  dataBagItems: dataBagItemsEntity.dataBagItemsEntityReducer,
  dataBagItemDetails: dataBagItemDetailsEntity.dataBagItemDetailsEntityReducer,
  destinations: destinationEntity.destinationEntityReducer,
  globalDataFeedConfig: destinationConfigEntity.globalConfigEntityReducer,
  environments: environmentEntity.environmentEntityReducer,
  environmentDetails: environmentDetailsEntity.environmentDetailsEntityReducer,
  infraNodes: infraNodeEntity.infraNodeEntityReducer,
  infraNodeDetails: infraNodeDetailsEntity.infraNodeDetailsEntityReducer,
  infraRoles: infraRoleEntity.infraRoleEntityReducer,
  infraRoleDetails: infraRoleDetailsEntity.infraRoleDetailsEntityReducer,
  jobs: jobEntity.jobEntityReducer,
  managers: manager.managerEntityReducer,
  nodes: nodesEntity.nodesEntityReducer,
  nodeRunlist: nodeRunlistEntity.nodeRunlistEntityReducer,
  licenseStatus: license.licenseStatusEntityReducer,
  notifications: notificationEntity.notificationEntityReducer,
  notificationRules: NotificationRuleEntity.notificationRuleEntityReducer,
  policies: policyEntity.policyEntityReducer,
  policyFiles: policyFileEntity.policyFileEntityReducer,
  policyFileDetails: policyFileDetailsEntity.policyFileDetailsEntityReducer,
  policyGroups: policyGroupEntity.policyGroupsEntityReducer,
  policyGroupDetails: policyGroupDetailsEntity.policyGroupDetailsEntityReducer,
  profiles: profileEntity.profileEntityReducer,
  projects: projectEntity.projectEntityReducer,
  recipes: recipeEntity.recipeEntityReducer,
  revisions: revisionEntity.revisionEntityReducer,
  roleEnvironments: roleEnvironmentEntity.roleEnvironmentEntityReducer,
  roles: roleEntity.roleEntityReducer,
  rules: ruleEntity.ruleEntityReducer,
  runlist: runlistEntity.runlistEntityReducer,
  servers: serverEntity.serverEntityReducer,
  orgs: orgEntity.orgEntityReducer,
  nodeCredential: nodeCredentialEntity.nodeCredentialEntityReducer,
  nodeCredentialDetails: nodeCredentialDetailsEntity.nodeCredentialDetailsEntityReducer,
  serviceGroups: serviceGroups.serviceGroupsEntityReducer,
  teams: teamEntity.teamEntityReducer,
  desktops: desktopEntity.desktopEntityReducer,
  userperms: permEntity.permEntityReducer,
  users: userEntity.userEntityReducer,
  userPreferences: userPreferencesEntity.userPreferencesEntityReducer,
  userSelf: userSelfEntity.userSelfEntityReducer
};

// Add other actions as needed if Redux Dev Tools starts crashing in the browser
export const actionSanitizer = (action: Action): Action =>
  action.type === LayoutActionTypes.UPDATE_SIDEBARS && (action as UpdateSidebars).payload
    ? { ...action, payload: '<<LONG_BLOB>>' } as UpdateSidebars
    : action;

// The UPDATE_SIDEBARS action above creates a large chunk of state in the sidebars property.
// Blank it out for Redux Dev Tools to reduce its memory footprint.
export const stateSanitizer = (state: NgrxStateAtom): NgrxStateAtom =>
  state?.layout?.sidebars
    ? {
      ...state,
      layout: {
        ...state.layout,
        sidebars: {}
      }
    }
    : state;
