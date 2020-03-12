import { Params, RouterStateSnapshot, UrlSegment } from '@angular/router';
import * as router from '@ngrx/router-store';
import { set, get, pipe, map } from 'lodash/fp';
import * as credentials from './pages/+compliance/+credentials/credentials.state';
import * as scanner from './pages/+compliance/+scanner/state/scanner.state';
import * as eventFeed from './services/event-feed/event-feed.reducer';
import * as sidebar from './services/sidebar/sidebar.reducer';
import * as projectsFilter from './services/projects-filter/projects-filter.reducer';
import * as apiToken from './entities/api-tokens/api-token.reducer';
import * as automateSettings from './entities/automate-settings/automate-settings.reducer';
import * as clientRuns from './entities/client-runs/client-runs.reducer';
import * as cookbookEntity from './entities/cookbooks/cookbook.reducer';
import * as integrationsAdd from './pages/integrations/add/integration-add.reducer';
import * as integrationsDetail from './pages/integrations/detail/integrations-detail.reducer';
import * as integrationsEdit from './pages/integrations/edit/integrations-edit.reducer';
import * as license from './entities/license/license.reducer';
import * as layout from './entities/layout/layout.reducer';
import * as notificationEntity from './entities/notifications/notification.reducer';
import * as credential from './entities/credentials/credential.reducer';
import * as jobAdd from './pages/job-add/job-add.reducer';
import * as jobEdit from './pages/job-edit/job-edit.reducer';
import * as jobEntity from './entities/jobs/job.reducer';
import * as jobList from './pages/job-list/job-list.reducer';
import * as manager from './entities/managers/manager.reducer';
import * as permEntity from './entities/userperms/userperms.reducer';
import * as policyEntity from './entities/policies/policy.reducer';
import * as profileEntity from './entities/profiles/profile.reducer';
import * as projectEntity from './entities/projects/project.reducer';
import * as roleEntity from './entities/roles/role.reducer';
import * as ruleEntity from './entities/rules/rule.reducer';
import * as serverEntity from './entities/servers/server.reducer';
import * as orgEntity from './entities/orgs/org.reducer';
import * as serviceGroups from './entities/service-groups/service-groups.reducer';
import * as teamEntity from './entities/teams/team.reducer';
import * as userEntity from './entities/users/user.reducer';
import * as userSelfEntity from './entities/users/userself.reducer';

// AOT likely won't allow dynamic object property names here even when the underlying
// typescript bug preventing it is fixed
export interface NgrxStateAtom {
  credentials: credentials.CredentialsState;
  event_feed: eventFeed.EventFeedState;
  router: RouterReducerState;
  scanner: scanner.ScannerState;
  sidebar: sidebar.SidebarState;
  layout: layout.LayoutEntityState;
  projectsFilter: projectsFilter.ProjectsFilterState;

  // UI State
  integrations_add: integrationsAdd.IntegrationsAddState;
  integrations_detail: integrationsDetail.IntegrationsDetailState;
  integrations_edit: integrationsEdit.IntegrationsEditState;
  job_add: jobAdd.JobAddState;
  job_edit: jobEdit.JobEditState;
  job_list: jobList.JobListState;

  // Entities
  apiTokens: apiToken.ApiTokenEntityState;
  automateSettings: automateSettings.AutomateSettingsEntityState;
  clientRunsEntity: clientRuns.ClientRunsEntityState;
  cookbooks: cookbookEntity.CookbookEntityState;
  jobs: jobEntity.JobEntityState;
  licenseStatus: license.LicenseStatusEntityState;
  managers: manager.ManagerEntityState;
  notifications: notificationEntity.NotificationEntityState;
  policies: policyEntity.PolicyEntityState;
  profiles: profileEntity.ProfileEntityState;
  projects: projectEntity.ProjectEntityState;
  roles: roleEntity.RoleEntityState;
  rules: ruleEntity.RuleEntityState;
  servers: serverEntity.ServerEntityState;
  orgs: orgEntity.OrgEntityState;
  serviceGroups: serviceGroups.ServiceGroupsEntityState;
  teams: teamEntity.TeamEntityState;
  userperms: permEntity.PermEntityState;
  users: userEntity.UserEntityState;
  userSelf: userSelfEntity.UserSelfEntityState;
  // Named credentialEntity until we refactor the credentials page
  credentialEntity: credential.CredentialState;
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
  credentials: credentials.initialState,
  event_feed: eventFeed.initialState,
  scanner: scanner.initialState,
  sidebar: sidebar.initialState,
  layout: layout.InitialState,
  projectsFilter: projectsFilter.projectsFilterInitialState,

  // UI State
  integrations_add: integrationsAdd.IntegrationsAddInitialState,
  integrations_detail: integrationsDetail.IntegrationsDetailInitialState,
  integrations_edit: integrationsEdit.IntegrationsEditInitialState,
  job_add: jobAdd.JobAddInitialState,
  job_edit: jobEdit.JobEditInitialState,
  job_list: jobList.JobListInitialState,

  // Entities
  apiTokens: apiToken.ApiTokenEntityInitialState,
  automateSettings: automateSettings.AutomateSettingsEntityInitialState,
  clientRunsEntity: clientRuns.ClientRunsEntityInitialState,
  cookbooks: cookbookEntity.CookbookEntityInitialState,
  jobs: jobEntity.JobEntityInitialState,
  licenseStatus: license.LicenseStatusEntityInitialState,
  managers: manager.ManagerEntityInitialState,
  notifications: notificationEntity.InitialState,
  policies: policyEntity.PolicyEntityInitialState,
  profiles: profileEntity.ProfileEntityInitialState,
  projects: projectEntity.ProjectEntityInitialState,
  roles: roleEntity.RoleEntityInitialState,
  rules: ruleEntity.RuleEntityInitialState,
  servers: serverEntity.ServerEntityInitialState,
  orgs: orgEntity.OrgEntityInitialState,
  serviceGroups: serviceGroups.ServiceGroupEntityInitialState,
  teams: teamEntity.TeamEntityInitialState,
  userperms: permEntity.initialState,
  users: userEntity.UserEntityInitialState,
  userSelf: userSelfEntity.UserSelfEntityInitialState
};

export const ngrxReducers = {
  // AOT doesn't like dynamic object property names here
  credentials: credentials.credentialsReducer,
  scanner: scanner.scannerReducer,
  router: routerReducer,
  event_feed: eventFeed.eventFeedReducer,
  projectsFilter: projectsFilter.projectsFilterReducer,
  sidebar: sidebar.sidebarReducer,
  layout: layout.layoutEntityReducer,

  // UI State
  job_add: jobAdd.jobAddReducer,
  job_edit: jobEdit.jobEditReducer,
  job_list: jobList.jobListReducer,
  integrations_add: integrationsAdd.integrationsAddReducer,
  integrations_detail: integrationsDetail.integrationsDetailReducer,
  integrations_edit: integrationsEdit.integrationsEditReducer,

  // Entities
  apiTokens: apiToken.apiTokenEntityReducer,
  automateSettings: automateSettings.automateSettingsEntityReducer,
  clientRunsEntity: clientRuns.clientRunsEntityReducer,
  cookbooks: cookbookEntity.cookbookEntityReducer,
  credentialEntity: credential.credentialReducer,
  jobs: jobEntity.jobEntityReducer,
  managers: manager.managerEntityReducer,
  licenseStatus: license.licenseStatusEntityReducer,
  notifications: notificationEntity.notificationEntityReducer,
  policies: policyEntity.policyEntityReducer,
  profiles: profileEntity.profileEntityReducer,
  projects: projectEntity.projectEntityReducer,
  roles: roleEntity.roleEntityReducer,
  rules: ruleEntity.ruleEntityReducer,
  servers: serverEntity.serverEntityReducer,
  orgs: orgEntity.orgEntityReducer,
  serviceGroups: serviceGroups.serviceGroupsEntityReducer,
  teams: teamEntity.teamEntityReducer,
  userperms: permEntity.permEntityReducer,
  users: userEntity.userEntityReducer,
  userSelf: userSelfEntity.userSelfEntityReducer
};
