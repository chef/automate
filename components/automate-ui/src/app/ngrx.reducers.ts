import { Params, RouterStateSnapshot, UrlSegment } from '@angular/router';
import * as router from '@ngrx/router-store';
import { set, get, pipe, map } from 'lodash/fp';
import * as credentials from './pages/+compliance/+credentials/credentials.state';
import * as scanner from './pages/+compliance/+scanner/state/scanner.state';
import * as eventFeed from './services/event-feed/event-feed.reducer';
import * as sidebar from './services/sidebar/sidebar.reducer';
import * as projectsFilter from './services/projects-filter/projects-filter.reducer';

import {
  ApiTokenEntityState,
  apiTokenEntityReducer
} from './entities/api-tokens/api-token.reducer';
import {
  AutomateSettingsEntityState,
  automateSettingsEntityReducer
} from './entities/automate-settings/automate-settings.reducer';
import {
  ClientRunsEntityState,
  clientRunsEntityReducer
} from './entities/client-runs/client-runs.reducer';
import {
  IntegrationsAddState,
  integrationsAddReducer
} from './pages/integrations/add/integration-add.reducer';
import {
  IntegrationsDetailState,
  integrationsDetailReducer
} from './pages/integrations/detail/integrations-detail.reducer';
import {
  IntegrationsEditState,
  integrationsEditReducer
} from './pages/integrations/edit/integrations-edit.reducer';
import {
  LicenseStatusEntityState,
  licenseStatusEntityReducer
} from './entities/license/license.reducer';
import {
  NotificationEntityState,
  notificationEntityReducer
} from './entities/notifications/notification.reducer';

import { CredentialState, credentialReducer } from './entities/credentials/credential.reducer';
import { JobAddState, jobAddReducer } from './pages/job-add/job-add.reducer';
import { JobEditState, jobEditReducer } from './pages/job-edit/job-edit.reducer';
import { JobEntityState, jobEntityReducer } from './entities/jobs/job.reducer';
import { JobListState, jobListReducer } from './pages/job-list/job-list.reducer';
import { ManagerEntityState, managerEntityReducer } from './entities/managers/manager.reducer';
import { PermEntityState, permEntityReducer } from './entities/userperms/userperms.reducer';
import { PolicyEntityState, policyEntityReducer } from './entities/policies/policy.reducer';
import { ProfileEntityState, profileEntityReducer } from './entities/profiles/profile.reducer';
import { ProjectEntityState, projectEntityReducer } from './entities/projects/project.reducer';
import { RoleEntityState, roleEntityReducer } from './entities/roles/role.reducer';
import {
  ServiceGroupEntityState,
  serviceGroupEntityReducer
} from './entities/service-groups/service-groups.reducer';
import { TeamEntityState, teamEntityReducer } from './entities/teams/team.reducer';
import { UserEntityState, userEntityReducer } from './entities/users/user.reducer';

// AOT likely won't allow dynamic object property names here even when the underlying
// typescript bug preventing it is fixed
export interface NgrxStateAtom {
  credentials: credentials.CredentialsState;
  event_feed: eventFeed.EventFeedState;
  router: RouterReducerState;
  scanner: scanner.ScannerState;
  sidebar: sidebar.SidebarState;
  projectsFilter: projectsFilter.ProjectsFilterState;

  // UI State
  integrations_add: IntegrationsAddState;
  integrations_detail: IntegrationsDetailState;
  integrations_edit: IntegrationsEditState;
  job_add: JobAddState;
  job_edit: JobEditState;
  job_list: JobListState;

  // Entities
  apiTokens: ApiTokenEntityState;
  automateSettings: AutomateSettingsEntityState;
  clientRunsEntity: ClientRunsEntityState;
  jobs: JobEntityState;
  licenseStatus: LicenseStatusEntityState;
  managers: ManagerEntityState;
  notifications: NotificationEntityState;
  policies: PolicyEntityState;
  profiles: ProfileEntityState;
  projects: ProjectEntityState;
  roles: RoleEntityState;
  serviceGroups: ServiceGroupEntityState;
  teams: TeamEntityState;
  userperms: PermEntityState;
  users: UserEntityState;
  // Named credentialEntity until we refactor the credentials page
  credentialEntity: CredentialState;
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

const defaultRouterState = {
  state: {
    url: '/',
    queryParams: {},
    params: {},
    fragment: '',
    path: ['/']
  },
  previousRoute: {},
  navigationId: 0
};

export function routerReducer(state = defaultRouterState, action) {
  switch (action.type) {
    case 'ROUTER_NAVIGATION':
      const newRouterState =
        set('previousRoute', get('state', state), router.routerReducer(state, action));
      return newRouterState;
    default:
      return state;
  }
}

export const ngrxReducers = {
  // AOT doesn't like dynamic object property names here
  credentials: credentials.credentialsReducer,
  scanner: scanner.scannerReducer,
  router: routerReducer,
  event_feed: eventFeed.eventFeedReducer,
  projectsFilter: projectsFilter.projectsFilterReducer,
  sidebar: sidebar.sidebarReducer,

  // UI State
  job_add: jobAddReducer,
  job_edit: jobEditReducer,
  job_list: jobListReducer,
  integrations_add: integrationsAddReducer,
  integrations_detail: integrationsDetailReducer,
  integrations_edit: integrationsEditReducer,

  // Entities
  apiTokens: apiTokenEntityReducer,
  automateSettings: automateSettingsEntityReducer,
  clientRunsEntity: clientRunsEntityReducer,
  credentialEntity: credentialReducer,
  jobs: jobEntityReducer,
  managers: managerEntityReducer,
  licenseStatus: licenseStatusEntityReducer,
  notifications: notificationEntityReducer,
  policies: policyEntityReducer,
  profiles: profileEntityReducer,
  projects: projectEntityReducer,
  roles: roleEntityReducer,
  serviceGroups: serviceGroupEntityReducer,
  teams: teamEntityReducer,
  userperms: permEntityReducer,
  users: userEntityReducer
};
