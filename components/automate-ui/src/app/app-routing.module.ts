import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

// Views
import { ApiTokenDetailsComponent } from './pages/api-token/details/api-token-details.component';
import { ApiTokenListComponent } from './pages/api-token/list/api-token-list.component';
import { ApplicationsComponent } from './pages/applications/applications.component';
import { EventFeedComponent } from './pages/event-feed/event-feed.component';
import { IntegrationsAddComponent } from './pages/integrations/add/integrations-add.component';
import {
  IntegrationsDetailComponent
} from './pages/integrations/detail/integrations-detail.component';
import { IntegrationsEditComponent } from './pages/integrations/edit/integrations-edit.component';
import { IntegrationsListComponent } from './pages/integrations/list/integrations-list.component';
import { JobAddComponent } from './pages/job-add/job-add.component';
import { JobEditComponent } from './pages/job-edit/job-edit.component';
import { ClientRunsComponent } from './pages/client-runs/client-runs.component';
import { NotificationsComponent } from './pages/notifications/notifications.component';
import { NotificationFormComponent } from './pages/notification-form/notification-form.component';
import { SigninComponent } from './pages/signin/signin.component';
import { TeamAddUsersComponent } from './pages/team-add-users/team-add-users.component';
import { TeamDetailsComponent } from './pages/team-details/team-details.component';
import { TeamManagementComponent } from './pages/team-management/team-management.component';
import { UserDetailsComponent } from './pages/user-details/user-details.component';
import { UserManagementComponent } from './pages/user-management/user-management.component';

// Components
import { AutomateSettingsComponent } from './pages/automate-settings/automate-settings.component';
import { NodeDetailsComponent } from './pages/node-details/node-details.component';
import {
  NodeNoRunsDetailsComponent
} from './pages/node-noruns-details/node-noruns-details.component';
import { PolicyListComponent } from './pages/policy/list/policy-list.component';
import { PolicyDetailsComponent } from './pages/policy/details/policy-details.component';
import { PolicyAddMembersComponent } from './pages/policy/add-members/policy-add-members.component';
import { ProjectDetailsComponent } from './pages/project/details/project-details.component';
import { ProjectListComponent } from './pages/project/list/project-list.component';
import { ProjectRulesComponent } from './pages/project/rules/project-rules.component';
import { RolesListComponent } from './pages/roles/list/roles-list.component';
import { RoleDetailsComponent } from './pages/roles/details/role-details.component';
import { UIComponent } from 'app/ui.component';

// Services
import { ChefSessionService } from './services/chef-session/chef-session.service';
import { NodeDetailsResolverService } from './services/node-details/node-details-resolver.service';
import {
  NodeNoRunsDetailsResolverService
} from './services/node-details/node-noruns-details-resolver.service';

// Other
import { UserDetailsNonAdminResolve } from './pages/user-details/user-details.resolver';
import { SettingsLandingComponent } from './pages/settings-landing/settings-landing.component';

const routes: Routes = [
  {
    path: '',
    component: UIComponent,
    canActivate: [ChefSessionService],
    children: [{
      path: '',
      redirectTo: 'event-feed',
      pathMatch: 'full'
    },
    {
      path: 'settings',
      children: [
        {
          path: '',
          pathMatch: 'full',
          component: SettingsLandingComponent
        },
        {
          path: 'node-lifecycle',
          component: AutomateSettingsComponent
        },
        {
          path: 'tokens',
          component: ApiTokenListComponent
        },
        {
          path: 'tokens/:id',
          component: ApiTokenDetailsComponent
        },
        {
          path: 'teams',
          component: TeamManagementComponent
        },
        {
          path: 'teams/:id',
          component: TeamDetailsComponent
        },
        {
          path: 'teams/:id/add-users',
          component: TeamAddUsersComponent,
          data: { hideNavBar: true }
        },
        {
          path: 'users',
          component: UserManagementComponent
        },
        {
          path: 'users/:id',
          component: UserDetailsComponent
        },
        {
          path: 'policies',
          component: PolicyListComponent
        },
        {
          path: 'policies/:id',
          component: PolicyDetailsComponent
        },
        {
          path: 'policies/:id/add-members',
          component: PolicyAddMembersComponent,
          data: { hideNavBar: true }
        },
        {
          path: 'projects',
          component: ProjectListComponent
        },
        {
          path: 'projects/:id',
          component: ProjectDetailsComponent
        },
        {
          path: 'projects/:id/rules',
          component: ProjectRulesComponent,
          data: { hideNavBar: true }
        },
        {
          path: 'projects/:id/rules/:ruleid',
          component: ProjectRulesComponent,
          data: { hideNavBar: true }
        },
        {
          path: 'roles',
          component: RolesListComponent
        },
        {
          path: 'roles/:id',
          component: RoleDetailsComponent
        },
        {
          path: 'node-integrations',
          children: [
            {
              path: '',
              component: IntegrationsListComponent
            },
            {
              path: 'add',
              component: IntegrationsAddComponent
            },
            {
              path: 'edit/:id',
              component: IntegrationsEditComponent
            },
            {
              path: ':id',
              component: IntegrationsDetailComponent
            }
          ]
        },
        {
          path: 'node-credentials',
          loadChildren: './pages/+compliance/+credentials/credentials.module#CredentialsModule'
        },
        {
          path: 'notifications',
          children: [
            {
              path: '',
              component: NotificationsComponent
            },
            {
              path: 'form',
              component: NotificationFormComponent
            },
            {
              path: 'form/:id',
              component: NotificationFormComponent
            }
          ]
        }
      ]
    },
    {
      path: 'user-details/:id',
      component: UserDetailsComponent,
      resolve: { isNonAdmin: UserDetailsNonAdminResolve }
    },
    {
      path: 'compliance',
      loadChildren: 'app/pages/+compliance/compliance.module#ComplianceModule'
    },
    {
      path: 'infrastructure/client-runs',
      children: [
        {
          path: '',
          component: ClientRunsComponent
        },
        {
          path: ':node-id/missing-runs',
          component: NodeNoRunsDetailsComponent,
          resolve: {
            node: NodeNoRunsDetailsResolverService
          }
        },
        {
          path: ':node-id/runs/:run-id',
          component: NodeDetailsComponent,
          resolve: {
            nodeRun: NodeDetailsResolverService
          }
        }
      ]
    },
    {
      path: 'applications',
      children: [
        {
          path: '',
          component: ApplicationsComponent
        }
      ]
    },
    {
      path: 'event-feed',
      children: [
        {
          path: '',
          component: EventFeedComponent
        }
      ]
    },
    {
      path: 'profiles',
      redirectTo: '/compliance/compliance-profiles',
      pathMatch: 'full'
    },
    {
      path: 'profiles/profile-details',
      redirectTo: '/compliance/compliance-profiles/profile-details',
      pathMatch: 'full'
    },
    {
      path: 'jobs',
      children: [
        {
          path: '',
          // For now we are redirecting to the old jobs list. When we get the UX
          // worked out we can stop redirecting and use the new JobListComponent.
          redirectTo: '/compliance/scan-jobs/jobs',
          pathMatch: 'full'
        },
        {
          path: 'add',
          component: JobAddComponent
        },
        {
          path: ':id/edit',
          component: JobEditComponent
        }
      ]
    },
    {
      // TODO: we'd like this to only load in dev mode, but it doesn't work without require, and
      // we don't want require in the code base because it leads people to use libraries that
      // mutate the dom outside of angular's change detection.
      path: 'component_library',
      loadChildren: 'app/pages/component-library/component-library.module#ComponentLibraryModule'
    }
    ]
  },
  { // This component does not use app.component.html -- it's using the bare template,
    // since for signin, we don't want any of chef-session etc initialized.
    path: 'signin',
    component: SigninComponent
  },
  // START Deprecated routes. Redirected for backwards compatibility.
  {
    path: 'admin/settings',
    pathMatch: 'full',
    redirectTo: 'settings/node-lifecycle'
  },
  {
    path: 'admin',
    pathMatch: 'prefix',
    redirectTo: 'settings'
  },
  {
    path: 'integrations',
    pathMatch: 'prefix',
    redirectTo: 'settings/node-integrations'
  },
  {
    path: 'notifications',
    pathMatch: 'prefix',
    redirectTo: 'settings/notifications'
  },
  {
    path: 'compliance/credentials',
    pathMatch: 'prefix',
    redirectTo: 'settings/node-credentials'
  },
  { // ued by projects-filter.service.ts
    path: 'reload',
    children: []
  },
  // END Deprecated routes.
  { // everything unknown goes to client runs
    path: '**',
    redirectTo: 'infrastructure/client-runs'
  }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
