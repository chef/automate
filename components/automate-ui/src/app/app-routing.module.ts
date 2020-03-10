import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

// Views
import {
  ApiTokenDetailsComponent
} from './modules/token/token-details/api-token-details.component';
import { ApiTokenListComponent } from './modules/token/token-list/api-token-list.component';
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
import { DatafeedComponent } from './pages/data-feed/data-feed.component';
import { DatafeedFormComponent } from './pages/data-feed-form/data-feed-form.component';
import { SigninComponent } from './pages/signin/signin.component';

// Components
import { AutomateSettingsComponent } from './pages/automate-settings/automate-settings.component';
import { ChefServersListComponent } from './modules/infra-proxy/chef-servers-list/chef-servers-list.component';
import { ChefServerDetailsComponent } from './modules/infra-proxy/chef-server-details/chef-server-details.component';
import { CookbooksListComponent } from './modules/infra-proxy/cookbook-list/cookbooks-list.component';
import { NodeDetailsComponent } from './pages/node-details/node-details.component';
import {
  NodeNoRunsDetailsComponent
} from './pages/node-noruns-details/node-noruns-details.component';
import { PolicyListComponent } from './modules/policy/list/policy-list.component';
import { PolicyDetailsComponent } from './modules/policy/details/policy-details.component';
import { PolicyAddMembersComponent } from './modules/policy/add-members/policy-add-members.component';
import { ProjectDetailsComponent } from './pages/project/details/project-details.component';
import { ProjectListComponent } from './pages/project/list/project-list.component';
import { ProjectRulesComponent } from './pages/project/rules/project-rules.component';
import { RolesListComponent } from './modules/roles/list/roles-list.component';
import { RoleDetailsComponent } from './modules/roles/details/role-details.component';
import { UIComponent } from 'app/ui.component';
import { UserDetailsComponent } from './modules/user/user-details/user-details.component';
import { UserDetailsNonAdminResolve } from './modules/user/user-details/user-details.resolver';

// Services
import { ChefSessionService } from './services/chef-session/chef-session.service';
import { NodeDetailsResolverService } from './services/node-details/node-details-resolver.service';
import {
  NodeNoRunsDetailsResolverService
} from './services/node-details/node-noruns-details-resolver.service';

// Other
import { SettingsLandingComponent } from './pages/settings-landing/settings-landing.component';

const routes: Routes = [
  {
    path: '',
    component: UIComponent,
    canActivate: [ChefSessionService],
    children: [{
      path: '',
      redirectTo: 'dashboards/event-feed',
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
          path: 'teams',
          loadChildren: './modules/team/team.module#TeamModule'
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
          path: 'users',
          loadChildren: './modules/user/user.module#UserModule'
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
          loadChildren: () => import('./pages/+compliance/+credentials/credentials.module')
            .then(m => m.CredentialsModule)
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
        },
        {
          path: 'data-feed',
          children: [
            {
              path: '',
              component: DatafeedComponent
            },
            {
              path: 'form',
              component: DatafeedFormComponent
            },
            {
              path: 'form/:id',
              component: DatafeedFormComponent
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
      loadChildren: () => import('app/pages/+compliance/compliance.module')
        .then(m => m.ComplianceModule)
    },
    {
      path: 'infrastructure',
      children: [
        {
          path: '',
          redirectTo: '/infrastructure/client-runs',
          pathMatch: 'full'
        },
        {
          path: 'client-runs',
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
          path: 'chef-servers',
          children: [
            {
              path: '',
              component: ChefServersListComponent
            },
            {
              path: ':id',
              component: ChefServerDetailsComponent
            },
            {
              path: ':id/org/:orgid/cookbooks',
              component: CookbooksListComponent
            }
          ]
        }
      ]
    },
    {
      path: 'applications/service-groups',
      children: [
        {
          path: '',
          component: ApplicationsComponent
        }
      ]
    },
    {
      path: 'dashboards/event-feed',
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
      loadChildren: () => import('app/pages/component-library/component-library.module')
        .then(m => m.ComponentLibraryModule)
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
    path: 'data-feed',
    pathMatch: 'prefix',
    redirectTo: 'settings/data-feed'
  },
  {
    path: 'compliance/credentials',
    pathMatch: 'prefix',
    redirectTo: 'settings/node-credentials'
  },
  { // used by projects-filter.service.ts
    path: 'reload',
    children: []
  },
  // END Deprecated routes.
  {
    path: '**',
    redirectTo: 'dashboards/event-feed'
  }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
