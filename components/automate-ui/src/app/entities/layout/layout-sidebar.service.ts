import { Injectable, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { isProductDeployed } from 'app/staticConfig';
import { isIAMv2 } from 'app/entities/policies/policy.selectors';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { clientRunsWorkflowEnabled } from 'app/entities/client-runs/client-runs.selectors';
import * as fromClientRuns from 'app/entities/client-runs/client-runs.reducer';
import { UpdateSidebars } from './layout.actions';

@Injectable({
    providedIn: 'root'
})
export class LayoutSidebarService implements OnInit, OnDestroy {
    public applicationsFeatureFlagOn: boolean;
    public chefInfraServerViewsFeatureFlagOn: boolean;
    public isIAMv2: boolean;
    public ServiceNowFeatureFlagOn: boolean;
    private activeSidebar: string;
    private workflowEnabled: boolean;
    private isDestroyed = new Subject<boolean>();
    private sidebar = {
      active: '',
      dashboards: [{
        name: 'Dashboards',
        items: [
          {
            name: 'Event Feed',
            icon: 'today',
            route: '/dashboards/event-feed',
            visible: true
          }
        ],
        visible: true
      }],
      applications: [{
        name: 'Applications',
        items: [
          {
            name: 'Service Groups',
            icon: 'group_work',
            route: '/applications/service-groups',
            visible: true
          },
          {
            name: 'Habitat Builder',
            icon: 'build',
            route: isProductDeployed('builder') ? '/bldr' : 'https://bldr.habitat.sh',
            visible: true,
            openInNewPage: true
          }
        ],
        visible: this.applicationsFeatureFlagOn
      }],
      infrastructure: [{
        name: 'Infrastructure',
        items: [
          {
            name: 'Client Runs',
            icon: 'storage',
            route: '/infrastructure/client-runs',
            visible: true
          },
          {
            name: 'Chef Servers',
            icon: 'storage',
            route: '/infrastructure/chef-servers',
            visible: this.chefInfraServerViewsFeatureFlagOn
          },
          {
            name: 'Workflow',
            icon: 'local_shipping',
            route: '/workflow',
            visible: this.workflowEnabled
          }
        ],
        visible: true
      }],
      compliance: [{
        name: 'Compliance',
        items: [
          {
            name: 'Reports',
            icon: 'equalizer',
            route: '/compliance/reports',
            authorized: {
              name: 'reports',
              allOf: [['/compliance/reporting/stats/summary', 'post'],
              ['/compliance/reporting/stats/failures', 'post'],
              ['/compliance/reporting/stats/trend', 'post']]
            },
            visible: true
          },
          {
            name: 'Scan Jobs',
            icon: 'wifi_tethering',
            route: '/compliance/scan-jobs',
            authorized: {
              name: 'reports',
              allOf: [['/compliance/scanner/jobs', 'post'],
              ['/compliance/scanner/jobs/search', 'post']]
            },
            visible: true
          },
          {
            name: 'Profiles',
            icon: 'library_books',
            route: '/compliance/compliance-profiles',
            authorized: {
              name: 'profiles',
              allOf: [['/compliance/profiles/search', 'post']]
            },
            visible: true
          }
        ],
        visible: true
      }],
      settings: [
        {
          name: 'Node Management',
          items: [
            {
              name: 'Notifications',
              icon: 'notifications',
              route: '/settings/notifications',
              authorized: {
                name: 'notifications',
                anyOf: ['/notifications/rules', 'get']
              },
              visible: true
            },
            {
              name: 'Data Feeds',
              icon: 'assignment',
              route: '/settings/data-feed',
              authorized: {
                name: 'data_feed',
                anyOf: ['/datafeed/destinations', 'post']
              },
              visible: this.ServiceNowFeatureFlagOn
            },
            {
              name: 'Node Integrations',
              icon: 'settings_input_component',
              route: '/settings/node-integrations',
              authorized: {
                name: 'integrations',
                anyOf: ['/nodemanagers/search', 'post']
              },
              visible: true
            },
            {
              name: 'Node Credentials',
              icon: 'vpn_key',
              iconRotation: 90,
              route: '/settings/node-credentials',
              authorized: {
                name: 'credentials',
                anyOf: ['/secrets/search', 'post']
              },
              visible: true
            },
            {
              name: 'Node Lifecycle',
              icon: 'storage',
              route: '/settings/node-lifecycle',
              authorized: {
                name: 'lifecycle',
                anyOf: ['/retention/nodes/status', 'get']
              },
              visible: true
            }
          ],
          visible: true
        },
        {
          name: 'Identity',
          items: [
            {
              name: 'Users',
              icon: 'person',
              route: '/settings/users',
              authorized: {
                name: 'users',
                allOf: ['/auth/users', 'get']
              },
              visible: true
            },
            {
              name: 'Teams',
              icon: 'people',
              route: '/settings/teams',
              authorized: {
                name: 'teams',
                allOf: ['/auth/teams', 'get']
              },
              visible: true
            },
            {
              name: 'API Tokens',
              icon: 'vpn_key',
              route: '/settings/tokens',
              authorized: {
                name: 'tokens',
                anyOf: [['/auth/tokens', 'get'],
                ['/iam/v2/tokens', 'get']]
              },
              visible: true
            }
          ],
          visible: true
        },
        {
          name: 'Access Management',
          items: [
            {
              name: 'Policies',
              icon: 'security',
              route: '/settings/policies',
              authorized: {
                name: 'policies',
                allOf: ['/iam/v2/policies', 'get']
              },
              visible: true
            },
            {
              name: 'Roles',
              icon: 'assignment_ind',
              route: '/settings/roles',
              authorized: {
                name: 'roles',
                allOf: ['/iam/v2/roles', 'get']
              },
              visible: true
            },
            {
              name: 'Projects',
              icon: 'work',
              route: '/settings/projects',
              authorized: {
                name: 'projects',
                allOf: ['/iam/v2/projects', 'get']
              },
              visible: true
            }
          ],
          visible: this.isIAMv2
        }
      ],
      profile: [{
        name: 'User Menu',
        items: [
          {
            name: 'Profile',
            icon: 'person',
            route: '.',
            visible: true
          }
        ],
        visible: true
      }]
    };

    constructor(
        private store: Store<NgrxStateAtom>,
        private clientRunsStore: Store<fromClientRuns.ClientRunsEntityState>,
        private featureFlagsService: FeatureFlagsService
    ) {
        this.applicationsFeatureFlagOn = this.featureFlagsService.getFeatureStatus('applications');
        this.ServiceNowFeatureFlagOn = this.featureFlagsService.getFeatureStatus('servicenow_cmdb');
        this.store.select(isIAMv2).pipe(
          takeUntil(this.isDestroyed)
        ).subscribe(
            (IAMv2) => {
                this.isIAMv2 = IAMv2;
                this.updateSidebars();
            }
        );
        this.clientRunsStore.select(clientRunsWorkflowEnabled).pipe(
          takeUntil(this.isDestroyed)
        ).subscribe(
            (workflowEnabled) => {
                this.workflowEnabled = workflowEnabled;
                this.updateSidebars();
            }
        );
    }

    ngOnInit() {
      this.updateSidebars();
    }

    ngOnDestroy() {
      this.isDestroyed.next(true);
      this.isDestroyed.complete();
    }

    // For sidebars, we are constraining <app-authorized> attributes:
    // 1. You can use `anyOf` or `allOf` but not both.
    // 2. You cannot use `not`.

    public updateSidebars(sidebarName?: string): void {
      this.activeSidebar = sidebarName || this.activeSidebar;
      this.sidebar.active = this.activeSidebar;
      this.store.dispatch(new UpdateSidebars(this.sidebar));
    }
}
