import { Injectable } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable, BehaviorSubject } from 'rxjs';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { isProductDeployed } from 'app/staticConfig';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { clientRunsWorkflowEnabled } from 'app/entities/client-runs/client-runs.selectors';
import * as fromClientRuns from 'app/entities/client-runs/client-runs.reducer';
import { UpdateSidebars } from './layout.actions';
import { Sidebars } from './layout.model';
import { MenuItemGroup } from 'app/entities/layout/layout.model';

@Injectable({
    providedIn: 'root'
})
export class LayoutSidebarService {
    public chefInfraServerViewsFeatureFlagOn: boolean;
    public ServiceNowFeatureFlagOn: boolean;
    private activeSidebar: string;
    private workflowEnabled$: Observable<boolean>;
    private sidebars: Sidebars;

    constructor(
        private store: Store<NgrxStateAtom>,
        private clientRunsStore: Store<fromClientRuns.ClientRunsEntityState>,
        private featureFlagsService: FeatureFlagsService
    ) {
        this.ServiceNowFeatureFlagOn = this.featureFlagsService.getFeatureStatus('servicenow_cmdb');
        this.chefInfraServerViewsFeatureFlagOn = this.featureFlagsService.getFeatureStatus('chefInfraServerViews');
        this.workflowEnabled$ = this.clientRunsStore.select(clientRunsWorkflowEnabled);
        this.updateSidebars();
    }

    populateSidebar() {
      const sidebars: Sidebars = {
        active: '',
        dashboards: [{
          name: 'Dashboards',
          items: [
            {
              name: 'Event Feed',
              icon: 'today',
              route: '/dashboards/event-feed'
            }
          ]
        }],
        applications: [{
          name: 'Applications',
          items: [
            {
              name: 'Service Groups',
              icon: 'group_work',
              route: '/applications/service-groups'
            },
            {
              name: 'Habitat Builder',
              icon: 'build',
              route: isProductDeployed('builder') ? '/bldr' : 'https://bldr.habitat.sh',
              openInNewPage: true
            }
          ]
        }],
        infrastructure: [{
          name: 'Infrastructure',
          items: [
            {
              name: 'Client Runs',
              icon: 'storage',
              route: '/infrastructure/client-runs'
            },
            {
              name: 'Chef Servers',
              icon: 'storage',
              route: '/infrastructure/chef-servers',
              authorized: {
                anyOf: [['/infra/servers', 'get']]
              },
              visible$: new BehaviorSubject(this.chefInfraServerViewsFeatureFlagOn)
            },
            {
              name: 'Workflow',
              icon: 'local_shipping',
              route: '/workflow',
              visible$: this.workflowEnabled$
            }
          ]
        }],
        compliance: [{
          name: 'Compliance',
          items: [
            {
              name: 'Reports',
              icon: 'equalizer',
              route: '/compliance/reports',
              authorized: {
                allOf: [['/compliance/reporting/stats/summary', 'post'],
                ['/compliance/reporting/stats/failures', 'post'],
                ['/compliance/reporting/stats/trend', 'post']]
              }
            },
            {
              name: 'Scan Jobs',
              icon: 'wifi_tethering',
              route: '/compliance/scan-jobs',
              authorized: {
                allOf: ['/compliance/scanner/jobs/search', 'post']
              }
            },
            {
              name: 'Profiles',
              icon: 'library_books',
              route: '/compliance/compliance-profiles',
              authorized: {
                allOf: [['/compliance/profiles/search', 'post']]
              }
            }
          ]
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
                  anyOf: ['/notifications/rules', 'get']
                }
              },
              {
                name: 'Data Feeds',
                icon: 'assignment',
                route: '/settings/data-feed',
                authorized: {
                  anyOf: ['/datafeed/destinations', 'post']
                },
                visible$: new BehaviorSubject(this.ServiceNowFeatureFlagOn)
              },
              {
                name: 'Node Integrations',
                icon: 'settings_input_component',
                route: '/settings/node-integrations',
                authorized: {
                  anyOf: ['/nodemanagers/search', 'post']
                }
              },
              {
                name: 'Node Credentials',
                icon: 'vpn_key',
                iconRotation: 90,
                route: '/settings/node-credentials',
                authorized: {
                  anyOf: ['/secrets/search', 'post']
                }
              },
              {
                name: 'Node Lifecycle',
                icon: 'storage',
                route: '/settings/node-lifecycle',
                authorized: {
                  anyOf: ['/retention/nodes/status', 'get']
                }
              }
            ]
          },
          {
            name: 'Identity',
            items: [
              {
                name: 'Users',
                icon: 'person',
                route: '/settings/users',
                authorized: {
                  allOf: ['/iam/v2/users', 'get']
                }
              },
              {
                name: 'Teams',
                icon: 'people',
                route: '/settings/teams',
                authorized: {
                  allOf: ['/iam/v2/teams', 'get']
                }
              },
              {
                name: 'API Tokens',
                icon: 'vpn_key',
                route: '/settings/tokens',
                authorized: {
                  allOf: ['/iam/v2/tokens', 'get']
                }
              }
            ]
          },
          {
            name: 'Access Management',
            items: [
              {
                name: 'Policies',
                icon: 'security',
                route: '/settings/policies',
                authorized: {
                  allOf: ['/iam/v2/policies', 'get']
                }
              },
              {
                name: 'Roles',
                icon: 'assignment_ind',
                route: '/settings/roles',
                authorized: {
                  allOf: ['/iam/v2/roles', 'get']
                }
              },
              {
                name: 'Projects',
                icon: 'work',
                route: '/settings/projects',
                authorized: {
                  allOf: ['/iam/v2/projects', 'get']
                }
              }
            ]
          }
        ],
        profile: [{
          name: 'User Menu',
          items: [
            {
              name: 'Profile',
              icon: 'person',
              route: '.'
            }
          ]
        }]
      };
      this.sidebars = this.updateVisibleValues(sidebars);
    }

    private updateVisibleValues(sidebars: Sidebars): Sidebars {
      Object.values(sidebars)
      .filter(menuGroups => menuGroups instanceof Array)
      .forEach((menuGroups: MenuItemGroup[]) => {
        menuGroups.forEach(menuGroup => {
          this.setVisibleValuesToObservables(menuGroup);
          if (menuGroup.items) {
            menuGroup.items.forEach(menuItem => {
              this.setVisibleValuesToObservables(menuItem);
            });
          }
          menuGroup.visible$ = menuGroup.visible$ ? menuGroup.visible$ : new BehaviorSubject(true);
        });
      });
      return sidebars;
    }

    private setVisibleValuesToObservables(item: any): void {
      if (item.visible$ === undefined) {
        item.visible$ = new BehaviorSubject(true);
      } else if (!item.visible$.subscribe) {
        item.visible$ = new BehaviorSubject(item.visible$);
      }
    }

    // For sidebars, we are constraining <app-authorized> attributes:
    // 1. You cannot use `not`.

    public updateSidebars(sidebarName?: string): void {
      this.populateSidebar();
      this.activeSidebar = sidebarName || this.activeSidebar;
      this.sidebars.active = this.activeSidebar;
      this.store.dispatch(new UpdateSidebars(this.sidebars));
    }
}
