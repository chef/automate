import { Injectable } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable, BehaviorSubject } from 'rxjs';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { isProductDeployed } from 'app/staticConfig';
import { clientRunsWorkflowEnabled } from 'app/entities/client-runs/client-runs.selectors';
import * as fromClientRuns from 'app/entities/client-runs/client-runs.reducer';
import { UpdateSidebars } from './layout.actions';
import { Sidebars, MenuItem } from './layout.model';
import { MenuItemGroup } from 'app/entities/layout/layout.model';

@Injectable({
    providedIn: 'root'
})
export class LayoutSidebarService {
    public ServiceNowFeatureFlagOn: boolean;
    private activeSidebar: string;
    private workflowEnabled$: Observable<boolean>;
    private sidebars: Sidebars;

    constructor(
        private store: Store<NgrxStateAtom>,
        private clientRunsStore: Store<fromClientRuns.ClientRunsEntityState>
    ) {
        this.workflowEnabled$ = this.clientRunsStore.select(clientRunsWorkflowEnabled);
        this.updateSidebars();
    }

    private populateSidebar() {
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
              name: 'Chef Infra Servers',
              customIcon: 'chef-servers-icon',
              route: '/infrastructure/chef-servers',
              authorized: {
                anyOf: [['/api/v0/infra/servers', 'get']]
              }
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
                allOf: [['/api/v0/compliance/reporting/stats/summary', 'post'],
                ['/api/v0/compliance/reporting/stats/failures', 'post'],
                ['/api/v0/compliance/reporting/stats/trend', 'post']]
              }
            },
            {
              name: 'Scan Jobs',
              icon: 'wifi_tethering',
              route: '/compliance/scan-jobs',
              authorized: {
                allOf: ['/api/v0/compliance/scanner/jobs/search', 'post']
              }
            },
            {
              name: 'Profiles',
              icon: 'library_books',
              route: '/compliance/compliance-profiles',
              authorized: {
                allOf: [['/api/v0/compliance/profiles/search', 'post']]
              }
            }
          ]
        }],
        settings: [
          {
            name: 'General Settings',
            items: [
              {
                name: 'Notifications',
                icon: 'notifications',
                route: '/settings/notifications',
                authorized: {
                  anyOf: ['/api/v0/notifications/rules', 'get']
                }
              },
              {
                name: 'Data Feeds',
                icon: 'assignment',
                route: '/settings/data-feeds',
                authorized: {
                  anyOf: ['/api/v0/datafeed/destinations', 'post']
                }
              },
              {
                name: 'Data Lifecycle',
                icon: 'storage',
                route: '/settings/data-lifecycle',
                authorized: {
                  anyOf: ['/api/v0/retention/nodes/status', 'get']
                }
              }
            ]
          },
          {
            name: 'Node Management',
            items: [
              {
                name: 'Node Integrations',
                icon: 'settings_input_component',
                route: '/settings/node-integrations',
                authorized: {
                  anyOf: ['/api/v0/nodemanagers/search', 'post']
                }
              },
              {
                name: 'Node Credentials',
                icon: 'vpn_key',
                iconRotation: 90,
                route: '/settings/node-credentials',
                authorized: {
                  anyOf: ['/api/v0/secrets/search', 'post']
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
                  allOf: ['/apis/iam/v2/users', 'get']
                }
              },
              {
                name: 'Teams',
                icon: 'people',
                route: '/settings/teams',
                authorized: {
                  allOf: ['/apis/iam/v2/teams', 'get']
                }
              },
              {
                name: 'API Tokens',
                icon: 'vpn_key',
                route: '/settings/tokens',
                authorized: {
                  allOf: ['/apis/iam/v2/tokens', 'get']
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
                  allOf: ['/apis/iam/v2/policies', 'get']
                }
              },
              {
                name: 'Roles',
                icon: 'assignment_ind',
                route: '/settings/roles',
                authorized: {
                  allOf: ['/apis/iam/v2/roles', 'get']
                }
              },
              {
                name: 'Projects',
                icon: 'work',
                route: '/settings/projects',
                authorized: {
                  allOf: ['/apis/iam/v2/projects', 'get']
                }
              }
            ]
          },
          {
            name: 'Single Sign-On',
            items: [
              {
                name: 'Configure',
                icon: 'security',
                route: '/settings/sso',
                authorized: { // using /policies until /sso is there or else it wont show.
                  allOf: ['/apis/iam/v2/policies', 'get']
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
          menuGroup.visible$ = menuGroup.visible$ || new BehaviorSubject(true);
        });
      });
      return sidebars;
    }

    private setVisibleValuesToObservables(item: MenuItem | MenuItemGroup): void {
      if (item.visible$ === undefined) {
        item.visible$ = new BehaviorSubject(true);
      }
    }

    public updateSidebars(sidebarName?: string): void {
      this.populateSidebar();
      this.activeSidebar = sidebarName || this.activeSidebar;
      this.sidebars.active = this.activeSidebar;
      this.store.dispatch(new UpdateSidebars(this.sidebars));
    }
}
