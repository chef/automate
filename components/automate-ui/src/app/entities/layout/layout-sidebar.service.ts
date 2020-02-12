import { Injectable, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { Subject, Observable, BehaviorSubject } from 'rxjs';
import { forEach } from 'lodash';
// import { takeUntil } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { isProductDeployed } from 'app/staticConfig';
import { isIAMv2 } from 'app/entities/policies/policy.selectors';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { clientRunsWorkflowEnabled } from 'app/entities/client-runs/client-runs.selectors';
import * as fromClientRuns from 'app/entities/client-runs/client-runs.reducer';
import { UpdateSidebars } from './layout.actions';
import { Sidebars } from './layout.model';

@Injectable({
    providedIn: 'root'
})
export class LayoutSidebarService implements OnInit, OnDestroy {
    public applicationsFeatureFlagOn: boolean;
    public chefInfraServerViewsFeatureFlagOn: boolean;
    public isIAMv2: Observable<boolean>;
    public ServiceNowFeatureFlagOn: boolean;
    private activeSidebar: string;
    private workflowEnabled: Observable<boolean>;
    private isDestroyed = new Subject<boolean>();
    private sidebar: Sidebars;

    constructor(
        private store: Store<NgrxStateAtom>,
        private clientRunsStore: Store<fromClientRuns.ClientRunsEntityState>,
        private featureFlagsService: FeatureFlagsService
    ) {
        this.applicationsFeatureFlagOn = this.featureFlagsService.getFeatureStatus('applications');
        this.ServiceNowFeatureFlagOn = this.featureFlagsService.getFeatureStatus('servicenow_cmdb');
        this.isIAMv2 = this.store.select(isIAMv2);
        this.workflowEnabled = this.clientRunsStore.select(clientRunsWorkflowEnabled);
        // this.store.select(isIAMv2).pipe(
        //   takeUntil(this.isDestroyed)
        // ).subscribe(
        //     (IAMv2) => {
        //         this.isIAMv2 = IAMv2;
        //         this.updateSidebars();
        //     }
        // );
        // this.clientRunsStore.select(clientRunsWorkflowEnabled).pipe(
        //   takeUntil(this.isDestroyed)
        // ).subscribe(
        //     (workflowEnabled) => {
        //         this.workflowEnabled = workflowEnabled;
        //         this.updateSidebars();
        //     }
        // );
    }

    populateSidebar() {
      const sidebar = {
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
          ],
          visible: this.applicationsFeatureFlagOn
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
              visible: this.chefInfraServerViewsFeatureFlagOn
            },
            {
              name: 'Workflow',
              icon: 'local_shipping',
              route: '/workflow',
              visible: this.workflowEnabled
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
                allOf: [['/compliance/scanner/jobs', 'post'],
                ['/compliance/scanner/jobs/search', 'post']]
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
                visible: this.ServiceNowFeatureFlagOn
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
                  allOf: ['/auth/users', 'get']
                }
              },
              {
                name: 'Teams',
                icon: 'people',
                route: '/settings/teams',
                authorized: {
                  allOf: ['/auth/teams', 'get']
                }
              },
              {
                name: 'API Tokens',
                icon: 'vpn_key',
                route: '/settings/tokens',
                authorized: {
                  anyOf: [['/auth/tokens', 'get'],
                  ['/iam/v2/tokens', 'get']]
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
              route: '.'
            }
          ]
        }]
      };
      this.sidebar = this.updateVisibleValues(sidebar);
    }

    ngOnInit() {
      this.updateSidebars();
    }

    ngOnDestroy() {
      this.isDestroyed.next(true);
      this.isDestroyed.complete();
    }

    private updateVisibleValues(sidebar): Sidebars {
      forEach(sidebar, (_value, key) => {
        const menuGroups = sidebar[key];
        forEach(menuGroups, (menuGroup) => {
          if (menuGroup && menuGroup.name) {
            this.setVisibleValuesToObservables(menuGroup);
            if (menuGroup.items) {
              forEach(menuGroup.items, (menuItem) => {
                this.setVisibleValuesToObservables(menuItem);
              });
            }
            menuGroup.visible = this.setMenuGroupVisibility(menuGroup);
          }
        });
      });
      return sidebar;
    }

    private setVisibleValuesToObservables(item: any): void {
      if (item.visible === undefined) {
        item.visible = new BehaviorSubject(true);
      } else if (!item.visible.subscribe) {
        item.visible = new BehaviorSubject(item.visible);
      }
    }

    private setMenuGroupVisibility(menuGroup: any): any {
      let anyMenuItemsVisible = false;
      forEach(menuGroup.items, (menuItem) => {
        if (menuItem.visible === undefined) {
          anyMenuItemsVisible = true;
        } else if (menuItem.visible.subscribe) {
          menuItem.visible.subscribe((value) => {
            anyMenuItemsVisible = value ? true : anyMenuItemsVisible;
          });
        }
      });
      return new BehaviorSubject(anyMenuItemsVisible);
    }

    // For sidebars, we are constraining <app-authorized> attributes:
    // 1. You cannot use `not`.

    public updateSidebars(sidebarName?: string): void {
      this.populateSidebar();
      this.activeSidebar = sidebarName || this.activeSidebar;
      this.sidebar.active = this.activeSidebar;
      this.store.dispatch(new UpdateSidebars(this.sidebar));
    }
}
