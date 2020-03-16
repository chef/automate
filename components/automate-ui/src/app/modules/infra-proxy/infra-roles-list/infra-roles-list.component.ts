import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { Org } from 'app/entities/orgs/org.model';
import { GetOrg } from 'app/entities/orgs/org.actions';
import { EntityStatus, allLoaded } from 'app/entities/entities';
import { identity, isNil } from 'lodash/fp';

import { Observable, Subject, combineLatest } from 'rxjs';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import {
    getStatus, updateStatus, orgFromRoute
  } from 'app/entities/orgs/org.selectors';
  import { GetRolesForOrg } from 'app/entities/infra-roles/infra-role.action';

  import {
allInfraRoles,
    getAllStatus as getAllRolesForOrgStatus
  } from 'app/entities/infra-roles/infra-role.selectors';
  import { Router } from '@angular/router';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
  export type OrgTabName = 'roles' | 'details';

@Component({
    selector: 'app-infra-roles-list',
    templateUrl: './infra-roles-list.component.html',
    styleUrls: ['./infra-roles-list.component.scss']
  })
  export class InfraRolesListComponent implements OnInit, OnDestroy {
    public org: Org;
    public roles: InfraRole[] = [];
    public url: string;
    public serverId;
  public OrgId;
  private isDestroyed = new Subject<boolean>();
  public loading$: Observable<boolean>;
  public isLoading = true;
  public tabValue: OrgTabName = 'roles';


  
    constructor(
        private store: Store<NgrxStateAtom>,
        private layoutFacade: LayoutFacadeService,
        private router: Router
      ) { }
    
      ngOnInit() {
        this.layoutFacade.showSidebar(Sidebar.Infrastructure);

        this.store.select(routeURL).pipe()
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      this.tabValue = (fragment === 'details') ? 'details' : 'roles';
    });

    combineLatest([
        this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
        this.store.select(routeParams).pipe(pluck('orgid'), filter(identity))
      ]).pipe(
        takeUntil(this.isDestroyed)
      ).subscribe(([server_id, org_id]: string[]) => {
        this.serverId = server_id;
        this.OrgId = org_id;
        this.store.dispatch(new GetOrg({ server_id: server_id, id: org_id }));
        this.store.dispatch(new GetRolesForOrg({
          server_id: server_id, org_id: org_id
        }));
      });

      combineLatest([
        this.store.select(getStatus),
        this.store.select(updateStatus),
        this.store.select(getAllRolesForOrgStatus)
      ]).pipe(
        takeUntil(this.isDestroyed)
      ).subscribe(([getOrgSt, updateSt, getRolesSt]) => {
          this.isLoading =
            !allLoaded([getOrgSt, getRolesSt]) || updateSt === EntityStatus.loading;
        });
        
            combineLatest([
                this.store.select(getStatus),
                this.store.select(getAllRolesForOrgStatus),
                this.store.select(orgFromRoute),
                this.store.select(allInfraRoles)
              ]).pipe(
                  filter(([getOrgSt, getRolesSt, _orgState, _allInfraRolesState]) =>
                    getOrgSt === EntityStatus.loadingSuccess &&
                    getRolesSt === EntityStatus.loadingSuccess),
                  filter(([_getOrgSt, _getRolesSt, orgState, allInfraRolesState]) =>
                    !isNil(orgState) && !isNil(allInfraRolesState)),
                  takeUntil(this.isDestroyed)
                ).subscribe(([_getOrgSt, _getRolesSt, orgState, allInfraRolesState]) => {
                  this.org = { ...orgState };
                  this.roles = allInfraRolesState;
                //   this.updateOrgForm.controls['name'].setValue(this.org.name);
                //   this.updateOrgForm.controls['admin_user'].setValue(this.org.admin_user);
                //   this.updateOrgForm.controls['admin_key'].setValue(this.org.admin_key);
                });
    
    }

    onSelectedTab(event: { target: { value: OrgTabName } }) {
        this.tabValue = event.target.value;
        this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
      }

    ngOnDestroy(): void {

      }
}