import { Component, OnInit, EventEmitter } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { Node, Options } from 'ng-material-treetable';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { EntityStatus, allLoaded } from 'app/entities/entities';
import { identity, isNil } from 'lodash/fp';

import {
  allInfraRole, getStatusItem as getAllRoleItemForOrgStatus,
      roleFromRoute, ruleFromRoute, getStatus
    } from 'app/entities/infra-roles/infra-role.selectors';
import { GetOrg } from 'app/entities/orgs/org.actions';
import { GetRolesForOrg, GetRole } from 'app/entities/infra-roles/infra-role.action';
// import { roleFromRoute } from 'app/entities/roles/role.selectors';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Org } from 'app/entities/orgs/org.model';

export type InfraRoleTabName = 'runList' | 'attributes' | 'affectedNodes';

export interface Report {
  name: string;
  owner: string;
  protected: boolean;
  backup: boolean;
}

export interface Task {
  name: string;
  completed: boolean;
  owner: string;
}


@Component({
  selector: 'app-infra-role-details',
  templateUrl: './infra-role-details.component.html',
  styleUrls: ['./infra-role-details.component.scss']
})
export class InfraRoleDetailsComponent implements OnInit {
  public role: InfraRole;
  public roles: InfraRole[] = [];
  public tabValue: InfraRoleTabName = 'runList';
  public url: string;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public modalType: string;
  // private id: string;
  //private id: string;
  public updateServerForm: FormGroup;
  public org: Org;
  public serverId;
  public OrgId;
  public name;
  // isLoading represents the initial load as well as subsequent updates in progress.
  public isLoading = true;
  private isDestroyed = new Subject<boolean>();
  treeOptions: Options<Report> = {
    capitalisedHeader: true,
    customColumnOrder: [
      'owner', 'name', 'backup', 'protected'
    ]
  };

  arrayOfNodesTree: Node<Task>[] = [
    {
      value: {
        name: 'Tasks for Sprint 1',
        completed: true,
        owner: 'Marco'
      },
      children: [
        {
          value: {
            name: 'Complete feature #123',
            completed: true,
            owner: 'Marco'
          },
          children: []
        },
        {
          value: {
            name: 'Update documentation',
            completed: true,
            owner: 'Jane'
          },
          children: [
            {
              value: {
                name: 'Proofread documentation',
                completed: true,
                owner: 'Bob'
              },
              children: []
            }
          ]
        }
      ]
    },
    {
      value: {
        name: 'Tasks for Sprint 2',
        completed: false,
        owner: 'Erika',
      },
      children: [
        {
          value: {
            name: 'Fix bug #567',
            completed: false,
            owner: 'Marco'
          },
          children: []
        },
        {
          value: {
            name: 'Speak with clients',
            completed: true,
            owner: 'James'
          },
          children: []
        }
      ]
    }
  ]


  constructor(
    private fb: FormBuilder,
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService

  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    // Populate our tabValue from the fragment.
    this.updateServerForm = this.fb.group({
      name: [''],
    });

    this.store.select(routeURL).pipe()
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      this.tabValue = (fragment === 'runList') ? 'runList' : 'attributes';
    });

    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('orgid'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('name'), filter(identity))

    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id, name]: string[]) => {
      this.serverId = server_id;
      this.OrgId = org_id;
      this.name = name;
      this.store.dispatch(new GetOrg({ server_id: server_id, id: org_id }));
      this.store.dispatch(new GetRolesForOrg({
        server_id: server_id, org_id: org_id
      }));
      this.store.dispatch(new GetRole({
        server_id: server_id, org_id: org_id, name: name
      }));
    });
    
    combineLatest([
      this.store.select(getStatus),
      this.store.select(getAllRoleItemForOrgStatus)
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([getOrgSt, getRolesSt]) => {
      this.isLoading =
        !allLoaded([getOrgSt, getRolesSt]);
    });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(getAllRoleItemForOrgStatus),
      this.store.select(roleFromRoute),
        this.store.select(ruleFromRoute),
      this.store.select(allInfraRole)
    ]).pipe(
      filter(([getOrgSt, getRolesSt, _roleState, _itemState, _allInfraRolesState]) =>
      getOrgSt === EntityStatus.loadingSuccess &&
      getRolesSt === EntityStatus.loadingSuccess &&
        !isNil(_roleState) && !isNil(_itemState) &&
        !isNil(_allInfraRolesState)),
      takeUntil(this.isDestroyed)
    ).subscribe(([_getOrgSt, _getRolesSt, roleState, itemState,
       allInfraRolesState]) => {
      // this.org = { ...orgState };
      this.role = { ...roleState };
      this.roles = allInfraRolesState;
      console.log(itemState);
      console.log(this.role);
      console.log(allInfraRolesState);

      //this.updateServerForm.controls['name'].setValue(this.role.name);
    });

  }
    logNode(node: Node<Report>) {
    console.log(node);
  }

  onSelectedTab(event: { target: { value: InfraRoleTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }
}
