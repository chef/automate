import { Component, OnInit, EventEmitter } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { Node, Options } from 'ng-material-treetable';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { EntityStatus } from 'app/entities/entities';
import { identity, isNil } from 'lodash/fp';

import { getStatus, infaRoleFromRoute } from 'app/entities/infra-roles/infra-role.selectors';
import { GetRole } from 'app/entities/infra-roles/infra-role.action';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';

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
  public tabValue: InfraRoleTabName = 'runList';
  public url: string;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public modalType: string;
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
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService

  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    // Populate our tabValue from the fragment.

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
      this.store.dispatch(new GetRole({
        server_id: server_id, org_id: org_id, name: name
      }));
    });
    
    combineLatest([
      this.store.select(getStatus),
      this.store.select(infaRoleFromRoute)
    ]).pipe(
      filter(([status, role]) => status === EntityStatus.loadingSuccess && !isNil(role)),
      takeUntil(this.isDestroyed))
      .subscribe(([_, role]) => {
        this.role = { ...role };
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
