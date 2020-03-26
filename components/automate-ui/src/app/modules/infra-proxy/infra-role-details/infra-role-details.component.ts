import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { infaRoleFromRoute } from 'app/entities/infra-roles/infra-role-details.selectors';
import { GetRole } from 'app/entities/infra-roles/infra-role.action';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';

export type InfraRoleTabName = 'runList';


@Component({
  selector: 'app-infra-role-details',
  templateUrl: './infra-role-details.component.html',
  styleUrls: ['./infra-role-details.component.scss']
})

export class InfraRoleDetailsComponent implements OnInit, OnDestroy {
  public role: InfraRole;
  public tabValue: InfraRoleTabName = 'runList';
  public url: string;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public modalType: string;
  public serverId;
  public OrgId;
  public name;
  public runList: string[];
  public show = false;
  public data: any = [];

  private isDestroyed = new Subject<boolean>();

  roleDetailsLoading = true;

  configs: any = {
    id_field: 'id',
    parent_id_field: 'parent',
    parent_display_field: 'name',
    css: {
      // Optional
      expand_class: 'fa fa-caret-right',
      collapse_class: 'fa fa-caret-down'
    },
    columns: [
      {
        name: 'name',
        header: 'Name',
        width: '50px',
        float: 'left'
      }
    ]
  };

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.data = [];
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
    .subscribe((url: string) => {
      this.url = url;
      this.tabValue =  'runList';
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

    this.store.select(infaRoleFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(role => {
      this.show = true;
      this.role = role;

      this.runList = this.role.run_list;
      if (this.runList && this.runList.length) {
        this.show = true;
        this.treeNodes(this.runList);
      } else {
        this.show = false;
      }
      this.roleDetailsLoading = false;
    });
  }

  treeNodes(runList: string[]) {
    for ( let i = 0; i < runList.length; i++) {
      this.data.push({
        id : i + 1,
        name: runList[i],
        parent: 0
      });
    }
  }

  onSelectedTab(event: { target: { value: InfraRoleTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
