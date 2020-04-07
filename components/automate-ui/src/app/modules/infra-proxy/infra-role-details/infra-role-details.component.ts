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
import { InfraRole, ExpandList, Lists } from 'app/entities/infra-roles/infra-role.model';

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
  public expandedList: ExpandList[] = [];
  public idList: any = [];
  public expandRunList: Lists[] = [];
  public show = false;
  public data: any = [];
  public id = '_default';

  private isDestroyed = new Subject<boolean>();

  roleDetailsLoading = true;

  configs: any = {
    id_field: 'id',
    parent_id_field: 'parent',
    parent_display_field: 'name',
    css: {
      // Optional
      expand_class: 'fa fa-plus',
      collapse_class: 'fa fa-minus',
    },
    columns: [
      {
        name: 'name',
        header: '',
        width: '50px'
      },
      {
        name: 'version',
        header: 'Version',
        width: '50px',
        renderer: function(value) {
          return value ? value : 'N/A';
        }
      },
      {
        name: 'type',
        header: 'Type',
        width: '50px',
        renderer: function(value) {
          return value ? value : 'N/A';
        }
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
    this.idList = [];

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
      this.expandedList = role.expanded_run_list;

      this.runList = this.role.run_list;
      for ( let i = 0; i < this.expandedList.length; i++ ) {
        this.idList.push({
          id: this.expandedList[i].id
        });
      }

      if (this.expandedList && this.expandedList.length) {
        this.show = true;
        this.treeNodes( this.expandedList, this.id);
      } else {
        this.show = false;
      }
      this.roleDetailsLoading = false;
    });
  }

  selectChangeHandler (id: string) {
    const selectedId = id;
    this.treeNodes( this.expandedList, selectedId);
  }

  treeNodes( expandedList: ExpandList[], li: string) {
    this.data = [];
    for ( let i = -0; i < expandedList.length; i++ ) {
      if ( expandedList[i].id === li ) {
        this.expandRunList = expandedList[i].run_list;
        if (this.expandRunList && this.expandRunList.length) {
          for ( let j = 0; j < this.expandRunList.length; j++ ) {
            this.data.push({
              id: j + 1,
              name: this.expandRunList[j].name,
              version: this.expandRunList[j].version,
              type: this.expandRunList[j].type,
              parent: 0
            });
          }
        } else {
          this.data.push({
            id: 1,
            name: 'N/A',
            version: 'N/A',
            type: 'N/A',
            parent: 0
          });
        }
      }
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
