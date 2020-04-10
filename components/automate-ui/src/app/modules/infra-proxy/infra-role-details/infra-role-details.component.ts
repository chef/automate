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
import { InfraRole, ExpandList,
  ChildLists, Lists } from 'app/entities/infra-roles/infra-role.model';
import { Node, Options } from '../treetable/models';
export type InfraRoleTabName = 'runList' | 'attributes';


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
  public expandRunList: Lists[] = [];
  public show = false;
  public data: any = [];
  public env_id = '_default';
  public idList: any = [];
  public childNodes: Node<ChildLists>[] = [];
  private isDestroyed = new Subject<boolean>();
  arrayOfNodesTree: Node<ChildLists>[];
  roleDetailsLoading = true;
  treeOptions: Options<ChildLists> = {
    capitalisedHeader: true
  };

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.arrayOfNodesTree = [];
    this.childNodes = [];
    this.idList = [];
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
      .subscribe((url: string) => {
        this.url = url;
        const [, fragment] = url.split('#');
        this.tabValue = (fragment === 'attributes') ? 'attributes' : 'runList';
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
      this.idList = [];
      for (let i = 0; i < this.expandedList.length; i++) {
        this.idList.push(
          this.expandedList[i].id
        );
      }

      if (this.expandedList && this.expandedList.length) {
        this.show = true;
        if (this.tabValue === 'runList') {
          this.treeNodes(this.expandedList, this.env_id);
        }

      } else {
        this.show = false;
      }

      this.roleDetailsLoading = false;
    });
  }

  selectChangeHandler(id) {
    this.env_id = id;
    this.treeNodes(this.expandedList, this.env_id);
  }

  treeNodes(expandedList: ExpandList[], li: string) {
    this.arrayOfNodesTree = [];
    for (let i = -0; i < expandedList.length; i++) {
      if (expandedList[i].id === li) {
        this.expandRunList = expandedList[i].run_list;
        if (this.expandRunList && this.expandRunList.length) {
          for (let j = 0; j < this.expandRunList.length; j++) {
            const nodes: Node<ChildLists>[] = [];
            this.arrayOfNodesTree.push({
              value: {
                name: this.expandRunList[j].name,
                version: this.expandRunList[j].version === ''
                ? '...' : this.expandRunList[j].version,
                type: this.expandRunList[j].type
              },
              children:
                this.expandRunList[j].children && this.expandRunList[j].children.length ?
                  this.childNode(this.expandRunList[j].children, nodes) : []
            });
          }

        }
      }
    }
    this.show = true;

  }

  childNode(child: Lists[], nodes: Node<ChildLists>[]) {
    const childNodes: Node<ChildLists>[] = [];
    for (let i = 0; i < child.length; i++) {
      nodes.push({
        value: {
          name: child[i].name,
          version: child[i].version,
          type: child[i].type
        },
        children:
          child[i].children && child[i].children.length ?
            this.childNode(child[i].children, childNodes) : []
      });
    }
    return nodes;
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
