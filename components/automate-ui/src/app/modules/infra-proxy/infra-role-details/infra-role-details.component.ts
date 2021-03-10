import { Component, OnInit, OnDestroy, EventEmitter, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { infraRoleFromRoute } from 'app/entities/infra-roles/infra-role-details.selectors';
import { GetRole } from 'app/entities/infra-roles/infra-role.action';
import {
  InfraRole, ExpandedList,
  ChildList, List, RoleAttributes
} from 'app/entities/infra-roles/infra-role.model';
import { Node, Options } from '../tree-table/models';
import { JsonTreeTableComponent as JsonTreeTable } from './../json-tree-table/json-tree-table.component';

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
  public orgId;
  public name;
  public runList: string[];
  public expandedList: ExpandedList[] = [];
  public expandedRunList: List[] = [];
  public show = false;
  public env_id = '_default';
  public idList: string[] = [];
  public hasRun_List = true;
  public childNodes: Node<ChildList>[] = [];
  private isDestroyed = new Subject<boolean>();
  arrayOfNodesTree: Node<ChildList>[];
  roleDetailsLoading = true;
  treeOptions: Options<ChildList> = {
    capitalizedHeader: true
  };

  public attributes = new RoleAttributes({
    default_attributes: '',
    override_attributes: ''
  });

  public selectedAttrs: any;
  public selected_level = 'all';

  // precedence levels
  public default_attributes = 'default_attributes';
  public override_attributes = 'override_attributes';
  public all = 'all';

  @ViewChild(JsonTreeTable, { static: true })
  tree: JsonTreeTable;

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
      this.store.select(routeParams).pipe(pluck('org-id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('name'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id, name]: string[]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.name = name;
      this.store.dispatch(new GetRole({
        server_id: server_id, org_id: org_id, name: name
      }));
    });

    this.store.select(infraRoleFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(role => {
      this.show = true;
      this.role = role;
      this.expandedList = role.expanded_run_list;
      this.runList = this.role.run_list;
      this.idList = [];
      // this.attributes = new RoleAttributes(this.role);
      this.getRunListTree(this.expandedList);

      // Settimeout - In the case of passing large attribute data sets
      setTimeout(() => this.filter(this.selected_level), 10);
      this.roleDetailsLoading = false;
    });
  }

  // set selected item to selected_level
  filter(precedence_level: string): void {
    this.selected_level = precedence_level;
    this.selectedAttrs = this.retrieve(precedence_level);
  }

  // retrieve attributes based on their level of precedence
  retrieve(level: string): Object {
    switch (level) {
      case this.all: {
        return this.attributes.all;
      }
      case this.default_attributes: {
        return this.attributes.default_attributes;
      }
      case this.override_attributes: {
        return this.attributes.override_attributes;
      }
      default: {
        return {};
      }
    }
  }

  selectChangeHandler(id: string): void {
    this.env_id = id;
    this.treeNodes(this.expandedList, this.env_id);
  }

  // get the tree according to selected dropdown
  getRunListTree(expandedList: ExpandedList[]) {
    if (expandedList && expandedList.length) {
      this.show = true;
      for (let i = 0; i < expandedList.length; i++) {
        this.idList.push(
          expandedList[i].id
        );
      }
      if (this.tabValue === 'runList') {
        this.treeNodes(expandedList, this.env_id);
      }

    } else {
      this.show = false;
    }
  }

  // 1. According to the environment ID getting the array.
  // 2. Then Convert our Particular Array data with parent, child nodes as tree structure.
  treeNodes(expandedList: ExpandedList[], li: string) {
    this.arrayOfNodesTree = [];
    for (let i = 0; i < expandedList.length; i++) {
      if (expandedList[i].id === li) {
        this.expandedRunList = expandedList[i].run_list;
        if (this.expandedRunList && this.expandedRunList.length) {
          this.hasRun_List = true;
          for (let j = 0; j < this.expandedRunList.length; j++) {
            this.arrayOfNodesTree.push({
              value: {
                name: this.expandedRunList[j].name,
                version: this.expandedRunList[j].version === ''
                  ? '...' : this.expandedRunList[j].version,
                type: this.expandedRunList[j].type
              },
              children:
                this.expandedRunList[j].children && this.expandedRunList[j].children.length ?
                  this.childNode(this.expandedRunList[j].children, []) : []
            });
          }
        } else {
          this.hasRun_List = false;
        }
      }
    }
    this.show = true;
  }

  childNode(child: List[], nodes: Node<ChildList>[]) {
    const childNodes: Node<ChildList>[] = [];
    for (let i = 0; i < child.length; i++) {
      nodes.push({
        value: {
          name: child[i].name,
          version: child[i].version === ''
            ? '...' : child[i].version,
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
    this.filter(this.selected_level);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
