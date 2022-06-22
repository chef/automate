import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
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
import { GetRecipes } from 'app/entities/recipes/recipe.action';
import { GetRoleEnvironments } from 'app/entities/role-environments/role-environments.action';
import { GetRunlists } from 'app/entities/runlists/runlists.action';
import {
  allRecipes,
  getAllStatus as getAllRecipesForOrgStatus
} from 'app/entities/recipes/recipe.selectors';
import {
  allRoleEnvironments,
  getAllStatus as getAllRoleEnvironmentsForOrgStatus
} from 'app/entities/role-environments/role-environments.selectors';
import {
  allRunlist,
  getAllStatus as getAllRunlistForOrgStatus
} from 'app/entities/runlists/runlists.selectors';
import { isNil } from 'lodash/fp';
import {
  InfraRole,
   RoleAttributes
} from 'app/entities/infra-roles/infra-role.model';
import { EntityStatus } from 'app/entities/entities';
import { Node, Options } from '../tree-table/models';
import { ListItem } from '../select-box/src/lib/list-item.domain';
import { List, ExpandedChildList, Runlist } from 'app/entities/runlists/runlists.model';
import { AvailableType } from '../infra-roles/infra-roles.component';

export type InfraRoleTabName = 'runList' | 'attributes';

@Component({
  selector: 'app-infra-role-details',
  templateUrl: './infra-role-details.component.html',
  styleUrls: ['./infra-role-details.component.scss']
})

export class InfraRoleDetailsComponent implements OnInit, OnDestroy {
  public conflictError = false;
  public role: InfraRole;
  public tabValue: InfraRoleTabName = 'runList';
  public url: string;
  public serverId;
  public orgId;
  public name;
  public show = false;
  public showAttribute = false;
  public hasRun_List = false;
  public hasOverrideJson = true;
  public hasDefaultJson = true;
  public roleDetailsLoading = true;
  public roleAttributeLoading = true;
  public runListLoading = true;
  public openEdit = false;
  public editDisabled = false;
  public treeOptions: Options<ExpandedChildList> = {
    capitalizedHeader: true
  };
  public arrayOfNodesTree: Node<ExpandedChildList>[];
  public attributes = new RoleAttributes({
    default_attributes: '',
    override_attributes: ''
  });
  public availableType: AvailableType[] = [];
  public childNodes: Node<ExpandedChildList>[] = [];
  public expandedRunList: List[] = [];
  public idList: string[] = [];
  public recipes: string[] = [];
  public runList: string[];
  public selected: ListItem[] = [];
  public jsonText: string;
  public label: string;
  public per_page = 9;
  public selectedAttrs: any;

  // precedence levels
  public default_attributes = 'default_attributes';
  public override_attributes = 'override_attributes';
  public all = 'all';
  public env_id = '_default';
  public selected_level = 'all';

  public conflictErrorEvent = new EventEmitter<boolean>();
  public openEnvironmentModal = new EventEmitter<boolean>();
  private isDestroyed = new Subject<boolean>();

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
    this.loadRecipes();
    this.loadRoleEnvironments();
    this.loadRunlists(this.env_id);
    this.store.select(infraRoleFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(role => {
      this.show = true;
      this.showAttribute = true;
      this.role = role;
      this.runList = this.role.run_list;
      this.attributes = new RoleAttributes(this.role);
      this.hasDefaultJson = Object.keys(
        JSON.parse(this.role.default_attributes)).length > 0 ? true : false;
      this.hasOverrideJson = Object.keys(
        JSON.parse(this.role.override_attributes)).length > 0 ? true : false;

      // Settimeout - In the case of passing large attribute data sets
      setTimeout(() => this.filter(this.selected_level), 10);
      this.roleDetailsLoading = false;
      this.roleAttributeLoading = false;
      if (!this.idList.length) {
        this.idList[0] = this.env_id;
      }
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
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
    this.editDisabled = (this.env_id === '_default') ? false : true;
    this.runListLoading = true;
    this.hasRun_List = false;
    this.arrayOfNodesTree = [];
    this.loadRunlists(this.env_id);
  }

  openEditModal(value, label: string): void {
    this.openEdit = true;
    const obj = JSON.parse(value);
    this.jsonText = JSON.stringify(obj, null, 4);
    this.label = label;
    this.openEnvironmentModal.emit(true);
  }

  onSelectedTab(event: { target: { value: InfraRoleTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
    this.filter(this.selected_level);
  }

  updateRunlist() {
    this.selectChangeHandler(this.env_id);
  }

  private loadRecipes(): void {
    this.store.dispatch(new GetRecipes({
      server_id: this.serverId, org_id: this.orgId, name: '_default'
    }));
    combineLatest([
      this.store.select(getAllRecipesForOrgStatus),
      this.store.select(allRecipes)
    ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([getRecipesSt, allRecipesState]) => {
        if (getRecipesSt === EntityStatus.loadingSuccess && !isNil(allRecipesState)) {
          this.recipes = allRecipesState;
          if (this.recipes.length > 0) {
            this.recipes.forEach((recipe) => {
              this.availableType.push({
                name: recipe,
                type: 'recipe'
              });
            });
          }
        }
      });
  }

  private loadRoleEnvironments(): void {
    this.store.dispatch(new GetRoleEnvironments({
      server_id: this.serverId, org_id: this.orgId, name: this.name
    }));
    combineLatest([
      this.store.select(getAllRoleEnvironmentsForOrgStatus),
      this.store.select(allRoleEnvironments)
    ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([getRoleEnvironmentsSt, allRoleEnvironmentsState]) => {
        if (getRoleEnvironmentsSt === EntityStatus.loadingSuccess
          && !isNil(allRoleEnvironmentsState)) {
          this.idList = allRoleEnvironmentsState;
          if (this.idList.length > 0) {
            this.env_id = this.idList[0];
          } else {
            this.idList[0] = this.env_id;
          }
          this.show = true;
        }
      });
  }

  private loadRunlists(environmentId: string): void {
    this.store.dispatch(new GetRunlists({
      server_id: this.serverId, org_id: this.orgId, name: this.name, id: environmentId
    }));
    combineLatest([
      this.store.select(getAllRunlistForOrgStatus),
      this.store.select(allRunlist)
    ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([getRunlistSt, allRunlistState]) => {
        if (getRunlistSt === EntityStatus.loadingSuccess && !isNil(allRunlistState)) {
          if (allRunlistState && allRunlistState.length) {
            this.treeNodes(allRunlistState, environmentId);
          } else {
            this.runListLoading = false;
          }
          this.conflictError = false;
        } else if (getRunlistSt === EntityStatus.loadingFailure) {
          this.conflictError = true;
          this.hasRun_List = false;
          this.runListLoading = false;
          this.env_id = '_default';
        }
      });
  }

  // set selected item to selected_level
  private filter(precedence_level: string): void {
    this.selected_level = precedence_level;
    this.selectedAttrs = this.retrieve(precedence_level);
  }

  // 1. According to the environment ID getting the array.
  // 2. Then Convert our Particular Array data with parent, child nodes as tree structure.
  private treeNodes(expandedList: Runlist[], li: string) {
    this.arrayOfNodesTree = [];
    this.selected = [];
    for (let i = 0; i < expandedList.length; i++) {
      if (expandedList[i].id === li) {
        this.expandedRunList = expandedList[i].run_list;
        if (this.expandedRunList && this.expandedRunList.length) {
          for (let j = 0; j < this.expandedRunList.length; j++) {
            this.selected.push({
              selected: false,
              type: this.expandedRunList[j].type,
              value: this.expandedRunList[j].name
            });
            this.arrayOfNodesTree.push({
              value: {
                name: this.expandedRunList[j].name,
                version: this.expandedRunList[j].version ? this.expandedRunList[j].version :  '...',
                type: this.expandedRunList[j].type,
                error: this.expandedRunList[j].error,
                position: this.expandedRunList[j].position,
                skipped: this.expandedRunList[j].skipped
              },
              children:
                this.expandedRunList[j].children && this.expandedRunList[j].children.length ?
                  this.childNode(this.expandedRunList[j].children, []) : []
            });
          }
          this.hasRun_List = true;
          this.runListLoading = false;
        } else {
          this.hasRun_List = false;
          this.runListLoading = false;
        }
      }
    }
    this.show = true;
  }

  private childNode(child: List[], nodes: Node<ExpandedChildList>[]) {
    for (let i = 0; i < child.length; i++) {
      nodes.push({
        value: {
          name: child[i].name,
          version: child[i].version ? child[i].version : '...',
          type: child[i].type,
          error: child[i].error,
          position: child[i].position,
          skipped: child[i].skipped
        },
        children:
          child[i].children && child[i].children.length ?
            this.childNode(child[i].children, []) : []
      });
    }
    return nodes;
  }
}
