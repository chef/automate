import { Component, OnInit, OnDestroy, EventEmitter, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { isNil } from 'lodash/fp';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus, pending } from 'app/entities/entities';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams, routeURL } from 'app/route.selectors';
import { filter, pluck, takeUntil, take } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import {
  infraNodeFromRoute
} from 'app/entities/infra-nodes/infra-node-details.selectors';
import {
  updateTagsStatus,
  updateEnvStatus,
  nodeTags,
  nodeEnvironment
} from 'app/entities/infra-nodes/infra-nodes.selectors';
import {
  getAllStatus,
  environmentList
} from 'app/entities/environments/environment.selectors';
import {
  allRecipes,
  getAllStatus as getAllRecipesForOrgStatus
} from 'app/entities/recipes/recipe.selectors';
import {
  allNodeRunlist,
  getAllStatus as getAllNodeRunlistForOrgStatus
} from 'app/entities/nodeRunlists/nodeRunlists.selectors';
import {
  GetNode,
  UpdateNodeTags,
  UpdateNodeEnvironment
} from 'app/entities/infra-nodes/infra-nodes.actions';
import { GetEnvironments } from 'app/entities/environments/environment.action';
import { GetNodeRunlists } from 'app/entities/nodeRunlists/nodeRunlists.action';
import { GetRecipes } from 'app/entities/recipes/recipe.action';
import { InfraNode } from 'app/entities/infra-nodes/infra-nodes.model';
import { Environment } from 'app/entities/environments/environment.model';
import { NodeList, NodeExpandedChildList, NodeRunlist } from 'app/entities/nodeRunlists/nodeRunlists.model';
import { Node, Options } from '../tree-table/models';
import { AvailableType } from '../infra-roles/infra-roles.component';
import { ListItem } from '../select-box/src/lib/list-item.domain';
import { JsonTreeTableComponent as JsonTreeTable } from './../json-tree-table/json-tree-table.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

export type InfraNodeTabName = 'details' | 'runList' | 'attributes';

@Component({
  selector: 'app-infra-node-details',
  templateUrl: './infra-node-details.component.html',
  styleUrls: ['./infra-node-details.component.scss']
})

export class InfraNodeDetailsComponent implements OnInit, OnDestroy {
  public conflictError = false;
  public InfraNode: InfraNode;
  public tabValue: InfraNodeTabName = 'details';
  public serverId: string;
  public orgId: string;
  public openEdit = false;
  public label: string;
  public name: string;
  public environmentName: string;
  public nodeDetailsLoading = true;
  public nodeRunlistLoading = true;
  public url: string;

  public updateNodeForm: FormGroup;
  private isDestroyed = new Subject<boolean>();
  public openEnvironmentModal = new EventEmitter<boolean>();

  // for tags
  public tags: string[];
  public nodeTags: string[];
  public removeTags: string[] = [];
  public inputTxt = '';
  public updatingTags = false;

  // for environments
  public environmentListState: { items: Environment[], total: number };
  public environments: Environment[] = [];
  public environmentsBuffer: Environment[] = [];
  public PageBufferSize = 20;
  public numberOfItemsFromEndBeforeFetchingMore = 10;
  public loading = false;
  public currentPage = 1;
  public total: number;
  public saving = false;
  public confirming = false;

  // for runlist
  public arrayOfNodesTree: Node<NodeExpandedChildList>[];
  public availableType: AvailableType[] = [];
  public childNodes: Node<NodeExpandedChildList>[] = [];
  public expandedRunList: NodeList[] = [];
  public hasRun_List = false;
  public recipes: string[] = [];
  public runListLoading = true;
  public runlist: NodeRunlist[] = [];
  public selected: ListItem[] = [];
  public treeOptions: Options<NodeExpandedChildList> = {
    capitalizedHeader: true
  };

  // for attributes
  public attributes = [];
  public jsonText: any;
  public attributesLoading = true;
  public nodeAttributesLoading = true;
  public hasattributes = true;
  public openEditAttr = false;
  public isGetNode = true;
  public openAttributeModal = new EventEmitter<boolean>();

  @ViewChild(JsonTreeTable, { static: true })
  tree: JsonTreeTable;

  constructor(
    private fb: FormBuilder,
    private router: Router,
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) {
    this.updateNodeForm = this.fb.group({
      environment: [null, [Validators.required]]
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);
    this.store.select(routeURL).pipe(takeUntil(this.isDestroyed))
    .subscribe((url: string) => {
      this.url = url;
      const [, fragment] = url.split('#');
      switch (fragment) {
        case 'details':
          this.tabValue = 'details';
          break;
        case 'runList':
          this.tabValue = 'runList';
          break;
        case 'attributes':
          this.tabValue = 'attributes';
          break;
      }
    });
    // load node details
    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('org-id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('name'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).pipe(take(1))
    .subscribe(([server_id, org_id, name]: string[]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.name = name;
      this.store.dispatch(new GetNode({
        server_id: server_id, org_id: org_id, name: name
      }));
    });
    this.loadRecipes();
    this.store.select(infraNodeFromRoute).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(node => {
      this.InfraNode = node;
      this.nodeTags = node.tags;
      this.environmentName = node.environment;
      this.tags = this.nodeTags;
      this.nodeDetailsLoading = false;
      this.nodeRunlistLoading = false;
      this.nodeAttributesLoading = false;
      // set default value of node environment
      this.updateNodeForm.controls.environment.setValue(this.environmentName);
      // load runlist according to the environment
      this.loadNodeRunlists(this.InfraNode.environment);
      // load attributes
      this.attributes = (node.normal_attributes && JSON.parse(node.normal_attributes)) || {};
      this.hasattributes = Object.keys(
        JSON.parse(node.normal_attributes)).length > 0 ? true : false;
    });

    // show default list of environments
    this.loadEnvironmentsList();

    combineLatest([
      this.store.select(updateEnvStatus),
      this.store.select(nodeEnvironment)
    ]).pipe(
      takeUntil(this.isDestroyed),
      filter(([state]) => this.saving && !pending(state))
    ).subscribe(([getEnvSt, envStatus]) => {
      if (getEnvSt === EntityStatus.loadingSuccess && !isNil(envStatus)
        && envStatus !== this.environmentName) {
        this.environmentName = envStatus;
        this.updateNodeForm.markAsPristine();
        this.saving = false;
        this.closeConfirmationBox();
        this.updateRunlist();
      }
    });

    combineLatest([
      this.store.select(updateTagsStatus),
      this.store.select(nodeTags)
    ]).pipe(
        takeUntil(this.isDestroyed)
      ).subscribe(([getTagsSt, TagsState]) => {
      if (getTagsSt === EntityStatus.loadingSuccess && !isNil(TagsState)) {
        this.nodeTags = TagsState;
        this.tags = this.nodeTags;
        this.updatingTags = false;
      } else if (getTagsSt === EntityStatus.loadingFailure) {
        this.nodeTags = this.tags;
        this.updatingTags = false;
      }
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  // update tags
  updateTags(action: string, tags: string[]) {
    this.updatingTags = true;
    const updatedNode = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.name,
      action: action,
      tags: tags
    };
    this.store.dispatch(new UpdateNodeTags({node: updatedNode}));
  }

  addTags() {
    if (this.inputTxt !== '') {
      this.tags = this.tags.concat(this.inputTxt.replace(/^[,\s]+|[,\s]+$/g, '')
        .replace(/,[,\s]*,/g, ',').split(',').map(item => item.trim()));
      this.inputTxt = '';
      this.updateTags('add', this.tags);
      this.telemetryService.track('InfraServer_Nodes_AddTags');
    }
  }

  removeTag(tag: string) {
    this.tags.forEach((element, index) => {
      if (element === tag) {
        this.tags.splice(index, 1);
      }
    });

    this.removeTags.push(tag);
    this.updateTags('delete', this.removeTags);
    this.telemetryService.track('InfraServer_Nodes_RemoveTags');
  }

  // load list of environments
  onScrollToEnd() {
    this.fetchMore();
  }

  onScroll({ end }) {
    if (this.loading || this.total <= this.environmentsBuffer.length) {
      return;
    }

    if (end + this.numberOfItemsFromEndBeforeFetchingMore >= this.environmentsBuffer.length) {
      this.fetchMore();
    }
  }

  openEditModal(label: string): void {
    this.openEdit = true;
    this.label = label;
    this.openEnvironmentModal.emit(true);
  }

  onSelectedTab(event: { target: { value: InfraNodeTabName } }) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  selectChangeHandler(env: { name: string }) {
    if (this.environmentName !== env.name) {
      this.confirming = true;
    }
  }

  closeConfirmationBox() {
    this.updateNodeForm.controls.environment.setValue(this.environmentName);
    this.confirming = false;
  }

  saveEnvironment() {
    this.saving = true;
    const updatedNode = {
      org_id: this.orgId,
      server_id: this.serverId,
      name: this.name,
      environment: this.updateNodeForm.controls.environment.value.trim()
    };
    this.store.dispatch(new UpdateNodeEnvironment({node: updatedNode}));
    this.telemetryService.track('InfraServer_Nodes_ChangeNodeEnvironment');
  }

  updateRunlist() {
    this.loadNodeRunlists(this.environmentName);
  }

  private fetchMore() {
    this.loading = true;
    this.currentPage += 1;
    this.loadEnvironmentsList();

    // using timeout here to simulate backend API delay
    setTimeout(() => {
      this.loading = false;
    }, 200);
  }

  private loadEnvironmentsList(): void {
    this.loading = true;
    const payload = {
      environmentName: '',
      page: this.currentPage,
      per_page: this.PageBufferSize,
      server_id: this.serverId,
      org_id: this.orgId
    };

    this.store.dispatch(new GetEnvironments(payload));

    // load environments list
    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(environmentList)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([getEnvironmentsSt, EnvironmentsState]) => {
      if (getEnvironmentsSt === EntityStatus.loadingSuccess && !isNil(EnvironmentsState)) {
        this.environmentListState = EnvironmentsState;
        this.environments = EnvironmentsState?.items;
        this.environmentsBuffer.push(...this.environments);
        const result = this.removeDuplicateEnv();
        this.environmentsBuffer = result;
        this.total = this.environmentListState?.total;
        this.loading = false;
      } else if (getEnvironmentsSt === EntityStatus.loadingFailure) {
        this.loading = false;
      }
    });
  }

  private removeDuplicateEnv(): Environment[] {
    const setObj = new Set(); // create key value pair from array of array
    const result = this.environmentsBuffer.reduce((acc, item) => {
      if (!setObj.has(item.name)) {
        setObj.add(item.name);
        acc.push(item);
      }
      return acc;
    }, []); // converting back to array from map-object
    return result;
  }

  private loadNodeRunlists(environmentId: string): void {
    this.store.dispatch(new GetNodeRunlists({
      server_id: this.serverId, org_id: this.orgId, name: this.name, id: environmentId
    }));
    combineLatest([
      this.store.select(getAllNodeRunlistForOrgStatus),
      this.store.select(allNodeRunlist)
    ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([getNodeRunlistSt, allNodeRunlistState]) => {
        if (getNodeRunlistSt === EntityStatus.loadingSuccess && !isNil(allNodeRunlistState)) {
          if (allNodeRunlistState && allNodeRunlistState.length) {
            this.runListLoading = true;
            this.hasRun_List = false;
            this.runlist = allNodeRunlistState;
            this.treeNodes(allNodeRunlistState, environmentId);
          } else {
            this.runListLoading = false;
          }
          this.conflictError = false;
        } else if (getNodeRunlistSt === EntityStatus.loadingFailure) {
          this.conflictError = true;
          this.hasRun_List = false;
          this.runListLoading = false;
        }
      });
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

  // 1. According to the environment ID getting the array.
  // 2. Then Convert our Particular Array data with parent, child nodes as tree structure.
  private treeNodes(expandedList: NodeRunlist[], li: string) {
    this.arrayOfNodesTree = [];
    this.selected = [];
    for (const expandValue of expandedList) {
      if (expandValue.id === li) {
        this.expandedRunList = expandValue.run_list;
        if (this.expandedRunList && this.expandedRunList.length) {
          this.expandedTreeNode(this.expandedRunList);
          this.hasRun_List = true;
          this.runListLoading = false;
        } else {
          this.hasRun_List = false;
          this.runListLoading = false;
        }
      }
    }
  }

  private expandedTreeNode(expandedRunList: NodeList[]) {
    for (const value of expandedRunList) {
      this.selected.push({
        selected: false,
        type: value.type,
        value: value.name
      });
      this.arrayOfNodesTree.push({
        value: {
          name: value.name,
          version: value.version ? value.version :  '...',
          type: value.type,
          error: value.error,
          position: value.position,
          skipped: value.skipped
        },
        children:
          value.children && value.children.length ?
            this.childNode(value.children, []) : []
      });
    }
  }

  private childNode(child: NodeList[], nodes: Node<NodeExpandedChildList>[]) {
    for (const value of child) {
      nodes.push({
        value: {
          name: value.name,
          version: value.version ? value.version : '...',
          type: value.type,
          error: value.error,
          position: value.position,
          skipped: value.skipped
        },
        children:
          value.children && value.children.length ?
            this.childNode(value.children, []) : []
      });
    }
    return nodes;
  }

  openEditAttrModal(value): void {
    this.openEditAttr = true;
    const obj = JSON.parse(value);
    this.jsonText = JSON.stringify(obj, null, 4);
    this.openAttributeModal.emit(true);
  }

  public closeEditAttributeModal(): void {
    this.openEditAttr = false;
  }
}
