import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { EntityStatus } from 'app/entities/entities';
import { GetNodeRunlists } from 'app/entities/nodeRunlists/nodeRunlists.action';
import { GetNodes, DeleteNode, GetNode } from 'app/entities/infra-nodes/infra-nodes.actions';
import { GetRecipes } from 'app/entities/recipes/recipe.action';
import { InfraNode } from 'app/entities/infra-nodes/infra-nodes.model';
import {
  nodeList,
  getAllStatus,
  deleteStatus,
  infraNodeFromRoute,
  getStatus
} from 'app/entities/infra-nodes/infra-nodes.selectors';
import {
  allRecipes,
  getAllStatus as getAllRecipesForOrgStatus
} from 'app/entities/recipes/recipe.selectors';
import {
  allNodeRunlist,
  getAllStatus as getAllNodeRunlistForOrgStatus
} from 'app/entities/nodeRunlists/nodeRunlists.selectors';
import { AvailableType } from '../infra-roles/infra-roles.component';
import { ListItem } from '../select-box/src/lib/list-item.domain';
import { NodeList, NodeRunlist } from 'app/entities/nodeRunlists/nodeRunlists.model';
import { TimeFromNowPipe } from 'app/pipes/time-from-now.pipe';
import { Regex } from 'app/helpers/auth/regex';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-infra-nodes',
  templateUrl: './infra-nodes.component.html',
  styleUrls: ['./infra-nodes.component.scss']
})

export class InfraNodesComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  private isDestroyed = new Subject<boolean>();
  private timeFromNowPipe = new TimeFromNowPipe();
  public nodes: InfraNode[] = [];
  public nodeListState: { items: InfraNode[], total: number };
  public nodesListLoading = true;
  public authFailure = false;
  public loading = false;
  public searchValue = '';
  public currentPage = 1;
  public per_page = 100;
  public total: number;
  public nodeToDelete: InfraNode;
  public deleteModalVisible = false;
  public deleting = false;
  public editRunlistModalVisible = new EventEmitter<boolean>();

  // edit run list
  public availableType: AvailableType[] = [];
  public editRunlistLoading = false;
  public label = 'Run List';
  public nodeStatus;
  public nodeToEditRunlist: InfraNode;
  public recipes: string[] = [];
  public runlist: NodeRunlist[] = [];
  public runlistError = false;
  public runlistStatus;
  public selected: ListItem[] = [];

  // node reset
  public nodeName: string;
  public openNotificationModal = new EventEmitter<void>();

  // update node tag
  public updateNodeName: string;
  public openTagModal = new EventEmitter<void>();

  // edit attributes
  public editAttributesLoading = false;
  public editNodeAttributes: string;
  public node: InfraNode;
  public jsonText;
  public attributes;
  public isGetNode = false;
  public openAttributeModal = new EventEmitter<void>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.getNodesData();
    this.loadRecipes();
    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(nodeList)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([getNodesSt, NodesState]) => {
      if (getNodesSt === EntityStatus.loadingSuccess && !isNil(NodesState)) {
        this.nodeListState = NodesState;
        this.nodes = NodesState?.items;
        this.total = NodesState?.total;
        this.nodesListLoading = false;
        this.loading = false;
        this.deleting = false;
      } else if (getNodesSt === EntityStatus.loadingFailure) {
        this.nodesListLoading = false;
        this.authFailure = true;
      }
    });

    this.store.select(deleteStatus).pipe(
      filter(status => status === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.loading = true;
        if (this.nodes.length === 0 &&
          this.currentPage !== 1) {
          this.currentPage = this.currentPage - 1;
        }
        this.getNodesData();
      });
  }

  searchNodes(currentText: string) {
    this.currentPage = 1;
    this.loading = true;
    this.searchValue = currentText;
    if ( currentText !== ''  && !Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN.test(currentText)) {
      this.loading = false;
      this.nodes.length = 0;
      this.total = 0;
    } else {
      this.getNodesData();
    }
    this.telemetryService.track('InfraServer_Nodes_Search');
  }

  onPageChange(event: number): void {
    this.currentPage = event;
    this.loading = true;
    this.getNodesData();
    this.telemetryService.track('InfraServer_Nodes_GetNodesData');
  }

  getNodesData() {
    const payload = {
      nodeName: this.searchValue,
      server_id: this.serverId,
      org_id: this.orgId,
      page: this.currentPage,
      per_page: this.per_page
    };

    this.store.dispatch(new GetNodes(payload));
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  timeFromNow(epochFormat: string) {
    const epchoTime = Number(epochFormat);
    const fromNowValue = this.timeFromNowPipe.transform(epchoTime);
    return fromNowValue === '-' ? '--' : fromNowValue;
  }

  openManageTagModal(name: string) {
    this.updateNodeName = name;
    this.openTagModal.emit();
  }

  public editAttributes(node: InfraNode): void {
    this.editAttributesLoading = true;
    if ( this.nodeStatus !== EntityStatus.loading) {
      this.getNode(node);
    }
  }

  public loadAttributes(node: InfraNode) {
    this.node = node;
    this.jsonText = JSON.stringify(this.attributes, null, 4);
    this.openAttributeModal.emit();
  }

  public editRunlist(node: InfraNode): void {
    this.editRunlistLoading = true;
    this.selected = [];
    if ( this.nodeStatus !== EntityStatus.loading) {
      this.getNode(node);
    }

    if (this.runlistStatus !== EntityStatus.loading) {
      this.loadNodeRunlist(node);
    }
  }

  public startNodeDelete(node: InfraNode): void {
    this.nodeToDelete = node;
    this.deleteModalVisible = true;
  }

  public deleteNode(): void {
    this.loading = true;
    this.closeDeleteModal();
    this.store.dispatch(new DeleteNode({
      server_id: this.serverId, org_id: this.orgId, name: this.nodeToDelete.name
    }));
    this.telemetryService.track('InfraServer_Nodes_Delete');
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
    this.deleting = true;
  }

  openResetKeyModal(name: string) {
    this.nodeName = name;
    this.openNotificationModal.emit();
  }

  public closeRunlistModal(): void {
    this.selected = [];
    this.runlistStatus = '';
    this.nodeStatus = '';
    this.editRunlistModalVisible.emit(false);
  }

  getNode(node: InfraNode) {
    this.store.dispatch(new GetNode({
      server_id: this.serverId, org_id: this.orgId, name: node.name
    }));
    combineLatest([
      this.store.select(getStatus),
      this.store.select(infraNodeFromRoute)
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([getstat, allInfra]) => {
      this.nodeStatus = getstat;
      if (getstat === EntityStatus.loadingSuccess && !isNil(allInfra)) {
        this.nodeToEditRunlist = allInfra;
        this.attributes =
        (allInfra.normal_attributes && JSON.parse(allInfra.normal_attributes)) || {};
        if (this.editAttributesLoading === true) {
          this.loadAttributes(node);
        }
      }
    });
  }

  loadNodeRunlist(node: InfraNode): void {
    this.store.dispatch(new GetNodeRunlists({
      server_id: this.serverId, org_id: this.orgId, name: node.name, id: node.environment
    }));
    combineLatest([
      this.store.select(getAllNodeRunlistForOrgStatus),
      this.store.select(allNodeRunlist)
    ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([getNodeRunlistSt, allNodeRunlistState]) => {
        this.runlistStatus = getNodeRunlistSt;
        if (getNodeRunlistSt === EntityStatus.loadingSuccess && !isNil(allNodeRunlistState)) {
          if (allNodeRunlistState && allNodeRunlistState.length) {
            this.runlist = allNodeRunlistState;
            this.getRunlist(allNodeRunlistState);
          }
        } else if (getNodeRunlistSt === EntityStatus.loadingFailure) {
          this.runlistError = true;
          this.editRunlistLoading = false;
          this.editRunlistModalVisible.emit(true);
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
  private getRunlist(nodeRunlist: NodeRunlist[]) {
    this.selected = [];
    nodeRunlist.forEach(nodeValue => {
      if (nodeValue.run_list && nodeValue.run_list.length) {
        this.getSelectedRunlist(nodeValue.run_list);
        this.editRunlistLoading = false;
        this.runlistError = false;
        this.editRunlistModalVisible.emit(true);
      } else {
        this.selected = [];
        this.editRunlistLoading = false;
        this.runlistError = false;
        this.editRunlistModalVisible.emit(true);
      }
    });
  }

  private getSelectedRunlist(runlist: NodeList[]) {
    for (const value of runlist) {
      this.selected.push({
        selected: false,
        type: value.type,
        value: value.name
      });
    }
  }

  onUpdatePage($event: { pageIndex: number; pageSize: number; }) {
    this.loading = true;
    this.currentPage = $event.pageIndex + 1;
    this.per_page = $event.pageSize;
    this.getNodesData();
    this.telemetryService.track('InfraServer_Nodes_GetNodesData');
  }
}
