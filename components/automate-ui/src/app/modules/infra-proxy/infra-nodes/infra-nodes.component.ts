import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { EntityStatus } from 'app/entities/entities';
import { GetNodes } from 'app/entities/infra-nodes/infra-nodes.actions';
import { InfraNode } from 'app/entities/infra-nodes/infra-nodes.model';
import {
  nodeList,
  getAllStatus
} from 'app/entities/infra-nodes/infra-nodes.selectors';
import { TimeFromNowPipe } from 'app/pipes/time-from-now.pipe';
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
  public searching = false;
  public searchValue = '';
  public currentPage = 1;
  public per_page = 9;
  public total: number;
  public nodesToDelete: InfraNode;
  public deleteModalVisible = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.getNodesData();

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
        this.searching = false;
      } else if (getNodesSt === EntityStatus.loadingFailure) {
        this.nodesListLoading = false;
        this.authFailure = true;
      }
    });
  }

  searchNodes(currentText: string) {
    this.currentPage = 1;
    this.searching = true;
    this.searchValue = currentText;

    this.getNodesData();
  }

  onPageChange(event: number): void {
    this.currentPage = event;
    this.searching = true;
    this.getNodesData();
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

  public startNodeDelete(node: InfraNode): void {
    this.nodesToDelete = node;
    this.deleteModalVisible = true;
  }

  public deleteNode(): void {
    this.searching = true;
    this.closeDeleteModal();
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }
}
