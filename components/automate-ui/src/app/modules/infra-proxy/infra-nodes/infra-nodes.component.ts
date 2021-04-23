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
  allInfraNodes,
  getAllStatus as getAllNodesForOrgStatus
} from 'app/entities/infra-nodes/infra-nodes.selectors';


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
  public nodes: InfraNode[] = [];
  public nodesListLoading = true;
  public authFailure = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.store.dispatch(new GetNodes({
      server_id: this.serverId, org_id: this.orgId
    }));

    combineLatest([
      this.store.select(getAllNodesForOrgStatus),
      this.store.select(allInfraNodes)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([ getNodesSt, allInfraNodesState]) => {
      if (getNodesSt === EntityStatus.loadingSuccess && !isNil(allInfraNodesState)) {
        this.nodes = allInfraNodesState;
        this.nodesListLoading = false;
      } else if (getNodesSt === EntityStatus.loadingFailure) {
        this.nodesListLoading = false;
        this.authFailure = true;
      }
    });
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
