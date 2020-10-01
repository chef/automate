import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { GetEnvironments } from 'app/entities/environments/environment.action';
import { Environment } from 'app/entities/environments/environment.model';
import {
  allEnvironments,
  getAllStatus as getAllEnvironmentsForOrgStatus
} from 'app/entities/environments/environment.selectors';


@Component({
  selector: 'app-environments',
  templateUrl: './environments.component.html',
  styleUrls: ['./environments.component.scss']
})

export class EnvironmentsComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  private isDestroyed$ = new Subject<boolean>();
  public environments: Environment[] = [];
  public environmentsListLoading = true;
  public authFailure = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.store.dispatch(new GetEnvironments({
      server_id: this.serverId, org_id: this.orgId
    }));

    combineLatest([
      this.store.select(getAllEnvironmentsForOrgStatus),
      this.store.select(allEnvironments)
    ]).pipe(takeUntil(this.isDestroyed$))
    .subscribe(([ getEnvironmentsSt, allEnvironmentsState]) => {
      if (getEnvironmentsSt === EntityStatus.loadingSuccess && !isNil(allEnvironmentsState)) {
        this.environments = allEnvironmentsState;
        this.environmentsListLoading = false;
      } else if (getEnvironmentsSt === EntityStatus.loadingFailure) {
        this.environmentsListLoading = false;
        this.authFailure = true;
      }
    });
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  ngOnDestroy(): void {
    this.isDestroyed$.next(true);
    this.isDestroyed$.complete();
  }
}
