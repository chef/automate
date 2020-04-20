import { Component, Input, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { filter } from 'rxjs/operators';
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

export class EnvironmentsComponent implements OnInit {
  @Input() serverId: string;
  @Input() orgId: string;

  public environments: Environment[] = [];
  public environmentsListLoading = true;

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
    ]).pipe(
      filter(([getEnvironmentsSt, _allEnvironmentsState]) =>
      getEnvironmentsSt === EntityStatus.loadingSuccess && !isNil(_allEnvironmentsState))
    ).subscribe(([ _getEnvironmentsSt, allEnvironmentsState]) => {
      this.environments = allEnvironmentsState;
      this.environmentsListLoading = false;
    });
  }
}
