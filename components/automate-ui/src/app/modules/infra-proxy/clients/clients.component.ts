import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { EntityStatus } from 'app/entities/entities';
import { GetClients } from 'app/entities/clients/client.action';
import { Client } from 'app/entities/clients/client.model';
import {
  allClients,
  getAllStatus as getAllClientsForOrgStatus
} from 'app/entities/clients/client.selectors';


@Component({
  selector: 'app-clients',
  templateUrl: './clients.component.html',
  styleUrls: ['./clients.component.scss']
})

export class ClientsComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  private isDestroyed = new Subject<boolean>();
  public clients: Client[] = [];
  public clientsListLoading = true;
  public authFailure = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.store.dispatch(new GetClients({
      server_id: this.serverId, org_id: this.orgId
    }));

    combineLatest([
      this.store.select(getAllClientsForOrgStatus),
      this.store.select(allClients)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([getClientsSt, allClientsState]) => {
      if (getClientsSt === EntityStatus.loadingSuccess && !isNil(allClientsState)) {
        this.clients = allClientsState;
        this.clientsListLoading = false;
      } else if (getClientsSt === EntityStatus.loadingFailure) {
        this.clientsListLoading = false;
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
