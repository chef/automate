import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil, filter } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { EntityStatus } from 'app/entities/entities';
import { GetClients, ClientSearch } from 'app/entities/clients/client.action';
import { Client } from 'app/entities/clients/client.model';
import {
  allClients,
  getAllStatus as getAllClientsForOrgStatus,
  getSearchStatus
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
  public clientSearch: Client[];
  public clientName: string;
  public searching = false;
  public searchText = '';

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

    combineLatest([
      this.store.select(getSearchStatus),
      this.store.select(allClients)
    ]).pipe(
      filter(([getClientsSt, _ClientsState]) =>
      getClientsSt === EntityStatus.loadingSuccess),
      filter(([_getClientsSt, clientsState]) =>
        !isNil(clientsState)),
      takeUntil(this.isDestroyed))
    .subscribe(([_getClientsSt, clientsState]) => {
      this.clients = clientsState;
      this.searching = false;
    });
  }

  toggleFilters(currentText: string) {
    this.searching = true;
    this.searchText = currentText;
    const payload = {
      clientName: currentText,
      page: 0,
      per_page: this.clients.length,
      server_id: this.serverId,
      org_id: this.orgId
    };
    this.store.dispatch(new ClientSearch(payload));
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
