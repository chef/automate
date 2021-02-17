import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { GetClients } from 'app/entities/clients/client.action';
import { Client } from 'app/entities/clients/client.model';
import {
  getAllStatus,
  clientList
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
  public clientListState: { items: Client[], total: number };
  public clientsListLoading = true;
  public authFailure = false;
  public clientName: string;
  public searching = false;
  public searchValue = '';
  public page = 1;
  public per_page = 9;
  public total: number;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    const payload = {
      clientName: '',
      server_id: this.serverId,
      org_id: this.orgId,
      page: this.page,
      per_page: this.per_page
    };
    this.store.dispatch(new GetClients(payload));

    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(clientList)
    ]).pipe(
      takeUntil(this.isDestroyed))
    .subscribe(([_getClientsSt, ClientsState]) => {
      if (!isNil(ClientsState)) {
        this.clientListState = ClientsState;
        this.clients = ClientsState?.items;
        this.total = ClientsState?.total;
        this.clientsListLoading = false;
        this.searching = false;
      }
    });
  }

  searchClients(currentText: string) {
    this.page = 1;
    this.searching = true;
    this.searchValue = currentText;
    this.getClientsData();
  }

  onPageChange(event: number): void {
    this.page = event;
    this.searching = true;
    this.getClientsData();
  }

  getClientsData() {
    const payload = {
      clientName: this.searchValue,
      server_id: this.serverId,
      org_id: this.orgId,
      page: this.page,
      per_page: this.per_page
    };

    this.store.dispatch(new GetClients(payload));
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
