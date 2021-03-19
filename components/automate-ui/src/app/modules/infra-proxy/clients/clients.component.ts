import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { GetClients, DeleteClient } from 'app/entities/clients/client.action';
import { Client } from 'app/entities/clients/client.model';
import { getAllStatus, clientList, deleteStatus } from 'app/entities/clients/client.selectors';

@Component({
  selector: 'app-clients',
  templateUrl: './clients.component.html',
  styleUrls: ['./clients.component.scss']
})

export class ClientsComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  public clients: Client[] = [];
  public clientListState: { items: Client[], total: number };
  public clientsListLoading = true;
  public authFailure = false;
  public clientName: string;
  public searching = false;
  public searchValue = '';
  public current_page = 1;
  public per_page = 9;
  public total: number;
  public clientToDelete: Client;
  public deleteModalVisible = false;
  private isDestroyed = new Subject<boolean>();
  public openClientModal = new EventEmitter<void>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {}

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.getClientsData();

    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(clientList)
    ]).pipe(
      filter(([getClientsStatus, allClientsState]) =>
        getClientsStatus === EntityStatus.loadingSuccess &&
        !isNil(allClientsState)),
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

    this.store.select(deleteStatus).pipe(
      filter(status => status === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.searching = true;
        if (this.clients.length === 0) {
          this.current_page = this.current_page - 1;
        }
        this.getClientsData();
      });
    }

  searchClients(currentText: string) {
    this.current_page = 1;
    this.searching = true;
    this.searchValue = currentText;
    this.getClientsData();
  }

  onPageChange(event: number): void {
    this.current_page = event;
    this.searching = true;
    this.getClientsData();
  }

  getClientsData() {
    const payload = {
      clientName: this.searchValue,
      server_id: this.serverId,
      org_id: this.orgId,
      page: this.current_page,
      per_page: this.per_page
    };

    this.store.dispatch(new GetClients(payload));
  }

  openCreateClientModal() {
    this.openClientModal.emit();
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public startClientDelete(client: Client): void {
    this.clientToDelete = client;
    this.deleteModalVisible = true;
  }

  public deleteClient(): void {
    this.searching = true;
    this.closeDeleteModal();
    this.store.dispatch(new DeleteClient({
      server_id: this.serverId, org_id: this.orgId, name: this.clientToDelete.name
    }));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }
}
