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
import { Regex } from 'app/helpers/auth/regex';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

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
  public loading = false;
  public searchValue = '';
  public current_page = 1;
  public per_page = 100;
  public total: number;
  public clientToDelete: Client;
  public deleteModalVisible = false;
  public deleting = true;
  private isDestroyed = new Subject<boolean>();
  public openClientModal = new EventEmitter<void>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private telemetryService: TelemetryService
  ) {}

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.getClientsData();

    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(clientList)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([getClientsSt, ClientsState]) => {
      if (getClientsSt === EntityStatus.loadingSuccess && !isNil(ClientsState)) {
        this.clientListState = ClientsState;
        this.clients = ClientsState?.items;
        this.total = ClientsState?.total;
        this.clientsListLoading = false;
        this.loading = false;
        this.deleting = false;
      } else if (getClientsSt === EntityStatus.loadingFailure) {
        this.clientsListLoading = false;
        this.authFailure = true;
      }
    });

    this.store.select(deleteStatus).pipe(
      filter(status => status === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.loading = true;
        if (this.clients.length === 0 &&
          this.current_page !== 1) {
          this.current_page = this.current_page - 1;
        }
        this.getClientsData();
      });
    }

  searchClients(currentText: string) {
    this.current_page = 1;
    this.loading = true;
    this.searchValue = currentText;
    if ( currentText !== ''  && !Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN.test(currentText)) {
      this.loading = false;
      this.clients.length = 0;
      this.total = 0;
    } else {
      this.getClientsData();
    }
    this.telemetryService.track('InfraServer_Clients_Search');
  }

  onPageChange(event: number): void {
    this.current_page = event;
    this.loading = true;
    this.getClientsData();
    this.telemetryService.track('InfraServer_Clients_GetClientsData');
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
    this.loading = true;
    this.closeDeleteModal();
    this.store.dispatch(new DeleteClient({
      server_id: this.serverId, org_id: this.orgId, name: this.clientToDelete.name
    }));
    this.telemetryService.track('InfraServer_Clients_Delete');
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
    this.deleting = true;
  }

  onUpdatePage($event: { pageIndex: number; pageSize: number; }) {
    this.current_page = $event.pageIndex + 1;
    this.per_page = $event.pageSize;
    this.loading = true;
    this.getClientsData();
    this.telemetryService.track('InfraServer_Clients_GetClientsData');
  }
}
