import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil 
  //, filter
 } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { GetClients, DeleteClient } from 'app/entities/clients/client.action';
import { GetClients, CreateClient } from 'app/entities/clients/client.action';
import { Client } from 'app/entities/clients/client.model';
import {
  getAllStatus,
  clientList,
  // saveStatus,
  saveError,
  createClient
} from 'app/entities/clients/client.selectors';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Regex } from 'app/helpers/auth/regex';
// import { EntityStatus } from 'app/entities/entities';
import { HttpStatus } from 'app/types/types';

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
  public page = 1;
  public per_page = 9;
  public total: number;
  public clientToDelete: Client;
  public deleteModalVisible = false;
  private isDestroyed = new Subject<boolean>();

  public createClientModalVisible = false;
  public creatingClient = false;
  public createClientForm: FormGroup;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public createModalVisible = false;
  public created = false;
  public privateKey;
  public client_key;
  public validator = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private fb: FormBuilder,
  ) { 
    this.createClientForm = this.fb.group({
      name: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
      validator: ['']
    });
  }

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
        if (this.clientListState.items.length === 0 && this.clientListState.total !== 0) {
          this.store.dispatch(new GetClients(payload));
          this.clientsListLoading = false;
        } else {
          this.clients = ClientsState?.items;
          this.total = ClientsState?.total;
          this.clientsListLoading = false;
          this.searching = false;
        }
      }
    });

    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(saveError),
      this.store.select(createClient)
    ]).pipe(
      takeUntil(this.isDestroyed))
      // filter(() => this.createModalVisible),
      // filter(([state, error]) => state === EntityStatus.loadingFailure && !isNil(error)))
      .subscribe(([_getClientsSt, error ,createState]) => {
        if(!isNil(createState)){
          this.created = true;
          this.client_key = createState?.client_key
          this.privateKey = this.client_key?.private_key;
          console.log(this.privateKey);
        }
        if (error.status === HttpStatus.CONFLICT) {
          this.conflictErrorEvent.emit(true);
        } else {
          // Close the modal on any error other than conflict and display in banner.
          this.closeCreateClientModal();
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

  openCreateClientModal() {
    this.createClientModalVisible = true;
  }

  public closeCreateClientModal(): void {
    this.createClientModalVisible = false;
  }

  createClient() {
    this.creatingClient = true;
    const client = {
      name: this.createClientForm.controls['name'].value.trim(),
      validator: this.createClientForm.controls['validator'].value,
      server_id: this.serverId,
      org_id: this.orgId,
      create_key : true
    };
    this.store.dispatch(new CreateClient(client));
  }

  downloadContentItem() {

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
    this.closeDeleteModal();
    this.store.dispatch(new DeleteClient({
      server_id: this.serverId, org_id: this.orgId, name: this.clientToDelete.name
    }));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }
}
