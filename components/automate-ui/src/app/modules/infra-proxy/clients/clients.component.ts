import { Component, Input, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { filter } from 'rxjs/operators';
import { isNil } from 'lodash/fp';
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

export class ClientsComponent implements OnInit {
  @Input() serverId: string;
  @Input() orgId: string;

  public clients: Client[] = [];
  public clientsListLoading = true;

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
    ]).pipe(
      filter(([getClientsSt, _allClientsState]) =>
      getClientsSt === EntityStatus.loadingSuccess && !isNil(_allClientsState))
    ).subscribe(([ _getClientsSt, allClientsState]) => {
      this.clients = allClientsState;
      this.clientsListLoading = false;
    });
  }
}
