import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { Router } from '@angular/router';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { NgrxStateAtom } from '../../../ngrx.reducers';
import { LayoutFacadeService, Sidebar } from '../../../entities/layout/layout.facade';
import { routeParams } from '../../../route.selectors';
import { filter, pluck, takeUntil } from 'rxjs/operators';
import { identity } from 'lodash/fp';
import { clientFromRoute } from '../../../entities/clients/client-details.selectors';
import { GetClient } from '../../../entities/clients/client.action';
import { Client } from '../../../entities/clients/client.model';
import { Org } from '../../../entities/orgs/org.model';
export type ClientTabName = 'details';

@Component({
  selector: 'app-client-details',
  templateUrl: './client-details.component.html',
  styleUrls: ['./client-details.component.scss']
})

export class ClientDetailsComponent implements OnInit, OnDestroy {
  public org: Org;
  public client: Client;
  public tabValue: ClientTabName = 'details';
  public url: string;
  public conflictErrorEvent = new EventEmitter<boolean>();
  public modalType: string;
  public serverId: string;
  public orgId: string;
  public name: string;
  public show = false;
  public clientTabLoading = true;
  private isDestroyed = new Subject<boolean>();
  public clientDetailsLoading = true;
  public openNotificationModal = new EventEmitter<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('orgid'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('name'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id, name]: string[]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.name = name;
      this.store.dispatch(new GetClient({
        server_id: server_id, org_id: org_id, name: name
      }));
    });

    this.store.select(clientFromRoute as any).pipe(
      filter(identity),
      takeUntil(this.isDestroyed)
    ).subscribe(client => {
      this.show = true;
      this.client = client;
      this.clientDetailsLoading = false;
      this.clientTabLoading = false;
    });
  }

  onSelectedTab(event: { target: { value: ClientTabName } } | any) {
    this.tabValue = event.target.value;
    this.router.navigate([this.url.split('#')[0]], { fragment: event.target.value });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  openResetKeyClientModal() {
    this.openNotificationModal.emit();
  }
}
