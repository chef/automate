import { Component, OnInit, OnDestroy, EventEmitter, AfterViewInit } from '@angular/core';
import { Store, select } from '@ngrx/store';
import { get, toUpper, pick } from 'lodash/fp';
import { Observable, Subject } from 'rxjs';
import { map, takeUntil } from 'rxjs/operators';
import * as moment from 'moment/moment';
import { MatOptionSelectionChange } from '@angular/material/core';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { DateTime } from 'app/helpers/datetime/datetime';
import { NodeCredentialsSearch, DeleteNodeCredential } from 'app/entities/node-credentials/node-credential.actions';
import { loading } from 'app/entities/entities';
import {
  getAllStatus
} from 'app/entities/node-credentials/node-credential.selectors';
import { NodeCredential, NodeCredentialTypes } from 'app/entities/node-credentials/node-credential.model';
import {
  allCredentials,
  totalNodeCredential
} from 'app/entities/node-credentials/node-credential.selectors';

import { NodeCredentialOrder, SortParams } from './node-credential-list.reducer';
import { nodeCredentialListState } from './node-credential-list.selectors';
import { SortNodeCredentialList } from './node-credential-list.actions';
import { ActivatedRoute, ParamMap } from '@angular/router';
@Component({
  selector: 'app-node-credential-list',
  templateUrl: './node-credential-list.component.html',
  styleUrls: ['./node-credential-list.component.scss']
})
export class NodeCredentialListComponent implements OnInit, OnDestroy, AfterViewInit {
  private isDestroyed = new Subject<boolean>();
  public loading$: Observable<boolean>;
  public instanceNodeCredentials$: Observable<NodeCredential[]>;
  public openUserModal = new EventEmitter<boolean>();
  public orderBy: NodeCredentialOrder;
  public nodeCredentialToDelete: NodeCredential;
  public deleteModalVisible = false;
  public nodesListLoading = true;
  public sortBy: string;
  public params: SortParams;

  public throttle = 300;
  public scrollDistance = 2;
  public credentialArray: NodeCredential[] = [];
  public total: number;
  public scrollingLoader = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private route: ActivatedRoute
  ) {
    this.loading$ = store.select(getAllStatus).pipe(map(loading));
  }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.instanceNodeCredentials$ = this.store.pipe(
      takeUntil(this.isDestroyed),
      select(allCredentials)
    );
    this.instanceNodeCredentials$.subscribe((results) => {
      if (this.scrollingLoader) {
        this.credentialArray = [...this.credentialArray, ...results];
        this.scrollingLoader = false;
      } else {
        this.credentialArray = results;
        this.nodesListLoading = false;
      }
    });
    this.instanceNodeCredentials$ = this.store.pipe(select(allCredentials));
    this.store.select(nodeCredentialListState).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe((state) => {
      this.params = pick(['filters', 'page', 'per_page'], state);

      this.sortBy = get('sort', state);
      this.orderBy = get('order', state);

      if (this.orderBy !== 'none') {
        this.params = { ...this.params, order: toUpper(this.orderBy), sort: this.sortBy };
      }
    });
    this.store.select(totalNodeCredential).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe((total) => {
      this.total = total;
    });
    this.getNodeList();
  }

  ngAfterViewInit() {
    this.route.paramMap.subscribe((params: ParamMap) => {
      if (params.get('action') === 'add') {
        setTimeout(() => {
          this.openCreateModal();
        });
      }
    });
  }

  appendItems() {
    this.params.page++;
    this.getNodeList();
  }

  onScrollDown() {
    if (this.credentialArray.length < this.total) {
      this.scrollingLoader = true;
      this.appendItems();
    }
  }

  openCreateModal(): void {
    this.openUserModal.emit(true);
  }

  orderFor(column: string): string {
    return column === this.sortBy ? this.orderBy : 'none';
  }

  handleSortToggle({ detail: sortParams }): void {
    this.nodesListLoading = true;
    this.store.dispatch(new SortNodeCredentialList(sortParams));
    this.getNodeList();
  }

  formatKeyType(credentialType: NodeCredentialTypes): string {
    switch (credentialType) {
      case NodeCredentialTypes.SSH:
        return 'SSH';
      case NodeCredentialTypes.WinRM:
        return 'WinRM';
      case NodeCredentialTypes.Sudo:
        return 'Sudo';
      default:
        return 'SSH';
    }
  }

  startNodeCredentialDelete($event: MatOptionSelectionChange, p: NodeCredential): void {
    if ($event.isUserInput) {
      this.nodeCredentialToDelete = p;
      this.deleteModalVisible = true;
    }
  }

  getNodeList(): void {
    this.store.dispatch(new NodeCredentialsSearch(this.params));
  }

  deleteNodeCredential(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteNodeCredential(this.nodeCredentialToDelete));
  }

  closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  getLastModified(lastModifiedDate): string {
    return moment.utc(lastModifiedDate).format(DateTime.RFC2822);
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
