import { Component, OnInit, OnDestroy, EventEmitter } from '@angular/core';
import { Store, select } from '@ngrx/store';
import { get, toUpper, pick } from 'lodash/fp';
import { Observable, Subject } from 'rxjs';
import { map, takeUntil } from 'rxjs/operators';
// import * as moment from 'moment/moment';
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
  allCredentials
} from 'app/entities/node-credentials/node-credential.selectors';

import { NodeCredentialOrder, SortParams } from './node-credential-list.reducer';
import { nodeCredentialListState } from './node-credential-list.selectors';
import { SortNodeCredentialList } from './node-credential-list.actions';

import { UserPreferencesService } from 'app/services/user-preferences/user-preferences.service';

@Component({
  selector: 'app-node-credential-list',
  templateUrl: './node-credential-list.component.html',
  styleUrls: ['./node-credential-list.component.scss']
})
export class NodeCredentialListComponent implements OnInit, OnDestroy {
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
  public readonly DEMO_MODE = DateTime.DEMO_MODE;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    public userPrefsService: UserPreferencesService
  ) {
    this.loading$ = store.select(getAllStatus).pipe(map(loading));
  }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Settings);
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
    this.getNodeList();
  }

  openCreateModal(): void {
    this.openUserModal.emit(true);
  }

  orderFor(column: string): string {
    return column === this.sortBy ? this.orderBy : 'none';
  }

  handleSortToggle({ detail: sortParams }): void {
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
    this.nodesListLoading = false;
  }

  deleteNodeCredential(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteNodeCredential(this.nodeCredentialToDelete));
  }

  closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  // getLastModified(lastModifiedDate): string {
  //   return moment.utc(lastModifiedDate).format(DateTime.RFC2822);
  // }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
