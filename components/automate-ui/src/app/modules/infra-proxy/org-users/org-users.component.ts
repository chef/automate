import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { OrgUser } from '../../../entities/org-users/org-users.model';
import { GetUsers } from '../../../entities/org-users/org-users.action';
import { getAllStatus, orgUserList } from '../../../entities/org-users/org-users.selectors';
import { Store } from '@ngrx/store';

import { combineLatest, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';

@Component({
  selector: 'app-org-users',
  templateUrl: './org-users.component.html',
  styleUrls: ['./org-users.component.scss']
})

export class OrgUsersComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  public usersState: { users: OrgUser[] };
  public users: OrgUser[] = [];
  public usersListLoading = false;
  public authFailure = false;
  public loading = false;
  public current_page = 1;
  public per_page = 100;
  public total: number;
  public name: string;
  private isDestroyed = new Subject<boolean>();
  public openNotificationModal = new EventEmitter<void>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) {}

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.getOrgUsersData();

    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(orgUserList)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([getUsersSt, OrgUsersState]) => {
      if (getUsersSt === EntityStatus.loadingSuccess && !isNil(OrgUsersState)) {
        this.usersState = OrgUsersState;
        this.users = this.usersState.users;
        this.usersListLoading = false;
        this.loading = false;
      } else if (getUsersSt === EntityStatus.loadingFailure) {
        this.usersListLoading = false;
        this.authFailure = true;
      }
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  getOrgUsersData(): void {
    const payload = {
      server_id: this.serverId,
      org_id: this.orgId
    };

    this.store.dispatch(new GetUsers(payload));
  }

  onPageChange(event: number): void {
    this.current_page = event;
    this.loading = true;
  }

  resetPEMKey(name) {
    console.log('resetPEM', name);
    this.name = name;
    this.openNotificationModal.emit();
  }

  onUpdatePage($event: { pageIndex: number; pageSize: number; }) {
    this.current_page = $event.pageIndex + 1;
    this.per_page = $event.pageSize;
    this.loading = true;
  }
}
