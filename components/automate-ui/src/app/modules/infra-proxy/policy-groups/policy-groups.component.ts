import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { GetPolicyFiles } from 'app/entities/policy-files/policy-file.action';
import { PolicyFile } from 'app/entities/policy-files/policy-file.model';
import {
  allPolicyFiles,
  getAllStatus as getAllPolicyFilesForOrgStatus
} from 'app/entities/policy-files/policy-file.selectors';
@Component({
  selector: 'app-policy-groups',
  templateUrl: './policy-groups.component.html',
  styleUrls: ['./policy-groups.component.scss']
})
export class PolicyGroupsComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  private isDestroyed = new Subject<boolean>();
  public policyGroups: PolicyFile[] = [];
  public policyGroupsListLoading = true;
  public authFailure = false;
  public searchValue = '';
  public searchFlag = false;
  public searchArr: [];

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.getPolicyGropus();

    combineLatest([
      this.store.select(getAllPolicyFilesForOrgStatus),
      this.store.select(allPolicyFiles)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([ getPolicyFilesSt, allPolicyFilesState]) => {
      if (getPolicyFilesSt === EntityStatus.loadingSuccess && !isNil(allPolicyFilesState)) {
        this.policyGroups = allPolicyFilesState;
        this.policyGroupsListLoading = false;
        // console.log("this.policyGroups -->", this.policyGroups)
      } else if (getPolicyFilesSt === EntityStatus.loadingFailure) {
        this.policyGroupsListLoading = false;
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

  getPolicyGropus() {
    const payload = {
      server_id: this.serverId,
      org_id: this.orgId
    };
    this.store.dispatch(new GetPolicyFiles(payload));
  }
}
