import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { EntityStatus } from 'app/entities/entities';
import { GetPolicyFiles } from 'app/entities/policy-files/policy-file.action';
import { PolicyFile } from 'app/entities/policy-files/policy-file.model';
import {
  allPolicyFiles,
  getAllStatus as getAllPolicyFilesForOrgStatus
} from 'app/entities/policy-files/policy-file.selectors';


@Component({
  selector: 'app-policy-files',
  templateUrl: './policy-files.component.html',
  styleUrls: ['./policy-files.component.scss']
})

export class PolicyFilesComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  private isDestroyed = new Subject<boolean>();
  public policyFiles: PolicyFile[] = [];
  public policyFilesListLoading = true;
  public authFailure = false;
  public searching = false;
  public searchValue = '';
  public searchFlag = false;
  public serachArr: PolicyFile[];
  pageOfItems: Array<any>;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.store.dispatch(new GetPolicyFiles({
      server_id: this.serverId, org_id: this.orgId
    }));

    combineLatest([
      this.store.select(getAllPolicyFilesForOrgStatus),
      this.store.select(allPolicyFiles)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([ getPolicyFilesSt, allPolicyFilesState]) => {
      if (getPolicyFilesSt === EntityStatus.loadingSuccess && !isNil(allPolicyFilesState)) {
        this.policyFiles = allPolicyFilesState;
        this.policyFiles.push({
          name: 'test3',
          revision_id: '0',
          policy_group: 'e'
        });
        this.policyFiles.push({
          name: 'test4',
          revision_id: '0',
          policy_group: 'e'
        });
        this.policyFiles.push({
          name: 'test5',
          revision_id: '0',
          policy_group: 'e'
        });
        this.policyFiles.push({
          name: 'test6',
          revision_id: '0',
          policy_group: 'e'
        });
        this.policyFiles.push({
          name: 'new1',
          revision_id: '0',
          policy_group: 'e'
        });
        this.policyFilesListLoading = false;
      } else if (getPolicyFilesSt === EntityStatus.loadingFailure) {
        this.policyFilesListLoading = false;
        this.authFailure = true;
      }
    });
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  onChangePage(pageOfItems: Array<any>) {
    // update current page of items
    this.pageOfItems = pageOfItems;
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public searchPolicyFiles(searchText: string): void {
    this.searching = true;
    this.searchValue = searchText;
    if (!this.policyFiles || !searchText) {
      this.searchFlag = false;
    } else {
      this.serachArr = this.policyFiles.filter((key) => {
        this.searchFlag = true;
        if (key) {
          return key.name.includes(searchText);
        }
      });
    }
    this.searching = false;
  }
}
