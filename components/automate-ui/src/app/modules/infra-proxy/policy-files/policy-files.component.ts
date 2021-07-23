import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { EntityStatus } from 'app/entities/entities';
import { GetPolicyFiles, DeletePolicyFile } from 'app/entities/policy-files/policy-file.action';
import { PolicyFile } from 'app/entities/policy-files/policy-file.model';
import {
  allPolicyFiles,
  getAllStatus as getAllPolicyFilesForOrgStatus,
  deleteStatus
} from 'app/entities/policy-files/policy-file.selectors';
import { Revision } from 'app/entities/revisions/revision.model';

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
  public searchArr: PolicyFile[];
  pageOfItems: Array<any>;

  public policyfileToDelete: PolicyFile;
  public deleteModalVisible = false;
  public deleting = false;

  public policyfileName: string;
  public revisions: Revision[] = [];
  // open revision id slider
  public openRevisionIdSlider = new EventEmitter<boolean>();
  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.getPolicyFiles();

    combineLatest([
      this.store.select(getAllPolicyFilesForOrgStatus),
      this.store.select(allPolicyFiles)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([ getPolicyFilesSt, allPolicyFilesState]) => {
      if (getPolicyFilesSt === EntityStatus.loadingSuccess && !isNil(allPolicyFilesState)) {
        this.policyFiles = allPolicyFilesState;
        this.policyFilesListLoading = false;
        this.deleting = false;
      } else if (getPolicyFilesSt === EntityStatus.loadingFailure) {
        this.policyFilesListLoading = false;
        this.authFailure = true;
      }
    });

    this.store.select(deleteStatus).pipe(
      filter(status => status === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.deleting = true;
        this.getPolicyFiles();
      });
  }

  getPolicyFiles() {
    const payload = {
      server_id: this.serverId,
      org_id: this.orgId
    };
    this.store.dispatch(new GetPolicyFiles(payload));
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
      this.searchArr = this.policyFiles.filter((key) => {
        this.searchFlag = true;
        if (key) {
          return key.name.includes(searchText);
        }
      });
    }
    this.searching = false;
  }

  public revisionIdList(policyFile: PolicyFile): void {
    this.policyfileName = policyFile.name;
    this.openRevisionIdSlider.emit();
  }

  public startpolicyFilesDelete(policyFile: PolicyFile): void {
    this.policyfileToDelete = policyFile;
    this.deleteModalVisible = true;
  }

  public deletePolicyfile(): void {
    this.deleting = true;
    this.closeDeleteModal();
    this.store.dispatch(new DeletePolicyFile({
      server_id: this.serverId, org_id: this.orgId, name: this.policyfileToDelete.name
    }));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
    this.deleting = false;
  }
}
