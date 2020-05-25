import { Component, Input, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { filter } from 'rxjs/operators';
import { isNil } from 'lodash/fp';
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

export class PolicyFilesComponent implements OnInit {
  @Input() serverId: string;
  @Input() orgId: string;

  public policyFiles: PolicyFile[] = [];
  public policyFilesListLoading = true;

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
    ]).pipe(
      filter(([getPolicyFilesSt, _allPolicyFilesState]) =>
      getPolicyFilesSt === EntityStatus.loadingSuccess && !isNil(_allPolicyFilesState))
    ).subscribe(([ _getPolicyFilesSt, allPolicyFilesState]) => {
      this.policyFiles = allPolicyFilesState;
      this.policyFilesListLoading = false;
    });
  }
}
