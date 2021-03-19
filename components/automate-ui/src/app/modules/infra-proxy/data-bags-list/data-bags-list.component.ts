import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { filter, takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { GetDataBags, DeleteDataBag } from 'app/entities/data-bags/data-bags.actions';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { DataBag } from 'app/entities/data-bags/data-bags.model';
import {
  allDataBags,
  getAllStatus as getAllDatabagsForOrgStatus,
  deleteStatus
} from 'app/entities/data-bags/data-bags.selectors';

@Component({
  selector: 'app-data-bags-list',
  templateUrl: './data-bags-list.component.html',
  styleUrls: ['./data-bags-list.component.scss']
})

export class DataBagsListComponent implements OnInit, OnDestroy {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  public authFailure = false;
  public chefInfraViewsFeatureFlagOn: boolean;
  public dataBags: DataBag[];
  public dataBagsListLoading = true;
  public dataBagToDelete: DataBag;
  public deleteModalVisible = false;
  public openDataBagModal = new EventEmitter<void>();
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    private featureFlagsService: FeatureFlagsService
  ) {
    // feature flag enable and disable the create button
    this.chefInfraViewsFeatureFlagOn =
    this.featureFlagsService.getFeatureStatus('chefInfraTabsViews');
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    this.store.dispatch(new GetDataBags({
      server_id: this.serverId, org_id: this.orgId
    }));

    combineLatest([
      this.store.select(getAllDatabagsForOrgStatus),
      this.store.select(allDataBags)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([ getDataBagsSt, allDataBagsState]) => {
      if (getDataBagsSt === EntityStatus.loadingSuccess && !isNil(allDataBagsState)) {
        this.dataBags = allDataBagsState;
        this.dataBagsListLoading = false;
      } else if (getDataBagsSt === EntityStatus.loadingFailure) {
        this.dataBagsListLoading = false;
        this.authFailure = true;
      }
    });

    this.store.select(deleteStatus).pipe(
      filter(status => status === EntityStatus.loadingSuccess),
      takeUntil(this.isDestroyed))
      .subscribe(() => {
        this.store.dispatch(new GetDataBags({
          server_id: this.serverId, org_id: this.orgId
        })
      );
    });
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public startDataBagDelete(dataBag: DataBag): void {
    this.dataBagToDelete = dataBag;
    this.deleteModalVisible = true;
  }

  public deleteDataBag(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteDataBag({
      server_id: this.serverId, org_id: this.orgId, name: this.dataBagToDelete.name
    }));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  public openCreateModal(): void {
    this.openDataBagModal.emit();
  }
}
