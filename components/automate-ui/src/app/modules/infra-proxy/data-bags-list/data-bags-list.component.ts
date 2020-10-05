import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { GetDataBags } from 'app/entities/data-bags/data-bags.actions';
import { DataBags } from 'app/entities/data-bags/data-bags.model';
import {
  allDataBags,
  getAllStatus as getAllDatabagsForOrgStatus
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

  private isDestroyed = new Subject<boolean>();
  public dataBags: DataBags[];
  public dataBagsListLoading = true;
  public authFailure = false;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

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
  }

  resetKeyTabRedirection(resetLink: boolean) {
    this.resetKeyRedirection.emit(resetLink);
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
