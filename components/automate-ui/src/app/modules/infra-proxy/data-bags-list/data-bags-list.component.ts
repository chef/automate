import { Component, Input, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { combineLatest } from 'rxjs';
import { filter } from 'rxjs/operators';
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

export class DataBagsListComponent implements OnInit {
  @Input() serverId: string;
  @Input() orgId: string;

  public dataBags: DataBags[];
  public dataBagsListLoading = true;

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
    ]).pipe(
      filter(([getDataBagsSt, allDataBagsState]) =>
      getDataBagsSt === EntityStatus.loadingSuccess && !isNil(allDataBagsState))
    ).subscribe(([ _getDataBagsSt, allDataBagsState]) => {
      this.dataBags = allDataBagsState;
      this.dataBagsListLoading = false;
    });
  }
}
