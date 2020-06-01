import { Component, OnInit, OnDestroy, ViewChild } from '@angular/core';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { filter, takeUntil, pluck } from 'rxjs/operators';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams } from 'app/route.selectors';
import { identity, isNil } from 'lodash/fp';

import { GetDataBagDetails } from 'app/entities/data-bags/data-bag-details.action';
import { DataBags, DataBagsItemDetails, ItemAttributes } from 'app/entities/data-bags/data-bags.model';
import { allDataBagDetails, getAllStatus } from 'app/entities/data-bags/data-bag-details.selector';
import { GetDataBagItemDetails } from 'app/entities/data-bags/data-bag-item-details.action';
import { allDataBagItemDetails, getStatus } from 'app/entities/data-bags/data-bag-item-details.selector';
import { JsonTreeTableComponent as JsonTreeTable } from './../json-tree-table/json-tree-table.component';

export type DataBagsDetailsTab = 'details';

@Component({
  selector: 'app-data-bags-details',
  templateUrl: './data-bags-details.component.html',
  styleUrls: ['./data-bags-details.component.scss']
})
export class DataBagsDetailsComponent implements OnInit, OnDestroy {
  private isDestroyed = new Subject<boolean>();
  public dataBagDetails: DataBags[];
  public dataBagItemDetails: DataBagsItemDetails[];
  public serverId: string;
  public orgId: string;
  public dataBagsName: string;
  public tabValue: DataBagsDetailsTab = 'details';
  public dataBagsDetailsLoading = true;
  public selectedItem: string;

  public selectedItemDetails = new ItemAttributes({
    id: '',
    data: '',
    name: ''
  });

  @ViewChild(JsonTreeTable, { static: true })
  tree: JsonTreeTable;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    console.log(this.selectedItemDetails);
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('orgid'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('name'), filter(identity))
    ]).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe(([server_id, org_id, dataBags_name]) => {
      this.serverId = server_id;
      this.orgId = org_id;
      this.dataBagsName = dataBags_name;
      this.store.dispatch(new GetDataBagDetails({
        server_id: server_id,
        org_id: org_id,
        name: dataBags_name
      }));
    });

    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(allDataBagDetails)
    ]).pipe(
      filter(([getDataBagDetailsSt, _dataBagDetailsState]) =>
        getDataBagDetailsSt === EntityStatus.loadingSuccess),
      filter(([_getDataBagDetailsSt, dataBagDetailsState]) =>
        !isNil(dataBagDetailsState)),
      takeUntil(this.isDestroyed))
      .subscribe(([_getDataBagDetailsSt, dataBagDetailsState]) => {
        this.dataBagDetails = dataBagDetailsState;
        this.dataBagsDetailsLoading = false;
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(allDataBagItemDetails)
    ]).pipe(
      filter(([getDataBagItemDetailsSt, _dataBagItemDetailsState]) =>
        getDataBagItemDetailsSt === EntityStatus.loadingSuccess),
      filter(([_getDataBagItemDetailsSt, dataBagItemDetailsState]) =>
        !isNil(dataBagItemDetailsState)),
      takeUntil(this.isDestroyed))
      .subscribe(([_getDataBagItemDetailsSt, dataBagItemDetailsState]) => {
        if (dataBagItemDetailsState.length) {
          this.selectedItemDetails = new ItemAttributes(dataBagItemDetailsState[0]);
        }
      });
  }

  public handleItemSelected(item: string): void {
    this.selectedItem = item;
    this.store.dispatch(new GetDataBagItemDetails({
      server_id: this.serverId,
      org_id: this.orgId,
      name: this.dataBagsName,
      item_name: item
    }));
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
