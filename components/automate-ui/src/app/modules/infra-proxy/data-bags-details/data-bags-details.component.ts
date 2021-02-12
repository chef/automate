import { Component, OnInit, OnDestroy } from '@angular/core';
import { Store } from '@ngrx/store';
import { Subject, combineLatest } from 'rxjs';
import { filter, takeUntil, pluck } from 'rxjs/operators';
import { identity, isNil } from 'lodash/fp';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { routeParams } from 'app/route.selectors';

import { GetDataBagDetails, DataBagSearchDetails } from 'app/entities/data-bags/data-bag-details.actions';
import { DataBags, DataBagsItemDetails } from 'app/entities/data-bags/data-bags.model';
import { allDataBagDetails, getAllStatus, getSearchStatus } from 'app/entities/data-bags/data-bag-details.selector';
import { GetDataBagItemDetails } from 'app/entities/data-bags/data-bag-item-details.actions';
import { dataBagItemDetailsFromRoute, getStatus } from 'app/entities/data-bags/data-bag-item-details.selector';

export type DataBagsDetailsTab = 'details';

@Component({
  selector: 'app-data-bags-details',
  templateUrl: './data-bags-details.component.html',
  styleUrls: ['./data-bags-details.component.scss']
})
export class DataBagsDetailsComponent implements OnInit, OnDestroy {
  private isDestroyed = new Subject<boolean>();
  public dataBagDetails: DataBags[];
  public dataBagSearch: DataBags[];
  public dataBagItemDetails: DataBagsItemDetails;
  public serverId: string;
  public orgId: string;
  public dataBagsName: string;
  public tabValue: DataBagsDetailsTab = 'details';
  public dataBagsDetailsLoading = true;
  public selectedItem: string;
  public dataBagsItemDetailsLoading = false;
  public selectedItemDetails: object;
  public activeClassName: string;
  public searching = false;
  public searchValue: string;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Infrastructure);

    combineLatest([
      this.store.select(routeParams).pipe(pluck('id'), filter(identity)),
      this.store.select(routeParams).pipe(pluck('org-id'), filter(identity)),
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
        this.appendActiveToItems(this.dataBagDetails);
        this.dataBagsDetailsLoading = false;
      });

    combineLatest([
      this.store.select(getStatus),
      this.store.select(dataBagItemDetailsFromRoute)
    ]).pipe(
      filter(([getDataBagItemDetailsSt, _dataBagItemDetailsState]) =>
        getDataBagItemDetailsSt === EntityStatus.loadingSuccess),
      filter(([_getDataBagItemDetailsSt, dataBagItemDetailsState]) =>
        !isNil(dataBagItemDetailsState)),
      takeUntil(this.isDestroyed))
      .subscribe(([_getDataBagItemDetailsSt, dataBagItemDetailsState]) => {
        this.selectedItemDetails = JSON.parse(dataBagItemDetailsState.data);
        this.dataBagsItemDetailsLoading = false;
      });

    combineLatest([
      this.store.select(getSearchStatus),
      this.store.select(allDataBagDetails)
    ]).pipe(
      filter(([getDataBagsSt, _DataBagsState]) =>
      getDataBagsSt === EntityStatus.loadingSuccess),
      filter(([_getDataBagsSt, DataBagsState]) =>
        !isNil(DataBagsState)),
      takeUntil(this.isDestroyed))
    .subscribe(([_getDataBagsSt, DataBagsState]) => {
        this.dataBagDetails = DataBagsState;
        this.searching = false;
    });
  }

  public handleItemSelected(item: string, index: number): void {
    this.selectedItem = item;
    this.dataBagsItemDetailsLoading = true;
    this.store.dispatch(new GetDataBagItemDetails({
      server_id: this.serverId,
      org_id: this.orgId,
      name: this.dataBagsName,
      item_name: item
    }));

    this.dataBagDetails.filter(
      (d_item, i) => i !== index && d_item.active
    ).forEach(menu => menu.active = !menu.active);

    this.dataBagDetails[index].active = !this.dataBagDetails[index].active;
    this.activeClassName = 'autoHeight';
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  appendActiveToItems (items: DataBags[]) {
    items.forEach((i, index) => {
      const tempItem = i;
      tempItem['active'] = false;
      items[index] = tempItem;
    });
    this.dataBagDetails = items;
  }

  searchDataBagItems(currentText: string) {
    this.searchValue = currentText;
    this.searching = true;
    const payload = {
      databagName: currentText,
      server_id: this.serverId,
      org_id: this.orgId,
      name: this.dataBagsName,
      page: 0,
      per_page: this.dataBagDetails.length
    };

    this.store.dispatch(new DataBagSearchDetails(payload));
  }
}
