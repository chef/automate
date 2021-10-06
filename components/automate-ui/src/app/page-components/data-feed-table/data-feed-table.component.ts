import {
  Component,
  Input
} from '@angular/core';
import { MatOptionSelectionChange } from '@angular/material/core';
import { DeleteDestination, EnableDisableDestination } from 'app/entities/destinations/destination.actions';
import { Destination } from 'app/entities/destinations/destination.model';
import { Observable } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';

@Component({
  selector: 'app-data-feed-table',
  templateUrl: './data-feed-table.component.html',
  styleUrls: ['./data-feed-table.component.scss']
})
export class DataFeedTableComponent  {

  public loading$: Observable<boolean>;
  public sortedDestinations$: Observable<Destination[]>;
  public createModalVisible = false;
  public creatingDataFeed = false;
  public dataFeedToDelete: Destination;
  public deleteModalVisible = false;
  public sendingDataFeed = false;
  @Input()  destinations: Destination[] = [];
  public selectedSortField: string;
  columnDropdownVisible = false;
  public enable = false;
  public sortval = 'DESC';
  selectedFieldDirection = 'DESC';
  integration_typeShow = true;
  serviceShow = true;

  constructor(
    private store: Store<NgrxStateAtom>
  ) {}


  toggleColumnDropdown() {
    this.columnDropdownVisible = !this.columnDropdownVisible;
  }

  closeColumnDropdown() {
    this.columnDropdownVisible = false;
  }

  checkValue(e: any, val: string) {
    if (val === 'serviceShow') {
      this.serviceShow = e.target.checked;
    } else if (val === 'integration_typeShow') {
      this.integration_typeShow = e.target.checked;
    }
  }
  public startDataFeedDelete($event: MatOptionSelectionChange, destination: Destination): void {
    if ($event.isUserInput) {
      this.dataFeedToDelete = destination;
      this.deleteModalVisible = true;
    }
  }

  public deleteDataFeed(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteDestination(this.dataFeedToDelete));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  sortIcon(field: string): string {
    if (field === this.selectedSortField && (this.sortval === 'ASC' || 'DESC')) {
      return 'sort-' + this.sortval.toLowerCase();
    } else {
      return 'sort';
    }
  }

  onToggleSort(field: string) {
    this.selectedSortField = field;
    this.sortval = this.sortval === 'ASC' ? 'DESC' : 'ASC';
    if (this.sortval === 'ASC') {
       this.destinations = this.destinations.sort((a, b) =>  (a[field] > b[field] ? 1 : -1));
    } else {
      this.destinations = this.destinations.sort((a, b) => (a[field] > b[field] ? -1 : 1));
    }
  }

  enableDestination(val: boolean, id: string) {
    const destinationEnableObj = {
      id: id,
      enable: val
    };
    this.store.dispatch(new EnableDisableDestination({enableDisable: destinationEnableObj}));
  }


}

