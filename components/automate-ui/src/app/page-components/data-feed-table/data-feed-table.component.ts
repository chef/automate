import {
  Component,
  OnInit,
  OnChanges,
  Input} from '@angular/core';
import { MatOptionSelectionChange } from '@angular/material/core';
import { DeleteDestination, EnableDisableDestination, TestDestination } from 'app/entities/destinations/destination.actions';
import { Destination } from 'app/entities/destinations/destination.model';
import { Observable, Subject } from 'rxjs';
import { select, Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { filter, takeUntil } from 'rxjs/operators';
import { EntityStatus,pending} from 'app/entities/entities';
import { destinationEnableStatus } from 'app/entities/destinations/destination.selectors';

@Component({
  selector: 'app-data-feed-table',
  templateUrl: './data-feed-table.component.html',
  styleUrls: ['./data-feed-table.component.scss']
})
export class DataFeedTableComponent implements OnInit, OnChanges{

  public loading$: Observable<boolean>;
  public sortedDestinations$: Observable<Destination[]>;
  public createModalVisible = false;
  public creatingDataFeed = false;
  public dataFeedToDelete: Destination;
  public deleteModalVisible = false;
  public sendingDataFeed = false;
  @Input()  destinations: Destination[];
  columnDropdownVisible = false;
  selectedSortField: string;
  serviceShow: true;
  public enable = false;
  public sortval:string = 'ASC';
  selectedFieldDirection: string = 'DESC';
  private isDestroyed = new Subject<boolean>();
  integration_typeShow: boolean = true;


  constructor(
    private store: Store<NgrxStateAtom>,
  ) {}

  ngOnInit() {
  }

  ngOnChanges(){

  }

  toggleColumnDropdown() {
    this.columnDropdownVisible = !this.columnDropdownVisible;
  }

  closeColumnDropdown() {
    this.columnDropdownVisible = false;
  }

  checkValue(e:any, val:string){
    if(val=='serviceShow') {
      this.serviceShow=e.target.checked
      console.log(e)
    } else if (val=='integration_typeShow'){
      this.integration_typeShow =e.target.checked
    }
  }
  public startDataFeedDelete($event: MatOptionSelectionChange, destination: Destination): void {
    if ($event.isUserInput) {
      this.dataFeedToDelete = destination;
      this.deleteModalVisible = true;
    }
  }

  public startDataFeedSendTest($event: MatOptionSelectionChange, destination: Destination) {
    if ($event.isUserInput) {
      this.store.dispatch(new TestDestination({destination}));
    }
  }

  public deleteDataFeed(): void {
    this.closeDeleteModal();
    this.store.dispatch(new DeleteDestination(this.dataFeedToDelete));
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

  sortIcon(): string {
    if (this.sortval === 'ASC' || 'DESC') {
      return 'sort-' + this.sortval.toLowerCase();
    } else {
      return 'sort';
    }
  }

  onToggleSort(field: string) {
    // if (this.selectedSortField === field) {
    //   const fieldDirection = this.selectedFieldDirection === 'ASC' ? 'DESC' : 'ASC';
    //   this.updateSort.emit({field: field, fieldDirection: fieldDirection});
    // } else {
    //   this.updateSort.emit({field: field, fieldDirection: this.defaultFieldDirection[field]});
    // }
    console.log(this.destinations[0]['name']);

    
    const fieldDirection = this.sortval
    console.log(fieldDirection);
    if(fieldDirection=='ASC')
    {
       this.destinations.sort((a,b) =>  (a[field] > b[field] ? 1 : -1));
       this.sortval='DESC'
    } else {
      this.destinations.sort((a,b) => (a[field] > b[field] ? -1 : 1))
      this.sortval='ASC'
    }
  }

  enableDestination(val:boolean,id: string){
    const destinationEnableObj = {
      id: id,
      enable: val
    }
    this.store.dispatch(new EnableDisableDestination({enableDisable: destinationEnableObj}));
    this.store.pipe(
      select(destinationEnableStatus),
      takeUntil(this.isDestroyed),
      filter(state => !pending(state)))
      .subscribe(state => {
        if (state === EntityStatus.loadingSuccess) {
          console.log("state",state)
        }
      });
  }


}

