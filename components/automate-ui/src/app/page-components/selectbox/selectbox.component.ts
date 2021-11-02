import { Component, EventEmitter, Input, OnChanges, OnInit, Output } from '@angular/core';
import { Store, createSelector } from '@ngrx/store';
import { clientRunsState } from 'app/entities/client-runs/client-runs.selectors';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import {
  map
} from 'rxjs/operators';


@Component({
  selector: 'app-selectbox',
  templateUrl: './selectbox.component.html',
  styleUrls: ['./selectbox.component.scss']
})

export class SelectboxComponent implements OnInit, OnChanges {
  // make sure to pass type of list and pass type in data
  // eg: data = [{id:xyz, type:winrm}]
  @Input() data: any;
  // can can be anything u pass from parent component
  // eg: type = "winrm"
  @Input() type: any;
  // searchFlag is optional
  @Input() searchFlag: boolean;
  @Output() searchData = new EventEmitter<string>();
  @Output() selectData = new EventEmitter<any[]>();

  public listData: any[] = [];
  public selectedListData: any[] = [];
  public selectedListDataToMove: any[] = [];
  public moveLeftOrRight: string;
  nodeSuggestions$: any;
  public copyDataListFlags: boolean;
  newItemEvent: any;
  constructor(private store: Store<NgrxStateAtom>) { }

  ngOnInit() {
    this.copyDataListFlags = false;
  }

  ngOnChanges() {
    if (!this.copyDataListFlags && this.data === this.listData || this.data !== null ) {
      this.listData = JSON.parse(JSON.stringify(this.data));
      this.selectData.emit(this.selectedListData);
      this.selectedListData.forEach((listDataValue) => {
        if (listDataValue.type === this.type) {
          this.listData.splice(this.listData.indexOf(listDataValue), 1);
        }
      });
      this.copyDataListFlags = true;
    }
  }

  emitData(data: string) {
    this.searchData.emit(data);
  }

  selectItem(listData: string): void {
    this.moveLeftOrRight = 'Left';
      if (this.selectedListDataToMove.includes(listData)) {
        this.selectedListDataToMove.splice(this.selectedListDataToMove.indexOf(listData), 1);
      } else {
      this.selectedListDataToMove.push(listData);
      }
    console.log(this.selectedListDataToMove);
  }


  highlightElementLeft(listData: string): boolean {
    return this.selectedListDataToMove.includes(listData);
  }

  highlightElement(listData: string): boolean {
    return this.selectedListDataToMove.includes(listData);
}

  moveRight() {
    this.selectedListDataToMove.forEach(selectedListData => {
      this.selectedListData.push(selectedListData);
      this.listData.splice(this.listData.indexOf(selectedListData), 1);
    });
    this.selectedListDataToMove = [];
  }

  moveLeft() {
    this.selectedListDataToMove.forEach(selectedListData => {
      this.listData.push(selectedListData);
      this.selectedListData.splice(this.selectedListData.indexOf(selectedListData), 1);
    });
    this.selectedListDataToMove = [];
  }

  onSuggestValues(event: any) {
    console.log(event);
  }
  onFilterAdded(event: any) {
    console.log(event);
  }
  toggleFilters() {
    console.log('aaa');
  }
  suggestion() {
    return this.nodeSuggestions$ = this.store.select(createSelector(clientRunsState,
      (state) => state.nodeSuggestions)).pipe(map((nodeSuggestions: any[]) =>
      nodeSuggestions.map(item => item.text)));
  }
}
