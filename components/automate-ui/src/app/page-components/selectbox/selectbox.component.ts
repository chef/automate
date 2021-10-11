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
  // It can be anything pass type in parent component
  // eg: type = "winrm"
  @Input() type: any;
  // searchFlag is optional
  @Input() searchFlag: boolean;
  @Input() combination: boolean;
  @Output() searchData = new EventEmitter<string>();
  @Output() selectData = new EventEmitter<any[]>();
  @Output() onScrollListData = new EventEmitter();
  public scrollDistance = 2;


  public listData: any[] = [];
  public selectedListData: any[] = [];
  public selectedListDataToMove: any[] = [];
  public selectedListTypeToMove: string;
  public moveLeftOrRight: string;
  nodeSuggestions$: any;
  public copyDataListFlags: boolean;
  public scrollingLoader: boolean;
  newItemEvent: any;

  constructor(private store: Store<NgrxStateAtom>) { }

  ngOnInit() {
    this.scrollingLoader = false;
    this.copyDataListFlags = false;
    this.moveLeftOrRight = '';
    this.selectedListTypeToMove = '';
  }

  ngOnChanges() {
    if (!this.copyDataListFlags && this.data === this.listData || this.data !== null ) {
      this.listData = [...this.data];
      this.selectData.emit(this.selectedListData);
      this.selectedListData.forEach((listDataValue) => {
        if (listDataValue.type === this.type) {
          this.listData.splice(this.listData.indexOf(listDataValue), 1);
        }
      });
      this.copyDataListFlags = true;
    }

    if (!this.combination) {
      if (this.type !== this.selectedListTypeToMove) {
        this.selectedListDataToMove = [];
      }
    }

  }

  emitData(data: string) {
    this.searchData.emit(data);
  }

  appendItems() {

  }

  onScrollDown() {
    this.onScrollListData.emit();
    console.log('onScrollListData');
  }

  selectItem(listData: string, side: string, secretType: string): void {
    this.selectedListTypeToMove = secretType;
    if (this.selectedListTypeToMove === secretType || this.selectedListTypeToMove === '' ) {
      if (this.selectedListDataToMove.includes(listData)) {
        this.selectedListDataToMove.splice(this.selectedListDataToMove.indexOf(listData), 1);
      } else {
      this.selectedListDataToMove.push(listData);
      }
    }

    if (this.selectedListDataToMove.length === 0) {
      this.moveLeftOrRight = '';
      this.selectedListTypeToMove = '';
    } else {
      this.moveLeftOrRight = side;
    }
  }


  highlightElementLeft(listData: string): boolean {
    return this.selectedListDataToMove.includes(listData);
  }

  highlightElement(listData: string): boolean {
    return this.selectedListDataToMove.includes(listData);
}

  moveRight() {
    if (!this.combination) {
        this.selectedListData = [];
    }
    this.selectedListDataToMove.forEach(selectedListData => {
      this.selectedListData.unshift(selectedListData);
      this.listData.splice(this.listData.indexOf(selectedListData), 1);
    });
    this.selectedListDataToMove = [];
    this.moveLeftOrRight = '';
  }

  moveLeft() {
    this.selectedListDataToMove.forEach(selectedListData => {
      this.listData.push(selectedListData);
      this.selectedListData.splice(this.selectedListData.indexOf(selectedListData), 1);
    });
    this.selectedListDataToMove = [];
    this.moveLeftOrRight = '';
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
