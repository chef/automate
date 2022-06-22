import { Component, EventEmitter, Input, OnChanges, OnInit, Output } from '@angular/core';

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
  // eg: typeValue = "winrm",  typeFieldName= "type"
  @Input() typeValue: string;
  @Input() typeFieldName: string;
  // searchFlag is optional
  @Input() searchFlag: boolean;
  @Input() combination: boolean;
  // if are pagination pass value from parent component in scrollloading
  @Input() scrollLoadingRightSide: boolean;
  @Input() uniqueFiledName: string;
  @Input() selectedList: any[];

  @Output() searchData = new EventEmitter<string>();
  @Output() selectData = new EventEmitter<any[]>();
  @Output() onScrollListData = new EventEmitter();
  public scrollDistance = 2;
  public selectedListFlag = true;


  public listData: any[] = [];
  public selectedListData: any[] = [];
  public selectedListDataToMove: any[] = [];
  public selectedListTypeToMove: string;
  public moveLeftOrRight: string;
  public copyDataListFlags: boolean;
  newItemEvent: any;
  openEvent: any;

  constructor() { }

  ngOnInit() {
    this.copyDataListFlags = false;
    this.moveLeftOrRight = '';
    this.selectedListTypeToMove = '';
  }

  ngOnChanges() {
    // above logic is necessary to Deep copy data array
    if (!this.copyDataListFlags && this.data === this.listData || this.data !== null ) {
      this.listData = [...this.data];

      // Deep copy of selected data
      if (this.selectedList.length !== 0) {
        this.selectedListData = [...this.selectedList];
      }
      this.moveLeftOrRight = '';

      // eliminate selected data
      this.selectedListData.forEach((selectedListValue) => {
        if (selectedListValue[this.typeFieldName] === this.typeValue) {
          this.listData.forEach(
            (listDataValue, listDataIndex) => {
              if (listDataValue[this.uniqueFiledName] === selectedListValue[this.uniqueFiledName]) {
                this.listData.splice(listDataIndex, 1);
              }
            });
        }
      });
      this.copyDataListFlags = true;
    }

    if (!this.combination) {
      if (this.typeValue !== this.selectedListTypeToMove) {
        this.selectedListDataToMove = [];
      }
    }

  }

  emitData(data: string) {
    this.searchData.emit(data);
  }

  onScrollDown() {
    this.onScrollListData.emit();
  }

  selectItem(listData, side, secretType): void {
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


  onClickData (data, side, type) {
    const avoidDuplication = side === 'left' ? 'right' : 'left';
    if (this.moveLeftOrRight !== avoidDuplication) {
      this.selectItem(data, side, type);
    }
    if (this.moveLeftOrRight === avoidDuplication) {
      this.selectedListDataToMove = [];
      this.selectItem(data, side, type);
    }
  }

  highlightElement(listData: any): boolean {
    return this.selectedListDataToMove.includes(listData);
  }

  moveRight() {
    if (this.selectedListData.length !== 0) {
      if (this.selectedListData[0][this.typeFieldName] !== this.typeValue) {
        this.selectedListData = [];
      }
    }
    this.selectedListDataToMove.forEach(selectedListData => {
      this.selectedListData.unshift(selectedListData);
      this.listData.splice(this.listData.indexOf(selectedListData), 1);
    });
    this.selectedListDataToMove = [];
    this.moveLeftOrRight = '';
    this.selectData.emit(this.selectedListData);
  }

  moveLeft() {
    this.selectedListDataToMove.forEach(selectedListData => {
      this.listData.push(selectedListData);
      this.selectedListData.splice(this.selectedListData.indexOf(selectedListData), 1);
    });
    this.selectedListDataToMove = [];
    this.moveLeftOrRight = '';
    this.selectData.emit(this.selectedListData);
  }

}
