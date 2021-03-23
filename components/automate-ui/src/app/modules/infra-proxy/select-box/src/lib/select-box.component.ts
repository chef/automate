import { Component, OnInit, Input, forwardRef, EventEmitter, Output, OnChanges, SimpleChanges } from '@angular/core';
import { ControlValueAccessor, NG_VALUE_ACCESSOR } from '@angular/forms';
import { ListFilterPipe } from './list-filter.pipe';
import { ListItem } from './list-item.domain';

export const SELECT_BOX_ACCESSOR: any = {
  provide: NG_VALUE_ACCESSOR,
  useExisting: forwardRef(() => SelectBoxComponent),
  multi: true
};

@Component({
  selector: 'app-select-box',
  templateUrl: './select-box.component.html',
  styleUrls: ['./select-box.component.scss'],
  providers: [SELECT_BOX_ACCESSOR, ListFilterPipe]
})
export class SelectBoxComponent implements OnInit, ControlValueAccessor, OnChanges {

  /* paramter used to pass in the list items*/
  @Input() list;
  /* option to  turn on sort feature on the lists*/
  @Input() sort;
  /* option to turn on search feature on the lists */
  @Input() search;
  /* option to turn on select/unselect all feature on the lists*/
  @Input() selectAll;
  @Input() disabled = false;
  @Input() selectedItems: ListItem[] = [];

  /* selected items that will be passed back to form control */
  @Input() selectedList: string[] = [];
  @Output() selectedValues: EventEmitter<ListItem[]> =   new EventEmitter();

  /* filter text used to filter items on the left side */
  leftFilterText = '';
  /* filter text used to filter items on the right side */
  rightFilterText = '';

  /* working list of items on the left side */
  originalItems: ListItem[] = [];

  constructor() { }

  ngOnInit() {
    this.originalItems = [];
    this.selectedItems = [];
    this.list.forEach(element => {
      this.originalItems.push(new ListItem(element.name, element.type));
    });

    if (this.selectedList != null && this.selectedList !== []) {
      this.setSelectedValues(this.selectedList);
      this.onChange(this.value);
    }

  }

  ngOnChanges(changes: SimpleChanges): void {
    this.originalItems = [];
    if (changes?.list?.currentValue) {
      this.list?.forEach(element => {
          this.originalItems.push(new ListItem(element.name, element.type));
        });
    } else if (changes?.list?.currentValue.length > 0 ) {
        this.originalItems = changes.list.currentValue;
    }
  }

  /* This method moves items from original list to selected on button click*/
  addItems() {
    this.moveItems(this.originalItems, this.selectedItems, 0);
  }

  /*This method handles the drag event onto original list on the left */
  dragOntoLeftItems(event) {
    if (event.previousContainer === event.container) {
      if (this.sort && this.getLeftSelectedList().length === 1 && this.originalItems.length > 1) {
        this.changeItemPosition(this.originalItems, event.previousIndex, event.currentIndex);
      }
    } else {
      this.moveItems(this.selectedItems, this.originalItems, event.currentIndex);
    }
  }

  /*This method handles the drag event onto selected list on the right */
  dragOntoRightItems(event) {
    if (event.previousContainer === event.container) {
      if (this.sort && this.getRightSelectedList().length === 1 && this.selectedItems.length > 1) {
        this.changeItemPosition(this.selectedItems, event.previousIndex, event.currentIndex);
      }
    } else {
      this.moveItems(this.originalItems, this.selectedItems, event.currentIndex);
    }
  }

  /* This method returns the selected items on the original list on left side*/
  getLeftSelectedList(): ListItem[] {
    const leftSelectedList: ListItem[] = [];
    this.originalItems.forEach(
      element => {
        if (element.selected) { leftSelectedList.push(element); }
      }
    );
    return leftSelectedList;
  }

  /* This method returns the selected items on the selected list on right side*/
  getRightSelectedList(): ListItem[] {
    const rightSelectedList: ListItem[] = [];
    this.selectedItems.forEach(
      element => {
        if (element.selected) { rightSelectedList.push(element); }
      }
    );
    return rightSelectedList;
  }

  /* This method moves items from selected list to original on button click*/
  removeItems() {
    this.moveItems(this.selectedItems, this.originalItems, 0);
  }

  sortItemsInAsccOrder() {
    this.selectedItems.sort((a, b) => (a.value > b.value) ? 1 : -1);
    this.selectedItems.forEach((item) => {
      if (item.selected) {
        item.selected = false;
      }
    });
    this.onChange(this.value);
    this.selectedValues.emit(this.selectedItems);
  }

  sortItemsInDescOrder() {
    this.selectedItems.sort((a, b) => (a.value < b.value) ? 1 : -1);
    this.selectedItems.forEach((item) => {
      if (item.selected) {
        item.selected = false;
      }
    });
    this.onChange(this.value);
    this.selectedValues.emit(this.selectedItems);
  }

  get value(): any {
    const temp: string[] = [];
    this.selectedItems.forEach(
      element => {
        temp.push(element.value);
      }
    );
    return temp;
  }

  set value(val: any) {
    this.setSelectedValues(val);
  }

  /* Methods to implement ControlValueAccessor */
  onChange = (val: string[]) => { console.log( val); };

  onTouched = () => { };

  writeValue(value: string[]): void {
    this.setSelectedValues(value);
    this.onChange(this.value);
  }

  registerOnChange(fn: (val: string[]) => void): void {
    this.onChange = fn;
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn;
  }

  /* helper method that changes the position of items in the list*/
  private changeItemPosition(list: ListItem[], currPos: number, newPos: number) {
    const item: ListItem = list.splice(currPos, 1)[0];
    item.selected = false;
    list.splice(newPos, 0, item);
    this.onChange(this.value);
  }

  /*helper method that moves items between lists */
  private moveItems(fromList: ListItem[], toList: ListItem[], insertIndex: number) {
    for (let removeIndex = fromList.length - 1; removeIndex >= 0; removeIndex--) {
      const item: ListItem = fromList[removeIndex];
      if (item.selected) {
        fromList.splice(removeIndex, 1);
        item.selected = false;
        toList.splice(insertIndex, 0, item);
      }
    }
    this.onChange(this.value);
    this.selectedValues.emit(this.selectedItems);
  }

  private setSelectedValues(values: string[]) {
    if (values !== undefined && values != null && values !== []) {
      this.selectedList = values;
      if (this.selectedList.length > 0) {
        // Add to items selected items working list
        this.selectedList.forEach(
          element => {
            const item: ListItem = new ListItem(element, 'role');
            this.selectedItems.push(item);
          }
        );

        // remove from original items working list
        for (let delIndex = this.originalItems.length - 1; delIndex >= 0; delIndex--) {
          const item: ListItem = this.originalItems[delIndex];
          if (this.selectedList.indexOf(item.value) > -1) {
            this.originalItems.splice(delIndex, 1);
          }
        }
      }

    }
  }
}
