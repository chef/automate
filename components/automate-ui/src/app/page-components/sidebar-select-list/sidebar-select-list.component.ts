import { of as observableOf,  Observable } from 'rxjs';
import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';

@Component({
  selector: 'app-sidebar-select-list',
  templateUrl: './sidebar-select-list.component.html',
  styleUrls: ['./sidebar-select-list.component.scss']
})
export class SidebarSelectListComponent implements OnInit {

  @Input() allItemsObs: Observable<Array<string>> = observableOf([]);
  @Input() selectedItemsObs: Observable<Array<string>> = observableOf([]);
  @Input() label = '';
  @Output() selected: EventEmitter<Array<string>> = new EventEmitter<Array<string>>();
  allItems: Array<string> = [];
  selectedItems: Array<string> = [];
  total: number;

  constructor() { }

  ngOnInit() {
    this.selectedItemsObs.subscribe(selectedItems => {
      this.selectedItems = selectedItems;
    });

    this.allItemsObs.subscribe(allItems => {
      this.allItems = allItems;
      this.total = allItems.length;
    });
  }

  updateSelection(event, item: string) {
    event.stopPropagation();
    if (!this.allItems.includes(item)) {
      return;
    }

    let updatedSelectedItems = [];
    if (this.selectedItems.includes(item)) {
      updatedSelectedItems = this.deselect(item);
    } else {
      updatedSelectedItems = this.select(item);
    }
    this.selected.emit(updatedSelectedItems);
  }

  isSelected(item: string): boolean {
    return this.selectedItems.includes(item);
  }

  clearSelections(event) {
    event.stopPropagation();
    this.selectedItems = [];
    this.selected.emit(this.selectedItems);
  }

  private deselect(item) {
    const index = this.selectedItems.indexOf(item);
    return [ ...this.selectedItems.slice(0, index), ...this.selectedItems.slice(index + 1) ];
  }

  private select(item): Array<string> {
    return [].concat(this.selectedItems, item);
  }
}
