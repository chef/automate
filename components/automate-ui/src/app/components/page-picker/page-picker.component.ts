import { Component, EventEmitter, Input, OnChanges, Output, HostBinding } from '@angular/core';


@Component({
  selector: 'app-page-picker',
  templateUrl: './page-picker.component.html',
  styleUrls: ['./page-picker.component.scss']
})
export class PagePickerComponent implements OnChanges {

  @Input() total: number;
  @Input() perPage: number;
  @Input() page: number;
  @Input() maxPageItems = 10;
  @Input() @HostBinding('class.forDesktop') forDesktop = false;
  @Input() @HostBinding('class.fullScreened') fullScreened = false;

  @Output() pageChanged = new EventEmitter<number>();
  @Output() pageSizeChanged = new EventEmitter<number>();

  maxPageOptions = [10, 20, 50];
  selectablePages = [];
  first = 1;
  prev = 1;
  next = 1;
  last = 1;

  get itemStartCount() {
    return this.page === 1 ? '1' : ((this.page - 1) * this.perPage + 1).toString();
  }

  get itemEndCount() {
    const lastItemCount = this.page === this.last ? this.total : this.page * this.perPage;
    return lastItemCount.toString();
  }

  get totalPages() {
    const pageCount = Math.ceil(this.total / this.perPage);
    return Array(pageCount).fill(0).map((_x, i) => i + 1);
  }

  ngOnChanges() {
    this.last = Math.ceil(this.total / this.perPage) || 1;
    this.prev = (this.page === this.first) ? null : this.page - 1;
    this.next = (this.page === this.last) ? null : this.page + 1;

    const pages = [];
    const selectedIndex = ((this.page - 1) % this.maxPageItems);

    for (let pageIndex = selectedIndex; pageIndex >= 0; --pageIndex) {
      const prev = this.page + (pageIndex - selectedIndex);
      pages[pageIndex] = prev;
    }

    pages[selectedIndex] = this.page;

    for (let pageIndex = selectedIndex; pageIndex < this.maxPageItems; ++pageIndex) {
      const next = this.page + (pageIndex - selectedIndex);
      if (next <= this.last) {
        pages[pageIndex] = next;
      }
    }
    this.selectablePages = pages;
  }

  onItemTap(value) {
    this.pageChanged.emit(value);
  }

  isNull(value) {
    return value === null;
  }

  trackBy(_index, item) {
    return item;
  }

  handleSelectItem(event, value): void {
    if (event.isUserInput) {
      this.pageChanged.emit(value);
    }
  }

  handleSelectMaxPageItems(event, value): void {
    if (event.isUserInput) {
      console.log(value);
      console.log('fire event to update store value of pageSize');
      this.pageSizeChanged.emit(value);
    }
  }

}
