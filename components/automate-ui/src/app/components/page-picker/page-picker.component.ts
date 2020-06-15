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
  @Output() pageSizeChanged = new EventEmitter<any>();

  itemStartCount: number;
  itemEndCount: number;
  totalPages: number[];

  maxPageOptions = [10, 20, 50];
  selectablePages = [];
  first = 1;
  prev = 1;
  next = 1;
  last = 1;

  public getItemStartCount() {
    this.itemStartCount = this.page === 1 ? 1 : ((this.page - 1) * this.perPage + 1);
  }

  public getItemEndCount() {
    this.itemEndCount = this.page === this.last ? this.total : this.page * this.perPage;
  }

  public getTotalPages() {
    const pageCount = Math.ceil(this.total / this.perPage);
    this.totalPages = Array(pageCount).fill(0).map((_x, i) => i + 1);
  }

  private setCounts() {
    this.getItemStartCount();
    this.getItemEndCount();
    this.getTotalPages();
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

    if ( this.forDesktop ) {
      this.setCounts();
    }
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
      const updatedPageNumber = this.getUpdatedPageNumber(value);

      this.pageSizeChanged.emit({
        pageSize: value,
        updatedPageNumber
      });
    }
  }

  private getUpdatedPageNumber(value): number {
    let newPageNumber;
    const newTotalPages = Math.ceil(this.total / value);

    if (this.itemStartCount === 1) {
      newPageNumber = 1;

    // handle last page
    } else if (this.itemStartCount < this.total && (this.itemStartCount > (this.total - value))) {
      newPageNumber = newTotalPages;

    // handle middle pages
    } else {
      newPageNumber = Math.ceil(this.itemStartCount / value);
    }

    return newPageNumber;
  }

}
