import { OnInit,
  OnChanges,
  Component,
  Input,
  Output,
  EventEmitter,
  SimpleChanges,
  ChangeDetectionStrategy
} from '@angular/core';
import paginate from '../pagination.util';

@Component({
  selector: 'app-pagination',
  templateUrl: './pagination.component.html',
  styleUrls: ['./pagination.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})

export class PaginationComponent implements OnInit, OnChanges {
  @Input() items: Array<any>;
  @Input() initialPage = 1;
  @Input() pageSize: number;
  @Input() maxPages = 9;
  @Input() pageIndex: number;
  @Output() changePage = new EventEmitter<any>(true);

  public pageOfItems: Array<any>;
  pager: any = {};

  ngOnInit() {
    // set page if items array isn't empty
    if (this.items && this.items.length) {
      this.setPage(this.initialPage);
    }
  }

  ngOnChanges(changes: SimpleChanges) {
    // reset page if items array has changed
    if (changes.items) {
      if (changes.items.currentValue !== changes.items.previousValue) {
        this.setPage(this.initialPage);
      }
    } else {
      // reset page if page Index has changed
      if (changes.pageIndex && changes.pageIndex.currentValue !== changes.pageIndex.previousValue) {
        this.setPage(changes.pageIndex.currentValue);
      }

      // reset page if page size has changed
      if (changes.pageSize && changes.pageSize.currentValue !== changes.pageSize.previousValue) {
        this.pageSize = changes.pageSize.currentValue;
        this.setPage(this.initialPage);
      }
    }
  }

  setPage(page: number) {
    if (this.items) {
      // get new pager object for specified page
      this.pager = paginate(this.items.length, page, this.pageSize, this.maxPages);

      // get new page of items from items array
      const pageEvent = {
        page,
        pageSize : this.pageSize,
        pageOfItems : this.items.slice(this.pager.startIndex, this.pager.endIndex + 1)
      };

      // call change page function in parent component
      this.changePage.emit(pageEvent);
    }
  }
}
