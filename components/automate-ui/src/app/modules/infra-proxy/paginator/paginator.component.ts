import {
  Component,
  Input,
  Output,
  EventEmitter,
  OnInit,
  OnChanges,
  SimpleChanges,
  ViewChild,
  ChangeDetectionStrategy } from '@angular/core';
import { PageEvent } from '@angular/material/paginator';
import { MatPaginator } from '@angular/material/paginator';

@Component({
  selector: 'app-paginator',
  templateUrl: './paginator.component.html',
  styleUrls: ['./paginator.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})

export class PaginatorComponent implements OnInit, OnChanges {
  @Input() length: Number;
  @Input() pageSize: Number;
  @Input() pageIndex: number;
  @Output() changePage = new EventEmitter<any>(true);
  @ViewChild(MatPaginator) paginator: MatPaginator;

  pageSizeOptions: number[] = [100, 500, 1000];
  pageEvent: PageEvent;
  public isPageAvailable = false;

  ngOnInit() {
    if (this.length > this.pageSizeOptions[0]) {
      this.isPageAvailable = true;
    }
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.pageIndex) {
      if (changes.pageIndex.currentValue !== changes.pageIndex.previousValue
        && !changes.pageIndex.firstChange) {
        this.paginator.pageIndex = changes.pageIndex.currentValue - 1;
      }
    }
    if (changes.length) {
        if (this.paginator) {
          this.paginator.length = changes.length.currentValue;
        }
      this.isPageAvailable = ( this.length > this.pageSizeOptions[0] ? true : false );
    }
  }

  onPageChange(event: PageEvent) {
    this.changePage.emit(event);
  }

  setPageSizeOptions(setPageSizeOptionsInput: string) {
    if (setPageSizeOptionsInput) {
      this.pageSizeOptions = setPageSizeOptionsInput.split(',').map(str => +str);
    }
  }
}
