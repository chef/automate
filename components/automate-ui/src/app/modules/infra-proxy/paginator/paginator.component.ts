import {
  Component,
  Input,
  Output,
  EventEmitter,
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

export class PaginatorComponent implements OnChanges {
  @Input() length: Number;
  @Input() pageSize: Number;
  @Input() pageIndex: number;
  @Output() changePage = new EventEmitter<any>(true);
  @ViewChild(MatPaginator) paginator: MatPaginator;

  pageSizeOptions: number[] = [1, 5, 10];
  pageEvent: PageEvent;

  ngOnChanges(changes: SimpleChanges) {
    if ( changes.pageIndex.currentValue !== changes.pageIndex.previousValue
      && !changes.pageIndex.firstChange) {
      this.paginator.pageIndex = changes.pageIndex.currentValue - 1;
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
