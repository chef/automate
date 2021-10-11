import { Component, Input, Output, EventEmitter, ChangeDetectionStrategy } from '@angular/core';  
import { PageEvent } from '@angular/material/paginator';

@Component({
  selector: 'app-paginator',
  templateUrl: './paginator.component.html',
  styleUrls: ['./paginator.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})

export class PaginatorComponent {
  @Input() length: Number;
  @Input() pageSize: Number;
  @Input() pageIndex: Number;
  @Output() changePage = new EventEmitter<any>(true);
  
  pageSizeOptions: number[] = [1, 5, 10];
  pageEvent: PageEvent;

  onPageChange(event: PageEvent) {
    console.log("pageIndex -->", this.pageIndex);
    this.changePage.emit(event);
  }

  setPageSizeOptions(setPageSizeOptionsInput: string) {
    if (setPageSizeOptionsInput) {
      this.pageSizeOptions = setPageSizeOptionsInput.split(',').map(str => +str);
    }
  }
}
