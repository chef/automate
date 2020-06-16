import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'paginationTrueEnd'
})
export class PaginationTrueEndPipe implements PipeTransform {

  transform(pageEnd: number, totalCount: number): number {
    if (pageEnd > totalCount) {
      pageEnd = totalCount;
    }
    return pageEnd;
  }
}
