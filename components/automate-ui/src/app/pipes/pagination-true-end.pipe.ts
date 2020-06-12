import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'paginationTrueEnd'
})
export class PaginationTrueEndPipe implements PipeTransform {

  transform(value: number, totalCount: number): number {
    let pageEnd = Number(value);

    if (pageEnd > totalCount) {
      pageEnd = totalCount;
    }
    return pageEnd;
  }
}
