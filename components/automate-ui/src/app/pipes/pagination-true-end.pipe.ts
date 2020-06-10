import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'paginationTrueEnd'
})
export class PaginationTrueEndPipe implements PipeTransform {

  transform(value: string, totalCount: number): string {
    let pageEnd = Number(value);

    if (pageEnd > totalCount) {
      pageEnd = totalCount;
    }
    return pageEnd.toString();
  }
}
