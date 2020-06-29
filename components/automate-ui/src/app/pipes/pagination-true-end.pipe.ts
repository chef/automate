import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'paginationTrueEnd'
})
export class PaginationTrueEndPipe implements PipeTransform {

  transform(pageEnd: number, totalCount: number): number {
    return (pageEnd > totalCount || pageEnd === 0) ? totalCount : pageEnd;
  }
}
