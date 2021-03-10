import { Pipe, PipeTransform } from '@angular/core';
import { ListItem } from './list-item.domain';

@Pipe({
    name: 'listFilter'
  })
  export class ListFilterPipe implements PipeTransform {
    transform(list: ListItem[], filterText: string): any {
      if (filterText === '') { return list; }
      if (!list) { return list; }
      return list ? list.filter(item => item.value.search(new RegExp(filterText, 'i')) > -1) : [];
    }
  }
