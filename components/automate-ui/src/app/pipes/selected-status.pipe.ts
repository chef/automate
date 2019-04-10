import { Pipe, PipeTransform } from '@angular/core';

@Pipe({name: 'selectedStatus'})

// ToDo eng-ex figure out if this is the same or can be combined with status-selector.pipe
export class SelectedStatusPipe implements PipeTransform {
  transform(data: any, args: string): any {

    if (data) {
      switch (args) {
        case 'failed':
          return data.filter((datum) => datum.status === 'failed');
        case 'success':
          return data.filter((datum) => datum.status === 'updated');
        case 'unchanged':
          const upToDateItems = data.filter((datum) => datum.status === 'up-to-date');
          const skippedItems = data.filter((datum) => datum.status === 'skipped');
          return upToDateItems.concat(skippedItems);
        case 'unprocessed':
          return data.filter((datum) => datum.status === 'unprocessed');
        case 'total':
          return data;
      }
    }
  }
}
