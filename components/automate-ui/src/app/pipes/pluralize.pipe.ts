import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'pluralize'
})
export class PluralizePipe implements PipeTransform {

  transform(value, word, suffix: string): string {
    return value + ' ' + word + ((value && +value === 1) ? '' : suffix);
  }
}
