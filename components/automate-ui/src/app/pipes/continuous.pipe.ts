import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'continuous'
})
export class ContinuousPipe implements PipeTransform {

  transform(value: string): string {
    const lastIndex = (value.length - 1);
    if (value) {
      if (value.charAt(lastIndex) === 'e') {
        return value.slice(0, lastIndex) + 'ing';
      } else {
        return value + 'ing';
      }
    }
    return value;
  }
}
