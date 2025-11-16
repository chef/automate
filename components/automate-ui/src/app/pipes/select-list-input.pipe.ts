import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  standalone: false,
  name: 'selectListInput'
})
export class SelectListInputPipe implements PipeTransform {

  transform(value: any, args: any[]): any {
    if (value) {
      return value.filter((item) => item.match(args));
    }
  }

}
