import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'selectListInput'
})
export class SelectListInputPipe implements PipeTransform {

  transform(value: any, args: any[]): any {
    if (value) {
      return value.filter((item) => item.match(args));
    }
  }

}
