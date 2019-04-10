import { Pipe, PipeTransform } from '@angular/core';
import { isNil } from 'lodash';

/* Returns a default value if the input value
   is null, undefined, or false. If no default
   passed, it will return '--'.

  Examples:
    + false | or return '--'
    + undefined | or: 'Missing' return 'Missing' */
@Pipe({
  name: 'definedOrDefault'
})
export class DefinedOrDefaultPipe implements PipeTransform {

  transform(value: any, defaultValue?: any): any {
    if (isNil(value) || !value) {
      if (!isNil(defaultValue)) {
        return defaultValue;
      }
      return '--';
    }
    return value;
  }

}
