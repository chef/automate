import { Pipe, PipeTransform } from '@angular/core';
import * as moment from 'moment-timezone';

/**
 * The datetime pipe formats datetime strings or moment objects. It's
 * based around moment, so any of the format strings used with moment
 * should work. https://momentjs.com/docs/#/displaying/format/
 */
@Pipe({
  name: 'datetime'
})
export class DatetimePipe implements PipeTransform {

  // REFER TO DOCS FOR USE WITH UTC https://momentjs.com/timezone/docs/#/using-timezones/

  public transform(value: moment.Moment | Date | string,
    timezone: string = 'default', formatStr: string): string {
      ///// next line for development > needs further inspection
      if (timezone === null) { return; }
      console.log(timezone);

      // this if block will likely change
      if (timezone === 'default') {
        return moment.utc(value).format(formatStr);
      }

      return moment.utc(value).clone().tz(timezone).format(formatStr);
  }

}
