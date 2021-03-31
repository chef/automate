import { Pipe, PipeTransform } from '@angular/core';
import * as moment from 'moment/moment';

/**
 * The datetime pipe formats datetime strings or moment objects. It's
 * based around moment, so any of the format strings used with moment
 * should work. https://momentjs.com/docs/#/displaying/format/
 */
@Pipe({
  name: 'datetime'
})
export class DatetimePipe implements PipeTransform {

  // https://momentjs.com/timezone/docs/
  // Should we use moment-timezone?
  // Is this a good time to move away from moment (deprecated)

  // Psudocode
  // Add argument for timezone
  // Check if is a moment object, if not, make a moment object
  // convert object to selected timzone
  // format the time

  // public transform(value: moment.Moment | Date | string,
                      // timezone: any,
                      // formatStr: string): string {
  //   const datetime = moment.isMoment(value) ? value : moment(value);
  //   return datetime.tz(timezone).format(formatStr);
  // }

  public transform(value: moment.Moment | Date | string, formatStr: string): string {
    const datetime = moment.isMoment(value) ? value : moment.utc(value);
    return datetime.format(formatStr);
  }

}
