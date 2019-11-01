export class DateTime {
  // Common date formats for use with moment.js -- https://momentjs.com/docs/#/displaying/format/

  // Format for RFC2822 display
  // Wed, 03 Jul 2019 17:08:53 UTC
  public static readonly RFC2822: string = 'ddd, DD MMM YYYY HH:mm:ss [UTC]';

  // Format for date display
  // Tue, 24 Sept 2019
  public static readonly CHEF_DATE_TIME: string = 'ddd, DD MMM YYYY';

  // Format for time display
  // 09:59
  public static readonly CHEF_HOURS_MINS: string = 'HH:mm';

  // Format for filenames of report downloads
  // 2019-09-24-09:59:59
  public static readonly REPORT_DATE_TIME: string = 'YYYY-MM-DD-HHmmss';
}
