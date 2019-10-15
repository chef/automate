export class DateTime {
  // formatted for use with moment.js -- https://momentjs.com/docs/#/displaying/format/
  // RFC2822 format like: Wed, 03 Jul 2019 17:08:53 UTC
  public static readonly RFC2822: string = 'ddd, DD MMM YYYY HH:mm:ss [UTC]';

  // Tue, 24 Sept 2019
  public static readonly CHEF_DATE_TIME: string = 'ddd, DD MMM YYYY';

  public static readonly CHEF_HOURS_MINS: string = 'HH:mm';
}
