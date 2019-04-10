import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'chefStatusIcon'
})
export class ChefStatusIconPipe implements PipeTransform {

  transform(value: string): string {
    switch (value) {
      case 'success':
      case 'updated':
      case 'passed':
      case 'compliant':
        return 'check_circle';

      case 'up-to-date':
      case 'skipped':
        return 'remove_circle';

      case 'failure':
      case 'failed':
        return 'warning';

      case 'missing':
      case 'skipped_tests':
      case 'unprocessed':
        return 'help';

      case 'critical':
        return 'highlight_off';

      case 'major':
        return 'not_interested';

      case 'minor':
        return 'error';

      default:
        return 'lens';
    }
  }

}
