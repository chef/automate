import { Pipe, PipeTransform } from '@angular/core';

/**
 * Use the pluralize pipe in an Angular template to add an appropriate plural ending
 * when the quantity requires it, i.e. when *not* equal to 1.
 * Thus, "1 member", "2 members" or "1 tree", "0 trees".
 *
 * The primary argument (before the pipe) is the quantity.
 * The second argument is the word to be made plural (or not).
 * The third argument is the plural to apply, e.g. "+s" or "+es".
 * An actual code example:
 *
 *   {{ membersToAddValues().length | pluralize : 'member' : '+s' }} selected
 *
 * It gets more interesting for a word like "policy", which would become plural
 * by dropping the "y" and adding "ies". Or then there's "goose/geese" ...
 * That is done by simply omitting the "+", indicating you're providing the plural
 * instead of a suffix:
 *
 *   There are {{ policies.length | pluralize : 'policy' : 'policies' }} selected
 *   There are {{ birds.length | pluralize : 'goose' : 'geese' }} selected
 */

@Pipe({
  name: 'pluralize'
})
export class PluralizePipe implements PipeTransform {

  transform(value, word, suffix: string): string {
    const isSingular = value && +value === 1;
    const plainSuffix = suffix.replace(/^\+/, '');
    if (plainSuffix !== suffix) {
      return value + ' ' + word + (isSingular ? '' : plainSuffix);
    }
    return value + ' ' + (isSingular ? word : suffix);
  }
}
