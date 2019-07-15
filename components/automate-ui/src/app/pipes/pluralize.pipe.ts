import { Pipe, PipeTransform } from '@angular/core';

/**
 * Use the pluralize pipe in an Angular template to add an appropriate plural ending
 * when the quantity requires it, i.e. when *not* equal to 1.
 * Thus, "1 member", "2 members" or "1 tree", "0 trees".
 *
 * The primary argument (before the pipe) is the quantity.
 * The second argument is the word to be made plural (or not).
 * The third argument is the plural to apply, e.g. "s" or "es".
 * An actual code example:
 *
 *   {{ membersToAddValues().length | pluralize : 'member' : 's' }} selected
 *
 * It gets more interesting for a word like "policy", which would become plural
 * by dropping the "y" and adding "ies". This is done by indicating how many
 * characters to drop off the end of the word, in this case 1,
 * so a single "<" is added at the front of the plural indicator:
 *
 *   There are {{ policies.length | pluralize : 'policy' : '<ies' }} selected
 *
 * If there are more characters to drop, just add more shifters:
 *
 *   There are {{ fowlCount | pluralize : 'goose' : '<<<<eese' }} on the farm.
 */

@Pipe({
  name: 'pluralize'
})
export class PluralizePipe implements PipeTransform {

  transform(value, word, suffix: string): string {
    const shiftChar = '<';
    if (!suffix.includes(shiftChar)) {
      return value + ' ' + word + ((value && +value === 1) ? '' : suffix);
    }
    const shiftChars = suffix.split(shiftChar);
    const shifters = shiftChars.length - 1;
    const newSuffix = shiftChars[shifters];
    return value + ' ' +
      (value && +value === 1
        ? word
        : (word.substr(0, word.length - shifters) + newSuffix));
  }
}
