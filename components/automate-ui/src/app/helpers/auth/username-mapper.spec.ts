import { using } from 'app/testing/spec-helpers';
import { UsernameMapper } from './username-mapper';

describe('UsernameMapper', () => {

    using([
      ['-0a5l9z-', '-0a5l9z-', 'preserves numbers, lowercase letters, hyphens'],
      ['my_name__', 'my_name__', 'preserves underscores'],
      ['my.name+last_name@email.com', 'my.name+last_name@email.com', 'preserves email symbols'],
      ['my-name!', 'my-name-', 'replaces trailing unsupported single char with hyphen'],
      ['my-name$/', 'my-name--', 'replaces trailing unsupported multiple chars with hyphen'],
      ['my^%<>name', 'my----name', 'replaces middle unsupported chars with hyphen'],
      ['#my-name', '-my-name', 'replaces leading unsupported single char with hyphens'],
      [';=&my-name', '---my-name', 'replaces leading unsupported multiple chars with hyphens'],
      [' my_name  ', '-my_name--', 'replaces leading/trailing spaces with hyphens'],
      ['my   name', 'my---name', 'replaces middle spaces with hyphens'],
      ['My-Name', 'my-name', 'maps upper case to lower case']
    ], function (inputVal: string, outputVal: string, desc: string) {

      it('transform ' + desc, () => {
        expect(UsernameMapper.transform(inputVal)).toBe(outputVal);
      });
    });

});
