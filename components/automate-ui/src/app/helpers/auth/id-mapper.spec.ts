import { using } from 'app/testing/spec-helpers';
import { IdMapper } from './id-mapper';

describe('IdMapper', () => {

    using([
      ['my-name', 'my-name', 'mirrors nominal input'],
      ['-0a5l9z-', '-0a5l9z-', 'supports numbers, letters, and hyphens'],
      ['my-name!', 'my-name-', 'replaces trailing out-of-range char with hyphen'],
      ['#$my-name', '--my-name', 'replaces leading out-of-range chars with hyphens'],
      ['my name', 'my-name', 'replaces space char with hyphen'],
      ['My-Name', 'my-name', 'maps upper case to lower case']
    ], function (inputVal: string, outputVal: string, desc: string) {

      it('transform ' + desc, () => {
        expect(IdMapper.transform(inputVal)).toBe(outputVal);
      });
    });

});
