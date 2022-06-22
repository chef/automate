import { Utilities } from './utilities';
import { using } from 'app/testing/spec-helpers';

describe('Utilities', () => {

  describe('isNavigationKey', () => {

    using([
      ['Shift'],
      ['Tab'],
      ['Enter']
    ],
      function(key: string) {
        it('returns true if key pressed is ' + key, () => {
          const event = new KeyboardEvent('keypress', {
            'key': key
          });
          expect(Utilities.isNavigationKey(event)).toBe(true);
        });
      });

    using([
      ['A'],
      ['D'],
      ['Z'],
      ['c'],
      ['l'],
      ['y'],
      ['3'],
      ['!'],
      ['8'],
      ['_'],
      ['?']
    ],
      function (key: string) {
        it('returns true if key pressed is ' + key, () => {
          const event = new KeyboardEvent('keypress', {
            'key': key
          });
          expect(Utilities.isNavigationKey(event)).toBe(false);
        });
      });

  });
});
