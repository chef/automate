import { PluralizePipe } from './pluralize.pipe';
import { using } from 'app/testing/spec-helpers';

describe('PluralizePipe', () => {

  let pipe: PluralizePipe;

  beforeEach(() => {
    pipe = new PluralizePipe();
  });

  it('create an instance', () => {
    expect(pipe).toBeTruthy();
  });

  using([
      ['0', 'team', 's'],
      ['2', 'fox', 'es'],
      ['-5', 'color', 's']
    ], (value, unit, suffix, _result: string) => {
      it('pluralizes any NON-unitary value ', () => {
        expect(pipe.transform(value, unit, suffix)).toBe(value + ' ' + unit + suffix);
      });
    });

  it('providing UNITARY value returns singular form', () => {
    expect(pipe.transform('1', 'team', 's' )).toBe('1 team');
  });

});
