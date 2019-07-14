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
    ], (value, unit, suffix: string) => {
      it('pluralizes NON-unitary values with additive-suffix', () => {
        expect(pipe.transform(value, unit, suffix)).toBe(value + ' ' + unit + suffix);
      });
    });

  using([
      ['0', 'radius', '<<i', 'radii'],
      ['4', 'goose', '<<<<eese',  'geese'],
      ['-5', 'policy', '<ies', 'policies']
    ], (value, unit, suffix, result: string) => {
      it('pluralizes NON-unitary values with subtractive-suffix', () => {
        expect(pipe.transform(value, unit, suffix)).toBe(value + ' ' + result);
      });
    });

  it('providing UNITARY value with additive-suffix returns singular form', () => {
    expect(pipe.transform('1', 'team', 's' )).toBe('1 team');
  });

  it('providing UNITARY value with subtractive-suffix returns singular form', () => {
    expect(pipe.transform('1', 'policy', '<ies' )).toBe('1 policy');
  });

});
