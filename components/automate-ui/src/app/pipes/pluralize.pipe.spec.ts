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
      ['0', 'team', '+s', 'teams'],
      ['2', 'fox', '+es', 'foxes'],
      ['-5', 'color', '+s', 'colors']
    ], (value, base, suffix, result: string) => {
      it('pluralizes NON-unitary values with suffix', () => {
        expect(pipe.transform(value, base, suffix)).toBe(value + ' ' + result);
      });
    });

  using([
      ['0', 'radius', 'radii'],
      ['4', 'goose', 'geese'],
      ['-5', 'policy', 'policies']
    ], (value, singular, plural: string) => {
      it('pluralizes NON-unitary values with full plural', () => {
        expect(pipe.transform(value, singular, plural)).toBe(value + ' ' + plural);
      });
    });

  it('providing UNITARY value with suffix returns singular form', () => {
    expect(pipe.transform('1', 'team', '+s' )).toBe('1 team');
  });

  it('providing UNITARY value with full plural returns singular form', () => {
    expect(pipe.transform('1', 'policy', '<ies' )).toBe('1 policy');
  });

});
