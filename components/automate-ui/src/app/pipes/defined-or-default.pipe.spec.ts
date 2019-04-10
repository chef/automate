import { DefinedOrDefaultPipe } from './defined-or-default.pipe';

describe('DefinedOrDefaultPipe', () => {
  let defaultValue, pipe, value;

  beforeEach(() => {
    pipe = new DefinedOrDefaultPipe();
    value = undefined;
  });

  it('create an instance', () => {
    expect(pipe).toBeTruthy();
  });

  describe('when no default is passed', () => {
    describe('when the value is undefined', () => {
      beforeEach(() => {
        value = undefined;
      });

      it('returns --', () => {
        expect(pipe.transform(value)).toEqual('--');
      });
    });

    describe('when the value is null', () => {
      beforeEach(() => {
        value = null;
      });

      it('returns --', () => {
        expect(pipe.transform(value)).toEqual('--');
      });
    });

    describe('when the value is false', () => {
      beforeEach(() => {
        value = false;
      });

      it('returns --', () => {
        expect(pipe.transform(value)).toEqual('--');
      });
    });

    describe('when the value is truthy', () => {
      beforeEach(() => {
        value = 15;
      });

      it('returns the value', () => {
        expect(pipe.transform(value)).toEqual(value);
      });
    });
  });

  describe('when a custom default is passed', () => {
    beforeEach(() => {
      defaultValue = 42;
    });

    describe('when the value is undefined', () => {
      beforeEach(() => {
        value = undefined;
      });

      it('returns --', () => {
        expect(pipe.transform(value, defaultValue)).toEqual(defaultValue);
      });
    });

    describe('when the value is null', () => {
      beforeEach(() => {
        value = null;
      });

      it('returns --', () => {
        expect(pipe.transform(value, defaultValue)).toEqual(defaultValue);
      });
    });

    describe('when the value is false', () => {
      beforeEach(() => {
        value = false;
      });

      it('returns --', () => {
        expect(pipe.transform(value, defaultValue)).toEqual(defaultValue);
      });
    });

    describe('when the value is truthy', () => {
      beforeEach(() => {
        value = 'My string';
      });

      it('returns the value', () => {
        expect(pipe.transform(value)).toEqual(value);
      });
    });
  });
});
