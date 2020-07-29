import { PaginationTrueEndPipe } from './pagination-true-end.pipe';

describe('PaginationTrueEndPipe', () => {
  let pipe;

  beforeEach(() => {
    pipe = new PaginationTrueEndPipe();
  });

  it('returns the lesser number when compared against total', () => {
    const pageEnd = 40;
    const totalItems = 120;
    const result = pipe.transform(pageEnd, totalItems);

    expect(result).toEqual(pageEnd);
  });

  it('returns the totalItems number when the pageEnd is higher than the total', () => {
    const pageEnd = 85;
    const totalItems = 80;
    const result = pipe.transform(pageEnd, totalItems);

    expect(result).toEqual(totalItems);
  });

  it('returns the pageEnd number when the pageEnd is equal to totalItems', () => {
    const pageEnd = 134;
    const totalItems = 134;
    const result = pipe.transform(pageEnd, totalItems);

    expect(result).toEqual(134);
  });

  it('returns totalItems when the pageEnd is set to 0 and totalItems sent to non-zero', () => {
    const pageEnd = 0;
    const totalItems = 34;
    const result = pipe.transform(pageEnd, totalItems);

    expect(result).toEqual(totalItems);
  });
});
