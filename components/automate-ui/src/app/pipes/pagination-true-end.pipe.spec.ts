import { PaginationTrueEndPipe } from './pagination-true-end.pipe';

describe('PaginationTrueEndPipe', () => {
  let pipe;

  beforeEach(() => {
    pipe = new PaginationTrueEndPipe();
  });

  it('returns the lesser number when compared against total', () => {
    const endPage = 40;
    const totalPages = 120;
    const result = pipe.transform(endPage, totalPages);

    expect(result).toEqual(40);
  });

  it('returns the total number when the endPage is higher than the total', () => {
    const endPage = 85;
    const totalPages = 80;
    const result = pipe.transform(endPage, totalPages);

    expect(result).toEqual(80);
  });
});
