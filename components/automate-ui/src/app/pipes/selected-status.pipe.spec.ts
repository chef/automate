import { SelectedStatusPipe } from './selected-status.pipe';

describe('SelectedStatusPipe Tests', () => {
  let pipe: SelectedStatusPipe;
  const data = [
    ({'status': 'failed', 'id': '1'}),
    ({'status': 'updated', 'id': '2'}),
    ({'status': 'up-to-date', 'id': '3'}),
    ({'status': 'skipped', 'id': '4'}),
    ({'status': 'failed', 'id': '5'}),
    ({'status': 'unprocessed', 'id': '6'})
  ];

  beforeEach(() => {
    pipe = new SelectedStatusPipe();
  });

  it('should filter for failures', () => {
    const result = pipe.transform(data, 'failed');
    expect(result).toEqual([
      data[0],
      data[4]
    ]);
  });

  it('should filter for unchanged items', () => {
    const result = pipe.transform(data, 'unchanged');
    expect(result).toEqual([
      data[2],
      data[3]
    ]);
  });

  it('should filter for successes (updated items)', () => {
    const result = pipe.transform(data, 'success');
    expect(result).toEqual([
      data[1]
    ]);
  });

  it('should filter for unprocessed items', () => {
    const result = pipe.transform(data, 'unprocessed');
    expect(result).toEqual([
      data[5]
    ]);
  });

  it('return the array unchanged when called with total', () => {
    const result = pipe.transform(data, 'total');
    expect(result).toEqual([
      data[0],
      data[1],
      data[2],
      data[3],
      data[4],
      data[5]
    ]);
  });
});
