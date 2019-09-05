import { ContinuousPipe } from './continuous.pipe';

describe('ContinuousPipe', () => {
  it('create an instance', () => {
    const pipe = new ContinuousPipe();
    expect(pipe).toBeTruthy();
  });
});