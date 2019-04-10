import { IamTypePipe } from './iam-type.pipe';

describe('IamTypePipe', () => {
  it('create an instance', () => {
    const pipe = new IamTypePipe();
    expect(pipe).toBeTruthy();
  });

  it('transforms CUSTOM', () => {
    const pipe = new IamTypePipe();
    expect(pipe.transform('CUSTOM')).toEqual('Custom');
  });

  it('transforms CHEF_MANAGED', () => {
    const pipe = new IamTypePipe();
    expect(pipe.transform('CHEF_MANAGED')).toEqual('Chef-managed');
  });
});
