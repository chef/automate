import version from '../../../src/version';

describe('versionConstant', () => {

  it('should have a major number', () => {
    expect(version.major).toBeDefined();
  });

  it('should have a minor number', () => {
    expect(version.minor).toBeDefined();
  });

  it('should have a patch number', () => {
    expect(version.patch).toBeDefined();
  });

  it('should have an alias name', () => {
    expect(version.alias).toBeDefined();
  });

  it('should have a full version name', () => {
    expect(version.full).toBeDefined();
  });
});
