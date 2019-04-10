import ng from 'angular';
import 'angular-mocks';
import Project from '../../../../../src/common/models/project';

describe('Project', () => {

  beforeEach(ng.mock.module(Project, ($provide) => {
    $provide.decorator('$location', ($delegate) => {
      $delegate.host = () => 'domain.com';
      return $delegate;
    });
    $provide.decorator('Session', ($delegate) => {
      $delegate.get = () => ({ username: 'bob', enterprise: 'foo' });
      return $delegate;
    });
  }));

  it('should have a name', inject((Project) => {
    let project = Project.$buildRaw({ name: 'foo' });
    expect(project.name).toBe('foo');
  }));

  it('should provide a clone command', inject((Project) => {
    let project = Project.$buildRaw({
      name: 'baz',
      orgName: 'bar',
      entName: 'foo'
    });
    let expected =
      'delivery clone baz --ent=foo --org=bar --user=bob --server=domain.com';
    expect(project.cloneCmd()).toBe(expected);
  }));
});
