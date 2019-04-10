import ng from 'angular';
import 'angular-mocks';
import ApiUrl from '../../../../../src/common/api/api_url';

describe('ApiUrl', () => {
  
  beforeEach(() => {
    ng.mock.module(ApiUrl, ($provide) => {
      $provide.decorator('$location', ($delegate) => {
        $delegate.absUrl = () => 'http://domain.com/e/Foobar';
        $delegate.host = () => 'domain.com';
        return $delegate;
      });

      $provide.decorator('Session', ($delegate) => {
        $delegate.get = () => 'Foobar';
        return $delegate;
      });

      $provide.value('appConfig', {
        api: {
          base_url: '/workflow/api/',
          version: 'v0',
          endpoints: {
            projects: '/orgs/:org/projects/:project'
          }
        }
      });
    });
  });

  it('should return a proper enpoint url', inject((ApiUrl) => {
    expect(ApiUrl('projects'))
      .toBe('/workflow/api/v0/e/Foobar/orgs/:org/projects/:project');
  }));
});
