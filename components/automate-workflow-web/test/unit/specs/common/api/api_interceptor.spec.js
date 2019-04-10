import ng from 'angular';
import 'angular-mocks';
import ApiInterceptor from '../../../../../src/common/api/api_interceptor';

describe('apiInterceptor', () => {

  beforeEach(ng.mock.module(ApiInterceptor));

  describe('when the API returns a 500', () => {

    it('should display an error message', inject((apiInterceptor, Flash) => {
      let resp = { status: 500, config: {} };

      spyOn(Flash, 'error');

      apiInterceptor.responseError(resp);

      expect(Flash.error).toHaveBeenCalledWith('Error', 'Bad server response');
    }));
  });
});
