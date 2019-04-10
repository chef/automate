import * as util from './util';

describe('util', () => {

  describe('formatProgressResponse', () => {

    it('replaces the response of progress with status 401', () => {
      const expected = 'Bad authorization';
      const input = {
        file: 'profile.tgz',
        status: 401,
        response: '<h1>Bad Authorization from nginx</h1>',
        percent: 100,
        errors: []
      };

      expect(util.formatProgressResponse(input).response).toEqual(expected);
    });

    it('replaces the response of progress with any other status than 401 and 200', () => {
      const expected = 'The server was not able to process that file';
      const input = {
        file: 'profile.tgz',
        status: 413,
        response: '<head><title>413 Request Entity Too Large</h1>...',
        percent: 100,
        errors: []
      };

      expect(util.formatProgressResponse(input).response).toEqual(expected);
    });

    it('converts JSON response strings and replaces them if status is invalid', () => {
      const expected = 'Profile file invalid';
      const input = {
        file: 'profile.tgz',
        status: 200,
        response: '{"summary": {"valid": false}}',
        percent: 100,
        errors: []
      };

      expect(util.formatProgressResponse(input).response).toEqual(expected);
    });

    it('gives a warning summary if status is valid and there\'s only warnings', () => {
      const expected = '3 warnings';
      const input = {
        file: 'profile.tgz',
        status: 200,
        response: '{"summary": {"valid": true}, "errors": [], "warnings": [{}, {}, {}]}',
        percent: 100,
        errors: []
      };

      expect(util.formatProgressResponse(input).response).toEqual(expected);
    });

    it('gives an error summary if status is valid and there\'s only errors', () => {
      const expected = '2 errors';
      const input = {
        file: 'profile.tgz',
        status: 200,
        response: '{"summary": {"valid": true}, "errors": [{},{}], "warnings": []}',
        percent: 100,
        errors: []
      };

      expect(util.formatProgressResponse(input).response).toEqual(expected);
    });

    it('gives an mixed summary if status is valid and there\'s both warnings and errors', () => {
      const expected = '2 errors, 1 warning';
      const input = {
        file: 'profile.tgz',
        status: 200,
        response: '{"summary": {"valid": true}, "errors": [{},{}], "warnings": [{}]}',
        percent: 100,
        errors: []
      };

      expect(util.formatProgressResponse(input).response).toEqual(expected);
    });

    it('sets that value of progress.errors to report.errors', () => {
      const expected = [
        { msg: 'this is the error msg' }
      ];
      const input = {
        file: 'profile.tgz',
        status: 400,
        response: 'this is the error msg',
        percent: 100,
        errors: expected
      };

      expect(util.formatProgressResponse(input).errors).toEqual(expected);
    });
  });
});
