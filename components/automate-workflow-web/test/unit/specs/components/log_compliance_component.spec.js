import ng from 'angular';
import 'angular-mocks';
import logComplianceComponent from '../../../../src/components/log/log_compliance';

describe('logComplianceComponent', () => {
  let scope, isolateScope, element, display;

  beforeEach(ng.mock.module(logComplianceComponent));

  function createDirective(attrs) {
    function getElement() {
      let el = ng.element('<div cd-compliance-report log=log json=json></div>');
      return el;
    }

    return function ($compile, $rootScope) {
      scope = $rootScope.$new();
      scope.log = 'this is a log';
      scope.json = JSON.stringify({
        version: 'stuff',
        profiles: [
          {
            name: 'hardening-io-ssh',
            title: 'SSH Compliance Profile',
            summary: `Verify that SSH is configured to current industry guidelines for
                      security and compliance. Based on http:\/\/hardening.io project.`,
            maintainer: 'chef-compliance',
            copyright: 'chef',
            version: '1.1.0',
            supports: [
              {'os-family':'unix'}
            ],
            controls: [
              {
                title: 'cool',
                pending_message: 'title1',
                desc: 'the actual result',
                results: [
                  {
                    status:'passed',
                    code_desc:'File /tmp should be directory',
                    run_time:0.00377,
                    start_time:'2016-09-20 17:43:47 -0400'
                  }
                ]
              },
              {
                title: 'something',
                desc: 'the actual result2',
                results: [
                  {
                    status:'skipped',
                    code_desc:'File /tmp should be directory',
                    run_time:0.00377,
                    start_time:'2016-09-20 17:43:47 -0400'
                  }
                ]
              }
            ]
          }
        ]
      });
      element = $compile(getElement())(scope);

      display = function () {
        return ng.element(element[0].querySelector('.log-display'));
      };

      isolateScope = element.isolateScope();
      scope.$digest();
    };
  }

  beforeEach(inject(createDirective()));

  describe('setup', () => {
    it('sets data, summary, and profile as json input indicates', () => {
      // angular adds a hashkey to the object, which throws off testing, so we're removing it here
      delete isolateScope.data.profiles[0].controls[0]['$$hashKey'];
      delete isolateScope.data.profiles[0].controls[1]['$$hashKey'];
      delete isolateScope.data.profiles[0].controls[0].results[0]['$$hashKey'];
      delete isolateScope.data.profiles[0].controls[1].results[0]['$$hashKey'];
      expect(isolateScope.data).toEqual({
        version: 'stuff',
        profiles: [
          {
            name: 'hardening-io-ssh',
            title: 'SSH Compliance Profile',
            summary: `Verify that SSH is configured to current industry guidelines for
                      security and compliance. Based on http:\/\/hardening.io project.`,
            maintainer: 'chef-compliance',
            copyright: 'chef',
            version: '1.1.0',
            supports: [
              {'os-family':'unix'}
            ],
            controls: [
              {
                title: 'cool',
                pending_message: 'title1',
                desc: 'the actual result',
                results: [
                  {
                    status:'passed',
                    code_desc:'File /tmp should be directory',
                    run_time:0.00377,
                    start_time:'2016-09-20 17:43:47 -0400'
                  }
                ],
                indicator: 'Passed'
              },
              {
                title: 'something',
                desc: 'the actual result2',
                results: [
                  {
                    status:'skipped',
                    code_desc:'File /tmp should be directory',
                    run_time:0.00377,
                    start_time:'2016-09-20 17:43:47 -0400'
                  }
                ],
                indicator: 'Skipped'
              }
            ]
          }
        ]
      });
      expect(isolateScope.summary).toEqual({
        critical: 0,
        major: 0,
        minor: 0,
        passed: 1,
        skipped: 1,
        total: 2
      });
    });
  });

  describe('when test result json comes in', () => {

    beforeEach(() => {
      isolateScope.json = JSON.stringify({
        version: 'stuff',
        profiles: [
          {
            name: 'hardening-io-ssh',
            title: 'SSH Compliance Profile',
            summary: `Verify that SSH is configured to current industry guidelines for
                      security and compliance. Based on http:\/\/hardening.io project.`,
            maintainer: 'chef-compliance',
            copyright: 'chef',
            version: '1.1.0',
            supports: [
              {'os-family':'unix'}
            ],
            controls: [
              {
                title: 'cool',
                pending_message: 'title1',
                desc: 'the actual result',
                results: [
                  {
                    status:'skipped',
                    code_desc:'File /tmp should be directory',
                    run_time:0.00377,
                    start_time:'2016-09-20 17:43:47 -0400'
                  }
                ]
              },
              {
                title: 'title2',
                desc: 'the actual result2',
                impact: 0.5,
                results: [
                  {
                    status:'failed',
                    code_desc:'File /tmp should be directory',
                    run_time:0.00377,
                    start_time:'2016-09-20 17:43:47 -0400'
                  }
                ]
              }
            ]
          }
        ]
      });
      isolateScope.$digest();
    });

    it('updates the data and profile', () => {
      delete isolateScope.data.profiles[0].controls[0]['$$hashKey'];
      delete isolateScope.data.profiles[0].controls[1]['$$hashKey'];
      delete isolateScope.data.profiles[0].controls[0].results[0]['$$hashKey'];
      delete isolateScope.data.profiles[0].controls[1].results[0]['$$hashKey'];
      expect(isolateScope.data).toEqual({
        version: 'stuff',
        profiles: [
          {
            name: 'hardening-io-ssh',
            title: 'SSH Compliance Profile',
            summary: `Verify that SSH is configured to current industry guidelines for
                      security and compliance. Based on http:\/\/hardening.io project.`,
            maintainer: 'chef-compliance',
            copyright: 'chef',
            version: '1.1.0',
            supports: [
              {'os-family':'unix'}
            ],
            controls: [
              {
                title: 'cool',
                pending_message: 'title1',
                desc: 'the actual result',
                results: [
                  {
                    status:'skipped',
                    code_desc:'File /tmp should be directory',
                    run_time:0.00377,
                    start_time:'2016-09-20 17:43:47 -0400'
                  }
                ],
                indicator: 'Skipped'
              },
              {
                title: 'title2',
                desc: 'the actual result2',
                impact: 0.5,
                results: [
                  {
                    status: 'failed',
                    code_desc: 'File /tmp should be directory',
                    run_time: 0.00377,
                    start_time: '2016-09-20 17:43:47 -0400'
                  }
                ],
                indicator: 'Major'
              }
            ]
          }
        ]
      });
    });

    it('updates the summary', () => {
      expect(isolateScope.summary).toEqual({
        critical: 0,
        major: 1,
        minor: 0,
        passed: 0,
        skipped: 1,
        total: 2
      });
    });
  });
});
