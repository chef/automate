// import ng from 'angular';
// import 'angular-mocks';
// import app from '../../../../../src/app';

// describe('authInterceptor', () => {

//   let httpBackend;

//   beforeEach(() => {
//     ng.mock.module(app, ($provide) => {
//       // $provide.decorator('$location', ($delegate) => {
//       //   $delegate.absUrl = () => 'http://domain.com/workflow/e/Foobar';
//       //   $delegate.host = () => 'domain.com';
//       //   return $delegate;
//       // });

//       $provide.value('Auth', {
//         unAuthenticate: () => null
//       });
//     });
//   });

//   // beforeEach(inject(($httpBackend) => {
//   //   httpBackend = $httpBackend;
//   // }));

//   describe('when an HTTP response is 401', () => {

//     // beforeEach(inject(($httpBackend) => {
//     //   $httpBackend
//     //     .when('GET', 'endpoint')
//     //     .respond(401, '');

//     //   // Auth.authenticate('admin', 'password');

//     //   // httpBackend
//     //   //   .when('GET', '/workflow/api/_status')
//     //   //   .respond(200, '{"a2_mode": "false"}');
//     // }));

//     it('should redirect to the login page', inject(($http, $httpBackend, Auth) => {
//       $httpBackend
//         .when('GET', 'endpoint')
//         .respond(401, '');
//       spyOn(Auth, 'unAuthenticate');

//       $http.get('endpoint');
//       $httpBackend.flush();

//       expect(Auth.unAuthenticate).toHaveBeenCalled();
//     }));
//   });
// });
