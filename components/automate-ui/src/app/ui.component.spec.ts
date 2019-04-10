// import { ChefSessionService } from './shared';
// import { MockHttpProviders } from './shared/testing';
// import { FeatureFlagsComponent, FeatureFlagsService } from './feature-flags';
// import { CookieService } from 'ngx-cookie';
// import { TelemetryService } from './shared/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

xdescribe('UIComponent', () => {
  // let session: ChefSessionService;
  //
  // beforeEach(() => {
  //   TestBed.configureTestingModule({
  //     declarations: [
  //       UIComponent,
  //       FeatureFlagsComponent
  //     ],
  //     providers: [
  //       CookieService,
  //       MockHttpProviders,
  //       FeatureFlagsService,
  //       ChefSessionService,
  //       { provide: TelemetryService, useClass: MockTelemetryService }
  //     ],
  //     imports: [
  //       RouterTestingModule
  //     ]
  //   });
  //
  //   session = TestBed.get(ChefSessionService);
  // });

  describe('UI Component', () => {
  //
  //   describe('when user is not logged in', () => {
  //     beforeEach(() => {
  //       spyOn(session, 'loggedIn').and.returnValue(false);
  //       spyOn(session, 'logout').and.stub();
  //     });
  //
  //     it('logs the user out', () => {
  //       TestBed.createComponent(UIComponent);
  //       expect(session.loggedIn).toHaveBeenCalled();
  //       expect(session.logout).toHaveBeenCalled();
  //     });
  //   });
  //
  //   describe('when user is logged in', () => {
  //     beforeEach(() => {
  //       spyOn(session, 'loggedIn').and.returnValue(true);
  //       spyOn(session, 'logout').and.stub();
  //     });
  //
  //     it('loads the page as normal', () => {
  //       TestBed.createComponent(UIComponent);
  //       expect(session.loggedIn).toHaveBeenCalled();
  //       expect(session.logout).not.toHaveBeenCalled();
  //     });
  //   });
  });
});
// TODO: Add back in when we have authn
