import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReportingProfileComponent } from '../+reporting-profile/reporting-profile.component';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { CookieModule } from 'ngx-cookie';
import { StatsService, ReportQueryService, ScanResultsService } from '../../shared/reporting';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { of as observableOf } from 'rxjs';

describe('ReportingProfileComponent', () => {
  let fixture, component, element, statsService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        CookieModule.forRoot(),
        HttpClientTestingModule
      ],
      declarations: [
        ReportingProfileComponent
      ],
      providers: [
        { provide: ChefSessionService, useClass: MockChefSessionService },
        StatsService,
        ReportQueryService,
        ScanResultsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ReportingProfileComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    statsService = element.injector.get(StatsService);
  });

  describe('ngOnInit()', () => {
    it('calls fetchProfile to set the data', () => {
      component.ngOnInit();
      spyOn(statsService, 'getProfileResults').and.returnValue(observableOf([]));
      spyOn(statsService, 'getProfileResultsSummary').and.returnValue(observableOf({}));
      fixture.detectChanges();
      expect(component.profile).toEqual({});
      expect(component.controls).toEqual([]);
    });
  });

  describe('hideScanResults', () => {
    it('sets displayscanresults to false', () => {
      component.hideScanResults();
      expect(component.displayScanResultsSidebar).toBe(false);
    });
  });

  describe('getNodes', () => {
    it('calls statsService.getNodes with the paginationOverride value', () => {
      spyOn(statsService, 'getNodes').and.returnValue(observableOf({items: []}));
      component.getNodes([], {profileId: '123', controlId: '321'});
      expect(statsService.getNodes).toHaveBeenCalledWith(
        [
          {type: { name: 'profile_id' }, value: { text: '123'} },
          {type: { name: 'control_id' }, value: { text: '321'} }
        ],
        { perPage: 1000, page: 1, sort: 'latest_report.end_time', order: 'desc' });
    });
  });
});
