import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { CookieModule } from 'ngx-cookie';
import { ReportingProfilesComponent } from './reporting-profiles.component';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { StatsService, ReportQueryService, ReportDataService } from '../../shared/reporting';

describe('ReportingProfilesComponent', () => {
  let fixture: ComponentFixture<ReportingProfilesComponent>;
  let component: ReportingProfilesComponent;
  let reportQuery: ReportQueryService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        CookieModule.forRoot(),
        HttpClientTestingModule
      ],
      declarations: [
        ReportingProfilesComponent
      ],
      providers: [
        { provide: ChefSessionService, useClass: MockChefSessionService },
        StatsService,
        ReportQueryService,
        ReportDataService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ReportingProfilesComponent);
    component = fixture.componentInstance;
    reportQuery = TestBed.get(ReportQueryService);
  });

  describe('onProfilesListPageChanged()', () => {
    it('calls getData with applied filters', () => {
      const filters = reportQuery.filters.getValue();
      spyOn(component, 'getData');

      component.onProfilesListPageChanged({detail: { value: 5}});

      expect(component.getData).toHaveBeenCalledWith(filters);
    });
  });

  describe('onProfilesListSortToggled()', () => {
    it('calls getData with applied filters', () => {
      const filters = reportQuery.filters.getValue();
      spyOn(component, 'getData');

      component.onProfilesListSortToggled({detail: { sort: 'platform', order: 'desc'}});

      expect(component.getData).toHaveBeenCalledWith(filters);
    });
  });

  describe('addControlStatus()', () => {
    const data = {profiles: [
      {
        controls: [
          { results: [ {status: 'skipped'}, {status: 'passed'} ] },
          { results: [ {status: 'passed'}, {status: 'passed'} ] },
          { results: [ {status: 'failed'}, {status: 'passed'} ] },
          { results: [ {status: 'skipped'}, {status: 'skipped'} ] }
        ]
      }
    ]};
    const expected = [
      { results: [ { status: 'skipped' }, { status: 'passed' } ], status: 'passed' },
      { results: [ { status: 'passed' }, { status: 'passed' } ], status: 'passed' },
      { results: [ { status: 'failed' }, { status: 'passed' } ], status: 'failed' },
      { results: [ { status: 'skipped'}, {status: 'skipped'} ], status: 'skipped' }
    ];
    it('sets the status for the control according to results', () => {
      expect(component.addControlStatus(data)).toEqual(expected);
    });
  });
});
