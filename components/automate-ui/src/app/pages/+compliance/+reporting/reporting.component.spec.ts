import { CUSTOM_ELEMENTS_SCHEMA, DebugElement } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { CookieModule } from 'ngx-cookie';
import { Observable, of as observableOf } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { ReportingComponent } from '../+reporting/reporting.component';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import {
  StatsService,
  SuggestionsService,
  ReportQueryService,
  ReportDataService
} from '../shared/reporting';
import { TelemetryService } from '../../../services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}
describe('ReportingComponent', () => {
  let fixture: ComponentFixture<ReportingComponent>,
    component: ReportingComponent,
    element: DebugElement;
  let statsService: StatsService;
  let suggestionsService: SuggestionsService;
  let reportQueryService: ReportQueryService;
  let router: Router;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        CookieModule.forRoot(),
        HttpClientTestingModule
      ],
      declarations: [
        ReportingComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        ChefSessionService,
        StatsService,
        SuggestionsService,
        ReportQueryService,
        ReportDataService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ReportingComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    statsService = element.injector.get(StatsService);
    suggestionsService = element.injector.get(SuggestionsService);
    reportQueryService = element.injector.get(ReportQueryService);
    router = TestBed.get(Router);
  });

  it('displays tabs', () => {
    const qe = element.query(By.css('.nav-tabs-list'));
    expect(qe).not.toBeNull();
  });

  describe('sets availableFilterTypes', () => {
    it('to include profile, platform, environment, node, and control', () => {
      const availableFilterTypesNames = component.availableFilterTypes.map( type => type.name);

      const expected = [
        'chef_server', 'chef_tags', 'control', 'environment', 'node', 'organization', 'platform',
        'policy_group', 'policy_name', 'profile', 'recipe', 'role', 'inspec_version'];

      expect(expected.sort()).toEqual(availableFilterTypesNames.sort());
    });

    it('to have the titles sorted', () => {
      // Ensure availableFilterTypes.Titles are sorted
      const availableFilterTypesTitles = component.availableFilterTypes.map( type => type.title);
      const expectedAvailableFilterTypesTitle =
        component.availableFilterTypes.map(type => type.title).sort();

      expect(expectedAvailableFilterTypesTitle).toEqual(availableFilterTypesTitles,
        'Filter Types are not sorted');
    });
  });

  describe('ngOnDestroy()', () => {
    it('notifies subscriptions to unsubscribe', () => {
      const source = new Observable(() => {});
      const subscription = source.pipe(
          takeUntil((component as any).isDestroyed))
        .subscribe(() => {});

      spyOn(subscription, 'unsubscribe');
      component.ngOnDestroy();

      expect(subscription.unsubscribe).toHaveBeenCalled();
    });
  });

  describe('onEndDateChanged', () => {
    it('sets date range on report query', () => {
      spyOn(reportQueryService, 'setDateRange');
      const endDate = new Date();
      const event = {detail: endDate};
      component.onEndDateChanged(event);
      expect(reportQueryService.setDateRange).toHaveBeenCalled();
    });
  });

  describe('getSuggestions()', () => {
    describe('when the item has a version', () => {
      it('sets the title to text, version values to display to the user', () => {
        const type = 'profile';
        const text = 'dev';
        const filters = [];
        spyOn(suggestionsService, 'getSuggestions').and.returnValue(observableOf([
          {text: 'dev sec baseline', version: '2.0'}
        ]));

        component.getSuggestions(type, text, filters).subscribe(values => {
          expect(values).toEqual([
            {text: 'dev sec baseline', version: '2.0', title: 'dev sec baseline, v2.0'}
          ]);
        });
      });
    });
    describe('when the item does not have a version', () => {
      it('sets the title to the text value', () => {
        const type = 'node';
        const text = 'tea';
        const filters = [];
        spyOn(suggestionsService, 'getSuggestions').and.returnValue(observableOf([
          {text: 'teal-spohn'}
        ]));

        component.getSuggestions(type, text, filters).subscribe(values => {
          expect(values).toEqual([
            {text: 'teal-spohn', title: 'teal-spohn'}
          ]);
        });
      });
    });
  });

  describe('getData()', () => {
    const filters = [
      {'end_time': '2323'},
      {'type': 'Node', 'value': '1231'},
      {'type': 'Platform', 'value': 'ubuntu'}
    ];
    const reportingSummaryData = {
      'duration': 49258.00239,
      'start_date': '2017-06-22T00:21:21Z',
      'stats': {
        'environments': 15,
        'nodes': 15,
        'platforms': 15,
        'profiles': 8
      },
      'status': 'failed'
    };

    beforeEach(() => {
      spyOn(statsService, 'getSummary').and.returnValue(observableOf(reportingSummaryData));
      component.getData(filters);
    });

    it('sets the value for reportingSummary', () => {
      expect(component.reportData.reportingSummary).toEqual(reportingSummaryData);
    });
  });

  describe('toggleDownloadDropdown()', () => {
    it('toggles download button dropdown visibility', () => {
      component.downloadOptsVisible = false;
      component.toggleDownloadDropdown();
      expect(component.downloadOptsVisible).toEqual(true);
    });
  });

  describe('hideDownloadDropdown()', () => {
    it('hides download button dropdown', () => {
      component.downloadOptsVisible = true;
      component.hideDownloadDropdown();
      expect(component.downloadOptsVisible).toEqual(false);
    });
  });

  describe('onDownloadOptPressed', () => {
    it('calls downloadReport with correct format type', () => {
      spyOn(statsService, 'downloadReport').and.returnValue(new Observable(() => {}));
      component.onDownloadOptPressed('json');
      expect(statsService.downloadReport).toHaveBeenCalledWith('json', jasmine.any(Object));
    });
  });

  describe('applyParamFilters()', () => {
    it('parses multiple filters', () => {
      spyOn(reportQueryService, 'addFilter');
      component.applyParamFilters('job_uuid:123+job_uuid:456');
      expect(reportQueryService.addFilter).toHaveBeenCalledWith(
        {type: {name: 'job_uuid'}, value: {text: '123'} }
      );
      expect(reportQueryService.addFilter).toHaveBeenCalledWith(
        {type: {name: 'job_uuid'}, value: {text: '456'} }
      );
    });

    it('parses single filters', () => {
      spyOn(reportQueryService, 'addFilter');
      component.applyParamFilters('job_uuid:123');
      expect(reportQueryService.addFilter).toHaveBeenCalledWith(
        {type: {name: 'job_uuid'}, value: {text: '123'} }
      );
    });
  });

  describe('onFiltersClear()', () => {
    it('clears report filters', () => {
      spyOn(reportQueryService, 'clearFilters');
      component.onFiltersClear({});
      expect(reportQueryService.clearFilters).toHaveBeenCalled();
    });

    it('updates filter query params', () => {
      spyOn(router, 'navigate');
      component.onFiltersClear({});
      expect(router.navigate).toHaveBeenCalledWith([], {queryParams: {filters: undefined}});
    });
  });

  // Does not work in PhantomJS...
  //
  // describe('when filter-value-input event is fired', () => {
  //   fit('fetches suggestions for current input value', () => {
  //     spyOn(suggestionsService, 'getSuggestions').and.returnValue(observableOf([]));

  //     let type = 'environment';
  //     let text = 'foo';
  //     let querySearch = element.query(By.css('query-search')).nativeElement;
  //     querySearch.fire('filter-value-input', {type, text});

  //     expect(suggestionsService.getSuggestions).toHaveBeenCalledWith(type, text);
  //   });
  // });
});
