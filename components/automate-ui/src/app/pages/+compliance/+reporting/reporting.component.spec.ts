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
      expect(component.availableFilterTypes).toEqual([
        {
          'name': 'profile',
          'title': 'Profile',
          'description': 'Add the name or ID to filter this report against a profile',
          'placeholder': 'Name or ID'
        },
        {
          'name': 'node',
          'title': 'Node',
          'description':
          'Add the name, ID, or hostname to filter this report against a specific node',
          'placeholder': 'Name, ID, or hostname'
        },
        {
          'name': 'platform',
          'title': 'Platform',
          'description': 'Add the name to filter this report to a specific platform',
          'placeholder': 'Name'
        },
        {
          'name': 'environment',
          'title': 'Environment',
          'description': 'Add the name to filter this report to a specific environment',
          'placeholder': 'Name'
        },
        {
          'name': 'control',
          'title': 'Control',
          'description': 'Add the title to filter this report against a control',
          'placeholder': 'Title'
        },
        {
          'name': 'role',
          'title': 'Role',
          'description': 'Add the role to filter this report to a specific role',
          'placeholder': 'Role'
        },
        {
          'name': 'recipe',
          'title': 'Recipe',
          'description': 'Add the recipe to filter this report to a specific recipe',
          'placeholder': 'Recipe'
        },
        {
          'name': 'organization',
          'title': 'Organization',
          'description': 'Add the organization to filter this report to a specific organization',
          'placeholder': 'Organization'
        }
      ]);
    });
  });

  describe('ngOnDestroy()', () => {
    it('notifies subscriptions to unsubscribe', () => {
      const source = Observable.create(() => {});
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
      spyOn(statsService, 'downloadReport').and.returnValue(Observable.create(() => {}));
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
