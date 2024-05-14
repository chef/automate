import {
  ComponentFixture,
  TestBed,
  tick,
  fakeAsync,
} from "@angular/core/testing";
import { EventFeedComponent } from "./event-feed.component";
import { Store } from "@ngrx/store";
import { ActivatedRoute, Router } from "@angular/router";
import {
  LayoutFacadeService,
  Sidebar,
} from "app/entities/layout/layout.facade";
import { of } from "rxjs";
import * as eventFeedActions from "../../services/event-feed/event-feed.actions";
import * as eventFeedSelectors from "../../services/event-feed/event-feed.selectors";
import { EventTaskCount } from "../../types/types";
import * as moment from 'moment';

describe("EventFeedComponent", () => {
  let component: EventFeedComponent;
  let fixture: ComponentFixture<EventFeedComponent>;
  let mockStore: any;
  let mockActivatedRoute: any;
  let mockRouter: any;
  let mockLayoutFacadeService: any;
  let router: Router;

  beforeEach(async () => {
    mockStore = {
      select: jasmine.createSpy("select").and.returnValue(of([])),
      dispatch: jasmine.createSpy("dispatch"),
    };

    mockActivatedRoute = {
      snapshot: {
        queryParamMap: { getAll: () => [] },
      },
      queryParamMap: of({ getAll: () => [] }),
    };
      
    mockRouter = {
      navigate: jasmine.createSpy("navigate"),
    };

    mockLayoutFacadeService = {
      showSidebar: jasmine.createSpy("showSidebar"),
    };

    await TestBed.configureTestingModule({
      declarations: [EventFeedComponent],
      providers: [
        { provide: Store, useValue: mockStore },
        { provide: ActivatedRoute, useValue: mockActivatedRoute },
        { provide: LayoutFacadeService, useValue: mockLayoutFacadeService },
        { provide: Router, useValue: mockRouter },
      ],
    }).compileComponents();
    router = TestBed.inject(Router);
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(EventFeedComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it("should create", () => {
    expect(component).toBeTruthy();
  });

  it("should initialize component properly", () => {
    expect(mockLayoutFacadeService.showSidebar).toHaveBeenCalledWith(
      Sidebar.Dashboards
    );
  });

  it("should toggle filters visibility", () => {
    const initialVisibility = component.filtersVisible;
    component.toggleFilters();
    expect(component.filtersVisible).toEqual(!initialVisibility);
  });

  it("should dispatch suggestion action on suggest values", () => {
    const type = "test";
    const text = "test";
    component.onSuggestValues({ detail: { type, text } });
    expect(mockStore.dispatch).toHaveBeenCalledWith(
      eventFeedActions.getSuggestions(type, text)
    );
  });

  it("should update router on filter added", () => {
    const event = { detail: { type: "test", text: "test" } };
    component.onFilterAdded(event);
    expect(router.navigate).toHaveBeenCalledWith([], {
      queryParams: { test: ["test"] },
    });
  });

  it("should update router on filters clear", () => {
    const event = {};
    component.onFiltersClear(event);
    expect(mockRouter.navigate).toHaveBeenCalled();
  });

  it("should update router on filter removed", () => {
    const event = { detail: { type: "test", text: "test" } };
    component.onFilterRemoved(event);
    expect(mockRouter.navigate).toHaveBeenCalled();
  });

  it("should dispatch load more action on load more", () => {
    component.loadMore();
    expect(mockStore.dispatch).toHaveBeenCalledWith(
      eventFeedActions.loadMoreFeed()
    );
  });

  it("should dispatch date range filter action on select date range", fakeAsync(() => {
    const dateRange = { start: new Date(), end: new Date() };
    component.selectDateRange(dateRange);
    tick();
    expect(mockStore.dispatch).toHaveBeenCalled();
  }));

  // addFeedDateRangeFilter
  it("should disable resetTimescale and clear filterTimeScaleDates when date range is within 6 days", () => {
    const dateRange = { start: new Date(), end: new Date() };
    component.selectDateRange(dateRange);
  
    expect(component.resetTimescaleDisabled).toBe(true);
    expect(component.filterTimeScaleDates).toEqual([]);
    expect(mockStore.dispatch).toHaveBeenCalledWith(
      eventFeedActions.addFeedDateRangeFilter(dateRange.start, dateRange.end)
    );
  });
  
  // addFeedDateRangeFilter
  it("should enable resetTimescale and set filterTimeScaleDates when date range exceeds 6 days", () => {
    const startDate = new Date("2024-05-01");
    const endDate = new Date("2024-05-08");
    const dateRange = { start: startDate, end: endDate };
  
    component.selectDateRange(dateRange);
  
    expect(component.resetTimescaleDisabled).toBe(false);
    // Mock the expected date range within the function
    const expectedDateRange = ["2024-05-01", "2024-05-02", "2024-05-03", "2024-05-04", "2024-05-05", "2024-05-06", "2024-05-07", "2024-05-08"];
    expect(component.filterTimeScaleDates).toEqual(expectedDateRange);
    expect(mockStore.dispatch).toHaveBeenCalledWith(
      eventFeedActions.addFeedDateRangeFilter(dateRange.start, dateRange.end)
    );
  });
  
  // setHeadersCountOnFilterTimeScale
  it("should update header counts after a delay", fakeAsync(() => {
    const initialCounts: EventTaskCount = {
      total: 10,
      update: 3,
      create: 4,
      delete: 3,
    };
    spyOn(eventFeedSelectors, "eventTaskCounts").and.returnValue(
      of(initialCounts)
    );
    component.setHeadersCountOnFilterTimeScale();
    tick(1000);

    expect(component.totalTaskCounts).toEqual(initialCounts.total);
    expect(component.updateCounts).toEqual(initialCounts.update);
    expect(component.createCounts).toEqual(initialCounts.create);
    expect(component.deleteCounts).toEqual(initialCounts.delete);
  }));

  // getAllDatesInRange
  it('should generate correct array of dates between start and end date', () => {
    const startDate = new Date('2024-05-01');
    const endDate = new Date('2024-05-05');
    const expectedDates = ['2024-05-01', '2024-05-02', '2024-05-03', '2024-05-04', '2024-05-05'];
    const generatedDates = component.getAllDatesInRange(startDate, endDate);
    expect(generatedDates).toEqual(expectedDates);
  });

  it('should handle the case when start and end dates are the same', () => {
    const startDate = new Date('2024-05-01');
    const endDate = new Date('2024-05-01');
    const expectedDates = ['2024-05-01'];
    const generatedDates = component.getAllDatesInRange(startDate, endDate);
    expect(generatedDates).toEqual(expectedDates);
  });

  it('should handle the case when end date is before the start date', () => {
    const startDate = new Date('2024-05-05');
    const endDate = new Date('2024-05-01');
    const generatedDates = component.getAllDatesInRange(startDate, endDate);
    expect(generatedDates.length).toBe(0);
  });

  it('should handle edge cases for large date ranges', () => {
    const startDate = new Date('2024-05-01');
    const endDate = new Date('2024-05-10');
    const generatedDates = component.getAllDatesInRange(startDate, endDate);
    const expectedLength = moment(endDate).diff(startDate, 'days') + 1;
    expect(generatedDates.length).toBe(expectedLength);
  });
  
});
