import { ComponentFixture, TestBed, tick, fakeAsync } from '@angular/core/testing';
import { EventFeedComponent } from './event-feed.component';
import { Store } from '@ngrx/store';
import { ActivatedRoute, Router } from '@angular/router';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { of } from 'rxjs';
import * as eventFeedActions from '../../services/event-feed/event-feed.actions';

describe('EventFeedComponent', () => {
  let component: EventFeedComponent;
  let fixture: ComponentFixture<EventFeedComponent>;
  let mockStore: any;
  let mockActivatedRoute: any;
  let mockRouter: any;
  let mockLayoutFacadeService: any;
  let router: Router;

  beforeEach(async () => {
    mockStore = {
      select: jasmine.createSpy('select').and.returnValue(of([])),
      dispatch: jasmine.createSpy('dispatch')
    };

    mockActivatedRoute = {
        snapshot: {
          queryParamMap: of({}), // Mocking queryParamMap to return an observable with an empty object
        },
        // Provide a mock for the pipe method of queryParamMap
        queryParamMap: {
          pipe: () => of({}) // Mocking the pipe method to return an observable with an empty object
        }
      };
    

    mockRouter = {
      navigate: jasmine.createSpy('navigate')
    };

    mockLayoutFacadeService = {
      showSidebar: jasmine.createSpy('showSidebar')
    };

    await TestBed.configureTestingModule({
      declarations: [EventFeedComponent],
      providers: [
        { provide: Store, useValue: mockStore },
        { provide: ActivatedRoute, useValue: mockActivatedRoute },
        { provide: LayoutFacadeService, useValue: mockLayoutFacadeService },
        { provide: Router, useValue: mockRouter }, 
      ]
    }).compileComponents();
    router = TestBed.inject(Router);
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(EventFeedComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize component properly', () => {
    expect(mockLayoutFacadeService.showSidebar).toHaveBeenCalledWith(Sidebar.Dashboards);
  });

  it('should toggle filters visibility', () => {
    const initialVisibility = component.filtersVisible;
    component.toggleFilters();
    expect(component.filtersVisible).toEqual(!initialVisibility);
  });

  it('should dispatch suggestion action on suggest values', () => {
    const type = 'test';
    const text = 'test';
    component.onSuggestValues({ detail: { type, text } });
    expect(mockStore.dispatch).toHaveBeenCalledWith(eventFeedActions.getSuggestions(type, text));
  });

  it('should update router on filter added', () => {
    const event = { detail: { type: 'test', text: 'test' } };
    const navigateSpy = spyOn(router, 'navigate').and.returnValue(Promise.resolve(true));
    component.onFilterAdded(event);
    expect(navigateSpy).toHaveBeenCalledWith([], {
      queryParams: { test: ['test'] }
    }); 
  });
  

  it('should update router on filters clear', () => {
    const event = {};
    component.onFiltersClear(event);
    expect(mockRouter.navigate).toHaveBeenCalled();
  });

  it('should update router on filter removed', () => {
    const event = { detail: { type: 'test', text: 'test' } };
    component.onFilterRemoved(event);
    expect(mockRouter.navigate).toHaveBeenCalled();
  });

  it('should dispatch load more action on load more', () => {
    component.loadMore();
    expect(mockStore.dispatch).toHaveBeenCalledWith(eventFeedActions.loadMoreFeed());
  });

  it('should dispatch date range filter action on select date range', fakeAsync(() => {
    const dateRange = { start: new Date(), end: new Date() };
    component.selectDateRange(dateRange);
    tick();
    expect(mockStore.dispatch).toHaveBeenCalled();
  }));
});
