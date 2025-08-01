import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { StoreModule, Store } from '@ngrx/store';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { TimeComponent } from './time.component';
import { DatetimePipe } from 'app/pipes/datetime.pipe';
import { UserPreferencesService } from 'app/services/user-preferences/user-preferences.service';
import moment from 'moment';

describe('TimeComponent', () => {
  let component: TimeComponent;
  let fixture: ComponentFixture<TimeComponent>;
  let store: Store<NgrxStateAtom>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [TimeComponent, DatetimePipe],
      providers: [UserPreferencesService],
      imports: [StoreModule.forRoot(ngrxReducers, { runtimeChecks })],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    }).compileComponents();
  }));

  beforeEach(() => {
    store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture = TestBed.createComponent(TimeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeDefined();
  });

  it('render timeformat', () => {
    const todayDate = new Date().toISOString();
    component.time = todayDate;
    fixture.detectChanges();
    const element = fixture.debugElement.nativeElement;
    // Use UTC moment to match what the DatetimePipe uses
    expect(element.querySelector('span').textContent).toContain(moment.utc().format('ddd, DD MMM YYYY'));
  });
});
