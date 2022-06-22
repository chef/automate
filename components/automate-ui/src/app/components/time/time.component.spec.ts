import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { StoreModule, Store } from '@ngrx/store';
import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { TimeComponent } from './time.component';
import { DatetimePipe } from 'app/pipes/datetime.pipe';
import { UserPreferencesService } from 'app/services/user-preferences/user-preferences.service';
import * as moment from 'moment/moment';

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
    component.time = '2021-07-26T10:50:42.529876072Z';
    const element = fixture.debugElement.nativeElement;
    expect(element.querySelector('span').textContent).toContain(moment(new Date()).format('ddd, DD MMM YYYY'));
  });
});
