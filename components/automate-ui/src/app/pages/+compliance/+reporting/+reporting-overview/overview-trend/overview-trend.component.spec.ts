import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { UserPreferencesService } from 'app/services/user-preferences/user-preferences.service';
import { OverviewTrendComponent } from './overview-trend.component';
import { StoreModule } from '@ngrx/store';

describe('OverviewTrendComponent', () => {
  let fixture: ComponentFixture<OverviewTrendComponent>;
  let component: OverviewTrendComponent;
  let userPrefsService: UserPreferencesService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        StoreModule.forRoot({})
      ],
      declarations: [
      ],
      providers: [
        UserPreferencesService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(OverviewTrendComponent);
    userPrefsService = TestBed.inject(UserPreferencesService);
    component = fixture.componentInstance;
  });

  it('initializes', () => {
    expect(component).not.toBeNull();
  });

  it('create UTC date from string date with timezone', () => {
    for ( let hour = 0; hour < 10; hour++ ) {
      expect(component.createTimezoneDate('2019-09-14T0' + hour + ':59:59Z').date()).toEqual(14);
    }
    for ( let hour = 10; hour < 24; hour++ ) {
      expect(component.createTimezoneDate('2019-09-14T' + hour + ':59:59Z').date()).toEqual(14);
    }
  });
});
