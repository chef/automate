import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { OverviewTrendComponent } from './overview-trend.component';

describe('OverviewTrendComponent', () => {
  let fixture: ComponentFixture<OverviewTrendComponent>;
  let component: OverviewTrendComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
      ],
      declarations: [
        OverviewTrendComponent
      ],
      providers: [
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(OverviewTrendComponent);
    component = fixture.componentInstance;
  });

  it('initializes', () => {
    expect(component).not.toBeNull();
  });

  it('create UTC date from string date with timezone', () => {
    for ( let hour = 0; hour < 10; hour++ ) {
      expect(component.createUtcDate('2019-09-14T0' + hour + ':59:59Z').getDate()).toEqual(14);
    }
    for ( let hour = 10; hour < 24; hour++ ) {
      expect(component.createUtcDate('2019-09-14T' + hour + ':59:59Z').getDate()).toEqual(14);
    }
  });
});
