import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { SimpleLineGraphComponent } from './simple-line-graph.component';
import * as d3 from 'd3';

describe('OverviewTrendComponent', () => {
  let fixture: ComponentFixture<SimpleLineGraphComponent>;
  let component: SimpleLineGraphComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
      ],
      declarations: [
        SimpleLineGraphComponent
      ],
      providers: [
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    });

    fixture = TestBed.createComponent(SimpleLineGraphComponent);
    component = fixture.componentInstance;
  });

  it('initializes', () => {
    expect(component).not.toBeNull();
  });

  it('contains an svg area for the chart', () => {
    expect(d3.select(component.svg)).not.toBeNull();
  });
});
