import { TestBed } from '@angular/core/testing';
import { SimpleChanges, SimpleChange } from '@angular/core';
import { MockComponent } from 'ng2-mock-component';
import { NodeCount } from '../../types/types';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ConvergeRadialGraphComponent } from './converge-radial-graph.component';

describe('ConvergeRadialGraphComponent', () => {
  let fixture, component;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        ConvergeRadialGraphComponent,
        MockComponent({
          selector: 'chef-radial-chart',
          inputs: ['chartData', 'chartColors', 'labelIcon', 'labelText', 'dimensions']
        })
      ],
      providers: [ ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ConvergeRadialGraphComponent);
    component = fixture.componentInstance;
  });

  describe('initialization', () => {
    it('count should not be undefined', () => {
      fixture.detectChanges();
      expect(component.count).not.toEqual(undefined);
    });
  });
  describe('ngOnChanges', () => {
    const noNodes = {
      success: 0,
      failure: 0,
      missing: 0,
      total: 0
    };
    const someNodes = {
      total: 93,
      failure: 42,
      success: 12,
      missing: 41
    };
    describe('when there are nodes', () => {

      it('updates the chart data', () => {

        component.ngOnChanges(create_changes(noNodes, someNodes));
        const expectedObject = {
          total: 93,
          failure: 42,
          success: 12,
          missing: 41
        };
        expect(component.count).toEqual(expectedObject);
        expect(component.label).toEqual('93 Total Nodes');
      });
    });

    describe('when there are no nodes', () => {
      it('fetches data from the converge stats service', () => {
        component.ngOnChanges(create_changes(noNodes, noNodes));
        const expectedObject = {
          success: 0,
          failure: 0,
          missing: 0,
          total: 0
        };
        expect(component.count).toEqual(expectedObject);
        expect(component.label).toEqual('No Nodes Found');
      });
    });
  });

  function create_changes(
    previousValue: NodeCount,
    currentValue: NodeCount): SimpleChanges {
    const changesObj: SimpleChanges = {
      count: new SimpleChange(previousValue, currentValue, true)
    };
    return changesObj;
  }

});
