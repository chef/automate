import { waitForAsync, TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { By } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { AttributesComponent } from './attributes.component';
import { AttributesService } from '../../services/attributes/attributes.service';
import { JsonTreeComponent } from '../json-tree/json-tree.component';
import { NodeAttributes } from '../../types/types';

class MockAttributesService {
  nullNodeAttributes = new NodeAttributes({
    node_id: '',
    name: '',
    run_list: [],
    chef_environment: '',
    normal: '',
    normal_value_count: 0,
    default: '',
    default_value_count: 0,
    override: '',
    override_value_count: 0,
    automatic: '',
    automatic_value_count: 0,
    all_value_count: 0
  });

  fetch() {
    return Promise.resolve(
      new NodeAttributes({
        node_id: 'node_id',
        name: 'chef-client.solo',
        run_list: [],
        chef_environment: 'chef_environment',
        default: '{\"my-cookbook\": {\"port\": 80, \"code\": {\"location\": \"github\"}}}',
        default_value_count: 2,
        override: '{}',
        override_value_count: 0,
        normal: '{\"my-cookbook\": {\"norm\": \"no\"}}',
        normal_value_count: 1,
        automatic: '',
        automatic_value_count: 0,
        all_value_count: 3
      })
    );
  }
}

describe('AttributesComponent', () => {
  let fixture, component, element;

  const retrieve_default = {
    'my-cookbook': {
      port: 80,
      code: {
        location: 'github'
      }
    }
  };

  const retrieve_all = {
    'my-cookbook': {
      port: 80,
      code: {
        location: 'github'
      },
      norm: 'no'
    }
  };

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        FormsModule
      ],
      declarations: [
        AttributesComponent,
        JsonTreeComponent
      ],
      providers: [
        { provide: AttributesService, useClass: MockAttributesService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });
    fixture = TestBed.createComponent(AttributesComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('renders the attributes component correctly', () => {
    expect(element.query(By.css('.attributes-rollup'))).not.toBeNull();
  });

  it('fetches attributes and returns a NodeAttributes object', waitForAsync(() => {
    fixture.whenStable().then(() => {
      expect(component.retrieve('default')).toEqual(retrieve_default);
      expect(component.retrieve('all')).toEqual(retrieve_all);
    });
  }));

  describe('#filter', () => {

    it('applies the selected precedence level', waitForAsync(() => {
      fixture.whenStable().then(() => {
        expect(component.selected_level).toBe('all');

        component.filter('default');
        expect(component.selected_level).toBe('default');
      });
    }));

    it('applies the selected attributes collection', waitForAsync(() => {
      fixture.whenStable().then(() => {
        expect(component.selectedAttrs).toEqual(retrieve_all);

        component.filter('default');
        expect(component.selectedAttrs).toEqual(retrieve_default);
      });
    }));
  });

  describe('#search', () => {

    it('obtains a result count from the json-tree component', () => {
      expect(component.resultCount).toBeUndefined();

      spyOn(component.tree, 'search').and.returnValue(6);
      component.search();
      expect(component.resultCount).toBe(6);
    });

    it('applies a correct result summary', () => {
      expect(component.resultSummary).toBeUndefined();

      spyOn(component.tree, 'search').and.returnValues(0, 1, 2);
      component.search();

      expect(component.resultSummary).toBe('No results');

      component.search();
      expect(component.resultSummary).toBe('1 result');

      component.search();
      expect(component.resultSummary).toBe('2 results');
    });
  });

  describe('#resetSearch', () => {

    it('resets previously set search values', () => {
      expect(component.searchTerm).toBe(undefined);
      expect(component.resultCount).toBe(undefined);
      expect(component.resultSummary).toBe(undefined);

      spyOn(component.tree, 'search').and.returnValue(8);
      component.search();
      expect(component.resultCount).toBe(8);

      component.resetSearch();
      expect(component.searchTerm).toBe(null);
      expect(component.resultCount).toBe(null);
      expect(component.resultSummary).toBe(null);
    });

    it('calls tree.reset()', () => {
      spyOn(component.tree, 'reset');
      component.resetSearch();
      expect(component.tree.reset).toHaveBeenCalled();
    });
  });
});
