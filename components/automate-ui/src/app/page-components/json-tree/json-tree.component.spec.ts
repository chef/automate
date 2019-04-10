import { ComponentFixture, TestBed } from '@angular/core/testing';

import { JsonTreeComponent } from './json-tree.component';

describe('JsonTreeComponent', () => {
  let component: JsonTreeComponent;
  let fixture: ComponentFixture<JsonTreeComponent>;
  let element;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        JsonTreeComponent
      ]
    });

    fixture = TestBed.createComponent(JsonTreeComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
  });

  describe('given a valid JSON object', () => {

    it ('renders it', () => {

      component.json = {
        cat: true,
        hat: true,
        things: [
          'One',
          'Two'
        ]
      };

      component.ngOnChanges();

      const labels = element.nativeElement.querySelectorAll('.jsontree_label');
      const bools = element.nativeElement.querySelectorAll('.jsontree_value_boolean');
      const strings = element.nativeElement.querySelectorAll('.jsontree_value_string');

      expect(labels[0].innerText).toBe('"cat"');
      expect(labels[1].innerText).toBe('"hat"');
      expect(labels[2].innerText).toBe('"things"');
      expect(bools[0].innerText).toBe('true');
      expect(bools[1].innerText).toBe('true');
      expect(strings[0].innerText).toBe('"One"');
      expect(strings[1].innerText).toBe('"Two"');
    });
  });

  describe('#expand', () => {

    it('expands it', () => {
      component.json = {};
      component.ngOnChanges();
      spyOn(component.tree, 'expand');
      component.expand();
      expect(component.tree.expand).toHaveBeenCalled();
    });
  });

  describe('#collapse', () => {

    it('collapses it', () => {
      component.json = {};
      component.ngOnChanges();
      spyOn(component.tree, 'collapse');
      component.collapse();
      expect(component.tree.collapse).toHaveBeenCalled();
    });
  });

  describe('#reset', () => {

    it('calls collapse()', () => {
      spyOn(component, 'collapse');
      component.reset();
      expect(component.collapse).toHaveBeenCalled();
    });

    it ('calls unhighlight()', () => {
      spyOn(component, 'unhighlight');
      component.reset();
      expect(component.unhighlight).toHaveBeenCalled();
    });
  });

  describe('#unhighlight', () => {

    it('removes all highlights', () => {

      function highlights() {
        return element.nativeElement.querySelectorAll('.highlight');
      }

      component.json = {
        cat: true,
        hat: true,
        things: [
          'One',
          'Two'
        ]
      };

      component.ngOnChanges();
      component.search('true');
      expect(highlights().length).toBe(2);
      component.unhighlight();
      expect(highlights().length).toBe(0);
    });
  });

  describe('#search', () => {

    describe('without a term', () => {

      it('returns a result count of zero', () => {
        // expect(component.search()).toBe(0);
        expect(component.search(null)).toBe(0);
        expect(component.search(undefined)).toBe(0);
        expect(component.search('')).toBe(0);
      });
    });

    describe('with a term', () => {

      it('highlights matching properties, case-insensitively', () => {

        component.json = {
          things: [
            'Thing One',
            'Thing Two'
          ]
        };

        component.ngOnChanges();
        component.search('thing');
        expect(element.nativeElement.querySelectorAll('.highlight').length).toBe(3);
      });

      it('expands the branches of matching properties', () => {

        function expanded() {
          return element.nativeElement.querySelectorAll('.jsontree_node_expanded');
        }

        component.json = {
          we: [
            'Sally',
            'I'
          ],
          pets: [
            'fish'
          ],
          things: [
            'One',
            'Two'
          ]
        };

        component.ngOnChanges();
        expect(expanded().length).toBe(1);

        component.search('thing');
        expect(expanded().length).toBe(2);

        component.search('fish');
        expect(expanded().length).toBe(2);

        component.search('I');
        expect(expanded().length).toBe(4);
      });

      it('returns the correct result count', () => {

        component.json = {
          things: [
            'Thing One',
            'Thing Two'
          ]
        };

        component.ngOnChanges();
        expect(component.search('thing')).toBe(3);
      });
    });
  });
});
