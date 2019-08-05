import { TestBed } from '@angular/core/testing';
import { SearchBarComponent } from './search-bar.component';
import { MockComponent } from 'ng2-mock-component';
import { List } from 'immutable';
import { Chicklet } from '../../types/types';
import { SimpleChange } from '@angular/core';

describe('SearchBarComponent', () => {
  let component: SearchBarComponent;
  let fixture;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        SearchBarComponent,
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-button' })
      ]
    });

    fixture = TestBed.createComponent(SearchBarComponent);
    component = fixture.componentInstance;
    component.categories = [
      { // 0
        type: 'attribute',
        text: 'Attribute'
      },
      { // 1
        type: 'cookbook',
        text: 'Cookbook'
      },
      { // 2
        type: 'environment',
        text: 'Environment'
      },
      { // 3
        type: 'name',
        text: 'Node Name',
        allowWildcards: true
      },
      { // 4
        type: 'platform',
        text: 'Platform'
      },
      { // 5
        type: 'policy_group',
        text: 'Policy Group'
      },
      { // 6
        type: 'policy_name',
        text: 'Policy Name'
      },
      { // 7
        type: 'policy_revision',
        text: 'Policy Revision'
      },
      { // 8
        type: 'recipe',
        text: 'Recipe'
      },
      { // 9
        type: 'resource_name',
        text: 'Resource Name'
      },
      { // 10
        type: 'role',
        text: 'Role'
      },
      { // 11
        type: 'event_type',
        text: 'Event Type',
        providedValues: [
          {name: 'client', title: 'Clients', icon: 'assignment_ind'},
          {name: 'cookbook', title: 'Cookbooks', icon: 'chrome_reader_mode'},
          {name: 'bag', title: 'Data Bags', icon: 'business_center'}
        ]
      }
    ];
    component.visibleCategories = List<Chicklet>(component.categories);
  });

  it('when clicking the category name, the selected category is removed', () => {
    component.selectedCategoryType = component.categories[0];

    component.handleCategoryClick();

    // check that the category is deselected
    expect(component.inputField.nativeElement.value).toEqual('');
    expect(component.selectedCategoryType).toEqual(undefined);
  });

  describe('when pressing arrowdown', () => {
    it('no category is selected, no input text, and nothing is highlighted: ' +
    'highlight the first category', () => {
      component.highlightedIndex = -1;

      component.handleInput('arrowdown', '');

      expect(component.highlightedIndex).toEqual(0);
      expect(component.suggestionsVisible).toEqual(true);
    });

    it('no category is selected, no input text, and the last category is highlighted: ' +
    'do not change the highlight', () => {
      component.highlightedIndex = component.visibleCategories.size - 1;

      component.handleInput('arrowdown', '');

      expect(component.highlightedIndex).toEqual(component.visibleCategories.size - 1);
      expect(component.suggestionsVisible).toEqual(true);
    });

    it('a category is selected, 3 suggestions, and none of the suggestions are highlighted: ' +
    'highlight the first suggestion', () => {
      component.selectedCategoryType = { type: 'attribute', text: 'Attribute' };
      component.dynamicSuggestions = ['1', '2', '3'];
      component.ngOnChanges({
        dynamicSuggestions: new SimpleChange(null, component.dynamicSuggestions, false)
      });
      fixture.detectChanges();

      component.highlightedIndex = -1;
      component.handleInput('arrowdown', '');

      expect(component.highlightedIndex).toEqual(0);
      expect(component.suggestionsVisible).toEqual(true);
    });

    it('a category is selected, 3 suggestions, and the second suggestions is highlighted: ' +
    'highlight the third suggestion', () => {
      component.selectedCategoryType = { type: 'attribute', text: 'Attribute' };
      component.highlightedIndex = 1;
      component.dynamicSuggestions = ['1', '2', '3'];
      component.ngOnChanges({
        dynamicSuggestions: new SimpleChange(null, component.dynamicSuggestions, false)
      });
      fixture.detectChanges();

      component.highlightedIndex = 1;
      component.handleInput('arrowdown', '');

      expect(component.highlightedIndex).toEqual(2);
      expect(component.suggestionsVisible).toEqual(true);
    });

    it('a category is selected, 3 suggestions, and the last suggestions is highlighted: ' +
    'do not change the highlight', () => {
      component.selectedCategoryType = { type: 'attribute', text: 'Attribute' };
      component.dynamicSuggestions = ['1', '2', '3'];
      component.ngOnChanges({
        dynamicSuggestions: new SimpleChange(null, component.dynamicSuggestions, false)
      });
      fixture.detectChanges();

      component.highlightedIndex = 2;
      component.handleInput('arrowdown', '');

      expect(component.highlightedIndex).toEqual(2);
      expect(component.suggestionsVisible).toEqual(true);
    });
  });

  describe('when pressing arrowup', () => {
    it('no category is selected, no input text, and nothing is highlighted: ' +
    'do not change the highlight', () => {
      component.highlightedIndex = -1;

      component.handleInput('arrowup', '');

      expect(component.highlightedIndex).toEqual(-1);
      expect(component.suggestionsVisible).toEqual(true);
    });

    it('no category is selected, no input text, and the last category is highlighted: ' +
    'highlight the last category', () => {
      component.highlightedIndex = component.visibleCategories.size - 1;

      component.handleInput('arrowup', '');

      expect(component.highlightedIndex).toEqual(component.visibleCategories.size - 2);
      expect(component.suggestionsVisible).toEqual(true);
    });

    it('a category is selected, 3 suggestions, and the last suggestions is highlighted: ' +
    'highlight the previous suggestion', () => {
      component.selectedCategoryType = { type: 'attribute', text: 'Attribute' };
      component.dynamicSuggestions = ['1', '2', '3'];
      component.ngOnChanges({
        dynamicSuggestions: new SimpleChange(null, component.dynamicSuggestions, false)
      });
      fixture.detectChanges();

      component.highlightedIndex = component.suggestions.size - 1;

      component.handleInput('arrowup', '');

      expect(component.highlightedIndex).toEqual(component.suggestions.size - 2);
      expect(component.suggestionsVisible).toEqual(true);
    });

    it('a category is selected, 3 suggestions, and the second suggestions is highlighted: ' +
    'highlight the first suggestion', () => {
      component.selectedCategoryType = { type: 'attribute', text: 'Attribute' };
      component.highlightedIndex = 1;
      component.dynamicSuggestions = ['1', '2', '3'];

      component.ngOnChanges({
        dynamicSuggestions: new SimpleChange(null, component.dynamicSuggestions, false)
      });
      fixture.detectChanges();

      component.handleInput('arrowup', '');

      expect(component.highlightedIndex).toEqual(0);
      expect(component.suggestionsVisible).toEqual(true);
    });

    it('a category is selected, 3 suggestions, and none of the suggestions are highlighted: ' +
    'do not change the highlight', () => {
      component.selectedCategoryType = { type: 'attribute', text: 'Attribute' };
      component.highlightedIndex = -1;
      component.dynamicSuggestions = ['1', '2', '3'];

      component.handleInput('arrowup', '');

      expect(component.highlightedIndex).toEqual(-1);
      expect(component.suggestionsVisible).toEqual(true);
    });
  });

  it('when pressing escape the dropdown is hidden', () => {
    component.suggestionsVisible = true;

    component.handleInput('escape', '');

    expect(component.suggestionsVisible).toEqual(false);
  });

  it('when text is typed the dropdown is displayed', () => {
    component.suggestionsVisible = false;

    component.handleInput('a', 'boba');

    expect(component.suggestionsVisible).toEqual(true);
  });

  describe('when a category is selected', () => {
    it('when backspace is pressed and the input text empty ' +
    'remove the selected category', () => {
      // Send a backspace key event when the search text is empty
      component.inputText = '';
      component.selectedCategoryType = component.categories[0];

      component.handleInput('backspace', '');

      // check that the category is deselected
      expect(component.inputField.nativeElement.value).toEqual('');
      expect(component.selectedCategoryType).toEqual(undefined);
    });

    describe('characters are typed', () => {
      it('ensure a request for suggestions is made', (done) => {
        component.selectedCategoryType = component.categories[0];

        component.suggestValues.subscribe(({detail: {type: type, text: text}}) => {
          expect(text).toEqual('boba');
          expect(type).toEqual('attribute');
          done();
        });

        component.handleInput('a', 'boba');
      });

      it('the dropdown is displayed', () => {
        component.selectedCategoryType = component.categories[0];
        component.suggestionsVisible = false;

        component.handleInput('a', 'boba');

        expect(component.suggestionsVisible).toEqual(true);
      });
    });

    describe('enter is pressed', () => {
      it('input text is empty, ' +
      'and a suggested item is highlighted: select the highlighted item', (done) => {
        component.selectedCategoryType = component.categories[0];
        component.dynamicSuggestions = ['1', '2', '3'];
        component.highlightedIndex = 1;

        component.ngOnChanges({
          dynamicSuggestions: new SimpleChange(null, component.dynamicSuggestions, false)
        });
        fixture.detectChanges();

        component.itemSelected.subscribe(({detail: {type: type, text: text}}) => {
          expect(text).toEqual('2');
          expect(type).toEqual('attribute');
          done();
        });

        component.handleInput('enter', '');
      });

      it('no items are highlighted, and the input text matching a suggestion is available: ' +
      'select the matching suggestion item', (done) => {
        component.selectedCategoryType = component.categories[0];
        component.dynamicSuggestions = ['1', '2', '3'];
        component.highlightedIndex = -1;

        component.ngOnChanges({
          dynamicSuggestions: new SimpleChange(null, component.dynamicSuggestions, false)
        });
        fixture.detectChanges();

        component.itemSelected.subscribe(({detail: {type: type, text: text}}) => {
          expect(text).toEqual('2');
          expect(type).toEqual('attribute');
          done();
        });

        component.handleInput('enter', '2');
      });

      it('text matching a suggestion is not available: do not select an item', () => {
        jasmine.clock().uninstall();
        jasmine.clock().install();
        component.selectedCategoryType = component.categories[0];
        component.dynamicSuggestions = ['1', '2', 'bbbb'];
        component.highlightedIndex = -1;

        let isItemSelectedCalled = false;

        component.itemSelected.subscribe((_result: any) => {
          fail('Should not select an item');
          isItemSelectedCalled = true;
        });

        component.handleInput('enter', 'bbb');

        jasmine.clock().tick(1000);
        jasmine.clock().uninstall();

        expect(isItemSelectedCalled).toBe(false);
      });

      describe('input text contains wildcards', () => {
        it('no items are highlighted: select the wildcard * item', (done) => {
          component.selectedCategoryType = component.categories[3];
          component.dynamicSuggestions = ['chef-load-1', 'chef-load-2', 'chef-load-3'];
          component.ngOnChanges({
            dynamicSuggestions: new SimpleChange(null, component.dynamicSuggestions, false)
          });
          fixture.detectChanges();
          component.highlightedIndex = -1;

          component.itemSelected.subscribe(({detail: {type: type, text: text}}) => {
            expect(text).toEqual('chef-load-*');
            expect(type).toEqual('name');
            done();
          });

          component.handleInput('enter', 'chef-load-*');
        });

        it('no items are highlighted: select the wildcard ? item', (done) => {
          component.selectedCategoryType = component.categories[3];
          component.dynamicSuggestions = ['chef-1-nodes', 'chef-2-nodes', 'chef-3-nodes'];
          component.ngOnChanges({
            dynamicSuggestions: new SimpleChange(null, component.dynamicSuggestions, false)
          });
          fixture.detectChanges();
          component.highlightedIndex = -1;

          component.itemSelected.subscribe(({detail: {type: type, text: text}}) => {
            expect(text).toEqual('chef-?-nodes');
            expect(type).toEqual('name');
            done();
          });

          component.handleInput('enter', 'chef-?-nodes');
        });

        it('catagory does not allow wildcards', () => {
          jasmine.clock().uninstall();
          jasmine.clock().install();
          component.selectedCategoryType = component.categories[0];
          component.dynamicSuggestions = ['chef-1-nodes', 'chef-2-nodes', 'chef-3-nodes'];
          component.ngOnChanges({
            dynamicSuggestions: new SimpleChange(null, component.dynamicSuggestions, false)
          });
          fixture.detectChanges();
          component.highlightedIndex = -1;

          let isItemSelectedCalled = false;
          component.itemSelected.subscribe((_result: any) => {
            fail('Should not select an item');
            isItemSelectedCalled = true;
          });

          component.handleInput('enter', 'chef-*');

          jasmine.clock().tick(1000);
          jasmine.clock().uninstall();

          expect(isItemSelectedCalled).toBe(false);
        });
      });
    });
  });

  describe('when a category with providedValues is selected', () => {
    it('characters are typed; ensure a request for suggestions is not made', () => {
      jasmine.clock().uninstall();
      jasmine.clock().install();
      component.selectedCategoryType = component.categories[11];
      component.highlightedIndex = -1;

      let isItemSelectedCalled = false;
      component.suggestValues.subscribe( () => {
        fail('Should not select an item');
        isItemSelectedCalled = true;
      });

      component.handleInput('a', 'boba');

      jasmine.clock().tick(1000);
      jasmine.clock().uninstall();

      expect(isItemSelectedCalled).toBe(false);
    });
  });

  describe('when a category is not selected', () => {

    it('selecting a category with static suggestions', () => {
      component.highlightedIndex = 11;
      component.selectedCategoryType = undefined;

      // Set the text to name, which will match three categories
      component.pressEnter('Event Type');

      expect(component.selectedCategoryType.type).toEqual('event_type');

      expect(component.suggestions.size).toEqual(3);
    });

    describe('enter is pressed', () => {
      it('there are more than one visible categories and none are highlighted: ' +
      'do not select a category', () => {
        component.highlightedIndex = -1;
        component.selectedCategoryType = undefined;

        // Set the text to name, which will match three categories
        component.handleInput('e', 'name');

        expect(component.visibleCategories.size).toEqual(3);

        // pressing enter with 'name' in the text field
        component.handleInput('enter', 'name');

        // A category should not be selected.
        expect(component.selectedCategoryType).toEqual(undefined);
      });

      it('there are only one visible categories: select a category', () => {
        component.highlightedIndex = -1;
        component.selectedCategoryType = undefined;

        // Set the text to node, which will match only one category 'Node Name'
        component.handleInput('e', 'node');

        expect(component.visibleCategories.size).toEqual(1);

        // pressing enter with 'node' in the text field
        component.handleInput('enter', 'node');

        // A category should be selected.
        expect(component.selectedCategoryType.type).toEqual('name');
        expect(component.selectedCategoryType.text).toEqual('Node Name');
      });

      it('there are more than one visible categories, and one of the categories is highlighted: ' +
        'select the highlighted category', () => {
        component.selectedCategoryType = undefined;
        component.highlightedIndex = 0;

        component.handleInput('enter', '');

        // A category should be selected.
        expect(component.selectedCategoryType).toEqual({ type: 'attribute', text: 'Attribute'});
      });
    });
  });
});
