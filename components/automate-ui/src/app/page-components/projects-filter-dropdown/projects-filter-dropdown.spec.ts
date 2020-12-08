import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';

import { using } from 'app/testing/spec-helpers';
import { ProjectsFilterOption } from 'app/services/projects-filter/projects-filter.reducer';
import { ProjectsFilterDropdownComponent } from './projects-filter-dropdown.component';

describe('ProjectsFilterDropdownComponent', () => {
  let component: ProjectsFilterDropdownComponent;
  let fixture: ComponentFixture<ProjectsFilterDropdownComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
      declarations: [
        ProjectsFilterDropdownComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectsFilterDropdownComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  describe('dropdown label', () => {
    let label, count;

    beforeEach(() => {
      component.selectionLabel = 'Multiple projects';
      component.selectionCount = 3;
      component.selectionCountVisible = true;
      component.selectionCountActive = true;
      component.filterVisible = true;
      fixture.detectChanges();
    });

    it('displays label text', () => {
      label = fixture.nativeElement.querySelector('.dropdown-label');
      expect(label).not.toBeNull();
      expect(label.textContent).toContain(component.selectionLabel);
    });

    it('has an "aria-label"', () => {
      label = fixture.nativeElement.querySelector('.dropdown-label');
      expect(label.getAttribute('aria-label')).toEqual('Filter projects');
    });

    describe('when .selectionCountVisible is false', () => {
      beforeEach(() => {
        component.selectionCountVisible = false;
        fixture.detectChanges();
      });

      it('the selection count is hidden', () => {
        count = fixture.nativeElement.querySelector('.selection-count');
        expect(count).toBeNull();
      });
    });

    describe('when .selectionCountVisible is true', () => {
      beforeEach(() => {
        component.selectionCountVisible = true;
        fixture.detectChanges();
      });

      it('the selection count is visible', () => {
        count = fixture.nativeElement.querySelector('.selection-count');
        expect(count).not.toBeNull();
        expect(count.textContent).toEqual(component.selectionCount.toString());
      });
    });

    describe('when .selectionCountActive is false', () => {
      beforeEach(() => {
        component.selectionCountActive = false;
        fixture.detectChanges();
      });

      it('the selection count does not have an "active" className', () => {
        count = fixture.nativeElement.querySelector('.selection-count');
        expect(count.classList.contains('active')).toEqual(false);
      });
    });

    describe('when .selectionCountActive is true', () => {
      beforeEach(() => {
        component.selectionCountActive = true;
        fixture.detectChanges();
      });

      it('the selection count has an "active" className', () => {
        count = fixture.nativeElement.querySelector('.selection-count');
        expect(count.classList.contains('active')).toEqual(true);
      });
    });
  });

  describe('dropdown', () => {
    beforeEach(() => {
      component.dropdownActive = true;
      component.editableOptions = genOptions([false, true]);
      component.filteredOptions = component.editableOptions;
      fixture.detectChanges();
    });

    it('displays a list of checkbox options', () => {
      const options = Array.from(fixture.nativeElement.querySelectorAll('chef-checkbox'));
      expect(options.length).toEqual(2);
      options.forEach((option: HTMLInputElement, index: number) => {
        const { label, checked } = component.editableOptions[index];
        expect(option.textContent).toEqual(label);
        expect(option.checked).toEqual(checked);
      });
    });

    it('displays an "Apply Changes" button', () => {
      const button = fixture.nativeElement.querySelector('#projects-filter-apply-changes');
      expect(button).not.toBeNull();
      expect(button.textContent).toEqual('Apply Changes');
      expect(button.hasAttribute('disabled')).toEqual(true);
    });

    describe('when .dropdownActive is false', () => {
      beforeEach(() => {
        component.dropdownActive = false;
        fixture.detectChanges();
      });

      it('the dropdown is hidden', () => {
        const dropdown = fixture.nativeElement.querySelector('.dropdown');
        expect(dropdown).toBeNull();
      });
    });

    describe('when .dropdownActive is true', () => {
      beforeEach(() => {
        component.dropdownActive = true;
        fixture.detectChanges();
      });

      it('the dropdown is visible', () => {
        const dropdown = fixture.nativeElement.querySelector('.dropdown');
        expect(dropdown).not.toBeNull();
      });
    });

    describe('when .optionsEdited is false', () => {
      beforeEach(() => {
        component.dropdownActive = true;
        component.optionsEdited = false;
        fixture.detectChanges();
      });

      it('the "Apply" button is disabled', () => {
        const button = fixture.nativeElement.querySelector('#projects-filter-apply-changes');
        expect(button.getAttribute('disabled')).toEqual('true');
      });
    });

    describe('when .optionsEdited is true', () => {
      beforeEach(() => {
        component.dropdownActive = true;
        component.optionsEdited = true;
        fixture.detectChanges();
      });

      it('the "Apply" button is enabled', () => {
        const button = fixture.nativeElement.querySelector('#projects-filter-apply-changes');
        expect(button.getAttribute('disabled')).toEqual('false');
      });
    });
  });

  describe('The applyButton is enabled when', () => {
    beforeEach(() => {
      component.optionsEdited = false;
      fixture.detectChanges();
    });

    describe('with no projects checked...', () => {
      beforeEach(() => {
        component.editableOptions = genOptions([false, false, false, false, false]);
        component.filteredOptions = component.editableOptions;
        component.initialFilters = component.filteredOptions.map(option => option.checked);
      });

      it('we check one project', () => {
        component.handleOptionChange({ detail: true }, 'Project 1');
        expect(component.optionsEdited).toBe(true);
      });

      it('we check multiple projects', () => {
        component.handleOptionChange({ detail: true }, 'Project 1');
        component.handleOptionChange({ detail: true }, 'Project 2');
        component.handleOptionChange({ detail: true }, 'Project 4');

        expect(component.optionsEdited).toBe(true);
      });

      it('we check all projects', () => {
        component.editableOptions.forEach(
          (_o, index) => component.handleOptionChange({ detail: true }, `Project ${index + 1}`));

        expect(component.optionsEdited).toBe(true);
      });
    });

    describe('with some projects checked...', () => {
      beforeEach(() => {
        component.editableOptions = genOptions([false, true, true, false, false]);
        component.filteredOptions = component.editableOptions;
        component.initialFilters = component.filteredOptions.map(option => option.checked);
      });

      it('we check one other project', () => {
        component.handleOptionChange({ detail: true }, 'Project 1');
        expect(component.optionsEdited).toBe(true);
      });

      it('we check multiple other projects', () => {
        component.handleOptionChange({ detail: true }, 'Project 1');
        component.handleOptionChange({ detail: true }, 'Project 5');

        expect(component.optionsEdited).toBe(true);
      });

      it('we check all other projects', () => {
        component.handleOptionChange({ detail: true }, 'Project 1');
        component.handleOptionChange({ detail: true }, 'Project 4');
        component.handleOptionChange({ detail: true }, 'Project 5');

        expect(component.optionsEdited).toBe(true);
      });

      it('we uncheck one other project', () => {
        component.handleOptionChange({ detail: false }, 'Project 2');

        expect(component.optionsEdited).toBe(true);
      });

      it('we uncheck all projects', () => {
        component.editableOptions.forEach(
          (_o, index) => component.handleOptionChange({ detail: false }, `Project ${index + 1}`));

        expect(component.optionsEdited).toBe(true);
      });
    });

    describe('with all projects checked...', () => {
      beforeEach(() => {
        component.editableOptions = genOptions([true, true, true, true, true]);
        component.filteredOptions = component.editableOptions;
        component.initialFilters = component.filteredOptions.map(option => option.checked);
      });

      it('we uncheck one project', () => {
        component.handleOptionChange({ detail: false }, 'Project 1');
        expect(component.optionsEdited).toBe(true);
      });

      it('we uncheck multiple projects', () => {
        component.handleOptionChange({ detail: false }, 'Project 1');
        component.handleOptionChange({ detail: false }, 'Project 2');
        component.handleOptionChange({ detail: false }, 'Project 4');

        expect(component.optionsEdited).toBe(true);
      });

      it('we uncheck all projects', () => {
        component.editableOptions.forEach(
          (_o, index) => component.handleOptionChange({ detail: false }, `Project ${index + 1}`));

        expect(component.optionsEdited).toBe(true);
      });
    });
  });

  describe('The applyButton is disabled when', () => {
    beforeEach(() => {
      component.optionsEdited = false;
      fixture.detectChanges();
    });

    describe('with no projects checked...', () => {
      beforeEach(() => {
        component.editableOptions = genOptions([false, false, false, false, false]);
        component.filteredOptions = component.editableOptions;
        component.initialFilters = component.filteredOptions.map(option => option.checked);
      });

      it('we check then uncheck one project', () => {
        component.handleOptionChange({ detail: true }, 'Project 1');
        expect(component.optionsEdited).toBe(true);

        component.handleOptionChange({ detail: false }, 'Project 1');
        expect(component.optionsEdited).toBe(false);
      });

      it('we check then uncheck multiple projects', () => {
        component.handleOptionChange({ detail: true }, 'Project 1');
        component.handleOptionChange({ detail: true }, 'Project 2');
        component.handleOptionChange({ detail: true }, 'Project 4');
        expect(component.optionsEdited).toBe(true);

        component.handleOptionChange({ detail: false }, 'Project 1');
        component.handleOptionChange({ detail: false }, 'Project 2');
        component.handleOptionChange({ detail: false }, 'Project 4');
        expect(component.optionsEdited).toBe(false);
      });

      it('we check then uncheck all projects', () => {
        component.editableOptions.forEach(
          (_o, index) => component.handleOptionChange({ detail: true }, `Project ${index + 1}`));
        expect(component.optionsEdited).toBe(true);

        component.editableOptions.forEach(
          (_o, index) => component.handleOptionChange({ detail: false }, `Project ${index + 1}`));
        expect(component.optionsEdited).toBe(false);
      });
    });

    describe('with some projects checked...', () => {
      beforeEach(() => {
        component.editableOptions = genOptions([false, true, true, false, false]);
        component.filteredOptions = component.editableOptions;
        component.initialFilters = component.filteredOptions.map(option => option.checked);
      });

      it('we check then uncheck one other project', () => {
        component.handleOptionChange({ detail: true }, 'Project 1');
        expect(component.optionsEdited).toBe(true);

        component.handleOptionChange({ detail: false }, 'Project 1');
        expect(component.optionsEdited).toBe(false);
      });

      it('we check and uncheck, then recheck multiple other projects', () => {
        component.handleOptionChange({ detail: true }, 'Project 1');
        component.handleOptionChange({ detail: false }, 'Project 2');
        component.handleOptionChange({ detail: false }, 'Project 3');
        expect(component.optionsEdited).toBe(true);

        component.handleOptionChange({ detail: false }, 'Project 1');
        component.handleOptionChange({ detail: true }, 'Project 2');
        component.handleOptionChange({ detail: true }, 'Project 3');
        expect(component.optionsEdited).toBe(false);
      });

      it('check then uncheck all other projects', () => {
        component.handleOptionChange({ detail: true }, 'Project 1');
        component.handleOptionChange({ detail: true }, 'Project 4');
        component.handleOptionChange({ detail: true }, 'Project 5');
        expect(component.optionsEdited).toBe(true);

        component.handleOptionChange({ detail: false }, 'Project 1');
        component.handleOptionChange({ detail: false }, 'Project 4');
        component.handleOptionChange({ detail: false }, 'Project 5');
        expect(component.optionsEdited).toBe(false);
      });
    });

    describe('with all projects checked...', () => {
      beforeEach(() => {
        component.editableOptions = genOptions([true, true, true, true, true]);
        component.filteredOptions = component.editableOptions;
        component.initialFilters = component.filteredOptions.map(option => option.checked);
      });

      it('we uncheck then check one project', () => {
        component.handleOptionChange({ detail: false }, 'Project 1');
        expect(component.optionsEdited).toBe(true);

        component.handleOptionChange({ detail: true }, 'Project 1');
        expect(component.optionsEdited).toBe(false);
      });

      it('we uncheck then check multiple projects', () => {
        component.handleOptionChange({ detail: false }, 'Project 1');
        component.handleOptionChange({ detail: false }, 'Project 4');
        component.handleOptionChange({ detail: false }, 'Project 5');
        expect(component.optionsEdited).toBe(true);

        component.handleOptionChange({ detail: true }, 'Project 1');
        component.handleOptionChange({ detail: true }, 'Project 4');
        component.handleOptionChange({ detail: true }, 'Project 5');
        expect(component.optionsEdited).toBe(false);
      });

      it('we uncheck then recheck all projects', () => {
        component.editableOptions.forEach(
          (_o, index) => component.handleOptionChange({ detail: false }, `Project ${index + 1}`));
        expect(component.optionsEdited).toBe(true);

        component.editableOptions.forEach(
          (_o, index) => component.handleOptionChange({ detail: true }, `Project ${index + 1}`));
        expect(component.optionsEdited).toBe(false);
      });
    });

    describe('when options have filters applied', () => {
      beforeEach(() => {
        component.dropdownActive = true;
        component.options = genOptionsWithId([
          ['proj-one', true],
          ['proj-three', false],
          ['other-one', false],
          ['other-two', true],
          ['proj-two', false],
          ['other-three', false],
          ['proj-four', false]
        ]);
        component.filteredOptions = component.editableOptions;
        component.initialFilters = component.filteredOptions.map(option => option.checked);
        component.resetOptions();
      });

      it('apply button is disabled with no filters and no changes', () => {
        expect(component.optionsEdited).toBe(false);
      });

      it('apply button is active with one new checked but filtered out option', () => {
        component.handleOptionChange({ detail: true }, 'other-one');
        component.handleFilterKeyUp('proj');
        expect(component.optionsEdited).toBe(true);
      });

      it('apply button is disabled with a filter, then checked and unchecked option', () => {
        component.handleFilterKeyUp('proj');
        component.handleOptionChange({ detail: true }, 'proj-two');
        expect(component.optionsEdited).toBe(true);

        component.handleOptionChange({ detail: false }, 'proj-two');
        expect(component.optionsEdited).toBe(false);
      });

      it('apply button enabled when selection is' +
            ' cleared while filtered and there are changes', () => {
          component.handleFilterKeyUp('proj'); // this leaves selected options hidden
          component.handleClearSelection();
          expect(component.optionsEdited).toBe(true);
      });
    });
  });

  describe('resetOptions()', () => {
    beforeEach(() => {
      component.options = genOptions([false, true]);
      component.editableOptions = [];
      component.optionsEdited = true;

      component.resetOptions();
    });

    it('resets any previous selection changes by copying the provided options', () => {
      component.optionsEdited = false;
      component.resetOptions();

      expect(component.editableOptions).not.toEqual([]);
      expect(component.editableOptions).not.toBe(component.options);
      expect(component.editableOptions).toEqual(component.options);
    });
  });

  describe('handleLabelClick()', () => {
    describe('when more than one option is available', () => {
      beforeEach(() => {
        component.editableOptions = genOptions([false, false]);
        component.dropdownActive = false;
        spyOn(component, 'resetOptions');

        component.handleLabelClick();
      });

      it('toggles dropdown visibility', () => {
        expect(component.dropdownActive).toEqual(true);
      });

      it('resets any previous selection changes', () => {
        expect(component.resetOptions).toHaveBeenCalled();
      });
    });

    describe('when only one option is available', () => {
      beforeEach(() => {
        component.editableOptions = genOptions([false]);
        component.dropdownActive = false;
        spyOn(component, 'resetOptions');

        component.handleLabelClick();
      });

      it('only resets options', () => {
        expect(component.resetOptions).toHaveBeenCalled();
        expect(component.dropdownActive).toEqual(false);
      });
    });
  });

  describe('handleEscape()', () => {
    beforeEach(() => {
      spyOn(component, 'resetOptions');
      component.dropdownActive = true;
      component.handleEscape();
    });

    it('hides the dropdown', () => {
      expect(component.dropdownActive).toEqual(false);
    });

    it('resets any previous selection changes', () => {
      expect(component.resetOptions).toHaveBeenCalled();
    });
  });

  describe('handleOptionChange() for single option', () => {
    beforeEach(() => {
      component.editableOptions = genOptions([false]);
      component.optionsEdited = false;
      expect(component.editableOptions[0].checked).toEqual(false);
      component.handleOptionChange({ detail: true }, 'Project 1');
    });

    it('updates the value of `checked` to the emitted value', () => {
      expect(component.editableOptions[0].checked).toEqual(true);
    });

    it('marks the list of options as edited', () => {
      expect(component.optionsEdited).toEqual(true);
    });
  });

  describe('handleOptionChange() for multiple options', () => {
    beforeEach(() => {
      component.editableOptions = genOptions([false, false, true, true, false]);
      component.optionsEdited = false;
    });

    it('updates an unchecked value to the emitted value', () => {
      expect(component.editableOptions[1].checked).toEqual(false);
      component.handleOptionChange({ detail: true }, 'Project 2');
      expect(component.editableOptions[1].checked).toEqual(true);
    });

     it('updates a checked value to the emitted value', () => {
      expect(component.editableOptions[2].checked).toEqual(true);
      component.handleOptionChange({ detail: false }, 'Project 3');
      expect(component.editableOptions[2].checked).toEqual(false);
    });

    it('marks the list of options as edited', () => {
      component.handleOptionChange({ detail: true }, 'Project 1');
      expect(component.optionsEdited).toEqual(true);
    });

    it('checks for equality against the initial filters applied', () => {
      spyOn(component, 'checkInitialEquality');
      component.handleOptionChange({ detail: true }, 'Project 1');
      expect(component.checkInitialEquality).toHaveBeenCalled();
    });
  });

  describe('handleApplySelection()', () => {
    beforeEach(() => {
      spyOn(component.onSelection, 'emit');
      spyOn(component.onOptionChange, 'emit');
      component.dropdownActive = true;
      component.optionsEdited = true;
      component.handleApplySelection();
    });

    it('hides the dropdown', () => {
      expect(component.dropdownActive).toEqual(false);
    });

    it('disables the "Apply Changes" button', () => {
      expect(component.optionsEdited).toEqual(false);
    });

    it('emits "onSelection" event with list of updated options', () => {
      expect(component.onSelection.emit).toHaveBeenCalledWith(component.editableOptions);
    });

    it('emits "onOptionChange" event with list of updated options', () => {
      expect(component.onOptionChange.emit).toHaveBeenCalledWith(component.editableOptions);
    });
  });

  describe('handleClearSelection()', () => {
    beforeEach(() => {
      spyOn(component.onSelection, 'emit');
      spyOn(component.onOptionChange, 'emit');
      component.dropdownActive = true;
      component.optionsEdited = false;
      component.options = genOptions([false, true, true, false, true]);
      component.resetOptions();
      expect(component.editableOptions.some(o => o.checked)).toEqual(true);
    });

    // Note: most of these would be phantom tests (see https://bit.ly/2UPrprX)
    // except for the fact presence of the tests for handleApplySelection above.
    it('does not hide the dropdown', () => {
      component.handleClearSelection();
      expect(component.dropdownActive).toEqual(true);
    });

    it('enables the "Apply Changes" button', () => {
      component.handleClearSelection();
      expect(component.optionsEdited).toEqual(true);
    });

    it('hides the "Clear Selection" button', () => {
      fixture.detectChanges();
      let button: HTMLElement = fixture.nativeElement.querySelector('#projects-filter-clear-selection');
      expect(button.classList.contains('active')).toEqual(true);

      component.handleClearSelection();
      fixture.detectChanges();

      button = fixture.nativeElement.querySelector('#projects-filter-clear-selection');
      expect(button.classList.contains('active')).toEqual(false);
    });

    it('does not emit "onSelection" event', () => {
      component.handleClearSelection();
      expect(component.onSelection.emit).not.toHaveBeenCalled();
    });

    it('does not emit "onOptionChange" event', () => {
      component.handleClearSelection();
      expect(component.onOptionChange.emit).not.toHaveBeenCalled();
    });

    it('clears all checked options with no filter applied', () => {
      component.handleClearSelection();
      expect(component.editableOptions.some(o => o.checked)).toEqual(false);
    });

    it('checks for equality against the initial filters applied', () => {
      spyOn(component, 'checkInitialEquality');
      component.handleClearSelection();
      expect(component.checkInitialEquality).toHaveBeenCalled();
    });

    using([
      ['matching some', 'proj', 4, 3], // cleared 4 selected by filter; 3 out of 7 remain
      ['no filter', '', 7, 0],
      ['matching all', '-', 7, 0],
      ['matching none', 'non-match', 0, 7]
    ], function (
        description: string,
        filter: string,
        resultCheckedWithFilterBeforeClearing: number,
      resultCheckedAfterRemovingFilter: number) {
        it(`when filtered with ${description}, clears only filtered items`, () => {
          component.options = genOptionsWithId([
            ['proj-one', true],
            ['proj-three', true],
            ['other-one', true],
            ['other-two', true],
            ['proj-two', true],
            ['other-three', true],
            ['proj-four', true]
          ]);
          component.resetOptions();
          component.handleFilterKeyUp(filter);
          expect(component.filteredSelectedCount)
            .toEqual(resultCheckedWithFilterBeforeClearing.toString());

          component.handleClearSelection();
          expect(component.filteredSelectedCount).toEqual('0');

          component.handleFilterKeyUp('');
          expect(component.filteredSelectedCount)
            .toEqual(resultCheckedAfterRemovingFilter.toString());
        });
    });

  });

  describe('handleArrowUp()', () => {
    let event, previousElementSibling;

    beforeEach(() => {
      previousElementSibling = { focus: jasmine.createSpy('focus') };
      event = {
        preventDefault: jasmine.createSpy('preventDefault'),
        target: { previousElementSibling }
      };
      component.handleArrowUp(event);
    });

    it('prevents the default event behavior', () => {
      expect(event.preventDefault).toHaveBeenCalled();
    });

    it('focuses the previous element sibling', () => {
      expect(previousElementSibling.focus).toHaveBeenCalled();
    });
  });

  describe('handleArrowDown()', () => {
    let event, nextElementSibling;

    beforeEach(() => {
      nextElementSibling = { focus: jasmine.createSpy('focus') };
      event = {
        preventDefault: jasmine.createSpy('preventDefault'),
        target: { nextElementSibling }
      };
      component.handleArrowDown(event);
    });

    it('prevents the default event behavior', () => {
      expect(event.preventDefault).toHaveBeenCalled();
    });

    it('focuses the next element sibling', () => {
      expect(nextElementSibling.focus).toHaveBeenCalled();
    });
  });

  describe('filteredOptions', () => {
    beforeEach(() => {
      component.dropdownActive = true;
      component.options = genOptionsWithId([
        ['proj-one', true],
        ['proj-three', false],
        ['other-one', false],
        ['other-two', true],
        ['proj-two', false],
        ['other-three', false],
        ['proj-four', false]
      ]);
      component.resetOptions();
    });

    it('with no filter displays all options', () => {
      expect(component.filteredOptions.length).toEqual(7);
    });

    using([
      ['prefix', 'proj', 4],
      ['mid-value', 'her', 3],
      ['suffix', 'two', 2],
      ['empty string', '', 7],
      ['whitespace', ' ', 0],
      ['exact match', 'proj-one', 1],
      ['superset', 'proj-one-plus-one', 0],
      ['everything', '-', 7]
    ], function (description: string, filter: string, count: number) {
      it(`with ${description} filter displays ${count} matching options`, () => {
        component.handleFilterKeyUp(filter);
        expect(component.filteredOptions.length).toEqual(count);
      });
    });
  });

  describe('filteredSelectedCount', () => {
    beforeEach(() => {
      component.dropdownActive = true;
    });
    describe('with no filters applied', () => {
      it('reports none selected with no projects', () => {
        component.editableOptions = genOptions([]);
        component.filteredOptions = component.editableOptions;
        expect(component.filteredSelectedCount).toEqual('0');
      });

      it('reports one selected with one project, checked', () => {
        component.editableOptions = genOptions([true]);
        component.filteredOptions = component.editableOptions;
        expect(component.filteredSelectedCount).toEqual('1');
      });

      it('reports none selected with one project, unchecked', () => {
        component.editableOptions = genOptions([false]);
        component.filteredOptions = component.editableOptions;
        expect(component.filteredSelectedCount).toEqual('0');
      });

      it('reports none selected with multiple projects, none checked', () => {
        component.editableOptions = genOptions([false, false, false]);
        component.filteredOptions = component.editableOptions;
        expect(component.filteredSelectedCount).toEqual('0');
      });

      it('reports two selected with multiple projects, two checked', () => {
        component.editableOptions = genOptions([false, true, true]);
        component.filteredOptions = component.editableOptions;
        expect(component.filteredSelectedCount).toEqual('2');
      });

      using([
        ['at threshold', 99, '99'],
        ['one above threshold', 100, '99+'],
        ['well above threshold', 150, '99+']
      ], function (description: string, count: number, result: string) {
        it(`${description} ($count projects checked) reports ${result}`, () => {
          component.editableOptions = genManyOptions(count);
          component.filteredOptions = component.editableOptions;
          expect(component.filteredSelectedCount).toEqual(result);
        });
      });
    });

    describe('with filters', () => {

      // test all combinations of the two inputs (checked and matched) for single project
      using([
        [true, 'match', 1, 1],
        [false, 'match', 0, 0],
        [true, 'non-match', 1, 0],
        [false, 'non-match', 0, 0]
      ], function (checked: boolean, filter: string, beforeCount: number, afterCount: number) {
        it(`matched (${'match'.includes(filter)}) and a single project checked (${checked}),`
          + ` reports count of ${afterCount}`, () => {
            component.options = genOptionsWithId([
              ['match', checked]
            ]);
            component.resetOptions();
            expect(component.filteredSelectedCount).toEqual(beforeCount.toString());

            component.handleFilterKeyUp(filter);

            expect(component.filteredSelectedCount).toEqual(afterCount.toString());
          });
      });

      using([
        ['no filter', '', 3],
        ['matching all', '-', 3],
        ['matching some', 'proj', 2],
        ['matching some with none checked', '-three', 0],
        ['matching none', 'non-match', 0]
      ], function (description: string, filter: string, afterCount: number) {
        it(`and multiple projects, filter ${description} reports correct count`, () => {
          component.options = genOptionsWithId([
            ['proj-one', true],
            ['proj-three', false],
            ['other-one', false],
            ['other-two', true],
            ['proj-two', false],
            ['other-three', false],
            ['proj-four', true]
          ]);
          component.resetOptions();

          component.handleFilterKeyUp(filter);

          expect(component.filteredSelectedCount).toEqual(afterCount.toString());
        });
      });
    });

  });

  describe('"Clear Selection" button', () => {
    beforeEach(() => {
      component.dropdownActive = true;
    });
    it('becomes visible after no projects checked and then checking one project', () => {
      component.options = genOptionsWithId([
        ['proj-one', false],
        ['proj-two', false],
        ['proj-three', false]
      ]);
      component.resetOptions();
      fixture.detectChanges();
      expect(hasClass('#projects-filter-clear-selection', 'active')).toEqual(false);

      component.handleOptionChange({ detail: true }, 'proj-three');
      fixture.detectChanges();

      expect(hasClass('#projects-filter-clear-selection', 'active')).toEqual(true);
    });

    it('becomes hidden after unchecking the last checked project', () => {
      component.options = genOptionsWithId([
        ['proj-one', true],
        ['proj-two', true],
        ['proj-three', false]
      ]);
      component.resetOptions();
      fixture.detectChanges();
      expect(hasClass('#projects-filter-clear-selection', 'active')).toEqual(true);

      component.handleOptionChange({ detail: false }, 'proj-one');
      fixture.detectChanges();
      expect(hasClass('#projects-filter-clear-selection', 'active')).toEqual(true);

      component.handleOptionChange({ detail: false }, 'proj-two');
      fixture.detectChanges();

      expect(hasClass('#projects-filter-clear-selection', 'active')).toEqual(false);
    });

    it('becomes visible after loosening filter to reveal some checked projects', () => {
      component.options = genOptionsWithId([
        ['proj-one', true],
        ['filtered-one', false],
        ['proj-two', true],
        ['filtered-two', false],
        ['proj-three', false]
      ]);
      component.resetOptions();
      component.handleFilterKeyUp('filtered');
      fixture.detectChanges();
      expect(hasClass('#projects-filter-clear-selection', 'active')).toEqual(false);

      component.handleFilterKeyUp('');
      fixture.detectChanges();
      expect(hasClass('#projects-filter-clear-selection', 'active')).toEqual(true);
    });

    it('becomes hidden after tightening filter to hide all checked projects', () => {
      component.options = genOptionsWithId([
        ['proj-one', true],
        ['filtered-one', false],
        ['proj-two', true],
        ['filtered-two', false],
        ['proj-three', false]
      ]);
      component.resetOptions();
      component.handleFilterKeyUp('proj');
      fixture.detectChanges();
      expect(hasClass('#projects-filter-clear-selection', 'active')).toEqual(true);

      component.handleFilterKeyUp('filtered');
      fixture.detectChanges();
      expect(hasClass('#projects-filter-clear-selection', 'active')).toEqual(false);
      });
  });

  function hasClass(selector: string, cssClass: string): boolean {
    const element: HTMLElement = fixture.nativeElement.querySelector(selector);
    return element.classList.contains(cssClass);
  }

});

function genOptions(checkedItems: boolean[]): ProjectsFilterOption[] {
  const options: ProjectsFilterOption[] = [];
  for (let i = 0; i < checkedItems.length; i++) {
    options.push(
      {
        value: `project-${i + 1}`,
        label: `Project ${i + 1}`,
        type: 'CUSTOM',
        checked: checkedItems[i]
      });
  }
  return options;
}

function genManyOptions(count: number): ProjectsFilterOption[] {
  const options: ProjectsFilterOption[] = [];
  for (let i = 0; i < count; i++) {
    options.push(
      {
        value: `project-${i + 1}`,
        label: `Project ${i + 1}`,
        type: 'CUSTOM',
        checked: true
      });
  }
  return options;
}

function genOptionsWithId(checkedItems: [string, boolean][]): ProjectsFilterOption[] {
  const options: ProjectsFilterOption[] = [];
  checkedItems.forEach(([id, checked]) =>
    options.push({
        value: id,
        label: id,
        type: 'CUSTOM',
        checked
    })
  );
  return options;
}
