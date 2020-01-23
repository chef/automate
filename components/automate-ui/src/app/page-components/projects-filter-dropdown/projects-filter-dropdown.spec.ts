import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { ProjectsFilterDropdownComponent } from './projects-filter-dropdown.component';

describe('ProjectsFilterDropdownComponent', () => {
  let component: ProjectsFilterDropdownComponent;
  let fixture: ComponentFixture<ProjectsFilterDropdownComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
      declarations: [
        ProjectsFilterDropdownComponent
      ],
      imports: [
        FormsModule
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
      expect(label.getAttribute('aria-label')).toEqual('Projects filter');
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
      component.editableOptions = [
        {
          value: 'project-1',
          label: 'Project 1',
          type: 'CUSTOM',
          checked: false
        },
        {
          value: 'project-2',
          label: 'Project 2',
          type: 'CUSTOM',
          checked: true
        }
      ];
      // need filteredOptions in order for elements to be displayed in the UI
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

  describe('resetOptions()', () => {
    beforeEach(() => {
      component.options = [
        {
          value: 'project-1',
          label: 'Project 1',
          type: 'CUSTOM',
          checked: false
        },
        {
          value: 'project-2',
          label: 'Project 2',
          type: 'CUSTOM',
          checked: true
        }
      ];
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
        component.editableOptions = [
          {
            value: 'project-1',
            label: 'Project 1',
            type: 'CUSTOM',
            checked: false
          },
          {
            value: 'project-2',
            label: 'Project 2',
            type: 'CUSTOM',
            checked: false
          }
        ];
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
        component.editableOptions = [
          {
            value: 'project-1',
            label: 'Project 1',
            type: 'CUSTOM',
            checked: false
          }
        ];
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

  describe('handleOptionChange()', () => {
    beforeEach(() => {
      component.editableOptions = [
        {
          value: 'project-1',
          label: 'Project 1',
          type: 'CUSTOM',
          checked: false
        }
      ];
      component.optionsEdited = false;
      component.handleOptionChange({ detail: true }, 'Project 1');
    });

    it('updates the value of `checked` to the emitted value', () => {
      expect(component.editableOptions[0].checked).toEqual(true);
    });

    it('marks the list of options as edited', () => {
      expect(component.optionsEdited).toEqual(true);
    });
  });

  describe('handleApplySelection()', () => {
    beforeEach(() => {
      spyOn(component.onSelection, 'emit');
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
});
