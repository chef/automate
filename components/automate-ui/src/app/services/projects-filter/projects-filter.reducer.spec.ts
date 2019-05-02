import { map } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import {
  ProjectsFilterState,
  projectsFilterInitialState,
  projectsFilterReducer,
  ProjectsFilterOption
} from './projects-filter.reducer';
import { LoadOptions, LoadOptionsSuccess } from './projects-filter.actions';

// TODO Get these from reducer.ts
const UNASSIGNED_PROJECT = '(unassigned)';
const UNASSIGNED_PROJECT_LABEL = 'Unassigned resources';
const ALL_RESOURCES_LABEL = 'All resources';
const ALL_PROJECTS_LABEL = 'All projects';
const MULTIPLE_PROJECTS_LABEL = 'Multiple projects';
const BADGE_GREY_STATE = false;
const BADGE_BLUE_STATE = true;

describe('projectsFilterReducer', () => {
  const initialState: ProjectsFilterState = projectsFilterInitialState;

  describe('LOAD_OPTIONS', () => {
    const action = new LoadOptions();

    it('sets status to loading', () => {
      const { optionsLoadingStatus } = projectsFilterReducer(initialState, action);
      expect(optionsLoadingStatus).toEqual(EntityStatus.loading);
    });
  });

  describe('LOAD_OPTIONS_SUCCESS', () => {

    it('sets status to success', () => {
      const payload: ProjectsFilterOption[] = [
        genProject('p1', false)
      ];
      const action = new LoadOptionsSuccess(payload);

      const { optionsLoadingStatus } = projectsFilterReducer(initialState, action);

      expect(optionsLoadingStatus).toEqual(EntityStatus.loadingSuccess);
    });

    it('without unassigned, sorts projects when storing them', () => {
      const payload: ProjectsFilterOption[] = [
        genProject('d-proj', false),
        genProject('b-proj', false),
        genProject('c-proj', false),
        genProject('a-proj', false)
      ];
      const action = new LoadOptionsSuccess(payload);

      const { options } = projectsFilterReducer(initialState, action);

      expect(map('label', options)).toEqual(['a-proj', 'b-proj', 'c-proj', 'd-proj']);
    });

    it('with unassigned, sorts projects except unassigned at bottom when storing them', () => {
      const payload: ProjectsFilterOption[] = [
        genProject('d-proj', false),
        genProject(UNASSIGNED_PROJECT_LABEL, true, UNASSIGNED_PROJECT),
        genProject('b-proj', false),
        genProject('zz-proj', false),
        genProject('a-proj', false)
      ];
      const action = new LoadOptionsSuccess(payload);

      const { options } = projectsFilterReducer(initialState, action);

      expect(map('value', options))
        .toEqual(['a-proj', 'b-proj', 'd-proj', 'zz-proj', UNASSIGNED_PROJECT]);
    });

    describe('with exactly one allowed project', () => {
      const payload: ProjectsFilterOption[] = [
        genProject('zz-proj', false)
      ];
      const action = new LoadOptionsSuccess(payload);

      it('displays project name', () => {
        const { selectionLabel } = projectsFilterReducer(initialState, action);
        expect(selectionLabel).toEqual('zz-proj');
      });

      it('displays no count badge', () => {
        const { selectionCountVisible } = projectsFilterReducer(initialState, action);
        expect(selectionCountVisible).toBe(false);
      });

      it('does not display caret to open dropdown', () => {
        const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
        expect(dropdownCaretVisible).toBe(false);
      });
    });

    describe('with only unassigned resources allowed', () => {
      const payload: ProjectsFilterOption[] = [
        genProject(UNASSIGNED_PROJECT_LABEL, true, UNASSIGNED_PROJECT)
      ];
      const action = new LoadOptionsSuccess(payload);

      it('displays "Unassigned resources"', () => {
        const { selectionLabel } = projectsFilterReducer(initialState, action);
        expect(selectionLabel).toEqual(UNASSIGNED_PROJECT_LABEL);
      });

      it('displays no count badge', () => {
        const { selectionCountVisible } = projectsFilterReducer(initialState, action);
        expect(selectionCountVisible).toBe(false);
      });

      it('does not display caret to open dropdown', () => {
        const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
        expect(dropdownCaretVisible).toBe(false);
      });
    });

    describe('with exactly one project plus unassigned allowed', () => {

      describe('when nothing is selected', () => {
        const payload: ProjectsFilterOption[] = [
          genProject(UNASSIGNED_PROJECT_LABEL, false, UNASSIGNED_PROJECT),
          genProject('zz-proj', false)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays "All resources"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(ALL_RESOURCES_LABEL);
        });

        it('displays no count badge', () => {
          const { selectionCountVisible } = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(false);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('when project is selected', () => {
        const payload: ProjectsFilterOption[] = [
          genProject(UNASSIGNED_PROJECT_LABEL, false, UNASSIGNED_PROJECT),
          genProject('zz-proj', true)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays that project', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual('zz-proj');
        });

        it('displays no count badge', () => {
          const { selectionCountVisible } = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(false);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('when only unassigned is selected', () => {
        const payload: ProjectsFilterOption[] = [
          genProject(UNASSIGNED_PROJECT_LABEL, true, UNASSIGNED_PROJECT),
          genProject('zz-proj', false)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays "Unassigned resources"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(UNASSIGNED_PROJECT_LABEL);
        });
        it('displays no count badge', () => {
          const { selectionCountVisible } = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(false);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('selecting project and selecting unassigned', () => {
        const payload: ProjectsFilterOption[] = [
          genProject(UNASSIGNED_PROJECT_LABEL, true, UNASSIGNED_PROJECT),
          genProject('zz-proj', true)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays "All resources"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(ALL_RESOURCES_LABEL);
        });
        it('displays no count badge', () => {
          const { selectionCountVisible } = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(false);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });
    });

    describe('with multiple projects plus unassigned allowed', () => {

      // Also check that every case displays the caret to open dropdown

      describe('when nothing is selected', () => {
        const payload: ProjectsFilterOption[] = [
          genProject('zz-proj', false),
          genProject('c-proj', false),
          genProject(UNASSIGNED_PROJECT_LABEL, false, UNASSIGNED_PROJECT),
          genProject('b-proj', false),
          genProject('c-proj', false)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays "All resources"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(ALL_RESOURCES_LABEL);
        });

        it('displays no count badge', () => {
          const { selectionCountVisible } = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(false);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('when single project is selected', () => {
        const payload: ProjectsFilterOption[] = [
          genProject('zz-proj', false),
          genProject('c-proj', false),
          genProject(UNASSIGNED_PROJECT_LABEL, false, UNASSIGNED_PROJECT),
          genProject('b-proj', true),
          genProject('c-proj', false)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays that project', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual('b-proj');
        });

        it('displays no count badge', () => {
          const { selectionCountVisible } = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(false);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('when only unassigned is selected', () => {
        const payload: ProjectsFilterOption[] = [
          genProject('zz-proj', false),
          genProject('c-proj', false),
          genProject(UNASSIGNED_PROJECT_LABEL, true, UNASSIGNED_PROJECT),
          genProject('b-proj', false),
          genProject('c-proj', false)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays "Unassigned resources"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(UNASSIGNED_PROJECT_LABEL);
        });
        it('displays no count badge', () => {
          const { selectionCountVisible } = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(false);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('selecting single project and selecting unassigned', () => {
        const payload: ProjectsFilterOption[] = [
          genProject('zz-proj', true),
          genProject('c-proj', false),
          genProject(UNASSIGNED_PROJECT_LABEL, true, UNASSIGNED_PROJECT),
          genProject('b-proj', false),
          genProject('c-proj', false)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays selected project', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual('zz-proj');
        });
        it('displays no count badge', () => {
          const { selectionCountVisible } = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(false);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('selecting multiple but not all projects and not selecting unassigned', () => {
        const payload: ProjectsFilterOption[] = [
          genProject('zz-proj', true),
          genProject('c-proj', true),
          genProject(UNASSIGNED_PROJECT_LABEL, false, UNASSIGNED_PROJECT),
          genProject('b-proj', true),
          genProject('c-proj', false)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays "Multiple projects"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(MULTIPLE_PROJECTS_LABEL);
        });
        it('displays blue count badge with count of checked projects', () => {
          const { selectionCount, selectionCountActive, selectionCountVisible }
            = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(true);
          expect(selectionCountActive).toBe(BADGE_BLUE_STATE);
          expect(selectionCount).toEqual(payload.filter(p => p.checked).length);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('selecting multiple but not all projects and selecting unassigned', () => {
        const payload: ProjectsFilterOption[] = [
          genProject('zz-proj', true),
          genProject('c-proj', true),
          genProject(UNASSIGNED_PROJECT_LABEL, true, UNASSIGNED_PROJECT),
          genProject('b-proj', true),
          genProject('c-proj', false)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays "Multiple projects"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(MULTIPLE_PROJECTS_LABEL);
        });
        it('displays blue count badge excluding unassigned in count', () => {
          const { selectionCount, selectionCountActive, selectionCountVisible }
            = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(true);
          expect(selectionCountActive).toBe(BADGE_BLUE_STATE);
          expect(selectionCount).toEqual(payload.filter(p => p.checked).length - 1);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('selecting all projects and not selecting unassigned', () => {
        const payload: ProjectsFilterOption[] = [
          genProject('zz-proj', true),
          genProject('c-proj', true),
          genProject(UNASSIGNED_PROJECT_LABEL, false, UNASSIGNED_PROJECT),
          genProject('b-proj', true),
          genProject('c-proj', true)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays "All projects"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(ALL_PROJECTS_LABEL);
        });
        it('displays blue count badge with count of checked projects', () => {
          const { selectionCount, selectionCountActive, selectionCountVisible }
            = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(true);
          expect(selectionCountActive).toBe(BADGE_BLUE_STATE);
          expect(selectionCount).toEqual(payload.filter(p => p.checked).length);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('selecting all projects and selecting unassigned', () => {
        const payload: ProjectsFilterOption[] = [
          genProject('zz-proj', true),
          genProject('c-proj', true),
          genProject(UNASSIGNED_PROJECT_LABEL, true, UNASSIGNED_PROJECT),
          genProject('b-proj', true),
          genProject('c-proj', true)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays "All resources"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(ALL_RESOURCES_LABEL);
        });
        it('displays no count badge', () => {
          const { selectionCountVisible } = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(false);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });
    });

    describe('with at least 2 projects allowed but not unassigned', () => {

      // Also check that every case displays the caret to open dropdown

      describe('when nothing is selected', () => {
        const payload: ProjectsFilterOption[] = [
          genProject('zz-proj', false),
          genProject('c-proj', false),
          genProject('b-proj', false),
          genProject('c-proj', false)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays "All projects"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(ALL_PROJECTS_LABEL);
        });

        it('displays grey count badge with count of ALL projects', () => {
          const { selectionCount, selectionCountActive, selectionCountVisible }
            = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(true);
          expect(selectionCountActive).toBe(BADGE_GREY_STATE);
          expect(selectionCount).toEqual(payload.length);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('when single project is selected', () => {
        const payload: ProjectsFilterOption[] = [
          genProject('zz-proj', false),
          genProject('c-proj', false),
          genProject('b-proj', true),
          genProject('c-proj', false)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays that project', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual('b-proj');
        });

        it('displays no count badge', () => {
          const { selectionCountVisible } = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(false);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('selecting multiple but not all projects', () => {
        const payload: ProjectsFilterOption[] = [
          genProject('zz-proj', true),
          genProject('c-proj', true),
          genProject('b-proj', true),
          genProject('c-proj', false)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays "Multiple projects"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(MULTIPLE_PROJECTS_LABEL);
        });
        it('displays blue count badge with count of checked projects', () => {
          const { selectionCount, selectionCountActive, selectionCountVisible }
            = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(true);
          expect(selectionCountActive).toBe(BADGE_BLUE_STATE);
          expect(selectionCount).toEqual(payload.filter(p => p.checked).length);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('selecting all projects', () => {
        const payload: ProjectsFilterOption[] = [
          genProject('zz-proj', true),
          genProject('c-proj', true),
          genProject('b-proj', true),
          genProject('c-proj', true)
        ];
        const action = new LoadOptionsSuccess(payload);

        it('displays "All projects"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(ALL_PROJECTS_LABEL);
        });
        it('displays blue count badge with count of checked projects', () => {
          const { selectionCount, selectionCountActive, selectionCountVisible }
            = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(true);
          expect(selectionCountActive).toBe(BADGE_BLUE_STATE);
          expect(selectionCount).toEqual(payload.filter(p => p.checked).length);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });
    });
  });

  function genProject(label: string, checked: boolean, value?: string): ProjectsFilterOption {
    return <ProjectsFilterOption>{
      label: label,
      checked: checked,
      value: value ? value : label
    };
  }
});
