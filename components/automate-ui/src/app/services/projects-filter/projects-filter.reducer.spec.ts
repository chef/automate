import { map } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { ProjectConstants } from 'app/entities/projects/project.model';
import {
  ProjectsFilterState,
  projectsFilterInitialState,
  projectsFilterReducer,
  ProjectsFilterOption,
  ProjectsFilterOptionTuple
} from './projects-filter.reducer';
import { LoadOptions, LoadOptionsSuccess } from './projects-filter.actions';

const {
  UNASSIGNED_PROJECT_ID,
  UNASSIGNED_PROJECT_LABEL,
  ALL_RESOURCES_LABEL,
  ALL_PROJECTS_LABEL,
  MULTIPLE_PROJECTS_LABEL
} = ProjectConstants;

const BADGE_GREY_STATE = false;
const BADGE_BLUE_STATE = true;

describe('projectsFilterReducer', () => {
  const initialState: ProjectsFilterState = projectsFilterInitialState;

  describe('undefined action', () => {
    it('should return the default state', () => {
      const action = { type: 'NOOP' } as any;
      const result = projectsFilterReducer(undefined, action);

      expect(result).toBe(initialState);
    });
  });

  describe('LOAD_OPTIONS', () => {
    const action = new LoadOptions();

    it('sets status to loading', () => {
      const { optionsLoadingStatus } = projectsFilterReducer(initialState, action);
      expect(optionsLoadingStatus).toEqual(EntityStatus.loading);
    });
  });

  describe('LOAD_OPTIONS_SUCCESS', () => {

    it('sets status to success', () => {
      const action = genAction(
        genProject('p1')
      );

      const { optionsLoadingStatus } = projectsFilterReducer(initialState, action);

      expect(optionsLoadingStatus).toEqual(EntityStatus.loadingSuccess);
    });

    it('without unassigned, sorts projects when storing them', () => {
      const action = genAction(
        genProject('d-proj'),
        genProject('b-proj'),
        genProject('c-proj'),
        genProject('a-proj')
      );

      const { options } = projectsFilterReducer(initialState, action);

      expect(map('label', options)).toEqual(['a-proj', 'b-proj', 'c-proj', 'd-proj']);
    });

    it('with unassigned, sorts projects except unassigned at bottom when storing them', () => {
      const action = genAction(
        genProject('d-proj'),
        genUnassignedProject(),
        genProject('b-proj'),
        genProject('zz-proj'),
        genProject('a-proj')
      );

      const { options } = projectsFilterReducer(initialState, action);

      expect(map('value', options))
        .toEqual(['a-proj', 'b-proj', 'd-proj', 'zz-proj', UNASSIGNED_PROJECT_ID]);
    });

    describe('with exactly one allowed project', () => {
      const action = genAction(
        genProject('zz-proj')
      );

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
      const action = genAction(
        genUnassignedProject()
      );

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
        const action = genAction(
          genUnassignedProject(),
          genProject('zz-proj')
        );

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
        const action = genAction(
          genUnassignedProject(),
          genProject('zz-proj', true)
        );

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
        const action = genAction(
          genUnassignedProject(true),
          genProject('zz-proj')
        );

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
        const action = genAction(
          genUnassignedProject(true),
          genProject('zz-proj', true)
        );

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
        const action = genAction(
          genProject('zz-proj'),
          genProject('c-proj'),
          genUnassignedProject(),
          genProject('a-proj'),
          genProject('b-proj')
        );

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
        const action = genAction(
          genProject('zz-proj'),
          genProject('c-proj'),
          genUnassignedProject(),
          genProject('a-proj', true),
          genProject('b-proj')
        );

        it('displays that project', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual('a-proj');
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
        const action = genAction(
          genProject('zz-proj'),
          genProject('c-proj'),
          genUnassignedProject(true),
          genProject('a-proj'),
          genProject('b-proj')
        );

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
        const action = genAction(
          genProject('zz-proj', true),
          genProject('c-proj'),
          genUnassignedProject(true),
          genProject('a-proj'),
          genProject('b-proj')
        );

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
        const action = genAction(
          genProject('zz-proj', true),
          genProject('c-proj', true),
          genUnassignedProject(),
          genProject('a-proj', true),
          genProject('b-proj')
        );

        it('displays "Multiple projects"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(MULTIPLE_PROJECTS_LABEL);
        });
        it('displays blue count badge with count of checked projects', () => {
          const { selectionCount, selectionCountActive, selectionCountVisible }
            = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(true);
          expect(selectionCountActive).toBe(BADGE_BLUE_STATE);
          expect(selectionCount).toEqual(action.payload.fetched.filter(p => p.checked).length);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('selecting multiple but not all projects and selecting unassigned', () => {
        const action = genAction(
          genProject('zz-proj', true),
          genProject('c-proj', true),
          genUnassignedProject(true),
          genProject('a-proj', true),
          genProject('b-proj')
        );

        it('displays "Multiple projects"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(MULTIPLE_PROJECTS_LABEL);
        });
        it('displays blue count badge excluding unassigned in count', () => {
          const { selectionCount, selectionCountActive, selectionCountVisible }
            = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(true);
          expect(selectionCountActive).toBe(BADGE_BLUE_STATE);
          expect(selectionCount).toEqual(action.payload.fetched.filter(p => p.checked).length - 1);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('selecting all projects and not selecting unassigned', () => {
        const action = genAction(
          genProject('zz-proj', true),
          genProject('c-proj', true),
          genUnassignedProject(),
          genProject('a-proj', true),
          genProject('b-proj', true)
        );

        it('displays "All projects"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(ALL_PROJECTS_LABEL);
        });
        it('displays blue count badge with count of checked projects', () => {
          const { selectionCount, selectionCountActive, selectionCountVisible }
            = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(true);
          expect(selectionCountActive).toBe(BADGE_BLUE_STATE);
          expect(selectionCount).toEqual(action.payload.fetched.filter(p => p.checked).length);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('selecting all projects and selecting unassigned', () => {
        const action = genAction(
          genProject('zz-proj', true),
          genProject('c-proj', true),
          genUnassignedProject(true),
          genProject('a-proj', true),
          genProject('b-proj', true)
        );

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
        const action = genAction(
          genProject('zz-proj'),
          genProject('c-proj'),
          genProject('a-proj'),
          genProject('b-proj')
        );

        it('displays "All projects"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(ALL_PROJECTS_LABEL);
        });

        it('displays grey count badge with count of ALL projects', () => {
          const { selectionCount, selectionCountActive, selectionCountVisible }
            = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(true);
          expect(selectionCountActive).toBe(BADGE_GREY_STATE);
          expect(selectionCount).toEqual(action.payload.fetched.length);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('when single project is selected', () => {
        const action = genAction(
          genProject('zz-proj'),
          genProject('c-proj'),
          genProject('a-proj', true),
          genProject('b-proj')
        );

        it('displays that project', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual('a-proj');
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
        const action = genAction(
          genProject('zz-proj', true),
          genProject('c-proj', true),
          genProject('a-proj', true),
          genProject('b-proj')
        );

        it('displays "Multiple projects"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(MULTIPLE_PROJECTS_LABEL);
        });
        it('displays blue count badge with count of checked projects', () => {
          const { selectionCount, selectionCountActive, selectionCountVisible }
            = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(true);
          expect(selectionCountActive).toBe(BADGE_BLUE_STATE);
          expect(selectionCount).toEqual(action.payload.fetched.filter(p => p.checked).length);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });

      describe('selecting all projects', () => {
        const action = genAction(
          genProject('zz-proj', true),
          genProject('c-proj', true),
          genProject('a-proj', true),
          genProject('b-proj', true)
        );

        it('displays "All projects"', () => {
          const { selectionLabel } = projectsFilterReducer(initialState, action);
          expect(selectionLabel).toEqual(ALL_PROJECTS_LABEL);
        });
        it('displays blue count badge with count of checked projects', () => {
          const { selectionCount, selectionCountActive, selectionCountVisible }
            = projectsFilterReducer(initialState, action);
          expect(selectionCountVisible).toBe(true);
          expect(selectionCountActive).toBe(BADGE_BLUE_STATE);
          expect(selectionCount).toEqual(action.payload.fetched.filter(p => p.checked).length);
        });

        it('displays caret to open dropdown', () => {
          const { dropdownCaretVisible } = projectsFilterReducer(initialState, action);
          expect(dropdownCaretVisible).toBe(true);
        });
      });
    });
  });

  function genProject(label: string, checked?: boolean, value?: string): ProjectsFilterOption {
    return <ProjectsFilterOption>{
      label: label,
      checked: checked === undefined ? false : checked,
      value: value ? value : label
    };
  }

  function genUnassignedProject(checked?: boolean): ProjectsFilterOption {
    return genProject(UNASSIGNED_PROJECT_LABEL, checked, UNASSIGNED_PROJECT_ID);
  }

  function genAction(...projects: ProjectsFilterOption[]): LoadOptionsSuccess {
    return new LoadOptionsSuccess(
      <ProjectsFilterOptionTuple>{ fetched: projects, restored: {} });
  }
});
