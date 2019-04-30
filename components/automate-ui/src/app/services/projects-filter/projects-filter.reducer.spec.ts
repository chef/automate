import { map } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import {
  ProjectsFilterState,
  projectsFilterInitialState,
  projectsFilterReducer,
  ProjectsFilterOption
} from './projects-filter.reducer';
import { LoadOptions, LoadOptionsSuccess } from './projects-filter.actions';

const UNASSIGNED_PROJECT = '(unassigned)';

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
        <ProjectsFilterOption>{
          label: 'Unassigned resources',
          checked: true,
          value: UNASSIGNED_PROJECT
        },
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

      it('displays project name', () => {
      });

      it('does not display caret to open dropdown', () => {
      });
    });

    describe('with only unassigned resources allowed', () => {

      it('displays "Unassigned resources"', () => {
      });

      it('does not display caret to open dropdown', () => {
      });
    });

    describe('with exactly one project plus unassigned allowed', () => {

      // Also check that every case displays the caret to open dropdown

      it('when nothing is selected displays "All resources"', () => {
      });
      it('when nothing is selected displays no count badge', () => {
      });
      it('when project selected displays that project', () => {
      });
      it('when project selected displays no count badge', () => {
      });
      it('when only unassigned selected displays "Unassigned resources"', () => {
      });
      it('when only unassigned selected displays no count badge', () => {
      });
      it('selecting project and selecting unassigned displays "All resources"', () => {
      });
      it('selecting project and selecting unassigned displays no count badge', () => {
      });
    });

    describe('with multiple projects plus unassigned allowed', () => {

      // Also check that every case displays the caret to open dropdown

      it('when nothing is selected displays "All resources"', () => {
      });
      it('when nothing is selected displays no count badge', () => {
      });
      it('when single project selected displays that project', () => {
      });
      it('when single project selected displays no count badge', () => {
      });
      it('when only unassigned selected displays "Unassigned resources"', () => {
      });
      it('when only unassigned selected displays no count badge', () => {
      });
      it('when single project plus unassigned selected displays that project', () => {
      });
      it('when single project plus unassigned selected displays no count badge', () => {
      });
      it('selecting multiple but not all projects and not selecting unassigned'
        + ' displays "Multiple projects"', () => {
        });
      it('selecting multiple but not all projects and not selecting unassigned'
        + ' displays blue count badge', () => {
        });
      it('selecting multiple but not all projects and selecting unassigned'
        + ' displays "Multiple projects"', () => {
        });
      it('selecting multiple but not all projects and selecting unassigned'
        + ' displays blue count badge excluding unassigned', () => {
        });
      it('selecting all projects and not selecting unassigned'
        + ' displays "All projects"', () => {
        });
      it('selecting all projects and not selecting unassigned'
        + ' displays blue count badge', () => {
        });
      it('selecting all projects and selecting unassigned'
        + ' displays "All resources"', () => {
        });
      it('selecting all projects and selecting unassigned'
        + ' displays no count badge', () => {
        });
    });

    describe('with at least 2 projects allowed but not unassigned', () => {

      // Also check that every case displays the caret to open dropdown

      it('when nothing is selected displays "All projects"', () => {
      });
      it('when nothing is selected displays grey count badge', () => {
      });
      it('when single project selected displays that project', () => {
      });
      it('when single project selected displays no count badge', () => {
      });
      it('selecting multiple but not all projects displays "Multiple projects"', () => {
      });
      it('selecting multiple but not all projects displays blue count badge', () => {
      });
      it('selecting all projects displays "All projects"', () => {
      });
      it('selecting all projects displays blue count badge', () => {
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
