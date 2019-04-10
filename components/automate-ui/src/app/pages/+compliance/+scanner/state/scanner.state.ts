import { assign } from 'lodash';
import { pipe, set } from 'lodash/fp';
import { Job, Node } from '../scanner.types';
import * as actions from './scanner.actions';

export interface ScannerState {
  jobsList: {
    loading: boolean,
    items: Job[],
    total: number,
    per_page: number,
    page: number,
    sort: string,
    order: string,
    filters: any[]
  };
  jobDeletePrompt: {
    visible: boolean,
    job: Job
  };
  jobDetail: {
    name: string,
    results: any[]
  };
  jobScansList: {
    items: Job[],
    total: number,
    per_page: number,
    page: number,
    sort: string,
    order: string,
    filters: any[]
  };
  nodesList: {
    loading: boolean,
    items: Node[],
    total: number,
    per_page: number,
    page: number,
    sort: string,
    order: string,
    filters: any[]
  };
  nodeTotals: {
    all: number,
    unreachable: number,
    reachable: number,
    unknown: number
  };
  nodeDetail: {
    id: string,
    name: string,
    last_job: any
  };
}

export const initialState: ScannerState = {
  jobsList: {
    loading: false,
    items: [],
    total: 0,
    per_page: 100,
    page: 1,
    sort: 'end_time',
    order: 'desc',
    filters: [
      {key: 'job_type', values: ['exec']},
      {key: 'parent_job', values: ['']}
    ]
  },
  jobDeletePrompt: {
    visible: false,
    job: null
  },
  jobDetail: null,
  jobScansList: {
    items: [],
    total: 0,
    per_page: 100,
    page: 1,
    sort: 'end_time',
    order: 'desc',
    filters: [
      {key: 'job_type', values: ['exec']},
      {key: 'parent_job', values: ['']}
    ]
  },
  nodesList: {
    loading: false,
    items: [],
    total: 0,
    per_page: 100,
    page: 1,
    sort: 'name',
    order: null,
    filters: [
      { key: 'status', values: ['reachable', 'unreachable', 'unknown'] },
      { key: 'manager_type', values: [''], exclude: true }
    ]
  },
  nodeTotals: {
    all: 0,
    unreachable: 0,
    reachable: 0,
    unknown: 0
  },
  nodeDetail: null
};

export function scannerReducer(state: ScannerState = initialState,
                               action: actions.ScannerAction): ScannerState {
  switch (action.type) {
    case actions.GET_JOBS: {
      const {page, per_page, sort, order, filters} = action.payload;
      const loading = action.payload.loading || false;
      const jobsList = assign({}, state.jobsList, {loading, page, per_page, sort, order, filters});
      return assign({}, state, {jobsList});
    }

    case actions.GET_JOBS_SUCCESS: {
      const jobsList = assign({}, state.jobsList, {
        loading: false,
        items: action.payload.jobs || [],
        total: action.payload.total || 0
      });
      return assign({}, state, {jobsList});
    }

    case actions.GET_JOB_SUCCESS: {
      const jobDetail = assign({}, state.jobDetail, action.payload);
      return assign({}, state, {jobDetail});
    }

    case actions.GET_JOB_SCANS: {
      const {page, per_page, sort, order, filters} = action.payload;
      const jobScansList = assign({}, state.jobScansList, {page, per_page, sort, order, filters});
      return assign({}, state, {jobScansList});
    }

    case actions.PROMPT_DELETE_JOB: {
      return set('jobDeletePrompt', {
        visible: true,
        job: action.payload
      }, state);
    }

    case actions.CANCEL_DELETE_JOB: {
      return set('jobDeletePrompt', {
        visible: false,
        job: null
      }, state);
    }

    case actions.CONFIRM_DELETE_JOB: {
      return set('jobDeletePrompt', {
        visible: false,
        job: action.payload
      }, state);
    }

    case actions.GET_JOB_SCANS_SUCCESS: {
      const jobScansList = assign({}, state.jobScansList, {
        items: action.payload.jobs || [],
        total: action.payload.total || 0
      });
      return assign({}, state, {jobScansList});
    }

    case actions.GET_NODES: {
      const {page, per_page, sort, order, filters} = action.payload;
      const loading = action.payload.loading || false;
      const nodesList = assign({}, state.nodesList,
        {loading, page, per_page, sort, order, filters});
      return assign({}, state, {nodesList});
    }

    case actions.GET_NODES_SUCCESS: {
      const { nodes, total, total_reachable, total_unreachable, total_unknown } = action.payload;
      return pipe(
        set('nodesList.loading', false),
        set('nodesList.items', nodes || []),
        set('nodesList.total', total || 0),
        set('nodeTotals', {
          all: total_reachable + total_unreachable + total_unknown,
          reachable: total_reachable,
          unreachable: total_unreachable,
          unknown: total_unknown
        })
      )(state) as ScannerState;
    }

    case actions.GET_NODE_SUCCESS: {
      const nodeDetail = assign({}, state.nodeDetail, action.payload);
      return assign({}, state, {nodeDetail});
    }

    default: {
      return state;
    }
  }
}
