import { Action } from '@ngrx/store';

export interface ScannerAction extends Action {
  payload?: any;
}

export const GET_JOBS = 'GET_JOBS';
export const getJobs = (payload): ScannerAction => ({ type: GET_JOBS, payload });

export const GET_JOBS_SUCCESS = 'GET_JOBS_SUCCESS';
export const getJobsSuccess = (payload): ScannerAction => ({ type: GET_JOBS_SUCCESS, payload });

export const GET_JOB = 'GET_JOB';
export const getJob = (payload): ScannerAction => ({ type: GET_JOB, payload });

export const GET_JOB_SUCCESS = 'GET_JOB_SUCCESS';
export const getJobSuccess = (payload): ScannerAction => ({ type: GET_JOB_SUCCESS, payload });

export const GET_JOB_SCANS = 'GET_JOB_SCANS';
export const getJobScans = (payload): ScannerAction => ({ type: GET_JOB_SCANS, payload });

export const GET_JOB_SCANS_SUCCESS = 'GET_JOB_SCANS_SUCCESS';
export const getJobScansSuccess = (payload): ScannerAction => {
  return { type: GET_JOB_SCANS_SUCCESS, payload };
};

export const CREATE_JOB = 'CREATE_JOB';
export const createJob = (payload): ScannerAction => ({ type: CREATE_JOB, payload });

export const CREATE_JOB_SUCCESS = 'CREATE_JOB_SUCCESS';
export const createJobSuccess = (payload): ScannerAction => ({ type: CREATE_JOB_SUCCESS, payload });

export const UPDATE_JOB = 'UPDATE_JOB';
export const updateJob = (payload): ScannerAction => ({ type: UPDATE_JOB, payload });

export const UPDATE_JOB_SUCCESS = 'UPDATE_JOB_SUCCESS';
export const updateJobSuccess = (payload): ScannerAction => ({ type: UPDATE_JOB_SUCCESS, payload });

export const PROMPT_DELETE_JOB = 'PROMPT_DELETE_JOB';
export const promptDeleteJob = (payload): ScannerAction => ({ type: PROMPT_DELETE_JOB, payload });

export const CANCEL_DELETE_JOB = 'CANCEL_DELETE_JOB';
export const cancelDeleteJob = (payload): ScannerAction => ({ type: CANCEL_DELETE_JOB, payload });

export const CONFIRM_DELETE_JOB = 'CONFIRM_DELETE_JOB';
export const confirmDeleteJob = (payload): ScannerAction => ({ type: CONFIRM_DELETE_JOB, payload });

export const DELETE_JOB = 'DELETE_JOB';
export const deleteJob = (payload): ScannerAction => ({ type: DELETE_JOB, payload });

export const DELETE_JOB_SUCCESS = 'DELETE_JOB_SUCCESS';
export const deleteJobSuccess = (payload): ScannerAction => ({ type: DELETE_JOB_SUCCESS, payload });

export const GET_NODES = 'GET_NODES';
export const getNodes = (payload): ScannerAction => ({ type: GET_NODES, payload });

export const GET_NODES_SUCCESS = 'GET_NODES_SUCCESS';
export const getNodesSuccess = (payload): ScannerAction => ({ type: GET_NODES_SUCCESS, payload });

export const GET_NODE = 'GET_NODE';
export const getNode = (payload): ScannerAction => ({ type: GET_NODE, payload });

export const GET_NODE_SUCCESS = 'GET_NODE_SUCCESS';
export const getNodeSuccess = (payload): ScannerAction => ({ type: GET_NODE_SUCCESS, payload });

export const RERUN_NODE = 'RERUN_NODE';
export const rerunNode = (payload): ScannerAction => ({ type: RERUN_NODE, payload });

export const RERUN_NODE_SUCCESS = 'RERUN_NODE_SUCCESS';
export const rerunNodeSuccess = (payload): ScannerAction => ({ type: RERUN_NODE_SUCCESS, payload });

export const DELETE_NODE = 'DELETE_NODE';
export const deleteNode = (payload): ScannerAction => ({ type: DELETE_NODE, payload });

export const DELETE_NODE_SUCCESS = 'DELETE_NODE_SUCCESS';
export const deleteNodeSuccess = (payload): ScannerAction => {
  return { type: DELETE_NODE_SUCCESS, payload };
};

export const GET_PROFILES = 'GET_PROFILES';
export const getProfiles = (payload): ScannerAction => ({ type: GET_PROFILES, payload });

export const GET_PROFILES_SUCCESS = 'GET_PROFILES_SUCCESS';
export const getProfilesSuccess = (payload): ScannerAction => {
  return { type: GET_PROFILES_SUCCESS, payload };
};
