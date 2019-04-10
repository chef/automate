import { fetchDocs } from './doc.requests';

export const GET_DOCS = 'DOCS::GET';
export const GET_DOCS_SUCCESS = 'DOCS::GET::SUCCESS';
export const GET_DOCS_FAILED = 'DOCS::GET::FAILED';

export const getDocs = () =>
  async (dispatch) => {
    dispatch({ type: GET_DOCS });

    const resp = await fetchDocs();

    if (resp.ok) {
      const payload = await resp.json();
      dispatch({ type: GET_DOCS_SUCCESS, payload });
    } else {
      dispatch({ type: GET_DOCS_FAILED, payload: resp.status });
    }
  };
