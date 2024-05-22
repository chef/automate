import { fetchDocs } from './doc.requests';
import { docsSlice } from "./doc.reducer";
import { store } from "../../store";


const { getDocs: getDocsAction, getDocsSuccess, getDocsFailed } = docsSlice.actions;

export const getDocs = async () => {

  store.dispatch(getDocsAction());

  const resp = await fetchDocs();

  if (resp.ok) {
    const payload = await resp.json();
    store.dispatch(getDocsSuccess({ children: payload && payload.children }));
  } else {
    store.dispatch(getDocsFailed());
  }

};

