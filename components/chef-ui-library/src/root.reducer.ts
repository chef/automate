import { combineReducers } from 'redux';

import { docReducer } from './entities/docs/doc.reducer';


export const rootReducer = combineReducers({
  docs: docReducer
});
