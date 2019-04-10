import { applyMiddleware, createStore } from 'redux';
import { composeWithDevTools } from 'redux-devtools-extension/developmentOnly';
import thunk from 'redux-thunk';

import { rootReducer } from './root.reducer';

export const configureStore = (initialState) =>
  createStore(rootReducer,
              initialState,
              composeWithDevTools(
                applyMiddleware(thunk)));
