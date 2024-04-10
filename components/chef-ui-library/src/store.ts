// import { applyMiddleware, createStore } from 'redux';
// import { composeWithDevTools } from '@redux-devtools/extension';
// import thunk from 'redux-thunk';
import { configureStore } from '@reduxjs/toolkit'

import { rootReducer } from './root.reducer';

// export const configureStore = (initialState) =>
//   createStore(rootReducer,
//     initialState,
//     composeWithDevTools(
//       applyMiddleware(thunk)));


export const store = configureStore({ reducer: rootReducer });
