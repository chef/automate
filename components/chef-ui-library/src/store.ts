import { configureStore } from '@reduxjs/toolkit';
import { docsSlice } from './entities/docs/doc.reducer';

export const store = configureStore({
  reducer: {
    docs: docsSlice.reducer
  },
  middleware: (getDefaultMiddleware) => getDefaultMiddleware({
    serializableCheck: false
  }),
})

