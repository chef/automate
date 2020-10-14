import { createFeatureSelector } from '@ngrx/store';

import { NodeCredentialListState } from './node-credential-list.reducer';

export const nodeCredentialListState = createFeatureSelector<NodeCredentialListState>('nodeCredential_list');
