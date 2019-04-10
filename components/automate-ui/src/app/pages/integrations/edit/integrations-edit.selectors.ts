import { createFeatureSelector } from '@ngrx/store';

import { IntegrationsEditState } from './integrations-edit.reducer';

export const integrationsEditState =
  createFeatureSelector<IntegrationsEditState>('integrations_edit');
