import { createFeatureSelector } from '@ngrx/store';

import { IntegrationsDetailState } from './integrations-detail.reducer';

export const integrationsDetail =
  createFeatureSelector<IntegrationsDetailState>('integrations_detail');
