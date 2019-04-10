import { createFeatureSelector } from '@ngrx/store';

import { IntegrationsAddState } from './integration-add.reducer';

export const integrationsAddState =
  createFeatureSelector<IntegrationsAddState>('integrations_add');
