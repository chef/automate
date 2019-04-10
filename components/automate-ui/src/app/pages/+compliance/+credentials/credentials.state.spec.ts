import {
  credentialsReducer,
  GET_CREDENTIALS_SUCCESS,
  initialState } from './credentials.state';

describe('credentialsReducer', () => {
  it('returns an empty list of credentials with an empty array of input', () => {
    const secrets = [];
    const total = secrets.length;
    expect(credentialsReducer(initialState, {
      type: GET_CREDENTIALS_SUCCESS,
      payload: {secrets, total}
    }).credentialsList.total).toEqual(0);
  });

  it('returns a list of two credentials with two credentials as input', () => {
    const secrets = [
      {
        'id': 'ecde02bb-f3ee-4e98-56be-59b1d3868927',
        'name': 'Hannah\'s Awesome WinRM credentials',
        'type': 'WinRM',
        'last_modified': '2017-08-25T21:37:27.007158Z',
        'tags': []
      },
      {
        'id': '375dff23-3f32-4b56-6470-31b55bf311bf',
        'name': 'Hannah\'s Awesome SSH Key',
        'type': 'SSH',
        'last_modified': '2017-08-28T02:30:53.753413Z',
        'tags': []
      }
    ];
    const total = secrets.length;
    expect(credentialsReducer(initialState, {
      type: GET_CREDENTIALS_SUCCESS,
      payload: {secrets, total}
    }).credentialsList.total).toEqual(2);
  });
});
