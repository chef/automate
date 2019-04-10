// We have to require core-js first and then require the actual app.
// If we don't load like this Internet Explorer doesn't cooperate.
// NOTE: There maybe some webpack magic we can use to make this a bit
// more elegant, but with all of the transpiling and whatenot the way
// to do that escapes me. This should all be cleared up when we merge
// the two code bases.
require('core-js');

// EventSource polyfill for IE11
// Note: core-js doesn't want this, so we have to include it ourselves:
// https://github.com/zloirock/core-js/issues/229
require('eventsource-polyfill');

import ng from 'angular';

ng.injector(['ng'])
  .get('$http')
  .get('/workflow/api/v0/canonical_enterprise')
  .then(resp => {
    localStorage.setItem('canonical-enterprise', resp.data.ent_name);
  })
  .catch(() => {
    localStorage.removeItem('canonical-enterprise');
  })
  .finally(() => {
    require('./app.js');
  });
