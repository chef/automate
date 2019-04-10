import ng from 'angular';

function Store() {
  let namespace = '';

  return {
    // If namespace is set keys will be prefixed with said namespace.
    // Namespace can only be set once.
    setNamespace: function (val) {
      if (!namespace) {
        namespace = val;
        return true;
      }
      return false;
    },

    put: function (key, value, persist) {
      let jsonValue;

      key = namespace + key;

      persist = persist || false;

      jsonValue = JSON.stringify(value);

      if (persist) {
        localStorage.setItem(key, jsonValue);
      } else {
        sessionStorage.setItem(key, jsonValue);
      }

      return value;
    },

    // When returning values from store sessionStorage takes precedence
    get: function (key) {
      let value, raw;

      key = namespace + key;

      raw = sessionStorage.getItem(key) || localStorage.getItem(key);

      if (raw) {
        value = JSON.parse(raw);
      }

      return value;
    },

    // Clear will clear sessionStorage first and then localStorage.
    clear: function (key) {
      key = namespace + key;

      if (sessionStorage[key] || localStorage[key]) {
        delete sessionStorage[key];
        delete localStorage[key];
        return true;
      }
      return false;
    }
  };
}

export default ng
  .module('cd.common.store', [])
  .service('Store', Store)
  .name;
