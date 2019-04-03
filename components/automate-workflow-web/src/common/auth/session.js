import ng from 'angular';
import Store from '../store/store';
import ngCookies from 'angular-cookies';
import moment from 'moment';
import * as jwtDecode from 'jwt-decode';
import { upgradeAdapter } from '../../upgrade-adapter.ts';

Session.$inject = [
  '$http',
  '$location',
  'Store',
  '$cookies'
];

function Session ($http, $location, Store, $cookies) {

  const a2key = 'chef-automate-user';
  const HTTP_STATUS_OK = 200;
  const HTTP_STATUS_UNAUTHORIZED = 401;
  const XHR_DONE = 4;
  const minute = 60 * 1000;

  var session;
  session = Store.get('session') || {};

  // Retrieve the canonical enterprise from localstorage.
  // If there isn't one there, it means there's none yet configured.
  var enterprise = localStorage.getItem('canonical-enterprise');

  if (!enterprise) {
    $location.url('/welcome');
  }
  else {
    Store.setNamespace(enterprise);
    session.enterprise = enterprise;
    Store.put('session', session, true);
  }

  let cookieOpts = {
    secure: $location.protocol() === 'https',
    domain: $location.host(),
    path: '/workflow'
  };

  loadA2User();

  var callbackFunction = function (event) {
    var xhr = event.target;
    if (xhr.readyState === XHR_DONE) {
      if (xhr.status === HTTP_STATUS_OK) {
        ingestIDToken(xhr.response.id_token);
        loadA2User();
      } else if (xhr.status === HTTP_STATUS_UNAUTHORIZED) {
        logout();
      } else {
        // TODO 2017/12/15 (sr): is there anything we could do that's better than
        // this?
        console.log(`Session refresh failed: ${xhr.statusText}`);
      }
    }
  };

  window.setInterval(() => {
    /* Using XMLHttpRequest instead of angular/http to avoid
       dependency hell. */
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = callbackFunction.bind(this);
    xhr.open('GET', '/session/refresh', true);
    xhr.responseType = 'json';
    xhr.setRequestHeader('Authorization', `Bearer ${session.token}`);
    xhr.send(null);
  }, minute);

  function ingestIDToken(idToken) {
    var id;
    try {
      id = jwtDecode(idToken);
      if (id === null) {
        return;
      }
      setA2Session(id.sub, id.name,
      // What dex considers email, we consider username (for local users at least)
      // We might have to revisit this given more information about actual LDAP/SAML
      // usage.
        id.email, idToken, id.groups);
    }
    catch (e) {
      return null;
    }
  }

  function setA2Session(uuid, fullname, username, id_token, groups) {
    const user = {
      uuid: uuid,
      fullname: fullname,
      username: username,
      groups: groups,
      id_token: id_token
    };
    localStorage.setItem(a2key, JSON.stringify(user));
  }

  function logout(url = '/') {
    remove();
    window.location.href = `/session/new?state=${url}`;
  }

  function setHeaders() {
    $http.defaults.headers.common['chef-delivery-user'] = session.username;
    $http.defaults.headers.common['chef-delivery-token'] = session.token;
  }

  function hasSession() {
    return !!session.token && !!session.username;
  }

  function setCookies() {
    if (hasSession()) { //Protect the cookies!
      let expires;
      if (!!session.ttl) {
        expires = moment().add(session.ttl, 's').toDate();
      } else {
        // Setting a default expiration of 1 day just in case.
        // Theoretically we should always have a session ttl in a valid session.
        expires = moment().add(1, 'day').toDate();
      }

      $cookies.put('chef-delivery-user', session.username, Object.assign(cookieOpts, { expires }));
      $cookies.put('chef-delivery-token', session.token, Object.assign(cookieOpts, { expires }));
      $cookies.put('chef-delivery-enterprise', session.enterprise, Object.assign(cookieOpts, { expires }));
    }
  }

  function loadA2User() {
    const user = localStorage.getItem(a2key);

    if (user) {
      try {
        const { username, id_token: token } = JSON.parse(user);
        session.username = username;
        session.token = token;
        setHeaders();
        setCookies();
        Store.put('session', session, true);
      }
      catch(e) {
        window.console.error(`Failed to translate A2 user: ${e.message}`);
      }
    }
  }

  function unloadA2User() {
    localStorage.removeItem(a2key);
  }

  function remove() {
    session = {};
    $cookies.remove('chef-delivery-user', cookieOpts);
    $cookies.remove('chef-delivery-token', cookieOpts);
    $cookies.remove('chef-delivery-enterprise', cookieOpts);

    unloadA2User();

    session.enterprise = enterprise;
    return session;
  }

  if (hasSession()) {
    setHeaders();
  }

  return {
    hasSession: hasSession,

    setCookies: setCookies,

    remove: remove,

    set: function (obj) {
      session = ng.extend(session, obj);

      setHeaders();
      setCookies();
      Store.put('session', session, true);
      return session;
    },

    get: function (key) {
      if (key) {
        return session[key];
      } else {
        return session;
      }
    }
  };
}

export default ng
  .module('cd.common.auth.session', [
    Store,
    ngCookies
  ])
  .service('Session', Session)
  .name;

upgradeAdapter.upgradeNg1Provider('Session');
