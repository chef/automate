import ng from 'angular';
import ngCookies from 'angular-cookies';
import ApiUrl from '../../../common/api/api_url';
import Auth from '../../../common/auth/auth';
import Session from '../../../common/auth/session';
import Store from '../../../common/store/store';

loginController.$inject = ['$scope', '$state', '$location', 'Auth', 'Session', 'Store', '$http', 'ApiUrl', '$cookies'];

function loginController($scope, $state, $location, Auth, Session, Store, $http, ApiUrl, $cookies) {
  var defaultUrl = '/#/dashboard';
  var url = ($location.search().returnUrl || defaultUrl).split('#').reverse()[0];

  // Check if we were given session information via a cookie
  // (this will happen in the final redirect of a successful SAML authentication).
  var cookie_token = $cookies.get("saml-chef-delivery-token");
  var cookie_user = $cookies.get("saml-chef-delivery-user");
  var cookie_ttl = $cookies.get("saml-chef-delivery-ttl");

  if (cookie_user && cookie_token && cookie_ttl) {
    // this will trigger the switch to the dashboard below
    Session.set({token: cookie_token,
                 username: cookie_user,
                 ttl: cookie_ttl});

    // Delete the cookies
    $cookies.remove("saml-chef-delivery-token", {path: "/", secure: $location.protocol() == "https"});
    $cookies.remove("saml-chef-delivery-user", {path: "/", secure: $location.protocol() == "https"});
    $cookies.remove("saml-chef-delivery-ttl", {path: "/", secure: $location.protocol() == "https"});
  }

  // If the user is authorized send them to root (or returnUrl, if set)
  // Otherwise remove the session
  Auth.isAuthenticated()
    .then((resp) => {
      Session.setCookies();
      $location.url(url);
    })
    .catch((error) => {
      Session.remove();
    });

  if ($location.search().error) {
    $scope.$parent.state = "error";
    $scope.$parent.message = atob($location.search().error);
  }
  else {
    $scope.$parent.state = "";
  }
  $scope.passwordPage = false;

  $scope.$watch('user.username', function (newValue, oldValue) {
    if(newValue && newValue.length > 0) {
      $scope.usernamestate = "valid";
    } else {
      $scope.usernamestate = "";
    }
  });

  $scope.$watch('user.password', function (newValue, oldValue) {
    if(newValue && newValue.length > 0) {
      $scope.passwordstate = "valid";
    } else {
      $scope.passwordstate = "";
    }
  });

  $scope.user = Store.get('user') || {};

  $scope.saml = function () {
    window.location.href  = ApiUrl(`/saml/auth?returnUrl=${url}`);
  };

  // fetch whether saml is enabled from the API
  $http.get(ApiUrl('/saml/enabled'))
    .success(function(data, status, headers, config) {
      $scope.samlEnabled = data.enabled;
      $scope.initializeLoginState();
    }).error(function(data, status, headers, config) {
      $scope.initializeLoginState();
    });

  $scope.initializeLoginState = function () {
    var state = $scope.samlEnabled ? 'saml' : 'local';
    $scope.setLoginState(state);
  };

  $scope.setLoginState = function (state) {
    $scope.loginState = state;
    $scope.$parent.loginState = state;
  };
}

export default ng
  .module('cd.routes.authenticate.login.controller', [
    Auth,
    Store,
    Session
  ])
  .controller('loginController', loginController)
  .name;
