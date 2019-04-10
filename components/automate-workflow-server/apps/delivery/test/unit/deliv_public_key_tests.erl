-module(deliv_public_key_tests).

-include_lib("public_key/include/public_key.hrl").
-include_lib("eunit/include/eunit.hrl").

parse_valid_dsa_test() ->
    ?assertMatch([{{_, {'Dss-Parms', _,_,_}}, [{comment, _}]}],
                 public_key:ssh_decode(ssh_dsa(), public_key)).

parse_valid_rsa_keys_test_() ->
    [ ?_assertMatch(#'RSAPublicKey'{},
                    deliv_public_key:parse(I))
      || I <- [spki_pub(), pkcs1(), ssh_rsa()] ].

extract_invalid_public_key_test_() ->
    [ ?_assertEqual({error, bad_key},
                    deliv_public_key:parse(I))
      || I <- [bogus_ssh(), bogus_spki()] ].

bogus_ssh() ->
    <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDDhOF0FdWQmb3AdCUXOHDX72C0M"
      "vJI+Z/R9tYehENaii7BD1qB6+Tqudj/ikAO9">>.

bogus_spki() ->
    <<"-----BEGIN PUBLIC KEY-----
MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCyVPW9YXa5PR0rgEW1updSxygB
wmVp">>.

spki_pub() ->
    <<"-----BEGIN PUBLIC KEY-----
MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCyVPW9YXa5PR0rgEW1updSxygB
wmVpDnHurgQ7/gbh+PmY49EZsfrZSbKgSKy+rxdsVoSoU+krYtHvYIwVfr2tk0FP
nhAWJaFH654KpuCNG6x6iMLtzGO1Ma/VzHnFqoOeSCKHXDhmHwJAjGDTPAgCJQiI
eau6cDNJRiJ7j0/xBwIDAQAB
-----END PUBLIC KEY-----">>.

pkcs1() ->
    <<"-----BEGIN RSA PUBLIC KEY-----
MIIBCgKCAQEA6zD1zpsYJDsV95435y2az9AccqnwBhjRl08DUkjK2YnoMXMgrrwj
RRzS4dyAmKIvgV22CzAZeXn4fRn9bSJHu+bpBqf+CGpOruZ4KUn16xn4pk5UMo/B
A63uTUy1xiNxElsmfce/IvEw0WLO4ARP8mYYzv9E8EsYl4dfuJxCsD7FxTKgzPQF
748ZMsVZDBeIfPiQpdb+thQdWmkzo3BkFohGfJoRTLqwMQwvyw4S/RY+8rbTtmzj
bXsuyw2MX1J6GyA5a957uBy8lSzLCM4yreiOLUdTWSRC60X/+kFFKKuYvjBEqfCO
RFSJwtB8AzKcDMvcAROjLEdW+eTbHeYj0wIDAQAB
-----END RSA PUBLIC KEY-----">>.

ssh_rsa() ->
    <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDDhOF0FdWQmb3AdCUXOHDX72C0M"
      "vJI+Z/R9tYehENaii7BD1qB6+Tqudj/ikAO9b9EKZEhEUpXkN+d8yfSmH6kY8OIkB"
      "4Ikrp4artWc2H+1vWMqZ1sfGNrHC9al71xkBIBsJj38erD1Ef6g7njooDpxM/JIiD2"
      "HKwLj4tTfKhev7vZCgRATxqZtq2oRYMg3pYwQHySkb3Ny+w7Dcwi/Iz3lFgQsqf3iCX"
      "XCbNxfLu5tan/aUu6pL77dKRPv6oiYenW9BczND5A2MZV1KqiAl5BexUW1V2OZSH+KP"
      "bozapP9o9dxHe4cfUqDknl+vd3zY9zUSjUIdXxr85g8QQJDhDP "
      "seth@MacBook-Pro.local">>.

ssh_dsa() ->
    <<"ssh-dss AAAAB3NzaC1kc3MAAACBAJ/HQvJgf7ZnKU4AXHt9imQBHUdXebTjjWKUkR6J2WjMr338T/okUo/8I09C9VRRk4rEtDAlmb+O7Wc7uLGCK0xjF9YWN+M42q0QPVpRXpsDU5QNE4GeHIySZrA2O30oGUmUePzRoRK2ISoeU/6S7WjtcJvwVlVzwA1CR6ElSF/hAAAAFQDo/M8vFmU6kSst5qUsf4KIbWMxAQAAAIAoOkW0esDXTG6UVx+iVMbAr5g13tMb00jloy0IED5TEicsXGl16CGtQ/FAdKbLAwuTfw3Aw6J4HDVH6KQ0Lt+Ok2yvgEVCq55+TiCK+OtP7CPgkCldNSudFW6RmdJnCU+E9c45Duxj/DItboE3OPJMoN0TOVIt75NVnnnWK/IpaAAAAIAXffaP454Dow6RCsl52jtGSGe8Qw9YEVIH0xnd6U2ftZm5uVZIG5JenqLCEKgWr6i5qPrDkziUaR0rKkhIckU3Wg6xt5qA6R4zSP6Ru820atXHDx3tWfB6aVcTwPpP2qs3NA7ge0J4PnusUw+lztbayJv8uju1Of/ZtGf+bnlzcA== seth@MacBook-Pro.local">>.
