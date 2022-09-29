# frozen_string_literal: true

require "mkmf"

# rubocop:disable Style/GlobalVars
$CFLAGS << " -Wall -O3 -pedantic -std=c99"
# rubocop:enable Style/GlobalVars

create_makefile "ed25519_ref10"
