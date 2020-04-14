#!/bin/bash

#
# ANSI Escape Codes:
#
# Just so you can be pretty in your messages :smile:
#
# Example:
# => echo -e "I $(red love) $(blue chef) $(yellow friends)!"
#
NC='\033[0m' # No Color
RED='\033[0;91m'
GREEN='\033[0;92m'
YELLOW='\033[0;93m'
BLUE='\033[96m'

function yellow(){
  echo -e "${YELLOW}$1${NC}"
}

function red() {
  echo -e "${RED}$1${NC}"
}

function green() {
  echo -e "${GREEN}$1${NC}"
}

function blue() {
  echo -e "${BLUE}$1${NC}"
}

function warn() {
  echo -e "  $(yellow WARN:) $1"
}

function error() {
  echo -e "  $(red ERROR:) $1"
}

function build_line() {
  echo -e "  $(blue hab-studio:) $1"
}

function log_line() {
  build_line "$@"
}

function log {
  log_line "$@" >&2
}

function log_info {
  log_line "$@" >&2
}

function log_warning {
 warn "$@"
}

function log_error {
 error "$@"
}
