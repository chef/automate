#!/usr/bin/env ruby
$:.unshift(File.expand_path(File.join(File.dirname(__FILE__), "..", "lib")))

require 'json'
require 'fileutils'
require 'automate_ctl/ctl'

ctl = AutomateCtl::Ctl.new()
ctl.load_files(ENV["OMNIBUS_FILES"])
ctl.run(ARGV)
